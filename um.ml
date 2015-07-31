assert (Sys.word_size = 64)

let reg = Array.make 8 0

module Int =
  struct
    type t = int
    let compare = compare
  end

module M = Map.Make (Int)

let program = ref (Array.make 0 0)
let arr = ref M.empty

let alloc_array =
  let last_name = ref 0 in
  fun size ->
    incr last_name;
    arr := M.add !last_name (Array.make size 0) !arr;
    !last_name

let rec run prog pc =
  let x = prog.(pc) in
  let pc = pc+1 in
  let rA = ((x land (0x7 lsl 6)) lsr 6) in
  let rB = ((x land (0x7 lsl 3)) lsr 3) in
  let rC = ((x land (0x7 lsl 0)) lsr 0) in
(*  let y = (x land (0xf lsl 28)) lsr 28 in *)
(*  Printf.printf "n=%d op=%d a=%d b=%d c=%d\n" x y rA rB rC; *)
  match (x land (0xf lsl 28)) lsr 28 with
  | 0 ->
      if reg.(rC) <> 0 then reg.(rA) <- reg.(rB); run prog pc
  | 1 ->
      reg.(rA) <- (M.find reg.(rB) !arr).(reg.(rC)); run prog pc
  | 2 ->
      (M.find reg.(rA) !arr).(reg.(rB)) <- reg.(rC); run prog pc
  | 3 ->
      reg.(rA) <- (reg.(rB) + reg.(rC)) land 0xffffffff; run prog pc
  | 4 ->
      reg.(rA) <- (reg.(rB) * reg.(rC)) land 0xffffffff; run prog pc
  | 5 ->
      reg.(rA) <- ((reg.(rB) land 0xffffffff) / (reg.(rC) land 0xffffffff)) land
      0xffffffff;
      run prog pc
  | 6 ->
      reg.(rA) <- lnot (reg.(rB) land reg.(rC)); run prog pc
  | 7 ->
      ()
  | 8 ->
      reg.(rB) <- alloc_array reg.(rC); run prog pc
  | 9 ->
      arr := M.remove reg.(rC) !arr; run prog pc
  | 10 ->
      print_char (char_of_int reg.(rC)); flush stdout; run prog pc
  | 11 ->
      begin try reg.(rC) <- int_of_char (input_char stdin)
      with End_of_file -> reg.(rC) <- lnot 0x0 end;
      run prog pc
  | 12 ->
      if reg.(rB) <> 0 then begin
        let a = Array.copy (M.find reg.(rB) !arr) in
        arr := M.add 0 a !arr;
        run a reg.(rC)
      end else
        run prog reg.(rC)
  | 13 ->
      let rA = (x land (0x7 lsl 25)) lsr 25 in
      reg.(rA) <- x land 0x1ffffff; run prog pc
  | _ as d ->
      let s = Printf.sprintf "bad opcode %x at %d" d pc in
      failwith s

let read_program inch =
  let load b =
    let a = Array.create ((Buffer.length b) / 4) 0 in
    for i = 0 to (Array.length a)-1 do
      let b1 = int_of_char (Buffer.nth b (4*i)) in
      let b2 = int_of_char (Buffer.nth b (4*i+1)) in
      let b3 = int_of_char (Buffer.nth b (4*i+2)) in
      let b4 = int_of_char (Buffer.nth b (4*i+3)) in
      a.(i) <- (b1 lsl 24) lor (b2 lsl 16) lor (b3 lsl 8) lor b4
    done;
    a
  in
  let b = Buffer.create 101 in
  (try while true do
    Buffer.add_char b (input_char inch)
  done with End_of_file -> ());
  load b

let loadandrun f =
  let inch = open_in f in
  try
    let prog = read_program inch in
    arr := M.add 0 prog M.empty;
    run prog 0
  with e -> (close_in inch; raise e)

let () =
  Arg.parse [] loadandrun "Usage: um <file names ...>"
