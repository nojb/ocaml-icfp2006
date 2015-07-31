let checkpass username pass =
  let cmd = "expect -c \"spawn ../um.native ../mystery.um; " in
  let cmd = cmd ^ "set timeout 120; " in
  let cmd = cmd ^ "expect \"login:\"; " in
  let cmd = cmd ^ "send \"" ^ username ^ "\\\\n\"; " in
  let cmd = cmd ^ "expect \"password:\"; " in
  let cmd = cmd ^ "send \"" ^ pass ^ "\\\\n\"; " in
  let cmd = cmd ^ "expect \\\"ACCESS DENIED\\\" { exit 2 } " in
  let cmd = cmd ^ "\\\"logged in as\\\" { exit 3 }\"" in
  let r = Sys.command cmd in
  if r = 2 then false
  else if r = 3 then true
  else failwith "internal error"

let tryword username w =
  Printf.printf "trying %s for %s\n%!" w username;
  if checkpass username w then begin
    Printf.printf "found match!! for user %s\npassword: %s\n%!" username w;
    raise Exit
  end else
    Printf.printf "%s no use\n%!" w

let () =
  let user = ref "" in
  let pass = ref "" in
  Arg.parse ["-user", Arg.Set_string user, "username"; "-pass", Arg.Set_string pass,
  "password"] (fun _ -> ()) "Usage: ./hackuser.exe username password";
  tryword !user !pass
