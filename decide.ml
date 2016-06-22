module YS = Yojson.Safe

module C = Condition


let get_cmv data =
  `Assoc
  [( "0", `String (string_of_bool (C.condition0  data)));
   ( "1", `String (string_of_bool (C.condition1  data)));
   ( "2", `String (string_of_bool (C.condition2  data)));
   ( "3", `String (string_of_bool (C.condition3  data)));
   ( "4", `String (string_of_bool (C.condition4  data)));
   ( "5", `String (string_of_bool (C.condition5  data)));
   ( "6", `String (string_of_bool (C.condition6  data)));
   ( "7", `String (string_of_bool (C.condition7  data)));
   ( "8", `String (string_of_bool (C.condition8  data)));
   ( "9", `String (string_of_bool (C.condition9  data)));
   ("10", `String (string_of_bool (C.condition10 data)));
   ("11", `String (string_of_bool (C.condition11 data)));
   ("12", `String (string_of_bool (C.condition12 data)));
   ("13", `String (string_of_bool (C.condition13 data)));
   ("14", `String (string_of_bool (C.condition14 data)));
  ]

let get_pum data cmv = `Null 

let get_fuv data cmv pum = `Null 

let decide data = 
  let cmv = get_cmv data in
  let pum = get_pum data cmv in
  let fuv = get_fuv data cmv pum in

  `Assoc
  [("LAUNCH", `String "YES");
   ("CMV", cmv);
   ("PUM", pum);
   ("FUV", fuv)
  ]

  (*first arg is the filename*)
let filename = Sys.argv.(1)

let json = YS.from_file filename

let data = match json with
    `Assoc data' -> data'
  | _            -> failwith "Input is not assoc."

let _ = Printf.eprintf "%s\n%!" (YS.to_string (decide data))
