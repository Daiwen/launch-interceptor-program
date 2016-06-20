module YS = Yojson.Safe

module C = Condition


let get_cmv data =
  `Assoc
  [( "0", `String (string_of_bool (C.condion0  data)));
   ( "1", `String (string_of_bool (C.condion1  data)));
   ( "2", `String (string_of_bool (C.condion2  data)));
   ( "3", `String (string_of_bool (C.condion3  data)));
   ( "4", `String (string_of_bool (C.condion4  data)));
   ( "5", `String (string_of_bool (C.condion5  data)));
   ( "6", `String (string_of_bool (C.condion6  data)));
   ( "7", `String (string_of_bool (C.condion7  data)));
   ( "8", `String (string_of_bool (C.condion8  data)));
   ( "9", `String (string_of_bool (C.condion9  data)));
   ("10", `String (string_of_bool (C.condion10 data)));
   ("11", `String (string_of_bool (C.condion11 data)));
   ("12", `String (string_of_bool (C.condion12 data)));
   ("13", `String (string_of_bool (C.condion13 data)));
   ("14", `String (string_of_bool (C.condion14 data)));
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
