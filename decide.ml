module YS = Yojson.Safe
module J = Json_operations
module C = Condition

let get_cmv data =
  [C.condition0  data;
   C.condition1  data;
   C.condition2  data;
   C.condition3  data;
   C.condition4  data;
   C.condition5  data;
   C.condition6  data;
   C.condition7  data;
   C.condition8  data;
   C.condition9  data;
   C.condition10 data;
   C.condition11 data;
   C.condition12 data;
   C.condition13 data;
   C.condition14 data;
  ]

let get_pum data cmv =
  let lcm = J.get_lcm data in
  List.map 
    (fun (i, column) ->
       let c1 = List.nth cmv i in
       (i,
        List.map2 
          (fun op c2 -> op c1 c2)
          column
          cmv))
    lcm

let get_fuv data cmv pum = `Null 

let decide data = 
  let cmv = get_cmv data in
  let pum = get_pum data cmv in
  let fuv = get_fuv data cmv pum in

  `Assoc
  [("LAUNCH", `String "YES");
   ("CMV", J.json_of_boolean_list cmv);
   ("PUM", J.json_of_pum pum);
   ("FUV", fuv)
  ]

  (*first arg is the filename*)
let filename = Sys.argv.(1)

let json = YS.from_file filename

let data = match json with
    `Assoc data' -> data'
  | _            -> failwith "Input is not assoc."

let _ = Printf.eprintf "%s\n%!" (YS.to_string (decide data))
