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

let get_fuv data cmv pum =
  let puv = J.get_puv data in
  let sorted_pum = List.sort
      (fun (i1, _) (i2, _) -> i1 - i2)
      pum
  in
  List.map2
    (fun is_relevant (_, column) ->
       not (is_relevant) || not (List.mem false column))
    puv
    sorted_pum


let decide data = 
  let cmv = get_cmv data in
  let pum = get_pum data cmv in
  let fuv = get_fuv data cmv pum in
  let s =
    if not (List.mem false fuv)
    then "YES"
    else "NO"
  in

  `Assoc
    [("LAUNCH", `String s);
     ("CMV", J.json_of_boolean_list cmv);
     ("PUM", J.json_of_pum pum);
     ("FUV", J.json_of_boolean_list fuv)
    ]

(*first arg is the filename*)
let filenames = Array.sub Sys.argv 1 ((Array.length Sys.argv) - 1)

let jsons =
  Array.map (fun n -> (n, YS.from_file n)) filenames

let data =
  Array.fold_left
    (fun acc (n, json) ->
       match json with
         `Assoc data' ->
         if J.check_data_constraints data'
         then
           (n, data')::acc
         else
           acc
       | _ -> acc)
    [] jsons

let _ =
  List.iter
    (fun (_, data') ->
       Printf.printf "%s\n%!" (YS.to_string (decide data')))
    data
