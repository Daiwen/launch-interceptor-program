module YS = Yojson.Safe
module G  = Geometry

let assoc_of_json = function
    `Assoc assoc -> assoc
  | _            -> failwith "This is not an assoc."

let data_of_json = function
    `Float f -> f
  | `Int i   -> float_of_int i
  | _        -> failwith "This is not an int or a float."

let list_of_json = function
    `List  jsons
  | `Tuple jsons -> jsons
  | _            -> failwith "This is not a list."

let fun_of_string = function
    `String "ANDD"    -> (&&)
  | `String "ORR"     -> (||)
  | `String "NOTUSED" -> (fun _ _ -> true)
  | _ -> failwith "Unknown operator."

let bool_of_json = function
    `String "true"  -> true
  | `String "false" -> false
  | _ -> failwith "Unknown boolean."

let get_parameters data =
  let parameters' = List.assoc "PARAMETERS" data in
  assoc_of_json parameters'


let get_points data =
  let points = list_of_json (List.assoc "points" data) in
  List.map
    (fun json ->
       let point' = list_of_json json in
       let point = List.map data_of_json point' in
       {G.x = List.nth point 0;
        G.y = List.nth point 1}) points


let get_lcm data =
  let columns = assoc_of_json (List.assoc "LCM" data) in
  List.map
    (fun (s, json) ->
       let column = list_of_json json in
       (int_of_string s, List.map fun_of_string column))
    columns


let get_puv data =
  let jsons = list_of_json (List.assoc "PUV" data) in
  List.map bool_of_json jsons 


let json_of_boolean_list l = 
  `List
    (List.map
       (fun b -> `String (string_of_bool b))
       l) 


let json_of_pum pum =
  `Assoc
    (List.map
       (fun (i, column) ->
          (string_of_int i,
           `List (List.map (fun b -> `String (string_of_bool b)) column)))
       pum)
