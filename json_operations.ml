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

let get_parameters data =
  let parameters' = List.assoc "PARAMETERS" data in
  assoc_of_json parameters'


let get_points data =
  let points' = List.assoc "points" data in
  let points = list_of_json points' in
  List.map
  (fun json ->
    let point' = list_of_json json in
    let point = List.map data_of_json point' in
    {G.x = List.nth point 0;
     G.y = List.nth point 1}) points


