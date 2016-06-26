module YS = Yojson.Safe
module G  = Geometry

let assoc_of_json = function
    `Assoc assoc -> assoc
  | _            -> failwith "This is not an assoc."

let float_of_json = function
    `Float f -> f
  | _        -> failwith "This is not a float."

let int_of_json = function
    `Int i -> i
  | _      -> failwith "This is not an int."

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
    `String s -> bool_of_string s
  | `Bool b   -> b
  | y -> failwith "Unknown boolean."

let get_parameters data =
  let parameters' = List.assoc "PARAMETERS" data in
  assoc_of_json parameters'


let check_data_constraints data =
  let parameters = get_parameters data in
  let numpoints = int_of_json (List.assoc "NUMPOINTS" data) in
  let epsilon = float_of_json (List.assoc "EPSILON" parameters) in
  let area1 = float_of_json (List.assoc "AREA1" parameters) in
  let area2 = float_of_json (List.assoc "AREA2" parameters) in
  let quads = int_of_json (List.assoc "QUADS" parameters) in
  let dist = float_of_json (List.assoc "DIST" parameters) in
  let a_pts = int_of_json (List.assoc "A_PTS" parameters) in
  let b_pts = int_of_json (List.assoc "B_PTS" parameters) in
  let c_pts = int_of_json (List.assoc "C_PTS" parameters) in
  let d_pts = int_of_json (List.assoc "D_PTS" parameters) in
  let e_pts = int_of_json (List.assoc "E_PTS" parameters) in
  let f_pts = int_of_json (List.assoc "F_PTS" parameters) in
  let g_pts = int_of_json (List.assoc "G_PTS" parameters) in
  let k_pts = int_of_json (List.assoc "K_PTS" parameters) in
  let n_pts = int_of_json (List.assoc "N_PTS" parameters) in
  let q_pts = int_of_json (List.assoc "Q_PTS" parameters) in
  let length1 = float_of_json (List.assoc "LENGTH1" parameters) in
  let length2 = float_of_json (List.assoc "LENGTH2" parameters) in
  let radius1 = float_of_json (List.assoc "RADIUS1" parameters) in
  let radius2 = float_of_json (List.assoc "RADIUS2" parameters) in
  0. <= length1 && 0. <= radius1 && 0. <= epsilon && epsilon < G.pi &&
  0. <= area1 && 2 <= q_pts && q_pts <= numpoints && 1 <= quads &&
  quads <= 3 && 3 <= n_pts && n_pts <= numpoints && 0. <= dist &&
  1 <= k_pts && k_pts <= (numpoints - 2) && 1 <= a_pts && 1 <= b_pts &&
  (a_pts + b_pts) <= (numpoints - 3) && 1 <= c_pts && 1 <= d_pts &&
  (c_pts + d_pts) <= (numpoints - 3) && 1 <= e_pts && 1 <= f_pts &&
  (e_pts + f_pts) <= (numpoints - 3) && 1 <= g_pts &&
  g_pts <= (numpoints - 2) && 0. <= length2 && 0. <= radius2 &&
  0. <= area2


let get_points data =
  let points = list_of_json (List.assoc "points" data) in
  List.map
    (fun json ->
       let point' = list_of_json json in
       let point = List.map float_of_json point' in
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
