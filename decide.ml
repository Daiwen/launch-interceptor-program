module YS = Yojson.Safe

module G = Geometry
module J = Json_operations

let test_points points nb f =
  let q = Queue.create () in
  List.fold_left
  (fun acc p ->
    Queue.push p q;
    if Queue.length q < nb
    then acc
    else
      let result = f (Queue.copy q) in
      let _ = Queue.pop q in
      acc || result)
  false points


let condion0 data =
  let parameters = J.get_parameters data in
  let points = J.get_points data in
  let length1' = List.assoc "LENGTH1" parameters in
  let length1 = J.data_of_json length1' in
  test_points points 2
  (fun q ->
    let p1 = Queue.pop q in
    let p2 = Queue.pop q in
    (G.distance p1 p2) > length1)


let condion1 data =
  let parameters = J.get_parameters data in
  let points = J.get_points data in
  let radius1' = List.assoc "RADIUS1" parameters in
  let radius1 = J.data_of_json radius1' in
  test_points points 3
  (fun q ->
    let p1 = Queue.pop q in
    let p2 = Queue.pop q in
    let p3 = Queue.pop q in
    not (G.is_in_radius radius1 p1 p2 p3))


let condion2 data =
  let parameters = J.get_parameters data in
  let points = J.get_points data in
  let epsilon' = List.assoc "EPSILON" parameters in
  let epsilon = J.data_of_json epsilon' in
  test_points points 3
  (fun q ->
    let p1 = Queue.pop q in
    let p2 = Queue.pop q in
    let p3 = Queue.pop q in
    not (p1 = p2) &&
    not (p3 = p2) &&
    (G.angle p1 p2 p3 < G.pi -. epsilon ||
     G.angle p1 p2 p3 > G.pi +. epsilon))



let condion2 data = false
let condion3 data = false
let condion4 data = false
let condion5 data = false
let condion6 data = false
let condion7 data = false
let condion8 data = false
let condion9 data = false
let condion10 data = false
let condion11 data = false
let condion12 data = false
let condion13 data = false
let condion14 data = false

let get_cmv data =
  `Assoc
  [( "0", `String (string_of_bool (condion0  data)));
   ( "1", `String (string_of_bool (condion1  data)));
   ( "2", `String (string_of_bool (condion2  data)));
   ( "3", `String (string_of_bool (condion3  data)));
   ( "4", `String (string_of_bool (condion4  data)));
   ( "5", `String (string_of_bool (condion5  data)));
   ( "6", `String (string_of_bool (condion6  data)));
   ( "7", `String (string_of_bool (condion7  data)));
   ( "8", `String (string_of_bool (condion8  data)));
   ( "9", `String (string_of_bool (condion9  data)));
   ("10", `String (string_of_bool (condion10 data)));
   ("11", `String (string_of_bool (condion11 data)));
   ("12", `String (string_of_bool (condion12 data)));
   ("13", `String (string_of_bool (condion13 data)));
   ("14", `String (string_of_bool (condion14 data)));
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
