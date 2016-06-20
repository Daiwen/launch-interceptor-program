module J = Json_operations
module G = Geometry

let get_first =
  Queue.peek

let get_last q' =
  let q = Queue.copy q' in
  Queue.fold
    (fun _ p -> p)
    (Queue.pop q) q


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
  let length1 = J.data_of_json (List.assoc "LENGTH1" parameters) in
  test_points points 2
  (fun q ->
    let p1 = Queue.pop q in
    let p2 = Queue.pop q in
    (G.distance p1 p2) > length1)


let condion1 data =
  let parameters = J.get_parameters data in
  let points = J.get_points data in
  let radius1 = J.data_of_json (List.assoc "RADIUS1" parameters) in
  test_points points 3
  (fun q ->
    let p1 = Queue.pop q in
    let p2 = Queue.pop q in
    let p3 = Queue.pop q in
    not (G.is_in_radius radius1 p1 p2 p3))


let condion2 data =
  let parameters = J.get_parameters data in
  let points = J.get_points data in
  let epsilon = J.data_of_json (List.assoc "EPSILON" parameters) in
  test_points points 3
  (fun q ->
    let p1 = Queue.pop q in
    let p2 = Queue.pop q in
    let p3 = Queue.pop q in
    not (p1 = p2) &&
    not (p3 = p2) &&
    (G.angle p1 p2 p3 < G.pi -. epsilon ||
     G.angle p1 p2 p3 > G.pi +. epsilon))


let condion3 data =
  let parameters = J.get_parameters data in
  let points = J.get_points data in
  let area1 = J.data_of_json (List.assoc "AREA1" parameters) in
  test_points points 3
  (fun q ->
    let p1 = Queue.pop q in
    let p2 = Queue.pop q in
    let p3 = Queue.pop q in
    G.area p1 p2 p3 > area1)


let condion4 data =
  let parameters = J.get_parameters data in
  let points = J.get_points data in
  let quads =
    int_of_float (J.data_of_json (List.assoc "QUADS" parameters))
  in
  let q_pts =
    int_of_float (J.data_of_json (List.assoc "Q_PTS" parameters))
  in
  test_points points q_pts
  (fun q ->
    let qs =
      Queue.fold
        (fun quads p ->
           let quad = G.quadrant p in
           if List.mem quad quads
           then quads
           else quad::quads)
        [] q
    in
    List.length qs > quads)


let condion5 data =
  let points = J.get_points data in
  test_points points 2
  (fun q ->
    let p1 = Queue.pop q in
    let p2 = Queue.pop q in
    p2.G.x -. p1.G.x < 0.)


let condion6 data =
  let parameters = J.get_parameters data in
  let points = J.get_points data in
  let n_pts =
    int_of_float (J.data_of_json (List.assoc "N_PTS" parameters))
  in
  let dist =
    J.data_of_json (List.assoc "DIST" parameters)
  in
  test_points points n_pts
  (fun q ->
    let first = get_first q in
    let last  = get_last  q in
    let line  = G.line_of_points first last in
    Queue.fold
      (fun acc p ->
         acc || G.distance_to_line line p > dist) false q)


let condion7 data = false
let condion8 data = false
let condion9 data = false
let condion10 data = false
let condion11 data = false
let condion12 data = false
let condion13 data = false
let condion14 data = false


