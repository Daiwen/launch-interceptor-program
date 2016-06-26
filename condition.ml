module J = Json_operations
module G = Geometry

let pop_n q =
  let rec aux =
    function
      0 -> ()
    | n ->
      let _ = Queue.pop q in
      aux (n - 1)
  in
  aux


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


let test_length op points offset length =
  test_points points (2 + offset)
    (fun q ->
       let p1 = Queue.pop q in
       pop_n q offset;
       let p2 = Queue.pop q in
       op (G.distance p1 p2) length)


let test_circle f points offset1 offset2 radius =
  test_points points (3 + offset1 + offset2)
    (fun q ->
       let p1 = Queue.pop q in
       pop_n q offset1;
       let p2 = Queue.pop q in
       pop_n q offset2;
       let p3 = Queue.pop q in
       f (not (G.is_in_radius radius p1 p2 p3)))


let test_angle points offset1 offset2 epsilon =
  test_points points (3 + offset1 + offset2)
    (fun q ->
       let p1 = Queue.pop q in
       pop_n q offset1;
       let p2 = Queue.pop q in
       pop_n q offset2;
       let p3 = Queue.pop q in
       not (p1 = p2) &&
       not (p3 = p2) &&
       (G.angle p1 p2 p3 < G.pi -. epsilon ||
        G.angle p1 p2 p3 > G.pi +. epsilon))


let test_area op points offset1 offset2 area =
  test_points points (3 + offset1 + offset2)
    (fun q ->
       let p1 = Queue.pop q in
       pop_n q offset1;
       let p2 = Queue.pop q in
       pop_n q offset2;
       let p3 = Queue.pop q in
       op (G.area p1 p2 p3) area)


let test_sub points offset =
  test_points points (2 + offset)
    (fun q ->
       let p1 = Queue.pop q in
       pop_n q offset;
       let p2 = Queue.pop q in
       p2.G.x -. p1.G.x < 0.)


let condition0 data =
  let parameters = J.get_parameters data in
  let points = J.get_points data in
  let length1 = J.float_of_json (List.assoc "LENGTH1" parameters) in
  test_length (>) points 0 length1

let condition1 data =
  let parameters = J.get_parameters data in
  let points = J.get_points data in
  let radius1 = J.float_of_json (List.assoc "RADIUS1" parameters) in
  test_circle (fun x -> x) points 0 0 radius1

let condition2 data =
  let parameters = J.get_parameters data in
  let points = J.get_points data in
  let epsilon = J.float_of_json (List.assoc "EPSILON" parameters) in
  test_angle points 0 0 epsilon

let condition3 data =
  let parameters = J.get_parameters data in
  let points = J.get_points data in
  let area1 = J.float_of_json (List.assoc "AREA1" parameters) in
  test_area (>) points 0 0 area1

let condition4 data =
  let parameters = J.get_parameters data in
  let points = J.get_points data in
  let quads = J.int_of_json (List.assoc "QUADS" parameters) in
  let q_pts = J.int_of_json (List.assoc "Q_PTS" parameters) in
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


let condition5 data =
  let points = J.get_points data in
  test_sub points 0

let condition6 data =
  let parameters = J.get_parameters data in
  let points = J.get_points data in
  let n_pts = J.int_of_json (List.assoc "N_PTS" parameters) in
  let dist = J.float_of_json (List.assoc "DIST" parameters) in
  test_points points n_pts
    (fun q ->
       let first = Queue.pop q in
       pop_n q (n_pts - 2);
       let last  = Queue.pop q in
       let line  = G.line_of_points first last in
       Queue.fold
         (fun acc p ->
            acc || G.distance_to_line line p > dist) false q)


let condition7 data =
  let parameters = J.get_parameters data in
  let points = J.get_points data in
  let k_pts = J.int_of_json (List.assoc "K_PTS" parameters) in
  let length1 = J.float_of_json (List.assoc "LENGTH1" parameters) in
  test_length (>) points k_pts length1


let condition8 data =
  let parameters = J.get_parameters data in
  let points = J.get_points data in
  let a_pts = J.int_of_json (List.assoc "A_PTS" parameters) in
  let b_pts = J.int_of_json (List.assoc "B_PTS" parameters) in
  let radius1 = J.float_of_json (List.assoc "RADIUS1" parameters) in
  test_circle (fun x -> x) points a_pts b_pts radius1


let condition9 data =
  let parameters = J.get_parameters data in
  let points = J.get_points data in
  let c_pts = J.int_of_json (List.assoc "C_PTS" parameters) in
  let d_pts = J.int_of_json (List.assoc "D_PTS" parameters) in
  let epsilon = J.float_of_json (List.assoc "EPSILON" parameters) in
  test_angle points c_pts d_pts epsilon


let condition10 data =
  let parameters = J.get_parameters data in
  let points = J.get_points data in
  let e_pts = J.int_of_json (List.assoc "E_PTS" parameters) in
  let f_pts = J.int_of_json (List.assoc "F_PTS" parameters) in
  let area1 = J.float_of_json (List.assoc "AREA1" parameters) in
  test_area (>) points e_pts f_pts area1


let condition11 data =
  let parameters = J.get_parameters data in
  let points = J.get_points data in
  let g_pts = J.int_of_json (List.assoc "G_PTS" parameters) in
  test_sub points g_pts


let condition12 data =
  let parameters = J.get_parameters data in
  let points = J.get_points data in
  let k_pts = J.int_of_json (List.assoc "K_PTS" parameters) in
  let length1 = J.float_of_json (List.assoc "LENGTH1" parameters) in
  let length2 = J.float_of_json (List.assoc "LENGTH2" parameters) in
  test_length (>) points k_pts length1 &&
  test_length (<) points k_pts length2


let condition13 data =
  let parameters = J.get_parameters data in
  let points = J.get_points data in
  let a_pts = J.int_of_json (List.assoc "A_PTS" parameters) in
  let b_pts = J.int_of_json (List.assoc "B_PTS" parameters) in
  let radius1 = J.float_of_json (List.assoc "RADIUS1" parameters) in
  let radius2 = J.float_of_json (List.assoc "RADIUS2" parameters) in
  test_circle (fun x -> x)     points a_pts b_pts radius1 &&
  test_circle (fun x -> not x) points a_pts b_pts radius2


let condition14 data =
  let parameters = J.get_parameters data in
  let points = J.get_points data in
  let e_pts = J.int_of_json (List.assoc "E_PTS" parameters) in
  let f_pts = J.int_of_json (List.assoc "F_PTS" parameters) in
  let area1 = J.float_of_json (List.assoc "AREA1" parameters) in
  let area2 = J.float_of_json (List.assoc "AREA2" parameters) in
  test_area (>) points e_pts f_pts area1 &&
  test_area (<) points e_pts f_pts area2
