let pi = 3.1415926535

type coordinate =
  {x: float;
   y: float;
  }

type quadrant =
    I
  | II
  | III
  | IV

type line =
    Point of coordinate
  | Line of coordinate * coordinate

let line_of_points p1 p2 =
  if p1 = p2
  then Point p1
  else Line (p1, p2)

let (--) p1 p2 =
  {x = p1.x -. p2.x;
   y = p1.y -. p2.y;
  }

let (++) p1 p2 =
  {x = p1.x +. p2.x;
   y = p1.y +. p2.y;
  }

let scalar s p =
  {x = s *. p.x;
   y = s *. p.y;
  }

let norm v =
  sqrt (v.x *. v.x +. v.y *. v.y)

let distance p1 p2 =
  let v = p1 -- p2 in
  norm v

let distance_to_line line p =
  match line with
    Point  p1      -> distance p1 p
  | Line  (p1, p2) ->
    (abs_float
       ((p2.y -. p1.y) *. p.x -.
        (p2.x -. p1.x) *. p.y +.
        p2.x *. p1.y -.
        p2.y *. p1.x)) /.
    (distance p1 p2)

let angle p1 p2 p3 =
  let v1 = p1 -- p2 in
  let v2 = p3 -- p2 in
  (atan2 v2.y v2.x) -. (atan2 v1.y v1.x)

let quadrant p =
  if p.x >= 0. && p.y >= 0.
  then
    I
  else
  if p.x < 0. && p.y >= 0.
  then
    II
  else
  if p.x <= 0. && p.y < 0.
  then
    III
  else
    IV

let area p1 p2 p3 =
  let a = distance p1 p2 in
  let b = distance p1 p3 in
  let c = distance p2 p3 in
  let s = (a +. b +. c) /. 2. in
  sqrt (s *. (s -. a) *. (s -. b) *. (s -. c))

let is_in_radius r p1 p2 p3 =
  (* find 2 points on mediatrice of larger side
   * look if the distance to the last is lower
   * than the radius
   * *)
  let a = distance p1 p2 in
  let b = distance p2 p3 in
  let c = distance p3 p1 in
  let (p1', p2', p3') =
    if a >= b && a >= c
    then (p1, p2, p3)
    else
    if b >= c && b >= a
    then (p2, p3, p1)
    else (p1, p3, p2)
  in
  let v = (p2' -- p1') in
  let n = {
    x = -. v.y;
    y =    v.x;
  }
  in
  let middle = p1' ++ (scalar 0.5 v) in
  let center1 = middle ++ (scalar (r /. (norm n)) n) in
  let center2 = middle -- (scalar (r /. (norm n)) n) in
  distance center1 p3' <= r ||
  distance center2 p3' <= r
