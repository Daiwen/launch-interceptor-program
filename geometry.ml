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

let distance p1 p2 =
  let nx = p1.x -. p2.x in
  let ny = p1.y -. p2.y in
  sqrt (nx *. nx +. ny *. ny)

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
  let a = distance p1 p2 in
  let b = distance p2 p3 in
  let c = distance p3 p1 in
  let sumed_square = (a*.a +. b*.b +. c*.c) in 
  let sumed_4 = (a*.a*.a*.a +. b*.b*.b*.b +. c*.c*.c*.c) in 
  let area = sqrt (sumed_square*.sumed_square -. 2. *. sumed_4) in
  let cr = a *. b *. c /. 4. /. area in
  r < cr
