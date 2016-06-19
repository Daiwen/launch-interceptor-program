let pi = 3.1415926535

type coordinate =
  {x: float;
   y: float;
  }

let sub p1 p2 =
  {x = p2.x -. p1.x;
   y = p2.y -. p1.y;
  }

let distance p1 p2 =
  let nx = p1.x -. p2.x in
  let ny = p1.y -. p2.y in
  nx *. nx +. ny *. ny

let angle p1 p2 p3 =
  0.

let is_in_radius r p1 p2 p3 =
  let a = distance p1 p2 in
  let b = distance p2 p3 in
  let c = distance p3 p1 in
  let sumed_square = (a*.a +. b*.b +. c*.c) in 
  let sumed_4 = (a*.a*.a*.a +. b*.b*.b*.b +. c*.c*.c*.c) in 
  let area = sqrt (sumed_square*.sumed_square -. 2. *. sumed_4) in
  let cr = a *. b *. c /. 4. /. area in
  r < cr
