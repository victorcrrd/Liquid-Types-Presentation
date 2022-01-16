(* Uncomment to prove safety *)
(*
qualif POS(v): 0 <= v 
qualif LT(v): ~A <= v
qualif GT(v):  v < ~A
qualif BND(v): v < Array.length ~A 
*)

let show x = ()

(* max:: x:int -> y:int -> {v:int | (x <= v) && (y <= v)}*)
let max x y =
  if x > y then x else y

(* sum:: k:int ->{v:int | 0 <= v && k <= v}*)
let rec sum k =
  if k < 0 then 0 else
     let s = sum (k-1) in
     s + k

let _ = show sum

let _ = Array.get

let rec range i j = 
  if i >= j then [] else i::(range (i+1) j)

let rec fold_left f b xs =
  match xs with
  | []     -> b
  | x::xs' -> fold_left f (f b x) xs'

let arraymax a =
  let am = fun m i -> max (Array.get a i) m in
  let is = range 0 (Array.length a) in 
  fold_left am 0 is 

let arraytest a =
  let vec = Array.make (Random.int 40)  0 in
  arraymax vec
