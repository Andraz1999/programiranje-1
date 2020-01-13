let razlika_kvadratov x y =
  let kvadrat_vsote = (x + y) * (x + y) in
  let vsota_kvadratov = x*x + y*y in
  kvadrat_vsote - vsota_kvadratov

let uporabi_na_paru f (a, b) =
  f a, f b

let rec ponovi_seznam n sez =
  if n <= 0 then []
  else 
  if n = 1 then sez
  else
  sez @ ponovi_seznam (n - 1) sez

let razdeli sez =
  let rec aux accn accp = function
    | [] -> accn, accp
    | x :: sez' -> if x < 0 then aux (x :: accn) accp sez' else aux accn (x :: accp) sez'
    in 
    aux [] [] sez

type 'a drevo = 
  | Node of ('a drevo * 'a * 'a drevo)
  | Empty

let rec daljsi_seznam sez1 sez2 =
  match sez1, sez2 with
  | [], [] -> false
  | _ :: xs, [] -> true
  | [], _ :: xs -> false
  | _ :: xs, _ :: ys -> daljsi_seznam xs ys   


(* let max_seznam sez1 sez2 =
  if List.length sez1 > List.length sez2 then sez1
  else sez2

let rec padajoca ma = function
  | Empty -> []
  | Node (l, x, r) -> if x >= ma then []
                      else x :: (max_seznam (padajoca x l) (padajoca x r))

let rec narascajoca ma = function
  | Empty -> []
  | Node (l, x, r) -> if x <= ma then []
                      else x :: (max_seznam (narascajoca x l) (narascajoca x r))

let rec longest_monotone drevo = function
    | Empty -> []
    | Node (l, x, r) -> 
      let left = longest_monotone l in
      let right = longest_monotone r in
      max_seznam left right        *)
    
    
type 'a veriga =
| Filter of ('a -> bool) * 'a list * 'a veriga
| Ostalo of 'a list

let test1 = Ostalo([])
let test2 = Filter ((>)10, [], test1)
let test = Filter ((>)0, [], test2)

let test' = Filter ((>)0, [], Filter ((>)10, [], Ostalo ([])))  

let rec vstavi a = function
  | Ostalo (sez) -> Ostalo (List.rev(a :: sez))
  | Filter (f, sez, filter) -> if f a then Filter (f, List.rev(a :: sez), filter)
                               else Filter (f, sez, vstavi a filter)

let test' = vstavi (-5) test'
let test' = vstavi 7 test'
let test' = vstavi 100 test'
let test' = vstavi (-7) test'
let test' = vstavi 2 test'

let rec v_seznamu a = function
  | [] -> false
  | x :: xs -> if x = a then true else v_seznamu a xs

let rec poisci a = function
  | Ostalo (sez) -> v_seznamu a sez
  | Filter (f, sez, filter) -> if f a then v_seznamu a sez else poisci a filter  

let rec izprazni_filtre veriga =
  match veriga with
  | Ostalo (sez) -> Ostalo([]), sez
  | Filter (f, sez, filter) -> let (veriga', sez') = izprazni_filtre filter in Filter (f, [], veriga'), sez @ sez' 