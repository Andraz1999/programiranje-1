(* -------- 1 -------- *)

let vsota =
    let rec vsota' acc = function
        | [] -> acc
        | x :: xs -> vsota' (x + acc) xs
    in vsota' 0


(* -------- 2 -------- *)
let rec urejen = function
    | [] -> true
    | [x] -> true
    | x :: (y :: xs) -> if x > y then false
    else urejen (y :: xs)

(* -------- 3 -------- *)
let rec vstavi list n =
    match list with
    | [] -> [n]
    | y :: ys -> if n <= y then n :: list else y :: vstavi ys n   

let rec dva_urejena list1 list2 =
    match list2 with
    | [] -> list1
    | x :: xs -> dva_urejena (vstavi list1 x) xs

(* -------- 4 -------- *)

let rec vstavi' cmp list n =
    match list with
    | [] -> [n]
    | y :: ys -> if cmp n y then n :: list else y :: vstavi' cmp ys n 

let rec sortiri_po cmp list = 
    let rec sort' cmp acc = function
    | [] -> acc
    | x :: xs -> sort' cmp (vstavi' cmp acc x) xs
    in sort' cmp [] list

(* -------- 5 -------- *)
type prio =
| Top
| Group of int

type status =
| Staff
| Passenger of prio

type flyer = { status : status ; name : string }

let flyers = [ {status = Staff; name = "Quinn"}
             ; {status = Passenger (Group 0); name = "Xiao"}
             ; {status = Passenger Top; name = "Jaina"}
             ; {status = Passenger (Group 1000); name = "Aleks"}
             ; {status = Passenger (Group 1000); name = "Robin"}
             ; {status = Staff; name = "Alan"}
             ]

let quinn = {status = Staff; name = "Quinn"}
let aleks = {status = Passenger (Group 1000); name = "Aleks"}
let bine = {status = Passenger (Group 2000); name = "Robin"}
(* -------- 6 -------- *)
let pomemben x y =
    match x.status, y.status with
    | Staff, _ -> true
    | _, Staff -> false
    | Passenger Top, _ -> true
    | Passenger _, Passenger Top -> false
    | Passenger (Group a), Passenger (Group b) -> a > b


let sortiri_potnike seznam_potnikov = sortiri_po pomemben seznam_potnikov

(* -------- 7 -------- *)

let razdeli_potnike seznam =
    let potniki = sortiri_potnike seznam in
    let rec aux bloki blok stat = function
    | [] -> blok :: bloki
    | x :: xs -> if x.status = stat then aux bloki (x :: blok) stat xs
                else aux (blok :: bloki) [x] x.status xs
    in
    match potniki with
    | [] -> [[]]
    | y :: ys -> let koncno = aux [] [y] y.status ys in
    List.rev koncno

 