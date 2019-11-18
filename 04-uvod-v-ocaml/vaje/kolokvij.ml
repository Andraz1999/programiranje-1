(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

let is_root x y = y = x * x && x >= 0

let pack3 x y z = (x, y, z)

let sum_if_not f list =
    let rec aux f acc = function
        | [] -> acc
        | x :: xs -> if f x then aux f acc xs else aux f (acc + x) xs 
    in 
    aux f 0 list

let apply flist list =
    let list1 = list in
    let rec aux acc1 acc2 flist list = 
        match flist with 
        | [] -> acc1
        | f :: fs -> (match list with
                    | [] -> aux  (acc2 :: acc1) [] fs list1
                    | x :: xs -> aux acc1 ((f x) :: acc2) flist xs)
    in 
    aux [] [] flist list


(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

type vrsta_srecanja = Predavanja | Vaje

type srecanje = {predmet: string; vrsta: vrsta_srecanja; trajanje: int}

type urnik = srecanje list list

let vaje = {predmet = "Analiza 2a"; vrsta = Vaje; trajanje = 2}
let predavanje = {predmet = "Programiranje 1"; vrsta = Predavanja; trajanje = 2}

let urnik_profesor = [[{predmet = "Analiza 2a"; vrsta = Vaje; trajanje = 2}]; []; [{predmet = "Analiza 1"; vrsta = Predavanja; trajanje = 1}]; []; []; [{predmet = "Analiza 2a"; vrsta = Vaje; trajanje = 1}] ]

let je_preobremenjen profesor  =
    let rec aux vaj pre = function
        | [] -> false
        | dan :: ostalo -> match dan with
                            | [] -> aux vaj pre ostalo
                            | {predmet =_; vrsta = vrsta; trajanje = dol} :: druge_ure -> (match vrsta with
                                                | Predavanja -> if pre + dol > 4 then true else aux vaj (pre + dol) (druge_ure :: ostalo)
                                                | Vaje -> if vaj + dol > 4 then true else aux (vaj + dol) pre (druge_ure :: ostalo))
        
    in
    aux 0 0 profesor


let bogastvo profesor =
    let rec aux acc = function
        | [] -> acc
        | dan :: ostalo -> match dan with
                            | [] -> aux acc ostalo
                            | {predmet =_; vrsta = vrsta; trajanje = dol} :: druge_ure -> (match vrsta with
                                                | Predavanja -> aux (acc + 2 * dol) (druge_ure :: ostalo)
                                                | Vaje -> aux (acc + dol) (druge_ure :: ostalo)) 
    in
    aux 0 profesor
