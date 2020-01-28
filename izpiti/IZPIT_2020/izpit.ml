let option_sum a b =
  match a, b with
  | None, _ | _, None -> None
  | Some x, Some y -> Some (x + y)


let twostep_map bi f g x =
  let (a, b) = bi x in
    (f a, g b)

(* let make_list m x =
  let rec aux acc m x = 
  if m <= 0 then List.rev acc else aux (x :: acc) (m - 1) x
  in
  aux [] m x *)

let function_repeat f sez=
  let rec aux acc_ful f = function
    | [] -> List.rev acc_ful
    | x :: xs -> let make_list m x =
                    let rec aux acc m x = 

                    if m <= 0 then acc else aux (x :: acc) (m - 1) x
                    in
                    aux acc_ful m x
                    in
                aux (make_list (f x) x) f xs 
    in
    aux [] f sez

(* Funkcija je repno rekurzivna, saj shranjuje trenutno vrednost v akumolatorju, torej ne kliče prejšnjih funkcij.  *)


let rec iterate f stop a = 
  if stop a then a
  else iterate f stop (f a)


(* 2. naloga *)


type 'a improved_list =
  | Prazen
  | Sez of ('a array * 'a improved_list)

let test = Sez ([|1;2;20|], Sez ([|17;19;20;30|], Sez ([|100|], Prazen)))

let count list =
  let rec aux acc = function
    | Prazen -> acc
    | Sez (array, ostalo) -> aux (acc + Array.length array) ostalo
  in
  aux 0 list


let rec nth n list =
  if n < 0 then None else
  match list with
  | Prazen -> None
  | Sez (array, ostalo) -> if n < Array.length array then Some array.(n)
                           else nth (n - Array.length array) ostalo

let array_sort list prvi_min =
    if list = [||] then true else
    (* let prvi_min = list.(0) in  *)
    let rec aux lok min = function
    | [||] -> true 
    | list -> 
              if lok >= Array.length list then true
              else if min > list.(lok) then false 
              else aux (lok + 1) list.(lok) list
    in aux 0 prvi_min list

let rec prazen = function
  | Prazen -> true
  | Sez ([||], ostalo) -> prazen ostalo
  | Sez (_, _) -> false

let is_sorted list =
  if prazen list then true else 
  let rec doloci_prvi = function
  | Prazen -> failwith "Ni mogoce"
  | Sez ([||], ostalo) -> doloci_prvi ostalo
  | Sez (array, ostalo) -> array.(0)
  in
  let prvi_min = doloci_prvi list in
  let rec aux min = function
  | Prazen -> true
  | Sez ([||], ostalo) -> aux min ostalo
  | Sez (array, ostalo) -> if array_sort array min then aux array.((Array.length array) - 1) ostalo
                        else false
  in aux prvi_min list


let update list n a =
  let rec aux n a = function
  | Prazen -> Prazen
  | Sez ([||], ostalo) -> Sez ([||], aux n a ostalo)
  | Sez (array, ostalo) when n < Array.length array  -> let array' = Array.copy array in array'.(n) <- a; Sez (array', ostalo) 
  | Sez (array, ostalo) -> let n' = n - Array.length array in
                            Sez (array, aux n' a ostalo)
  in
  aux n a list