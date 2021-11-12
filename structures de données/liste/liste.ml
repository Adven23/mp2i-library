(* [somme l] renvoie la somme des éléments de la liste [l] *) 
let rec somme l = match l with 
    | [] -> 0 
    | e::q -> e + somme q;; 
    
(* [appartient e l] indique si un élément [e] appartient à la liste [l] *) 
let rec appartient e l = match l with 
    |[] -> false 
    |x::q -> x = e || appartient e q ;;
    
(* [taille l] renvoie la taille de la liste [l] *)
let rec taille l = match l with 
    |[] -> 0 
    |e::q -> 1 + taille q ;;
    
(* [maximum l] renvoie le maximum de la liste [l] *)
let rec maximum l = match l with 
    |[] -> min_int 
    |e::q -> max e (maximum q) ;;

(* [minimum l] renvoie le minimum de la liste [l] *)
let rec minimum l = match l with 
    |[] -> max_int 
    |e::q -> min e (minimum q) ;;

(* [separe l] sépare la liste [l] en deux listes de même taille (à 1 près) *)
let rec separe l = match l with 
    |[] -> [], []
    |[e] -> [e], []
    |e1::e2::q -> let q1, q2 = separe q in 
                  e1::q1, e2::q2 ;;

(* [concat l1 l2] concatène les deux listes [l1] et [l2] *)
let rec concat l1 l2 = match l1 with 
    |[] -> l2
    |e::q -> e::concat q l2;;
    
(* [egal l1 l2] indique si les deux listes [l1] et [l2] sont égales *) 
let rec egal l1 l2 = match l1, l2 with 
    |[], [] -> true 
    |e::q, [] -> false 
    |[], e::q -> false
    |e1::q1, e2::q2 -> e1 = e2 && egal q1 q2 ;;
    
(* [inverse acc l] inverse les éléments d'une liste [l], à l'aide d'un accumulateur*)
let rec inverse acc l = match l with 
    |[] -> acc
    |e::q -> inverse (e::acc) q ;;
