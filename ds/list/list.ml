(* [somme l] renvoie la somme des éléments de la liste [l] *) 
let rec somme l = match l with 
    | [] -> 0 
    | e::q -> e + somme q;; 
    
(* [appartient e l] renvoie true si l'élément [e] appartient à la liste [l], false sinon *) 
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
