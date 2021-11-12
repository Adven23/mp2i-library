(* [somme l] renvoie la somme des éléments de [l] *) 
let rec somme l = match l with 
    | [] -> 0 
    | e::q -> e + somme q;; 
    
(* [appartient e l] renvoie true si l'élément [e] appartient à la liste [l], false sinon *) 
let rec appartient e l = match l with 
    |[] -> false 
    |x::q -> x=q || appartient e q ;;
