(* Algorithme de recherche d'un élément [e] dans un tableau trié [t] par dichotomie *)
let dichotomie t e = 
    let rec aux i j = 
        if i > j then false
        else let m = (i+j)/2 in 
        if t.(m) = e then true 
        else if t.(m) < e then aux (m+1) j 
        else aux i (m-1) 
    in aux 0 (Array.length t - 1) ;;
