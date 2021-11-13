(* création du tableau dynamique *)
type 'a t_dyn = {mutable t: 'a array; mutable n: int} ;;

(* fonction auxiliaire permettant de copier le tableau [t1] dans le tableau [t2] *)
let copie t1 t2 = 
    for i=0 to Array.length t1 - 1 do 
        t2.(i) <- t1.(i) 
    done;;

(* [ajout e d] permet d'ajouter un élément [e] dans le tableau dynamique [d] *)
let ajout e d = 
    d.n <- d.n + 1 ;
    if d.n < Array.length d.t then d.t.(d.n) <- e
    else if d.n = 0 then d.t <- [|e|] 
    else let t' = Array.make (2*d.n) 0 in 
    (copie d.t t';
    d.t <- t';
    d.t.(d.n) <- e)
