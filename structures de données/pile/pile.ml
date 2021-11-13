(* création de la pile *)
type 'a pile = {t: 'a array; mutable n: int} ;;

(* [vide p] vérifie si la pile [p] est vide *)
let vide p = p.n = 0 ;;

(* [ajout e p] permet d'ajouter un élément [e] à une pile [p] *)
let ajout e p = 
    if p.n >= (Array.length p.t) then failwith "Pile pleine"
    else p.t.(p.n) <- e ; p.n <- p.n + 1;;

(* [suppr p] permet de supprimer et de renvoyer le dernier élément de la pile [p] *)
let suppr p = 
    if p.n = 0 then failwith "Pile vide"
    else (p.n <- p.n - 1 ; p.t.(p.n)) ;;
