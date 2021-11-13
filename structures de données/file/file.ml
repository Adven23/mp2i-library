(* création de la file à l'aide de deux listes *)
type 'a file = {l1: 'a list; l2: 'a list} ;;

(* [vide f] vérifie si la file [f] est vide *)
let vide f = f.l1 = [] && f.l2 = [] ;;

(* [ajout e f] permet d'ajouter un élément [e] à la file [f] *)
let ajout e f = {l1 = e::f.l1; l2 = f.l2} ;;

(* [suppr f] permet de supprimer et de renvoyer le premier élément de la file [f] *) 
let rec suppr f = match f.l1 with 
    |e::q -> e, {l1 = q; l2 = f.l2}
    |[] -> suppr ({l1 = List.rev f.l2; l2 = []}) ;;
