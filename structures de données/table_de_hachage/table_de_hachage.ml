(* création de la table de hachage *)
type ('k, 'v) hashtable = {t: ('k, 'v) option array; h: 'k -> int} ;;

(* [ajout ht (k, v)] permet d'ajouter un couple clé-élément à une table de hachage [ht] *)
let ajout ht (k, v) = 
    ht.t.(ht.h k) <- Some v ;;

(* [valeur ht k] renvoie la valeur associée à la clé [k] dans la table de hachage [ht] *)
let valeur ht k = 
    ht.t.(ht.h k) ;;
    
(* [suppr ht k] permet de supprimer la valeur associée à la clé [k] dans une table de hachage [ht] *)
let suppr ht k = 
    ht.t.(ht.h k) <- None ;;
