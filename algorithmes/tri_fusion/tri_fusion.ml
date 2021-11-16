(* [split l] permet de séparer une liste [l] en deux listes de même taille (à 1 près) *)
let rec split l = match l with 
    |[] -> [], []
    |[e] -> [e], [] 
    |e1::e2::q -> let q1, q2 = split q in 
                  e1::q1, e2::q2 ;;

(* [fusion l1 l2] permet de fusionner deux listes [l1] et [l2] *) 
let rec fusion l1 l2 = match l1, l2 with 
    |[], _ -> l2 
    |_, [] -> l1 
    |e1::q1, e2::q2 -> if e1 < e2 then e1::fusion q1 l2 
                       else e2::fusion l1 q2 ;;

(* [tri l] se sert des fonctions précédentes pour trier la liste [l] en petite complexité *) 
let rec tri l = match l with 
    |[] -> [] 
    |[e] -> [e] 
    |l -> let l1, l2 = split l in 
          fusion (tri l1) (tri l2) ;; 
