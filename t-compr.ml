(************************ Question 1.1 ************************)

(* to generate a random number between 1 and the length of the list *)
let random_number_in_list l =
  1 + Random.int (List.length l) ;;

(* return l without the rth element  *)
let l_without_r l r =
  let rec aux l r res=
    match l with
      |[]-> res
      |h::t -> if (r==1) then (aux t (r-1) res)
          else (aux t (r-1) (res@[h]))
  in aux l r [];;

(* return p with r on the head   *)
let r_on_the_head p r = [r]@p;;

let extraction_alea l p =
  (* The function randomly chooses an integer between 1 and the size of l *)
  let r1 = random_number_in_list l
  in
  (* c1 will be the list l without the r1th element *)
  let c1 = l_without_r l r1
  in
  (* getting the value of the r1th element of the list l  *)
  let r2 = List.nth l (r1-1)
  in
  (* putting the value on the head of the list p *)
  let c2 = r_on_the_head p r2
  (* returning the couple *)
  in (c1,c2);;


(************************ Question 1.2 ************************)

(* This function generates a list of integers from 1 to n (L is sorted in  *)
(*   ascending order) *)
let generate_list_1_to_n n =
  let rec aux n res =
    match n with
      | 1 -> 1::res
      | n -> aux (n-1) (n::res)
  in aux n [];;

(* this function completely empties the list l and populates the list p by *)
(* calling f (in our case it will call extraction_alea) *)
let empty_list_with_alea l p f =
  let rec aux l p =
    match l with
      |[]-> p
      |_ -> let c = f l p
          in aux (fst c) ( snd c )
  in aux l p;;


(* this function returns a random list with values between 1 and n *)
let gen_permutation n =
  let l = generate_list_1_to_n n
  in
  let p = []
  in
    empty_list_with_alea l p extraction_alea;;

(* ctrl y  et ctrl f*)
(************************ Question 1.3 ************************)
type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;

(*Insertion d'une valeur dans ABR*)
let insert_a tree a =
  let rec aux tree =
    match tree with
      |Empty -> Node (a,Empty,Empty)
      |Node(r,left,right) ->
          if (a<r) then Node (r,aux left, right)
          else Node (r,left, aux right)
  in aux tree ;;


let rec find_a v arb =
  match arb with
    |Empty -> false
    |Node(r,_,_) when r =v ->true
    |Node(r,left,right) ->
        if (v<r) then find_a v(left)
        else find_a v(right);;



(*Insertion dans ABR de plusieurs valeures stockées dans liste *)
let liste_to_arbre l =
  let rec aux l tree=
    match l with
      | [] -> tree
      | x::q -> aux q (insert_a tree x)
  in aux l Empty;;

(************************ Question 2.4 ************************)
(*Convertion de ABR vers ça representation paranthesé*)
let convert_tree_to_string tree =
  let rec aux tree res =
    match tree with
      |Empty -> res
      |Node(r,left,right) ->
          "("^(aux left "")^")"^(aux right "")
  in aux tree "";;

(********************** Question 2.5 ************************)
(*
Fonction qui prend un ABR et retourne un couple de listes:
   (listeConstruction1, listeConstruction2)

   -> listeConstruction1 : elle contient les couples composées de la valeur de
                          la racine de tree et l'expression paranthesé de tree.
   -> listeConstruction2 : elle contient les couples composées de la valeur de
                            la racine de l'ABR et la valeur de la racine où on
                            va insérer la première valeur.

 La fonction effectue un parcours préfixe de l'ABR, et à chaque étape, il
 calcule l'expression parenthesé de l'arbre.
      - Si on rencontre une nouvelle expression paranthésée on stocke le couple
       de la valeur de la racine et de l'expression paranthésée dans la première
       liste.

      - Sinon, (si une telle expression paranthésée existe déjà) on stocke
       la valeur de la racine actuelle à la valeur de la racine d'un arbre
       qui a l'expression équivalente ( qui est présente dans la première
       liste).

  A la fin dans les deux listes on obtient les valeurs qui n'auront pas les
  etiquettes (dans première liste) et les valeurs qui seront etiquettés
 (sans deuxième liste)

 Exemple:
 listes_construction (Node(9,(Node(5,Empty,Empty)),(Node(10,Empty,Empty))));;
 - : (int * string) list * (int * int) list =
 ([(9, "(())()"); (5, "()")], [(10, 5)])
 *)

let listes_construction tree =
  let res1 = ref [] and res2 = ref [] in
  let rec aux tree res1 res2 =
    match tree with
      | Empty -> ([],[])
      | Node(v,ag,ad) ->
          let rec appartient tree liste =
            match liste with
            | [] -> false
            | (x,stringX)::q -> if stringX=(convert_tree_to_string tree)
                                then true
                                else appartient tree q
          in (if not (appartient tree !res1)
            then res1:= (List.append !res1 [(v, (convert_tree_to_string tree))])
              else
                let rec racine_eq tree liste =
                  match liste with
                    | [] -> -1
                    | (x,stringX)::q -> if stringX=(convert_tree_to_string tree)
                                        then x
                                        else racine_eq tree q
                in res2:= (List.append !res2 [(v,racine_eq tree !res1)]));
            (if not (ag = Empty) then let (a,b) = aux ag res1 res2 in
              res1:=a; res2:=b);
            (if not (ad = Empty) then let (a,b) = aux ad res1 res2 in
              res1:=a; res2:=b);
            (!res1,!res2);
  in aux tree res1 res2;;

(* Contenu d'un noeud ABR compressé.
   On y stocke les valeurs et les étiquettes associées *)
type valeurABRC_listes = (int* int list) list;;

(* Structure d'un noeud ABR compressé.
   On y stocke le contenu d'un noeud, ainsi que la référence et l'etiquette
   associée à cette référence du fils gauche. Pareil pour le fils droit.
   Les étiquettes sont des entiers. Etiquette 0 correspond à une référence
  non etiquetté*)
type abrc_listes =
    | EmptyABRC
    | NodeABRC of valeurABRC_listes *
      (abrc_listes ref * int) * (abrc_listes ref * int);;

(* Création d'un noeud ABRC avec la valeur initiale et les références vers le
   fils gauche et le fils droit (qui sont EmptyABRC) ainsi qu'insertion de
   ce noeud au bon endroit *)
let rec insert tree a =
  match tree with
    | EmptyABRC  -> NodeABRC ([(a,[])],((ref EmptyABRC),0),((ref EmptyABRC),0))
    | NodeABRC(v,(refL,etL),(refR,etR))  ->
        if (a< fst(List.hd(v)))
        then NodeABRC (v,(ref (insert !refL a),etL),(refR,etR))
        else NodeABRC (v,(refL,etL),(ref (insert !refR a) ,etR));;

(*Construction d'ABRC à partir d'une liste *)
let liste_to_abrc l =
  let rec insert_liste tree l =
    match l with
      | [] -> tree
      | x::q -> insert_liste (insert tree x) q
  in insert_liste EmptyABRC l;;

(*Fonction qui prend comme argument la valeur d'un noeud et un ABRC et qui
  retourne la référence vers ce noeud *)
let rec ref_node_abrc abrc v =
  match abrc with
    | EmptyABRC -> raise Not_found
    | NodeABRC(x,(refL,etL),(refR,etR)) ->
        if (v< fst(List.hd(x)))
        then (match !refL with
          | EmptyABRC -> raise Not_found
          | NodeABRC(fils,_,_) ->
            if (fst (List.hd fils)) = v
            then refL
            else ref_node_abrc !refL v)
        else (match !refR with
              | EmptyABRC -> raise Not_found
              | NodeABRC(fils,_,_) ->
                if (fst (List.hd fils)) = v
                then refR
                else ref_node_abrc !refR v);;

(* Fonction qui prend la liste des valeurs des noeuds et un ABRC comme argument
   et qui retourne la liste des références vers les noeuds correspondants*)
let liste_refs l abrc = List.map (ref_node_abrc abrc) l;;

(*Insertion d'un couple (valeur, étiquettes associé) dans la liste ordonnée
 selon la valeur *)
let rec insert_ordered_list v l =
  match l with
    | [] -> [v]
    | x::q -> if(fst x< fst v) then x::(insert_ordered_list v q) else  v::x::q

let etiq = ref 0;;

(*Générateur d'étiquettes*)
let gen_etiq () = etiq:=!etiq+1; !etiq;;

(*Fonction qui réinitialise le générateur*)
let relancer_gen() = etiq:=0;;

(*Fonction qui vérifie l'égalité de 2 listes *)
let rec liste_egaux l1 l2 = match l1,l2 with
  | ([],[]) -> true
  | ([],x::q) -> false
  | (x::q,[]) -> false
  | (x1::q1, x2::q2) -> if (x1=x2) then liste_egaux q1 q2 else false;;

(*Fonction qui prend la racine d'un ABRC et la liste des étiquettes comme
  argument. La fonction cherche la valeur associée aux étiquettes
  passées en paramètre. Si elle trouve la valeur elle la retourne sinon elle
  retourne -1*)
let get_value_etiq abrc liste_etiq =
  match abrc with
    | EmptyABRC -> raise Not_found
    |  NodeABRC(x,(refL,etL),(refR,etR))->
        let rec aux l =
          match l with
            | [] -> -1
            | x::q -> if (liste_egaux (snd x) liste_etiq) then fst x else aux q
        in aux x;;

(*Insertion d'une valeur dans un ABRC.
  La fonction prend la valeur, la référence vers le noeud où elle doit être
  insérée et un ABRC comme paramètres.
  Elle parcourt l'ABRC en sauvegardant toutes les étiquettes par laquelle elle
  est passée (variable etiquettes Visitees).
  À chaque étape elle compare la valeur avec la valeur d'un noeud qui est
  associé aux mêmes étiquettes qu'etiquettes Visitees.
  Si on arrive à EmptyABRC on crée une nouvelle étiquette qui référence le noeud
  où la valeur doit être inséré. Si le noeud ne contient pas une valeur qui est
  associée aux mêmes étiquettes qu'etiquettes Visitees, on y insère la même
  valeur associée au etiquettes Visitees  *)

let insert_abrc_etiq v reference abrc =
  if abrc = EmptyABRC then insert abrc v
  else
    let rec aux abrc etiquettesVisitees=
      match abrc with
        | EmptyABRC -> raise Not_found
        | NodeABRC(x,(refL,etL),(refR,etR)) ->
            let valeur = get_value_etiq abrc etiquettesVisitees in
              if(valeur>=0) then
                (if(valeur>v) then
                   ((let etL = if(!refL=EmptyABRC) then gen_etiq() else etL and
                      refL=if(!refL=EmptyABRC) then reference else refL in
                       refL:=(aux !refL (if etL =0 then etiquettesVisitees
                                    else List.append etiquettesVisitees [etL]));
                       NodeABRC(x, (refL, etL),(refR, etR))))
                 else
                   (let etR = if(!refR=EmptyABRC) then gen_etiq() else etR and
                     refR=if(!refR=EmptyABRC) then reference else refR in
                      refR:=(aux !refR (if etR =0 then etiquettesVisitees
                                    else List.append etiquettesVisitees [etR]));
                      NodeABRC(x, (refL, etL), (refR, etR))))
              else
                (NodeABRC (insert_ordered_list
                  (v,etiquettesVisitees) x, (refL,etL),(refR,etR)))
    in aux abrc [];;

(* Insertion d'un ensemble des valeurs dans un ABRC
  (avec la création des étiquettes)*)
let liste_to_abrc_etiq l abrc =
  let rec aux l abrc=
    match l with
      | [] -> abrc
      | x::q ->aux q (insert_abrc_etiq (fst x) (snd x) abrc)
  in aux l abrc

(*Compression d'un ABR*)
let compresse_abr_listes abr =
  relancer_gen();
  (*Avec l'ABR de l'ennoncé:
    listeConstr1 = [(4, "((())())((())())()"); (2, "(())()"); (1, "()");
                    (8, "((())())()")]
    listeConstr2 = [(3, 1); (6, 2); (5, 1); (7, 1); (9, 1)]*)
  let (listeConstr1, listeConstr2) = listes_construction abr
  in
    (*Avec l'ABR de l'ennoncé:       (x [etiquetes]) - noeud
      abrc_init= (4 [])(2 [])(8 [])(1 []) *)
  let abrc_init = liste_to_abrc (List.map fst listeConstr1)
  in
    (*Avec l'ABR de l'ennoncé:
      liste_références = [(3, référence de (1 [])); (6, référence de (2 []));
                         (5, référence de (1 [])); (7, référence de (1 []));
                         (9, référence de (1 []))] *)
  let list_references = List.combine (List.map fst listeConstr2)
                        (liste_refs (List.map snd listeConstr2) abrc_init) in
    (*Avec l'ABR de l'ennoncé:
      abrc_fin=
      (4 [])(2 []; 6 [2])(8 [])(1 []; 3 [1]; 5 [2]; 7 [2,1]; 9 [3]) *)
  let abrc_fin =  liste_to_abrc_etiq list_references abrc_init in
    abrc_fin;;

let arbre_compress = compresse_abr_listes (liste_to_arbre [4;2;8;1;3;6;5;9;7])


(********************** Question 2.6 ************************)

(*Recherche d'un element dans un ABRC. Retourne true si on a trouvé ou false
  sinon *)
let recherche v arb =
  let rec recherche_liste v lst lst_arretes_rouges=
  (*compare v aux valeurs dans le noeud, en tenant compte des étiquettes*)
    match lst with
      |[]->raise Not_found
      |h::t when liste_egaux (snd h) lst_arretes_rouges ->
          (*egal*)
          if (fst h)=v then 0
          (*elt avec ces etiquettes est plus petit que v*)
          else if (fst h)>v  then -1
          (*elt avec ces etiquettes est plus grand que v*)
          else 1
      |h::t->recherche_liste v t lst_arretes_rouges;
  in
  let rec aux_rech v arb lst_arretes_rouges =
    let compare v x = recherche_liste v x lst_arretes_rouges in
      match arb with
        | EmptyABRC -> false
        | NodeABRC(x,(refL,etL),(refR,etR))
            when compare v x =0->true (*trouvé*)
        | NodeABRC(x,(refL,etL),(refR,etR))
            when compare v x=(-1) ->  (*descendre à gauche *)
            aux_rech v (!refL) (lst_arretes_rouges@
              (if etL=0 then [] else [etL]))
        | NodeABRC(x,(refL,etL),(refR,etR)) when compare v x=1->
            (*descendre a droite *)
            aux_rech v (!refR) (lst_arretes_rouges@
              (if etR=0 then [] else [etR]))
        | NodeABRC(_,_,_) -> failwith "Should not occure"
  in aux_rech v arb [];;

recherche 6 arbre_compress;;

(********************** Question 2.7 ************************)

module OrderedList =
struct
  type t = int list
  let compare l1 l2 =
    let rec aux l1 l2 =
      match l1,l2 with
      | [],[] -> 0
      | x::q,[] -> 1
      | [],x::q -> -1
      | x::q,y::t -> if x=y then aux q t else (if x>y then 1 else -1)
    in aux l1 l2;;
end;;

module ListMap = Map.Make(OrderedList);;

(*Contenu d'un noeud d'un ABR compressé. On y stocke les valeurs et les
étiquettes associées*)
type valeurABRC_maps = int ListMap.t;;

(*Structure d'un noeud ABR compressé. On y stocke le contenu d'un noeud,
  ainsi que la référence et l'etiquette associé à cette référence du fils
  gauche. Pareil pour le fils droit. Les étiquettes = sont des entiers.
  Étiquette 0 correspond à une référence non étiquette *)
type abrc_maps =
  | EmptyABRC
  | NodeABRC of valeurABRC_maps *
                (abrc_maps ref * int) * (abrc_maps ref * int);;

open ListMap;;
(*Création d'un noeud ABRC avec la valeur initiale et création des références
  vers son fils gauche et son fils droit (qui sont des Empty Abrc) ainsi
  qu'insertion de ce noeud au bon endroit*)
let rec insert_map tree a =
  match tree with
  | EmptyABRC  ->
    NodeABRC (add [] a empty,((ref EmptyABRC),0),((ref EmptyABRC),0))
  | NodeABRC(v,(refL,etL),(refR,etR))  ->
    if (a< (find [] v))
    then NodeABRC (v,(ref (insert_map !refL a),etL),(refR,etR))
    else NodeABRC (v,(refL,etL),(ref (insert_map !refR a) ,etR));;

(*Construction d'ABRC à partir d'une liste *)
let liste_to_abrc_map l =
  let rec insert_liste tree l =
    match l with
    | [] -> tree
    | x::q -> insert_liste (insert_map tree x) q
  in insert_liste EmptyABRC l;;

(*Fonction qui prend une valeur et un ABRC comme arguments et retorune la
  référence vers ce noeud*)
let rec ref_node_abrc_map abrc v =
  match abrc with
  | EmptyABRC -> raise Not_found
  | NodeABRC(x,(refL,etL),(refR,etR)) ->
    if (v< (find [] x)) then (match !refL with
        | EmptyABRC -> raise Not_found
        | NodeABRC(fils,_,_) ->
          if (find [] fils) = v then refL
          else ref_node_abrc_map !refL v)
    else (match !refR with
        | EmptyABRC -> raise Not_found
        | NodeABRC(fils,_,_) ->
          if (find [] fils) = v then refR
          else ref_node_abrc_map !refR v);;

(* Fonction qui prend la liste des valeurs des noeuds et un ABRC comme
argument et retourne la liste des références vers ces noeuds correspondants*)
let liste_refs_map l abrc = List.map (ref_node_abrc_map abrc) l;;

let etiq = ref 0;;
(*Générateur d'etiquetes*)
let gen_etiq () = etiq:=!etiq+1; !etiq;;
(*Fonction qui relance le générateur*)
let relancer_gen() = etiq:=0;;

(*Fonction qui prend le noeud d'ABRC et la liste des étiquettes comme argument.
  Fonction cherche la valeur associée aux étiquettes passées en paramètre.
  Si elle trouve la valeur elle le retourne sinon elle retourne -1 *)
let get_value_etiq_map abrc liste_etiq =
  match abrc with
  | EmptyABRC -> raise Not_found
  |  NodeABRC(x,(refL,etL),(refR,etR))->
    try
      find liste_etiq x
    with
    | Not_found -> -1;;

(*Insértion d'une valeur dans ABRC. La fonction prend la valeur, la référence
  vers noeud où elle doit être inséré et un ABRC comme paramètres.
   Elle parcourt ABRC en sauvegardant tous les étiquettes par laquelle elle est
   passée (variable etiquettesVisitees).A chaque etape elle compare la valeur
   avec la valeur d'un noeud qui est associé aux mêmes etiquettes qu'
   etiquettesVisitees. Si on arrive au EmptyABRC on crée une nouvelle étiquette
   qui référence le noeud où la valeur doit être inséré. Si le noued ne
   contient pas une valeur qui est associé au memes etiquettes que
   etiquettesVisitees, on y insère la valeur associée au etiquettesVisitees *)
let insert_abrc_etiq_map v reference abrc =
  if abrc = EmptyABRC then insert_map abrc v
  else
    let rec aux abrc etiquettesVisitees=
      match abrc with
      | EmptyABRC -> raise Not_found
      | NodeABRC(x,(refL,etL),(refR,etR)) ->
        let valeur = get_value_etiq_map abrc etiquettesVisitees in
        if(valeur>=0) then
          (if(valeur>v) then
             ((let etL = if(!refL=EmptyABRC) then gen_etiq() else etL and
                refL=if(!refL=EmptyABRC) then reference else refL in
               refL:=(aux !refL (if etL =0 then etiquettesVisitees
                                else List.append etiquettesVisitees [etL]));
               NodeABRC(x, (refL, etL),(refR, etR))))
           else
             (let etR = if(!refR=EmptyABRC) then gen_etiq() else etR and
               refR=if(!refR=EmptyABRC) then reference else refR in
              refR:=(aux !refR (if etR =0 then etiquettesVisitees
                                else List.append etiquettesVisitees [etR]));
              NodeABRC(x, (refL, etL), (refR, etR))))
        else
          (NodeABRC (add etiquettesVisitees v x, (refL,etL),(refR,etR)))
    in aux abrc [];;

(*Insértion d'un ensemble des valeurs dans ABRC
  (avec la création des etiquettes)*)
let liste_to_abrc_etiq_map l abrc =
  let rec aux l abrc=
    match l with
    | [] -> abrc
    | x::q ->aux q (insert_abrc_etiq_map (fst x) (snd x) abrc)
  in aux l abrc

(*Compression d'un ABR*)
let compresse_abr_map abr =
  relancer_gen();
  (*Avec l'ABR du ennoncé:
    listeConstr1 = [(4, "((())())((())())()"); (2, "(())()");
                    (1, "()"); (8, "((())())()")]
    listeConstr2 = [(3, 1); (6, 2); (5, 1); (7, 1); (9, 1)]*)
  let (listeConstr1, listeConstr2) = listes_construction abr
  in
  (*Avec l'ABR du ennoncé:       (x [etiquetes]) - noeud
    abrc_init=
    (4 [])
    (2 [])        (8 [])
    (1 [])                                *)
  let abrc_init = liste_to_abrc_map (List.map fst listeConstr1)
  in
  (*Avec l'ABR du ennoncé:
    liste_refernces = [(3, référence de (1 [])); (6, référence de (2 []));
                      (5, référence de (1 [])); (7, référence de (1 []));
                      (9, référence de (1 []))]*)
  let list_references = List.combine (List.map fst listeConstr2)
                      (liste_refs_map (List.map snd listeConstr2) abrc_init) in
  (*Avec l'ABR de l'ennoncé:
    abrc_fin=
    (4 [])(2 []; 6 [2]) (8 [])(1 []; 3 [1]; 5 [2]; 7 [2,1]; 9 [3])
  *)
  let abrc_fin =  liste_to_abrc_etiq_map list_references abrc_init in
  abrc_fin;;

let arbre_compress_map = compresse_abr_map (liste_to_arbre [4;2;8;1;3;6;5;9;7])

(********************** Question 2.8 ************************)

(*Recherche d'un element dans ABRC. Retourne true or false*)
let recherche_map v arb =
  let rec aux_rech v arb lst_arretes_rouges =
    let compare v x =
      let valeur = find lst_arretes_rouges x in
      if(valeur=v) then 0 else (if (valeur>v) then -1 else 1)
    in
    match arb with
    | EmptyABRC -> false
    | NodeABRC(x,(refL,etL),(refR,etR))
      when compare v x =0->true (*trouvé*)
    | NodeABRC(x,(refL,etL),(refR,etR))
      when compare v x=(-1) ->  (*descendre à gauche *)
      aux_rech v (!refL) (lst_arretes_rouges@ (if etL=0 then [] else [etL]))
    | NodeABRC(x,(refL,etL),(refR,etR))
      when compare v x=1->      (*descendre a droite *)
      aux_rech v (!refR) (lst_arretes_rouges@ (if etR=0 then [] else [etR]))
    | NodeABRC(_,_,_) -> failwith "Should not occure"
  in aux_rech v arb [];;

recherche_map 7 arbre_compress_map;;

(************************* Question 3.1 ************************)

let time2 f =
  let t = Sys.time () in
  let res = f () in
    Printf.printf "Execution time: %f seconds\n"
      (Sys.time () -. t);
    res;;

let rand_list_test = (gen_permutation 100);;


let space f =
  let t = Gc.allocated_bytes() in
  let res = f () in
  let z =  Gc.allocated_bytes() -. t in
    Printf.printf "Execution space: %f bytes\n"
      z;
    (z,res);;

print_endline("Construction ABR");;
snd (space (fun () -> liste_to_arbre [4;2;8;1;3;6;5;9;7]));;
let arbclassic= liste_to_arbre [4;2;8;1;3;6;5;9;7];;
print_endline("Construction ABRC");;
snd (space (fun () -> compresse_abr_listes arbclassic));;
let arbre_cmp=compresse_abr_listes arbclassic;;

print_endline("Recherche ABR");
snd (time (fun () -> find_a 4  arbclassic ));;
print_endline("Recherche ABRC");
snd (time (fun () -> recherche 4 arbre_cmp));;
#load "str.cma";;

let rec ecrire_fichier_temps fichierTime cur fin pas =
  	if cur <= fin then
    		let lst = gen_permutation cur in
    		let abr = liste_to_arbre lst in
			let abrc_liste = compresse_abr_listes abr
    			and abrc_map = compresse_abr_map abr
    			in
      			let rand = Random.int cur in
			Printf.fprintf fichierTime "%s\n"
      ((string_of_int cur)^";"^(string_of_float
        (fst (time (function () -> find_a rand abr)))));
			Printf.fprintf fichierTime "%s\n"
       ((string_of_int cur)^";"^(string_of_float
         (fst (time (function () -> recherche rand abrc_liste)))));
			Printf.fprintf fichierTime "%s\n"
      ((string_of_int cur)^";"^(string_of_float
        (fst (time (function () -> recherche_map rand abrc_map)))));
			ecrire_fichier_temps fichierTime (cur + pas) fin pas;;

let rec ecrire_fichier_space fichierSpace cur fin pas =
  	if cur <= fin then
    		let lst = gen_permutation cur in
    		let (esp1,abr) = (space (function () ->liste_to_arbre lst)) in
			let (esp2,abrc_liste) = (space (function () ->compresse_abr_listes abr))
    			and (esp3,abrc_map) = (space (function () ->compresse_abr_map abr))
    			in
			Printf.fprintf fichierSpace "%s\n"
        ((string_of_int cur) ^";"^ string_of_float (esp1));
			Printf.fprintf fichierSpace "%s\n"
        ((string_of_int cur) ^";"^ string_of_float (esp2));
			Printf.fprintf fichierSpace "%s\n"
        ((string_of_int cur) ^";"^ string_of_float (esp3));

        		ecrire_fichier_space fichierSpace (cur + pas) fin pas;;

let create_fichiers_test deb fin pas =
  	let fichierSpace = open_out "fichierSpace.csv"
    and fichierTime = open_out "fichierTime.csv" in
    ecrire_fichier_temps fichierTime deb fin pas;
    ecrire_fichier_space fichierSpace deb fin pas;
    close_out fichierSpace;
    close_out fichierTime;;


let rec nbr_noeud_interne (abrc:abrc_listes) =
match abrc with
   | EmptyABRC -> 0
   | NodeABRC(x,(refL,etL),(refR,etR))->
   if ((!refL = EmptyABRC) && (!refR = EmptyABRC)) then 0
   else (1 + (if (etL=0) then nbr_noeud_interne !refL else 0) +
   (if (etR=0) then nbr_noeud_interne !refR else 0));;

let rec nbr_cles (abrc:abrc_listes) =
match abrc with
   EmptyABRC -> 0
   | NodeABRC(x,(refL,etL),(refR,etR)) ->
   if ((!refL = EmptyABRC) && (!refR =  EmptyABRC)) then 0
					else (List.length x) + (if (etL=0) then nbr_cles !refL else 0)
           + (if (etR=0) then nbr_cles !refR else 0);;

let nbr_cles_moyenne_par_noeud (abrc:abrc_listes) =
match abrc with
   EmptyABRC -> 0.0
  | NodeABRC(_,_,_) ->
  (float_of_int(nbr_cles abrc))/.(float_of_int(nbr_noeud_interne abrc));;

let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
  while true; do
  lines := input_line chan :: !lines
  done; !lines
  with End_of_file ->
  close_in chan;
  List.rev !lines ;;

let to_lst lx=List.map int_of_string (Str.split (Str.regexp "[^0-9]+") lx );;



  (*let l1000=compresse_abr_listes (liste_to_arbre list1000);;*)
let testes = ["100";"150";"500";"750";"1000"];;

(*
let rec write_noeuds l file_noeud =
	match l with
|[]->close_out file_noeud;
|h::t ->
  let ltest = to_lst(List.hd
    (read_file
      ("./Jeu_de_tests/donnee"^h^".txt")))
       in
	let nbnoeud = nbr_cles_moyenne_par_noeud (compresse_abr_listes
      (liste_to_arbre ltest))in
	Printf.fprintf file_noeud "%s\n" (h^";"^(string_of_float(nbnoeud))^"\n");
	write_noeuds t file_noeud ;;

let fich = open_out "nbcle.csv";;
write_noeuds testes fich;;
*)


fst (time (fun () -> compresse_abr_listes (liste_to_arbre (rand_list_test ))));;
fst (space (fun () -> compresse_abr_listes arbclassic));;
