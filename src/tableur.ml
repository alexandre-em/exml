type resultat =
  | Vide
  | Chaine of string
  | Entier of int
  | Flottant of float
  | Erreur of erreur
and erreur =
  | Mauvais_indice of (int * int)
  | Cycle_detecte
  | Mauvais_argument of string
  | Divzero of string

type expr =
  | Vide
  | Chaine of string
  | Entier of int
  | Flottant of float
  | Case of int * int
  | Unaire of op_unaire
  | Binaire of op_binaire
  | Reduction of op_reduction
and op_unaire = {app1: resultat -> resultat; operande: expr}
and op_binaire = {app2: resultat -> resultat -> resultat; gauche: expr; droite: expr}
and op_reduction = {app: resultat -> resultat -> resultat;
                    init: resultat;
                    case_debut: int * int;
                    case_fin: int * int}

type grille = expr array array

let ( -- ) i j = List.init (j - i + 1) (fun x -> x + i)

let int_to_letter j =
  let ltval = j mod 26 in
  let nb = (j - ltval) / 26 in
  let nb = if nb = 0 then "" else string_of_int nb in
  let lt = char_of_int (int_of_char 'A' + ltval) in
  Format.sprintf "%c%s" lt nb

  let rec product a b =
    match a with
    | [] ->
        []
    | hd :: tl ->
        List.map (fun x -> (hd, x)) b @ product tl b

(** Fonctions *)

let cree_grille _i _j =
  let tab = Array.make_matrix _i _j (Vide: expr) in
    (tab: grille)

let coords_of_plage (i, j) (i', j') =
  let l = if i < i' then i -- i' else i' -- i in
  let j = if j < j' then j -- j' else j' -- j in
  product l j

let cases_of_plage (i, j) (i', j') =
  coords_of_plage (i, j) (i', j') |> List.map (fun (i, j) -> Case (i, j))

module type CELL = sig
  type t

  val get_expr : t -> expr
end

module CaseSet = Set.Make (struct
    type t = int * int

    let compare = compare
  end)

module type T = sig
  type grille

  val eval : grille -> int -> int -> resultat
end

let genere_grille (grille: grille) =
	for i=0 to (Array.length grille)-1 do
        for j=0 to (Array.length grille.(i))-1 do
            match grille.(i).(j) with
              | _ -> grille.(i).(j) <- (Case (i,j))
          done;
    done;
    grille

let expr_to_string (expr: expr) =
  match expr with
       | Chaine (s) -> s
       | Vide -> ""
       | Entier (n) -> (string_of_int n)
       | Flottant (f) -> (string_of_float f)
       | Case (x,y) -> ("@("^(string_of_int x)^","^(string_of_int y)^")")
       | Unaire (_) -> "opUn"
       | Binaire (_) -> "op2"
       | Reduction (_) -> "opRed"

 let affiche_grille (grille: grille) =
   for i=0 to (Array.length grille)-1 do
     for j=0 to (Array.length grille.(i))-1 do
        match grille.(i).(j) with
          | s -> Format.printf "|%6s|" (expr_to_string s)
      done;
     Format.printf "\n";
   done


let op_red_left (f: resultat->resultat->resultat) (ini:resultat) lcase =
  let res = ref ini in
  for i=0 to (List.length lcase)-1 do
    res:= (f !res (List.nth lcase i))
  done;
  !res


let op_red_right (f: resultat->resultat->resultat) (ini:resultat) lcase =
  let res = ref ini in
  for i=(List.length lcase)-1 downto 0 do
    res:= (f !res (List.nth lcase i))
  done;
  !res


let dfs (grille: grille) (mem: resultat array array) (x: int) (y: int) =
  let (visite: bool array array) = Array.make_matrix (Array.length grille) (Array.length grille.(0) ) false in
  let () = visite.(x).(y)<- true in
  let rec dfs_rec (grille: grille) (x: int) (y: int) =
    match grille.(x).(y) with
    | Case (k,l) ->
      if mem.(k).(l) <> Vide then let () = mem.(x).(y) <- mem.(k).(l) in
        if mem.(k).(l) = Erreur(Cycle_detecte) then true else false
      else
            if visite.(k).(l)=false then
              (dfs_rec grille k l)
            else
              let () = mem.(x).(y) <- mem.(k).(l) in
              true
    | s  ->
      let rec to_res s =
          match s with
          | Entier(n) -> (Entier(n): resultat)
          | Chaine(s) -> (Chaine(s): resultat)
          | Flottant(f) -> (Flottant(f): resultat)
          | Unaire({app1=f; operande=op}) -> (f (to_res op))
          | Binaire({app2=f; gauche=op1; droite=op2}) -> (f (to_res op1) (to_res op2))
          | Reduction({app=f; init=ini; case_debut=(k,l);case_fin=(x',y')}) ->
            let tab = ref [] in
            let j2 = ref 0 in
            let j3 = ref ((Array.length grille.(0))-1) in
            for i = k to x' do
              if (i=k) then j2 := l
              else j2:=0;
              if (i=x') then j3 := y';
              for j = !j2 to !j3 do
               if grille.(i).(j)<>Vide then
                tab := (to_res (Case(i,j)))::!tab
              done;
            done;
            if k<x' then (op_red_right f ini !tab)
            else (op_red_left f ini !tab) (* TODO fonction reduction*)
          | Vide -> (Vide: resultat)
          | _ -> (Erreur(Mauvais_indice(x,y)))
      in
      mem.(x).(y) <- to_res s;
      false
  in dfs_rec grille x y

let cycle (grille: grille) (mem: resultat array array) (expr: expr) =
  match expr with
  | Case(x, y) -> (dfs grille mem x y)
  | _ -> false

let rec eval_expr (grille: grille) (mem: resultat array array) (expr: expr) =
  match expr with
  | Case(x,y) ->
    if (mem.(x).(y)=Vide) then
      if ( (x>=(Array.length grille)) || (x<0)) then (Erreur(Mauvais_indice (x,y)): resultat)
      else  if ( (y>=(Array.length grille.(0))) || (y<0)) then (Erreur(Mauvais_indice (x,y)): resultat)
      else  if ( (cycle grille mem (Case(x,y))) = true) then (Erreur(Cycle_detecte):resultat)
      else eval_expr grille mem (Case(x,y))
    else
      mem.(x).(y)
  | Chaine (s) -> (Chaine(s): resultat)
  | Entier(n) -> (Entier(n): resultat)
  | Flottant(f) -> (Flottant(f): resultat)
  | Unaire({app1=f;operande=op}) -> (f (eval_expr grille mem op))
  | Binaire({app2=f;gauche=op1;droite=op2}) -> (f (eval_expr grille mem op1) (eval_expr grille mem op2))
  | Reduction({app=f;init=res;case_debut=(k,l);case_fin=(x',y')}) ->
    let tab = ref [] in
    let j2 = ref 0 in
    let j3 = ref ((Array.length grille.(0))-1) in
    let min_x= if k<x' then k else x' and max_x= if k<x' then x' else k in
    let min_y= if k<x' then l else y' and max_y= if k<x' then y' else l in
    for i = min_x to max_x do
              if (i=min_x) then j2 := min_y
              else j2:=0;
              if (i=max_x) then j3 := max_y;
              for j = !j2 to !j3 do
                if (grille.(i).(j)<>Vide) then
                  tab := (eval_expr grille mem (Case(i,j)))::!tab
              done;
            done;
            if k<x' then
              (op_red_right f res !tab)  (* Evaluation de droite a gauche *)
            else
              (op_red_left f res !tab) (* Evaluation de gauche a droite *)
  | Vide -> (Vide: resultat)

let eval_grille (grille: grille) =
  let mem = Array.make_matrix (Array.length grille) (Array.length grille.(0)) (Vide: resultat) in
  let res = Array.make_matrix (Array.length grille) (Array.length grille.(0)) (Vide: resultat) in
  for i=0 to (Array.length grille)-1 do
    for j=0 to (Array.length grille.(i))-1 do
      res.(i).(j) <- (eval_expr grille mem grille.(i).(j))
    done;
  done;
  res

exception Has_cycle of string

let resultat_to_string (res: resultat) =
  match res with
  | Vide -> Format.printf "|%5s|" ""
  | Chaine(s) -> Format.printf "|%5s|" s
  | Entier(n) -> Format.printf "|%5s|" (string_of_int n)
  | Flottant(f) -> Format.printf "|%5s|" (string_of_float f)
  | Erreur (s) -> match s with
    | Cycle_detecte ->
      raise (Has_cycle ("Cycle detecte"))
    | Mauvais_indice (x,y) ->
      raise (Invalid_argument ("index out of bounds @("^(string_of_int x)^","^(string_of_int y)^")"))
    | Mauvais_argument s ->
      raise (Invalid_argument s)
    | Divzero x ->
      raise (Invalid_argument (x^" Division par zero"))

let affiche_grille_resultat (res: resultat array array) =
  for i=0 to (Array.length res)-1 do
    for j=0 to (Array.length res.(i))-1 do
      match res.(i).(j) with
      | s ->
        resultat_to_string s
    done;
    Format.printf "\n"
  done



(** Formules  *)


let abs_expr (op:resultat) =
  match op with
  | Flottant(x) -> if x<0.0 then (Flottant(x *. -1.0):resultat) else (Flottant(x):resultat)
  | Entier(x) -> if x<0 then (Entier(x * -1):resultat) else (Entier(x):resultat)
  | _ -> Erreur(Mauvais_argument "Pas un nombre")

let abs (op: expr) =
  (Unaire({app1=abs_expr;operande=op}))

let addres (op1: resultat) (op2: resultat) =
  match op1,op2 with
  | Entier(x), Entier(y) -> (Entier(x+y):resultat)
  | Flottant(x), Flottant(y) -> (Flottant(x +. y):resultat)
  | Entier(n),Vide -> (Entier(n):resultat)
  | Flottant(f),Vide -> (Flottant(f):resultat)
  | Vide, Entier(n) -> (Entier(n):resultat)
  | Vide, Flottant(f) -> (Flottant(f):resultat)
  | Vide,Vide -> (Entier(0):resultat)
  | _ -> Erreur(Mauvais_argument "Valeurs Incompatibles")


let add (op1: expr) (op2: expr) =
  (Binaire({app2=addres;gauche=op1;droite=op2}))

let somme (cased:(int*int)) (casef:(int*int)) =
  (Reduction({app=addres;init=(Vide:resultat); case_debut=cased; case_fin=casef}))


let oppose_expr (op:resultat) =
  match op with
  | Flottant(x) -> (Flottant(x *. -1.0):resultat)
  | Entier(x) -> (Entier(x * -1):resultat)
  | _ -> Erreur(Mauvais_argument "Non applicable opp")

let oppose (op: expr) =
  (Unaire({app1=oppose_expr;operande=op}))

let inv_expr (op:resultat) =
  match op with
  | Flottant(x) -> (Flottant(1.0 /. x):resultat)
  | Entier(x) -> (Entier(1 / x):resultat)
  | _ -> Erreur(Mauvais_argument "Non applicable inv")

let inv (op: expr) =
  (Unaire({app1=inv_expr;operande=op}))

let mulres (op1: resultat) (op2: resultat) =
  match op1,op2 with
  | Entier(x), Entier(y) -> (Entier(x*y):resultat)
  | Flottant(x), Flottant(y) -> (Flottant(x *. y):resultat)
  | Entier(n),Vide -> (Entier(0*n):resultat)
  | Flottant(f),Vide -> (Flottant(0.0 *. f):resultat)
  | Vide, Entier(n) -> (Entier(0*n):resultat)
  | Vide, Flottant(f) -> (Flottant(0.0 *. f):resultat)
  | Vide,Vide -> (Vide:resultat)
  | _ -> Erreur(Mauvais_argument "Valeurs Incompatible avec l'operations")


let mul (op1: expr) (op2: expr) =
  (Binaire({app2=mulres;gauche=op1;droite=op2}))

let divres (op1: resultat) (op2: resultat) =
  match op1,op2 with
  | Entier(x), Entier(y) -> (Entier(x / y):resultat)
  | Flottant(x), Flottant(y) -> (Flottant(x /. y):resultat)
  | Vide, Entier(n) -> (Entier(n):resultat)
  | Vide, Flottant(f) -> (Flottant(f):resultat)
  | Vide,Vide -> (Vide:resultat)
  | Entier(x), Vide -> Erreur (Divzero (string_of_int x))
  | Flottant(x), Vide -> Erreur (Divzero (string_of_float x))
  | _ -> Erreur(Mauvais_argument "Valeurs Incompatible avec l'operations")


let div (op1: expr) (op2: expr) =
  (Binaire({app2=divres;gauche=op1;droite=op2}))

let minres (op2: resultat) (op1: resultat) =
  match op1,op2 with
  | Entier(x), Entier(y) -> (Entier(min x y):resultat)
  | Flottant(x), Flottant(y) -> (Flottant(min x y):resultat)
  | Vide, Entier(x) | Entier(x), Vide-> Entier(x)
  | Vide, Flottant(x) | Flottant(x), Vide -> Flottant(x)
  | _ -> Erreur(Mauvais_argument "Valeur incompatible")


let minred (cased:(int*int)) (casef:(int*int)) =
  (Reduction({app=minres;init=Vide;case_debut=cased;case_fin=casef}))

let maxres (op1:resultat) (op2:resultat) =
  match op1,op2 with
  | Entier(x), Entier(y) -> (Entier(max x y):resultat)
  | Flottant(x), Flottant(y) -> (Flottant(max x y):resultat)
  | Vide, Entier(x) | Entier(x), Vide -> Entier (x)
  | Vide, Flottant(x) | Flottant(x), Vide -> Flottant(x)
  | _ -> Erreur(Mauvais_argument "Valeur incompatible")

let maxred (cased:(int*int)) (casef:(int*int)) =
  (Reduction({app=maxres;init=Vide;case_debut=cased;case_fin=casef}))

let moyres (op1:resultat) (op2:resultat) =
  match op1,op2 with
  | Entier(x), Entier(y) -> (Entier((x+y)/2):resultat)
  | Flottant(x), Flottant(y) -> (Flottant((x +. y) /. 2.0):resultat)
  | Vide, Entier(x) | Entier(x), Vide -> Entier (x/2)
  | Vide, Flottant(x) | Flottant(x), Vide -> Flottant(x /. 2.0)
  | _ -> Erreur(Mauvais_argument "Valeur incompatible")

let moy (cased:(int*int)) (casef:(int*int)) =
  (Reduction({app=moyres;init=Vide;case_debut=cased;case_fin=casef}))
