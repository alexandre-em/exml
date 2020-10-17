
(*
   Ast : analyseur lexical et syntaxique des formules du tableur
         + construction de l'environnement de calcul des primitives
 *)

(* utilisation des streams et d'une grammaire descendante :

expr ::= atom suite_expr |  op(args)   |  epsilon
args ::= col1,lig1,col2,lig2 |  expr suite_args
suite_args ::= , expr | epsilon
suite_expr ::= + atom | - atom | * atom | / atom | epsilon
atom ::= int | float | string | #(col, lig) | (expr) | op1(expr)

où +,-,*,/ sont des opérations binaires
op est  une opération unaire, binaire ou une réduction
col : une suite de lettres
lig : un nombre entier

*)

open Tableur
open Genlex

(* l'analyseur lexicale  *)

let keywords = ["+"; "-"; "*" ; "/" ; "(" ; ")"; "#" ;  ","]
let line_lexer l = Genlex.make_lexer keywords (Stream.of_string l)

(* calcul de la coordonnée de la colonne *)

let to_int s =
  let l = String.length s in
  let codeA = int_of_char 'A' in
  let r = ref 0 in
  let i = ref 0 in
    while !i <  l do
       r := (int_of_char s.[!i] - codeA + 1) + 26 * !r ;
       i := !i + 1
    done;
  !r

(* environnement des opérations unaires *)

let abs_r (r: resultat) = match r with
  Entier i -> if i < 0 then (Entier (-i): resultat) else r
| Flottant f -> if f < 0.0 then (Flottant (-. f):resultat)  else r
| _ -> (Erreur (Mauvais_argument "Non applicable"):resultat)

let env_unaire = ref [ ("abs",abs_r) ]

let creer_unaire op1 e1 =
  Unaire {app1= List.assoc op1 !env_unaire ; operande = e1}

(* environnement des opérations binaires *)

let make_opbin opint opfloat  (r1: resultat) (r2: resultat) = match (r1,r2) with
  Entier i1, Entier i2 -> (Entier ( opint i1 i2): resultat)
| Flottant f1, Flottant f2 -> (Flottant (opfloat f1 f2): resultat)
| Entier i1,  Flottant f2 -> (Flottant (opfloat (float_of_int i1) f2): resultat)
| Flottant f1, Entier i2 -> (Flottant (opfloat f1 (float_of_int i2)): resultat)
| _ -> (Erreur (Mauvais_argument "Non applicable"): resultat)

let plus_r r1 r2 = make_opbin (+) (+.)  r1 r2
let sous_r r1 r2 = make_opbin (-) (-.) r1 r2
let mult_r  r1 r2 = make_opbin ( * ) ( *. ) r1 r2
let div_r  r1 r2 = make_opbin (/) (/.) r1 r2

let env_binaire = ref [ ("add",plus_r) ; ("+",plus_r) ; ("-",sous_r) ; ("*", mult_r) ; ("/",div_r)  ]

let creer_binaire op e1 e2 =
   Binaire {app2=List.assoc op !env_binaire ; gauche = e1 ; droite = e2}

let somme_r = plus_r

let env_reduction = ref [ ("sum",(Tableur.addres,(Entier 0:resultat))) ]

let creer_reduction op i1 i2 i3 i4 =
  let (app_r: resultat -> resultat -> resultat),(acc: resultat) = List.assoc op !env_reduction in
   Reduction { app = app_r ; init = acc ; case_debut = (i1,i2) ; case_fin = (i3,i4)}

(* la grammaire *)

let rec atom s = match s with parser
  [< 'String s >] -> Chaine s
| [< 'Int i >] -> Entier i
| [< 'Float f >] -> Flottant f
| [< 'Kwd "#" ; '(Kwd "(") ; 'Ident id ; 'Kwd "," ; 'Int i ; 'Kwd ")">]
      ->  Case (i - 1, to_int id - 1)
| [< 'Kwd "(" ; e = expr ; 'Kwd ")" >] -> e

and suite_expr e1 s = match s with parser
  [< 'Kwd "+" ; e2 = atom >] -> creer_binaire "+" e1 e2
| [< 'Kwd "-" ; e2 = atom >] -> creer_binaire "-" e1 e2
| [< 'Kwd "*" ; e2 = atom >] -> creer_binaire "*" e1 e2
| [< 'Kwd "/" ; e2 = atom >] -> creer_binaire "/" e1 e2
| [< >] -> e1

and args op  s = match s with parser
  [< 'Ident id1 ; 'Kwd "," ; 'Int i2 ; 'Kwd "," ; 'Ident id3 ; 'Kwd "," ; 'Int i4 >] ->
     creer_reduction op ((to_int id1)-1) (i2-1) ((to_int id3)-1) (i4-1)
| [< e1 = expr ; e2 = suite_args op e1  >] -> e2

and suite_args op e1  s = match s with parser
  [< 'Kwd "," ; e2 = expr >] -> creer_binaire op e1 e2
| [< >] -> creer_unaire  op e1

and expr s = match s with parser
| [< e1 = atom ; e = suite_expr e1   >] ->   e
| [< 'Ident op ; 'Kwd "(" ; e = args op ; 'Kwd ")" >] -> e
| [< >] -> Vide

(* la fonction de création de l'ast à partir d'une chaîne de caractères *)

let make s =
  try Ok (expr  (line_lexer s)) with
  | _ -> Error "Not an expr"


(* exemples

make_ast "#(AA,12)";;
make_ast "somme(AB,10,AD,14)" ;;
make_ast "abs(2 + 4)" ;;
make_ast "add(3 * 4, 2 - 3)";;
make_ast "add( (abs(3 - 4 )), (somme(AB,10,AD,14)))";;

attention "-3" est considéré comme un entier  par le lexer
et donc "2-3" est vu comme la suité de 2 entiers 2 et -3
il faut alors ecrire "2 - 3" et c'est bien l'opérateur "-"

make_ast "moyenne(AB,10,AD,14)" ;;
make_ast "oppose(3)" ;;

pour le moment il n'y a pas beaucoup de gestion d'erreurs
s elles sont assez simple à gérer :

  syntaxe
  fonction non définie

*)
