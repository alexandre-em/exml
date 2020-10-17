open Tableur

let test1 () =
  let grille = cree_grille 10 10 in
  ignore grille

let test2 () =
  let grille = cree_grille 10 10 in
  grille.(0).(0) <- Entier 1 ;
  assert (grille.(0).(0) = Entier 1) ;
  ()

let test3 () =
    let gr = cree_grille 4 4 in
    gr.(0).(0) <- Entier 1 ;
    gr.(0).(1) <- Case (0,0) ;
    gr.(1).(0) <- Case (1,1) ;
    gr.(1).(1) <- Case (1,2) ;
    gr.(1).(2) <- Case (2,1) ;
    gr.(2).(1) <- Case (1,0) ;
    let res = eval_grille gr in
    assert (res.(1).(1) = Erreur Cycle_detecte);
   ()

let test4 () =
  let gr = cree_grille 4 4 in
  gr.(0).(1) <- (Chaine("je"));
  gr.(0).(3) <- (Chaine("suis"));
  gr.(1).(2) <- (Chaine("la"));
  gr.(0).(0) <- (Case(1,2));
  gr.(3).(3) <- somme (1,2) (0,0);
  gr.(2).(2) <- abs (Entier (-2));
  gr.(2).(3) <- add (Entier (2)) (Entier(5));
  let res = eval_grille gr in
  assert (res.(3).(3) = (Chaine "lasuisjela":resultat));
  ()

let test5 () =
  let gr = cree_grille 4 4 in
  gr.(0).(0) <- Entier 1 ;
  gr.(0).(1) <- Case (0,0) ;
  gr.(1).(0) <- Case (1,1) ;
  gr.(1).(1) <- Case (1,2) ;
  gr.(1).(2) <- Case (2,1) ;
  gr.(2).(1) <- Case (2,0) ;
  gr.(2).(0) <- Entier 2;
  gr.(3).(1) <- Entier 7;
  let a = somme (1,2) (3,1) in
  let b = abs(somme (3,0) (0,0)) in
  gr.(3).(3) <- add a b;
  let res = eval_grille gr in
  assert (res.(3).(3) = Entier 25);
  ()

let testadd () =
  let gr = cree_grille 4 4 in
  gr.(0).(0) <- Entier 2;
  gr.(3).(2) <- Case (0,0);
  gr.(1).(1) <- add (Case(0,0):expr) (Case(3,2):expr);
  let res = eval_grille gr in
  assert (res.(1).(1) = Entier 4);
  ()

let testabs () =
  let gr = cree_grille 4 4 in
  gr.(2).(0) <- Entier (-20);
  gr.(3).(0) <- abs (Case(2,0):expr);
  let res = eval_grille gr in
  assert (res.(3).(0) = Entier 20);
  ()

let testoppo () = 
  let gr = cree_grille 4 4 in
  gr.(0).(0) <- oppose(Entier (-20));
  gr.(1).(1) <- oppose(Flottant 20.0);
  let res = eval_grille gr in
  assert (res.(0).(0) = Entier (20));
  assert (res.(1).(1) = Flottant (-20.0));
  ()

let testinv () =
  let gr = cree_grille 4 4 in
  gr.(2).(0) <- inv (Entier 2);
  gr.(3).(2) <- inv (Flottant 50.0);
  let res = eval_grille gr in
  assert (res.(2).(0) = Entier 0);
  assert (res.(3).(2) = Flottant 0.02);
  ()

let testmul () =
  let gr = cree_grille 4 4 in
  gr.(2).(0) <- Entier 5;
  gr.(3).(2) <- oppose (Entier 5);
  gr.(3).(3) <- mul (Case(2,0)) (Case(3,2));
  let res = eval_grille gr in
  assert (res.(3).(3) = Entier (-25));
  ()

let testdiv () =
  let gr = cree_grille 4 4 in
  gr.(2).(0) <- Flottant 10.0;
  gr.(3).(2) <- Flottant 2.0;
  gr.(3).(3) <- div (Case(2,0)) (Case(3,2));
  let res = eval_grille gr in
  assert (res.(3).(3) = Flottant(5.0));
  ()

let testmin () =
  let gr = cree_grille 4 4 in
  gr.(0).(0) <- Entier 2;
  gr.(0).(1) <- Entier (-99);
  gr.(0).(2) <- Entier 99;
  gr.(3).(3) <- minred (0,0) (1,0);
  let res = eval_grille gr in
  assert (res.(3).(3) = Entier(-99));
  ()

let testmax () =
  let gr = cree_grille 4 4 in
  gr.(0).(0) <- Entier 2;
  gr.(0).(1) <- Entier (-99);
  gr.(0).(2) <- Entier 99;
  gr.(3).(3) <- maxred (1,0) (0,0);
  let res = eval_grille gr in
  assert (res.(3).(3) = Entier 99);
  ()

let run_tests () =
  let liste_tests =
    [("création grille", test1); ("affectation grille", test2); ("Possede un cycle?", test3); ("Valeur Absolue", testabs) ;("Addition d'expr", testadd) ; ("Somme expression", test5); ("Calcul oppose", testoppo); ("Calcul inverse", testinv); ("Calcul Multip", testmul); ("Calcul division", testdiv); ("Min reduction", testmin); ("Max reduction", testmax)]
  in
  List.iteri
    (fun i (nom_test, f_test) ->
      Format.printf "Test #%d - %s:\t" (i + 1) nom_test ;
      (* try *)
        f_test () ;
        Format.printf "\027[32mOk\n\027[39m"
      (* with exn ->
        raise 
        Format.printf "\027[31mErreur - %s\n\027[39m" (Printexc.to_string exn) *)
        )
    liste_tests

(* Main *)
let () = run_tests ()
