open Js_utils
open Parser

let range i j = List.init (j - i + 1) (fun x -> x + i)

type cell_infos = {
  container : Dom.div;
  inp : Dom.input;
  txt : Dom.txt;
  mutable result : Tableur.resultat;
  mutable parent_deps : (int * int) list;
  mutable child_deps : (int * int) list;
}

let direct_deps expr =
  let rec aux expr =
    let open Tableur in
    match expr with
    | Vide | Entier _ | Flottant _ | Chaine _ ->
        CaseSet.empty
    | Case (i, j) ->
        CaseSet.singleton (i, j)
    | Unaire {operande; _} ->
        aux operande
    | Binaire {gauche; droite; _} ->
        CaseSet.union (aux gauche) (aux droite)
    | Reduction {case_debut; case_fin; _} ->
        List.fold_left (fun set c -> CaseSet.add c set) CaseSet.empty
        @@ coords_of_plage case_debut case_fin
  in
  Tableur.CaseSet.elements (aux expr)

type grid = Tableur.expr array array

type infos_grid = cell_infos array array

let mk_cell ?(inp = Dom.Create.input ()) ?(container = Dom.Create.div ())
    ?(txt = Dom.Create.txt " ") ?(result = (Tableur.Vide: Tableur.resultat)) ?(parent_deps = []) ?(child_deps = []) () =
  {inp; container; txt; result; parent_deps; child_deps}

let error_to_string e =
   match e with
      | Tableur.Cycle_detecte -> "#CYCLE"
      | Tableur.Mauvais_indice(i,j) -> "#IND("^(string_of_int i)^","^(string_of_int j)^")"
      | Tableur.Divzero(s) -> "#DIV_BY_0"
      | Tableur.Mauvais_argument(s) -> "#ARG"^s

let resultat_to_string r =
  match r with
  | (Tableur.Entier(n):Tableur.resultat) -> (string_of_int n)
  | Tableur.Flottant(x) -> (string_of_float x)
  | Tableur.Vide -> ""
  | Tableur.Chaine(s) -> s
  | Tableur.Erreur(e) -> (error_to_string e)

let update_display infos_grid i j r =
  let cell = infos_grid.(i).(j) in
  infos_grid.(i).(j).result <- r;
  Dom.Text.set_content cell.txt (resultat_to_string r);
  Dom.Input.set_value cell.inp (resultat_to_string r);
  match r with
  | Erreur(e) ->
    Dom.Class.add cell.inp "cell-error"
  | _ -> Dom.Class.remove cell.inp "cell-error"

let update_deps infos_grid i j expr =
  let parent = (direct_deps expr) in
  infos_grid.(i).(j).parent_deps <- parent;
  for i = 0 to (List.length parent)-1 do
    match (List.nth parent i) with
    | (k,l) -> infos_grid.(k).(l).child_deps <- (i,j)::infos_grid.(k).(l).child_deps;
  done

let propagate grid infos_grid i j = (* TODO *)
  let child= infos_grid.(i).(j).child_deps in
  for k=0 to (List.length child)-1 do
    match (List.nth child k) with
    | (a,b) -> update_display infos_grid a b infos_grid.(i).(j).result
  done

let grid_to_string grid infos_grid =
  let s = ref "" in
  let value = ref "" in
  for i=0 to (Array.length infos_grid)-1 do
    for j=0 to (Array.length infos_grid.(i))-1 do
      value := (let tmp = (Dom.Input.get_value infos_grid.(i).(j).inp) in
                   if((String.length tmp)=0) then (resultat_to_string infos_grid.(i).(j).result)
                   else tmp);
      if (!value)<>"" then
        let si = (string_of_int i) and sj = (string_of_int j) in
        s := !s^si^"|"^sj^"|"^(!value)^"\n"
    done;
  done;
  let n = String.length (!s) in
  if (n > 0) && ((!s).[n-1] = '\n') then
      String.sub (!s) 0 (n-1)
    else
      !s

let cells_of_string storage_grid =
  if storage_grid="" then
    []
  else
    let l = String.split_on_char '\n' storage_grid in
     List.map (function x ->
      let st = String.split_on_char '|' x in
      match st with
      | h::h2::h3::t -> ((int_of_string h), (int_of_string h2), h3)
      | _ -> raise (Invalid_argument "Error")) l


let update i j grid infos_grid =
  let inp2 = (Dom.Input.get_value infos_grid.(i).(j).inp) in
  (if (inp2<>"") then
    let expr = Ast.make inp2 in
    (match expr with
       Ok(a) -> grid.(i).(j) <- a;
       let res = try (Tableur.eval_grille grid).(i).(j) with Division_by_zero -> Erreur(Tableur.Divzero "div0") in
       update_deps infos_grid i j a;
       update_display infos_grid i j res
     | Error(e) -> Dom.Text.set_content infos_grid.(i).(j).txt "#ERROR"));
  let res = try (Tableur.eval_grille grid).(i).(j) with Division_by_zero -> Erreur(Tableur.Divzero "div0") in
    infos_grid.(i).(j).result <- res;
    update_display infos_grid i j res;
    propagate grid infos_grid i j;
    Storage.set (grid_to_string grid infos_grid)

let add_cell_events i j grid infos_grid =
  let cell = infos_grid.(i).(j) in
  let f () = (Dom.Class.add cell.inp "editing-input";
              Dom.Focus.focus cell.inp) in
  let g () = (Dom.Focus.blur cell.inp;
              update i j grid infos_grid;
              Dom.Class.remove cell.inp "editing-input") in
  let h (v:int) = (if v=13 then
                     (update i j grid infos_grid;
                     Dom.Class.remove cell.inp "editing-input");
                   true) in
  Dom.Events.set_ondblclick cell.container f;
  Dom.Events.set_onblur cell.inp g;
  Dom.Events.set_onkeydown cell.inp h

let build_cell cells =
  let cell = mk_cell () in
  (match cell with
   | {inp;container;txt;result} ->
     Dom.appendChild cells container;
     Dom.Class.add container "cell-container";
     Dom.appendChild container inp;
     Dom.appendChild container txt);
  cell

let load_grids height width =
  let cells = Dom.get_element_by_id "cells" in
  Init.set_grid_template cells height width ;
  let lines = range 0 (height - 1) in
  let columns = range 0 (width - 1) in
  Init.build_headers cells lines columns ;
  let grid = Array.make_matrix height width Tableur.Vide in
  let infos_grid =
    Array.init height @@ fun _ -> Array.init width @@ fun _ -> build_cell cells
  in
  (grid, infos_grid)

let load_storage grid infos_grid =
  match Storage.find () with
  | None -> ()
  | Some s ->
    let storage_grid = cells_of_string s in
      for k=0 to (List.length storage_grid)-1 do
        match (List.nth storage_grid k) with
          (i, j, value) -> let expr = Ast.make value
          in
          (match expr with
             Ok(a) -> grid.(i).(j) <- a;
             update_deps infos_grid i j a
           | Error(e) -> Dom.Text.set_content infos_grid.(i).(j).txt e);
          update i j grid infos_grid
    done

let main () =
  let height = 10 in
  let width = 10 in
  let (grid, infos_grid) = load_grids height width in
  let () = load_storage grid infos_grid in
  Array.iteri
    (fun i a -> Array.iteri (fun j c -> add_cell_events i j grid infos_grid) a)
    grid

let () =
  (*Storage.clean_all ();*)
  Init.onload main
