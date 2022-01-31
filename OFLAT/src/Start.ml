(*
 * Start.ml
 *
 * This file is part of the OFLAT app
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by Rita Macedo
 *)

(* 
 * Description: Start the application.
 *)

open OCamlFlat
open Js_of_ocaml
open HtmlPageClient

let start _ = 
  HtmlPageClient.changeLang();
  let examples = Dom_html.getElementById "examplesServer" in
  examples##.innerHTML := Js.string "";
    let lis = Examples.examples in
    List.iter (fun el -> HtmlPageClient.putButton el) lis;
  Js._true
  

let _ = 
  Dom_html.window##.onload := Dom_html.handler start
