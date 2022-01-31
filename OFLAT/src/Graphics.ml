(*
 * Graphics.ml
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

open Js_of_ocaml
open JS

module Graphics
=
struct

  (** Function to paint the states of the automaton in the cy div **)
  let paintNode node color = 
      JS.exec ("paintNode('" ^ node ^ "', '"^ color ^"')")

  (** Function to paint the states of the automaton in the cy2 div **)
  let paintNode1 node color = 
      JS.exec ("paintNode1('" ^ node ^ "', '"^ color ^"')")

  (** Function to create edge on the div cy **)
  let createEdge (f, s, t) = 
      JS.exec ("makeEdge('" ^ f ^ "', '" ^ t ^ "', '" ^ (String.make 1 s) ^ "')")

  (** Function to eliminate edge on the div cy **)
  let eliminateEdge (f, s, t) = 
      JS.exec ("removeEdge('" ^ f ^ "', '" ^ t ^ "', '" ^ (String.make 1 s) ^ "')")

  (** Function to create edge on the div cy2 **)
  let createEdge1 (f, s, t) = 
      JS.exec ("makeEdge2('" ^ f ^ "', '" ^ t ^ "', '" ^ (String.make 1 s) ^ "')")

  (** Function to create state on the div cy **)
  let createNode node isStart isFinal =
      JS.exec ("makeNode('" ^ node ^ "', '" ^ string_of_bool (isStart) ^ "', '" ^ string_of_bool (isFinal) ^ "')")

  (** Function to eliminate state on the div cy **)
  let eliminateNode node =
      JS.exec ("removeNode('" ^ node ^ "')")

  (** Function to create state on the div cy2 **)
  let createNode1 node isStart isFinal =
      JS.exec ("makeNode2('" ^ node ^ "', '" ^ string_of_bool (isStart) ^ "', '" ^ string_of_bool (isFinal) ^ "')")
  
  (** Function to start a new automaton in the div cy **)
  let startGraph () =   
      JS.exec ("start()")
  
  (** Function to start a new automaton in the div cy2 **)
  let startGraph1 () =   
      JS.exec ("start2()")

  (** Function to start a new Tree in the div cy **)
  let startGraph2 (s : Js.js_string Js.t) : unit =   
    Js.Unsafe.fun_call (Js.Unsafe.js_expr "startTree") [|Js.Unsafe.inject s|]

  (** Function to start a new Tree in the div cy2 **)
  let startGraph3 (nString) =   
      JS.exec ("startTree1('" ^ nString ^ "')")

  (** Function to eliminate the automaton/tree in the div cy **)
  let destroyGraph () =   
      JS.exec ("destroy1()")

  (** Function to eliminate the automaton/tree in the div cy2 **)
  let destroyGraph1 () =   
      JS.exec ("destroy2()")

  (** Clear all the color changes made to the automaton **)
  let resetStyle () = 
      JS.exec ("resetStyle()")

  (** Adjust the automaton to the size of the div cy **)
  let fit () =   
    JS.exec ("fit()")

  (** Generate a random color - used when the automaton is minimized and has the need of more colors than the pre-defined ones **)
  let getRandom() = 
      let test = Random.int 16777215 in
      Printf.sprintf "#%06x" test
  
  (** Used when the user wants to change the direction of the regular Expression tree **)
  let changeDirection () =
    JS.exec ("changeDirection()")
  
  let getWords () =
    JS.exec ("getNumber()")

  let startRegex () =
    JS.exec ("startRegex()")

  let complete () =
    JS.exec ("complete()")

  let startStep () =
    JS.exec ("startStep()")
  
  let turnFinal name =
    JS.exec ("turnFinal('" ^ name ^ "')")

  let removeFinal name =
    JS.exec ("removeFinal('" ^ name ^ "')")

  let changeLang name =
    JS.exec ("changeLanguage('" ^ name ^ "')")

end

