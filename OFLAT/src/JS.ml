(*
 * JS.ml
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

module JS = 
struct 
  let global =
      Js.Unsafe.global
  
  let eval s =
      Js.Unsafe.eval_string s
      
  let exec s =
      ignore (eval s)

  let console =
      Firebug.console

  let string s =
      Js.string s

  let log j =
      ignore (console##log j)

  let alert j =
      ignore (global##alert j)

  let alertStr s =
      alert (Js.string s)
  
end 
