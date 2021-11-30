(*
 * Listeners.ml
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
 * Description: Part of the controller that is acessible from the
 * Visualizer (HtmlPageClient). Using this module, we avoid the need to use
 * mutually recursive modules.
 *)

  module Listeners = 
    struct
      let closeRightListener = ref (fun () -> ())
      let changeListener = ref (fun () -> ())
      let getCompleteAutomatonListener = ref (fun () -> ())
      let closeCompleteAutomatonListener = ref (fun () -> ())
      let cleanUselessListener = ref (fun () -> ())
      let getDeterministicListener = ref (fun () -> ())
      let defineMinimizedListener = ref (fun () -> ())
      let paintAllProductivesListener = ref (fun () -> ())
      let paintAllReachableListener = ref (fun () -> ())
      let paintAllUsefulListener = ref (fun () -> ())
      let changeDirectionListener = ref (fun () -> ())
      let checkExerciseListener = ref (fun () -> ())
      let clearExerciseListener = ref (fun () -> ())
      let previousTreeListener = ref (fun () -> ())
      let nextTreeListener = ref (fun () -> ())
      let resultCountListener = ref (fun () -> ())
      let defineNumberTreesListener = ref (fun () -> ())
      let createTextListener = ref (fun () -> ())
      let defineInformationBoxListener = ref (fun () -> ())
      let text = ref ""

    end
