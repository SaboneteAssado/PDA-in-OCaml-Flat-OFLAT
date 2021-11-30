(*
 * Calling.ml
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

(* Description: This a component of the controller that is used for the
 * JavaScript to call OCaml functions in the Controller.
 *)

open OCamlFlat
open Js_of_ocaml
open Graphics
open Controller
open HtmlPageClient
open Lang
open Listeners
open RegularExpressionGraphics
open StateVariables

module JSCallingOCaml =
      struct
        Js.Unsafe.global##.jscode :=
            object%js
              method startGraph =
				Controller.startGraph ()
              
              method startRegex =
                Graphics.startRegex ()
    
              method fitGraph =
                Graphics.fit ()
              
              method generateWords =
                Graphics.getWords()
              
              method testComplete =
                Graphics.complete()
              
              method stepbystep =
                Graphics.startStep ();
                if StateVariables.getCy1Type() = StateVariables.getPDAType() then
					let pda = StateVariables.returnPDA() in
					if pda#isDeterministic = false then
						HtmlPageClient.replaceRE ""
				else ()

			method backwards =
			Controller.backStep();
			if StateVariables.getCy1Type() <> StateVariables.getPDAType() then ( (* code rep *)
				let element = Dom_html.getElementById "regExp" in
				let sentence = Controller.getNewSentence() in 
				element##.innerHTML := sentence
			)
              
			method forward =
			Controller.nextStep();
			if StateVariables.getCy1Type() <> StateVariables.getPDAType() then (
				let element = Dom_html.getElementById "regExp" in
				let sentence = Controller.getNewSentence() in 
				element##.innerHTML := sentence
			)

              method cleanAuto =
                let element = Dom_html.getElementById "regExp" in
                  element##.innerHTML := Js.string "";
                if StateVariables.getCy1Type() = StateVariables.getPDAType() then
					(StateVariables.returnPDA())#resetColors
				else (* its not pda *)
					Graphics.resetStyle ()
                  

              method selectConversions n =
                if n = 1 then
                  (Controller.automatonToRegExp ())
                else 
                  (if n = 2 then
                    Controller.fromExpressionToAutomaton ())

              method selectLang n =
                if n = 1 then
                  (Lang.set_language (Js.string "pt");
                  HtmlPageClient.changeLang())
                else 
                  (if n = 2 then
                    (Lang.set_language (Js.string "en");
                    HtmlPageClient.changeLang()))

              method readFromFile n =
                let str = Js.to_string n in 
                Listeners.text := str;
                !Listeners.createTextListener ()

              method feedback =
                Controller.feedback()
              
              method about =
                Controller.about()

              method tooltipRegex =
                let textBox = Dom_html.getElementById "tooltipDefine" in
                textBox##.innerHTML := Js.string (Lang.tooltipDefine !Lang.lang)
              
              method tooltipStartGraph =
                let textBox = Dom_html.getElementById "tooltipStartGraph" in
                textBox##.innerHTML := Js.string (Lang.tooltipStartGraph !Lang.lang)
              
              method tooltipFitGraph =
                let textBox = Dom_html.getElementById "tooltipFitGraph" in
                textBox##.innerHTML := Js.string (Lang.tooltipFitGraph !Lang.lang)

              method tooltipGenerate =
                let textBox = Dom_html.getElementById "tooltipGenerate" in
                textBox##.innerHTML := Js.string (Lang.tooltipGenerate !Lang.lang)
              
              method tooltipComplete =
                let textBox = Dom_html.getElementById "tooltipComplete" in
                textBox##.innerHTML := Js.string (Lang.tooltipComplete !Lang.lang)
              
              method tooltipStep =
                let textBox = Dom_html.getElementById "tooltipStep" in
                textBox##.innerHTML := Js.string (Lang.tooltipStep !Lang.lang)

              method tooltipClear =
                let textBox = Dom_html.getElementById "tooltipClear" in
                textBox##.innerHTML := Js.string (Lang.tooltipClear !Lang.lang)

              method tooltipConvert = 
                let textBox = Dom_html.getElementById "tooltipConvert" in
                textBox##.innerHTML := Js.string (Lang.tooltipConvert !Lang.lang)

              method tooltipFile = 
                let textBox = Dom_html.getElementById "tooltipFile" in
                textBox##.innerHTML := Js.string (Lang.tooltipFile !Lang.lang)

              method tooltipAbout =
                let textBox = Dom_html.getElementById "tooltipAbout" in
                textBox##.innerHTML := Js.string (Lang.tooltipAbout !Lang.lang)

              method tooltipFeedback =
                let textBox = Dom_html.getElementById "tooltipFeedback" in
                textBox##.innerHTML := Js.string (Lang.tooltipFeedback !Lang.lang)

              method tooltipLang =
              let textBox = Dom_html.getElementById "tooltipLang" in
              textBox##.innerHTML := Js.string (Lang.tooltipLang !Lang.lang)
    
              method m3 n =
                let name = Js.to_string n in
                Controller.eliminateNode name
    
              method m4 node =
                let name = Js.to_string node in
                Controller.addNode name
              
                method m5 node = 
                let name = Js.to_string node in
                Controller.addFinalNode name
              
                method m6 node =
                let name = Js.to_string node in
                Controller.addInitialNode name
    
                method m7 node1 node2 symbol = 
                let name1 = Js.to_string node1 in
                let name2 = Js.to_string node2 in
                let s = Js.to_string symbol in
                  let c4 = String.get s 0 in
                Controller.createTransition (name1, c4, name2)
              
                method m8 node1 node2 symb = 
                let name1 = Js.to_string node1 in
                let name2 = Js.to_string node2 in
                let s = Js.to_string symb in
                  let c4 = String.get s 0 in
                Controller.eliminateTransition (name1, c4, name2)

                method m10 number =
                  let numb = Js.to_string number in
                  let size = int_of_string numb in 
                  Controller.getWords size
    
              method m12 reg =
                let i = Js.to_string reg in
                let re = new RegularExpressionGraphics.model (Representation (RegExpSyntax.parse i)) in 
                Controller.defineRegularExpression re
                
              method m13 complete =
                let v = Js.to_string complete in
                Controller.accept v

			method m14 text = 
			let v = Js.to_string text in
			Controller.startStep v;
			if StateVariables.getCy1Type() <> StateVariables.getPDAType() then (
				let element = Dom_html.getElementById "regExp" in
				let sentence = Controller.getNewSentence() in 
				element##.innerHTML := sentence
			)
    
                method m15 text =
                let name = Js.to_string text in
                Controller.turnFinalNode name
              
              method m16 text =
                let name = Js.to_string text in
                Controller.removeFinalNode name
    
            end
      end
