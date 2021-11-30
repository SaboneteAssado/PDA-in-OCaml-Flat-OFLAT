(* 
 * StateVariables.ml
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
 * Description: Model component of the application.
 *)

open OCamlFlat
open FiniteAutomatonGraphics
open RegularExpressionGraphics
open PushdownAutomatonGraphics

module StateVariables = 
    struct
      (** -------------------------------  State Variables --------------------------------------- **)
      (** Can be "finite automaton", "regular expression", "info", "feedback" or "clean" **)
      let cyType = ref "clean"
  
      (** Can be "finite automaton", "regular expression", "enumeration", "info", "verify" or "clean" **)
      let cy2Type = ref "clean"
  
      let cySize = ref 100
  
      let automata = ref (new FiniteAutomatonGraphics.model (Representation {
        alphabet = Set.empty;
        states = Set.make ["START"]; 
        initialState = "START";
        transitions = Set.empty;
        acceptStates = Set.empty
      }))
  
      let automata1 = ref (new FiniteAutomatonGraphics.model (Representation {
        alphabet = Set.empty;
        states = Set.make ["STARTZ"]; 
        initialState = "STARTZ";
        transitions = Set.empty;
        acceptStates = Set.empty
      }))
	
      let re = ref (new RegularExpressionGraphics.model (Representation Empty))
  
      let enum = ref (new Exercise.exercise (Representation {
        problem = "No exercise yet";
        inside = Set.empty;
        outside = Set.empty;
      }))
  
      let resultTree = ref false
      
      (* ---  NEW  --- *)
      
      let pushdownAutomaton = ref (new PushdownAutomatonGraphics.model (Representation {
        inputAlphabet = Set.empty;
		stackAlphabet = Set.empty;
		states = Set.empty;
		initialState = "";
		initialStackSymbol = 'Z';
		transitions = Set.empty;
		acceptStates = Set.empty;
		criteria = true;
      }))
      
      let pushdownAutomaton1 = ref (new PushdownAutomatonGraphics.model (Representation {
        inputAlphabet = Set.empty;
		stackAlphabet = Set.empty;
		states = Set.empty;
		initialState = "";
		initialStackSymbol = 'Z';
		transitions = Set.empty;
		acceptStates = Set.empty;
		criteria = true;
      }))
	
		let sentence = ref ""
		
      (** -------------------------------  Functions --------------------------------------- **)
      
      let changeAutomata res =
        automata := res 
  
      let returnAutomata () =
        !automata
  
      let returnAutomataRE () =
        automata
  
      let changeAutomata1 res =
        automata1 := res 
  
      let returnAutomata1 () =
        !automata1
  
      let changeRe res =
        re := res 
  
      let returnRe () =
        !re
  
      let returnReNR () =
        re
  
      let changeEnum res =
        enum := res 
  
      let returnEnum () =
        !enum
  
      let changeResultTree res =
        resultTree := res 
  
      let returnResultTree () =
        !resultTree
  
      let changeCy1ToAutomaton () =
        cyType := "finite automaton"
  
      let changeCy1ToRegex () =
        cyType := "regular expression"
  
      let changeCy1ToText () =
        cyType := "info"
  
      let changeCy1ToFeedback () =
        cyType := "feedback"
  
      let cleanCy1Type () =
        cyType := "clean"
  
      let getCy1Type() =
        !cyType
  
      let getAutomatonType() = "finite automaton"
  
      let getRegexType() = "regular expression"
  
      let getEnumerationType() = "enumeration"
  
      let getInfoType() = "info"
  
       let getFeedbackType() = "feedback"
  
      let getVerifyType() = "verify"
  
      let getClean() = "clean"
  
      let changeCy2ToAutomaton () =
        cy2Type := !cyType
        
      let changeCy2ToRegex () =
        cy2Type := "regular expression"
  
      let changeCy2ToEnumeration () =
        cy2Type := "enumeration"
  
      let changeCy2ToInfo () =
        cy2Type := "info"
      
      let changeCy2ToVerify () =
        cy2Type := "verify"
  
      let cleanCy2Type () =
        cy2Type := "clean"
  
      let getCy2Type() =
        !cy2Type
  
      let completeSize() =
        cySize := 100
      
      let halfSize() =
        cySize := 45
        
        (* ---------------- *)
        
		let changeCy1ToPDA() =
			cyType := PushdownAutomaton.modelDesignation
			
		let getPDAType() = PushdownAutomaton.modelDesignation
		
		let changePDA res =
			pushdownAutomaton := res
			
		let changePDA1 res =
			pushdownAutomaton1 := res 
			
		let returnPDA () =
			!pushdownAutomaton
		
		let returnPDA1 () =
			!pushdownAutomaton1
			
		let changeSentence str =
			sentence := str
        
        let getSentence ()=
			!sentence
			
end
