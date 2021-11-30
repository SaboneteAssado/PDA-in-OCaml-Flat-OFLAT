(*
 * Controller.ml
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
 * Description: Controller component of the application.
 *)


open OCamlFlat
open BasicTypes
open Js_of_ocaml
open JS
open Graphics
open FiniteAutomatonGraphics
open RegularExpressionGraphics
open PushdownAutomatonGraphics
open Lang
open Listeners
open HtmlPageClient
open StateVariables
open Js.Opt

module Controller: sig 
  val defineRegularExpression: RegularExpressionGraphics.model -> unit
  val startGraph: unit -> unit

  val addNode: state -> unit
  val createTransition: state * symbol * state -> unit
  val addFinalNode: state -> unit
  val addInitialNode: state -> unit
  val eliminateTransition: state * symbol * state -> unit
  val turnFinalNode: state -> unit
  val removeFinalNode: state -> unit
  val eliminateNode: state -> unit 

  val getWords: int -> unit
  val accept: string -> unit
  val startStep: string -> unit
  val nextStep: unit -> unit
  val backStep: unit -> unit
  val acceptRegularExpression: string -> unit

  val getNewSentence: unit -> Js.js_string Js.t

  val automatonToRegExp: unit -> unit
  val fromExpressionToAutomaton: unit -> unit

  val printErrors: unit -> unit

  val about: unit -> unit 
  val feedback: unit -> unit

end 
=
  struct
    let listColors = [|"Red"; "Yellow"; "Cyan"; "Green"; "Indigo"; "Blue"; "Magenta"; "Sienna"; "Violet"; "Orange"; "Lime"; "Teal"; "SteelBlue"; "Silver"; "Olive"; "Salmon"; "Crimson"; "Purple"; "DarkKhaki"; "PowderBlue"|]
    let listColorsBig: string array ref = ref [||];;

    Listeners.closeRightListener := 
      fun () -> HtmlPageClient.oneBox();
                StateVariables.cleanCy2Type ();
                HtmlPageClient.clearBox2();
                if (StateVariables.getCy1Type() = StateVariables.getAutomatonType()) then
                  Graphics.resetStyle();;

    Listeners.defineInformationBoxListener :=
		fun () ->	if StateVariables.getCy1Type() = StateVariables.getAutomatonType() then 
						let automaton = StateVariables.returnAutomata() in 
						HtmlPageClient.defineInformationBoxAutomaton();
						let deter = automaton#isDeterministic in 
						HtmlPageClient.getDeterminim deter;
						let min = automaton#isMinimized in 
						HtmlPageClient.getMinimism min;
						let useful = automaton#areAllStatesUseful in
						let uStates = automaton#getUselessStates in 
						HtmlPageClient.getHasUselessStates useful uStates;
						let nStates = automaton#numberStates in 
						HtmlPageClient.getNumberStates nStates;
						let nTransitions = automaton#numberTransitions in
						HtmlPageClient.getNumberTransitions nTransitions
					else ( (* its PDA *) 
					
						let automaton = StateVariables.returnPDA() in 
						HtmlPageClient.defineInformationBoxAutomaton();
						let deter = automaton#isDeterministic in 
						HtmlPageClient.getDeterminim deter;
						let uStates = Set.diff automaton#representation.states automaton#getUsefulStates in 
						HtmlPageClient.putUsefulStatesInfoPDA uStates;
						
						let nStates = automaton#numberStates in 
						HtmlPageClient.getNumberStates nStates;
						let nTransitions = automaton#numberTransitions in
						HtmlPageClient.getNumberTransitions nTransitions
						
						);;

    let startGraph () = 
      StateVariables.changeCy1ToAutomaton();
      if StateVariables.getCy2Type() <> StateVariables.getEnumerationType() then
        (HtmlPageClient.oneBox();
        HtmlPageClient.defineMainTitle (StateVariables.getCy1Type()));
      HtmlPageClient.clearBox1();
      HtmlPageClient.disableButtons (StateVariables.getCy1Type());
      HtmlPageClient.enableAllButtons (StateVariables.getCy1Type());
      Graphics.destroyGraph ();
      HtmlPageClient.putCyAutomataButtons();
      Graphics.startGraph ();
      let example = new FiniteAutomatonGraphics.model (Representation {
        alphabet = Set.empty;
        states = Set.make ["START"]; 
        initialState = "START";
        transitions = Set.empty;
        acceptStates = Set.empty
      }) in
      StateVariables.changeAutomata example;
      (StateVariables.returnAutomata())#drawExample;
      !Listeners.defineInformationBoxListener()
	
	let rec updateTableAux stack table =
		match stack with
		| [] -> ()
		| x::xs ->
			HtmlPageClient.addTableRow table (String.make 1 x) "buttonBox";
			updateTableAux xs table
						
	let rec eventAux table stack =
		match stack with
		| [] -> ()
		| x::xs ->
			let row = table##insertRow 0 in
			row##.innerHTML := Js.string (Util.ch2str x);
			row##.align := Js.string "center";
			eventAux table xs
	
	let definePushdownAutomaton (pda: PushdownAutomatonGraphics.model) =
		StateVariables.changeCy1ToPDA();
      
		HtmlPageClient.oneBox();
		HtmlPageClient.defineMainTitle (StateVariables.getCy1Type());
		HtmlPageClient.clearBox1();
		HtmlPageClient.disableButtons (StateVariables.getCy1Type());
		HtmlPageClient.enableAllButtons (StateVariables.getCy1Type());
		HtmlPageClient.putCyPDAButtons();
		Graphics.destroyGraph();
		
		let actionShowStack =
			fun x ->
				let f = to_option x##.target in
				match f with
				| None ->
					()
				| Some x ->
					let t = Js.Unsafe.coerce x in
					let d = t##data in
					let id = Js.to_string d##.id in
					
					let confs = pda#getConfs in
					let position = ref 0 in
					HtmlPageClient.putCyPDAButtons();
					Set.iter( fun (a,b,c) ->
						if a = id then (
							let table = HtmlPageClient.createTable (Int.to_string !position) (id ^ (Int.to_string !position) ^ " | " ^ Util.word2str c) in
							position := !position + 1;
							eventAux table b;
							HtmlPageClient.putTable table "buttonBox";
							()
						)
						else ()
					) confs
		in
		let cyto = pda#startPDA("cy") in
		if pda#isDeterministic then (
			let table = HtmlPageClient.createTable "stack" "" in
			HtmlPageClient.putTable table "buttonBox"
		)
		else cyto##on (Js.string "click") (Js.string "node") actionShowStack;
		
		StateVariables.changePDA pda;
		
		!Listeners.defineInformationBoxListener();
		Graphics.fit()
	
    let defineExample example =
      StateVariables.changeCy1ToAutomaton();
      if StateVariables.getCy2Type() <> StateVariables.getEnumerationType() then
        (HtmlPageClient.oneBox();
        HtmlPageClient.defineMainTitle (StateVariables.getCy1Type()));
      HtmlPageClient.clearBox1();
      HtmlPageClient.disableButtons (StateVariables.getCy1Type());
      HtmlPageClient.enableAllButtons (StateVariables.getCy1Type());
      Graphics.destroyGraph();
      HtmlPageClient.putCyAutomataButtons ();
      Graphics.startGraph();
      StateVariables.changeAutomata example;
      (StateVariables.returnAutomata())#drawExample;
      !Listeners.defineInformationBoxListener()

    let defineEnum e =
      HtmlPageClient.twoBoxes ();
      HtmlPageClient.clearBox2();
      StateVariables.changeEnum e;
      StateVariables.changeCy2ToEnumeration();
      HtmlPageClient.defineMainTitle (StateVariables.getCy2Type());
      HtmlPageClient.putEnumButton ();
      HtmlPageClient.addEnumTitle();
      let prob = (StateVariables.returnEnum())#representation.problem in
        HtmlPageClient.defineEnumProblem prob;
      HtmlPageClient.addAcceptedTitle ();
      Set.iter (fun el -> HtmlPageClient.createSpanList el "nothing" "inside") (StateVariables.returnEnum())#representation.inside;
      HtmlPageClient.addNonAcceptTitle ();
      Set.iter (fun el -> HtmlPageClient.createSpanList el "nothing" "outside") (StateVariables.returnEnum())#representation.outside;
      HtmlPageClient.addEnumCheckButton ()

    let rec func (re: RegExpSyntax.t) = 
      match re with
        | Plus (l, r) -> "+" ^ func l ^ func r
        | Seq (l, r) -> "." ^ func l ^ func r
        | Star (re) -> "*" ^ func re
        | Symb (b) -> String.make 1 b
        | Empty -> "E"
        | Zero -> "Z"

    let defineRegularExpression example =
      StateVariables.changeCy1ToRegex ();
      if StateVariables.getCy2Type() <> StateVariables.getEnumerationType() then
        (HtmlPageClient.oneBox();
        HtmlPageClient.defineMainTitle (StateVariables.getCy1Type()));
      HtmlPageClient.clearBox1();
      HtmlPageClient.disableButtons (StateVariables.getCy1Type());
      HtmlPageClient.enableAllButtons (StateVariables.getCy1Type());
      StateVariables.changeRe example;
      Graphics.destroyGraph();
      HtmlPageClient.putCyREButtons();
      let test = RegExpSyntax.toString (StateVariables.returnRe())#representation in
      HtmlPageClient.defineRE test;
      let test1 = func ((StateVariables.returnRe())#representation) in 
      let text = (Js.string test1) in
      Graphics.startGraph2(text)

    let addNode node =
      if StateVariables.getCy1Type() = StateVariables.getAutomatonType() then
        (if (Set.belongs node (StateVariables.returnAutomata())#representation.states) then 
          (JS.alertStr (Lang.defineAlertExists !Lang.lang))
        else 
          (StateVariables.changeAutomata ( (StateVariables.returnAutomata())#addNode node false);
          Graphics.createNode node false false;
          !Listeners.defineInformationBoxListener()))
      else
        (if StateVariables.getCy2Type() <> StateVariables.getEnumerationType() then
          StateVariables.changeCy1ToAutomaton ();
          HtmlPageClient.disableButtons (StateVariables.getCy1Type());
          StateVariables.changeAutomata ( (StateVariables.returnAutomata())#addInitialNode node true false);
          Graphics.startGraph ();
          Graphics.createNode node true false;
          !Listeners.defineInformationBoxListener())
      
    let createTransition (v1, c3, v2) =
      if (StateVariables.getCy1Type() <> StateVariables.getAutomatonType()) then
        JS.alertStr (Lang.defineAlertUnexistentState !Lang.lang)
      else
        if (Set.belongs (v1, c3, v2) (StateVariables.returnAutomata())#representation.transitions) then
          JS.alertStr ((Lang.defineAlertTheTransition !Lang.lang) ^ "(" ^ v1 ^ ", " ^ String.make 1 c3 ^ ", " ^ v2 ^ ")" ^ (Lang.defineAlertAlreadyExists !Lang.lang)) 
        else 
          (if (Set.belongs v1 (StateVariables.returnAutomata())#representation.states) != true then
            JS.alertStr (Lang.defineAlertStartState !Lang.lang)
          else 
            if (Set.belongs v2 (StateVariables.returnAutomata())#representation.states) != true then
              JS.alertStr (Lang.defineAlertArrivalState !Lang.lang)
            else 
              ((ignore (StateVariables.changeAutomata ((StateVariables.returnAutomata())#newTransition (v1, c3, v2))));
              Graphics.createEdge (v1, c3, v2);
              !Listeners.defineInformationBoxListener()))
      
    let addFinalNode node =
      if StateVariables.getCy1Type() = StateVariables.getAutomatonType() then 
        (if (Set.belongs node (StateVariables.returnAutomata())#representation.states) then
          (JS.alertStr (Lang.defineAlertExists !Lang.lang))
        else 
          (StateVariables.changeAutomata ( (StateVariables.returnAutomata())#addFinalNode node false false);
          Graphics.createNode node false true))
      else
        (JS.alertStr (Lang.defineAlertWorkingWithAutomata !Lang.lang));
      !Listeners.defineInformationBoxListener()

    let addInitialNode node = (* CHECK *)
      if StateVariables.getCy1Type() = StateVariables.getAutomatonType() then
        let stateExists = Set.belongs node (StateVariables.returnAutomata())#representation.states in 
          StateVariables.changeAutomata ((StateVariables.returnAutomata())#addInitialNode node false stateExists);
        Graphics.destroyGraph ();
        Graphics.startGraph ();
        (StateVariables.returnAutomata())#drawExample
      
      else 
        (if StateVariables.getCy2Type() <> StateVariables.getEnumerationType() then
          (HtmlPageClient.oneBox());
        HtmlPageClient.clearBox1();
        StateVariables.changeCy1ToAutomaton ();
        HtmlPageClient.disableButtons (StateVariables.getCy1Type());
        HtmlPageClient.putCyAutomataButtons();
        StateVariables.changeAutomata ((StateVariables.returnAutomata())#addInitialNode node true false);
        Graphics.startGraph ();
        Graphics.createNode node true false);
      !Listeners.defineInformationBoxListener()

    let eliminateTransition (v1, c3, v2) =
      if (Set.belongs (v1, c3, v2) (StateVariables.returnAutomata())#representation.transitions) then
        (StateVariables.changeAutomata ((StateVariables.returnAutomata())#eliminateTransition(v1, c3, v2));
        Graphics.eliminateEdge (v1, c3, v2);
        !Listeners.defineInformationBoxListener())
      else 
        JS.alertStr ((Lang.defineAlertTheTransition !Lang.lang) ^ "(" ^ v1 ^ ", " ^ String.make 1 c3 ^ ", " ^ v2 ^ ")" ^ (Lang.defineAlertDoNotExists !Lang.lang)) 

    let turnFinalNode node =
      (if (Set.belongs node (StateVariables.returnAutomata())#representation.acceptStates) then
        (JS.alertStr (Lang.defineAlertAlreadyFinal !Lang.lang))
      else
        (StateVariables.changeAutomata ((StateVariables.returnAutomata())#changeToFinal node);
        Graphics.turnFinal (node));
      !Listeners.defineInformationBoxListener()
      )

    let removeFinalNode node =
      (if (Set.belongs node (StateVariables.returnAutomata())#representation.acceptStates) then
        (StateVariables.changeAutomata ((StateVariables.returnAutomata())#removeFinal node);
        Graphics.removeFinal (node))
      else
        (JS.alertStr (Lang.defineAlertAlreadyFinal !Lang.lang)); 
      !Listeners.defineInformationBoxListener()
      )

    let eliminateNodeTransitions (a, b, c) node = 
      if (a = node || c = node) then
        (StateVariables.changeAutomata ((StateVariables.returnAutomata())#eliminateTransition (a, b, c));
        !Listeners.defineInformationBoxListener())
  
    let eliminateNode node =
      if (node = (StateVariables.returnAutomata())#representation.initialState) then 
        JS.alertStr (Lang.defineAlertDelete !Lang.lang) 
      else 
        if (Set.belongs node (StateVariables.returnAutomata())#representation.states) then 
          (let isFinal = Set.belongs node (StateVariables.returnAutomata())#representation.acceptStates in 
          StateVariables.changeAutomata ((StateVariables.returnAutomata())#eliminateNode node false isFinal);
          Set.iter (fun el -> (eliminateNodeTransitions el node)) (StateVariables.returnAutomata())#representation.transitions;
          Graphics.eliminateNode (node);
          !Listeners.defineInformationBoxListener())
        else 
          JS.alertStr (Lang.defineAlertUnexistentState !Lang.lang);;

    Listeners.paintAllProductivesListener :=
      fun () -> if StateVariables.getCy1Type() = StateVariables.getAutomatonType() then
					(Graphics.resetStyle ();
					(StateVariables.returnAutomata())#productivePainting)
				else ( (* its PDA *)
					let pda = StateVariables.returnPDA() in
					pda#paintProductive
				);;

    Listeners.paintAllReachableListener := 
      fun () -> if StateVariables.getCy1Type() = StateVariables.getAutomatonType() then 
					(Graphics.resetStyle ();
					(StateVariables.returnAutomata())#reachablePainting)
                else ( (* its PDA *) 
					let pda = StateVariables.returnPDA() in
					pda#paintReachable
				);;

    Listeners.paintAllUsefulListener :=
      fun () -> if StateVariables.getCy1Type() = StateVariables.getAutomatonType() then 
					(Graphics.resetStyle ();
					(StateVariables.returnAutomata())#usefulPainting)
                else ( (* its PDA *)
					let pda = StateVariables.returnPDA() in
					pda#paintUseful
				);;

    Listeners.cleanUselessListener :=
      fun () -> if StateVariables.getCy1Type() = StateVariables.getAutomatonType() then 
					if ((StateVariables.returnAutomata())#areAllStatesUseful) then 
						JS.alertStr (Lang.defineAlertClean !Lang.lang)
					else (
						HtmlPageClient.twoBoxes();
						HtmlPageClient.putCy2Buttons();
						StateVariables.changeCy2ToAutomaton ();
						StateVariables.changeAutomata1 ((StateVariables.returnAutomata())#cleanUselessStates1);
						(StateVariables.returnAutomata1())#drawExample1)
                else ( (* its PDA *)  
					if ((StateVariables.returnPDA())#areAllStatesUseful) then 
						JS.alertStr (Lang.defineAlertClean !Lang.lang)
					else 
						HtmlPageClient.twoBoxes();
						HtmlPageClient.putCy2Buttons(); 
						
						let pdaLeft = StateVariables.returnPDA() in
						StateVariables.changePDA1 (pdaLeft#cleanUselessStates);
						StateVariables.changeCy2ToAutomaton ();
						let pdaRight = StateVariables.returnPDA1() in
						ignore (pdaRight#startPDA "cy2");
						
						Graphics.fit()
				);;
     
    Listeners.getDeterministicListener :=
      fun () -> if ((StateVariables.returnAutomata())#isDeterministic) then 
                  JS.alertStr (Lang.defineAlertDeterministic !Lang.lang)
                else 
                  (HtmlPageClient.twoBoxes ();
                  Graphics.destroyGraph1 ();
                  StateVariables.changeCy2ToAutomaton ();
                  HtmlPageClient.putCy2Buttons ();
                  Graphics.startGraph1 ();
                  Graphics.fit ();
                  StateVariables.changeAutomata1 ((StateVariables.returnAutomata())#toDeterministic1);
                  (StateVariables.returnAutomata1())#drawExample1);;

    let setColor number =
      if (number <= 20) then 
        listColorsBig := listColors
      else 
        (for i=0 to 19 do 
          Array.set !listColorsBig i (Array.get listColors i)
          done;
        for i=20 to number-1 do
          let newColor = Graphics.getRandom() in 
            Array.set !listColorsBig i newColor
        done);;

    Listeners.defineMinimizedListener :=
      fun () -> if ((StateVariables.returnAutomata())#isDeterministic) then
                  if ((StateVariables.returnAutomata())#isMinimized) then 
                    JS.alertStr (Lang.defineAlertMinimum !Lang.lang)
                  else 
                    (HtmlPageClient.twoBoxes();
                    StateVariables.changeCy2ToAutomaton();
                    HtmlPageClient.putCy2Buttons ();
                    let number = (StateVariables.returnAutomata())#getColors in
                      setColor number;
                    Graphics.startGraph1();
                    Graphics.fit();
                    (StateVariables.returnAutomata())#paintMinimization !listColorsBig;
                    StateVariables.changeAutomata1 ((StateVariables.returnAutomata())#minimize1);
                    (StateVariables.returnAutomata1())#drawMinimize !listColorsBig number)
                else 
                  JS.alertStr (Lang.defineAlertNeedsDeterministic !Lang.lang);;

    Listeners.createTextListener :=
      fun () -> (let txt = !Listeners.text in
        let j = JSon.from_string txt in
          let kind = JSon.field_string j "kind" in
            if FiniteAutomaton.modelDesignation = kind then
              (let fa = new FiniteAutomatonGraphics.model (JSon j) in 
                defineExample fa)
            else
              if RegularExpressionGraphics.modelDesignation() = kind then
                (let re = new RegularExpressionGraphics.model (JSon j) in 
                  defineRegularExpression re)
              else
				if PushdownAutomatonGraphics.modelDesignation() = kind then 
					(let pda = new PushdownAutomatonGraphics.model (JSon j) in 
					definePushdownAutomaton pda)
				else 
					( let enu = new Exercise.exercise (JSon j) in 
					defineEnum enu));;

	(* TODO isto e o botao de fechar *)
    Listeners.changeListener := 
      fun () -> if (StateVariables.getCy2Type() = StateVariables.getAutomatonType()) then
                    (HtmlPageClient.oneBox();
                    HtmlPageClient.clearBox1();
                    StateVariables.changeCy1ToAutomaton ();
                    HtmlPageClient.defineMainTitle (StateVariables.getCy1Type());
                    HtmlPageClient.disableButtons (StateVariables.getCy1Type());
                    Graphics.destroyGraph();
                    HtmlPageClient.putCyAutomataButtons();
                    Graphics.startGraph();
                    StateVariables.changeAutomata (StateVariables.returnAutomata1());
                    (StateVariables.returnAutomata())#drawExample;
                    !Listeners.defineInformationBoxListener();
                    HtmlPageClient.clearBox2();
                    let rexp = Dom_html.getElementById "regExp" in
                      rexp##.innerHTML := Js.string "";
                    StateVariables.cleanCy2Type ()) 
                else 
                  (HtmlPageClient.oneBox ();
                  let cy = Dom_html.getElementById "buttonBox" in
                    cy##.innerHTML := Js.string "";
                  let rexp = Dom_html.getElementById "regExp" in
                    rexp##.innerHTML := Js.string "";
                  let infoBox = Dom_html.getElementById "infoBox" in
                    infoBox##.innerHTML := Js.string "";
                  let title = Dom_html.getElementById "mainTitle" in
                    title##.innerHTML := Js.string "";
                  Graphics.destroyGraph ();
                  StateVariables.cleanCy1Type ();
                  StateVariables.cleanCy2Type ());
                HtmlPageClient.disableButtons (StateVariables.getCy1Type());;

    Listeners.getCompleteAutomatonListener := 
      fun () -> if StateVariables.getCy1Type() = StateVariables.getAutomatonType() then 
					(HtmlPageClient.twoBoxes ();
					HtmlPageClient.clearBox2();
					StateVariables.changeCy2ToInfo ();
					HtmlPageClient.closeInfo();
					let getInfo = JSon.to_string ((StateVariables.returnAutomata())#toJSon) in
					HtmlPageClient.putInfoAutomata getInfo;
					Graphics.fit())
                else ( (* its PDA *)
					HtmlPageClient.twoBoxes();
					HtmlPageClient.clearBox2();
					StateVariables.changeCy2ToInfo();
					HtmlPageClient.closeInfo();
					let getInfo = JSon.to_string ((StateVariables.returnPDA())#toJSon) in
					HtmlPageClient.putInfoAutomata getInfo;
					Graphics.fit()
				);;
      
    Listeners.closeCompleteAutomatonListener :=
      fun () -> HtmlPageClient.oneBox ();
                StateVariables.cleanCy2Type ();
                Graphics.fit();;

    let getWords v = 
      StateVariables.changeCy2ToInfo ();
      HtmlPageClient.twoBoxes();
      HtmlPageClient.putCy2Buttons ();
      let var =
        if (StateVariables.getCy1Type() = StateVariables.getAutomatonType()) then
          (StateVariables.returnAutomata())#generateUntil v
        else if StateVariables.getCy1Type() = StateVariables.getPDAType() then
			let pda = StateVariables.returnPDA() in
			let rec generateUntil n =
				if n = 0 then (
					pda#generate n
				)
				else (
					Set.union (generateUntil (n-1)) (pda#generate n)
				)
			in
			generateUntil v
        else 
          (StateVariables.returnRe())#generate v in 
            HtmlPageClient.putWords var

    let acceptAutomaton word = 
      let w = (StateVariables.returnAutomata())#stringAsList1 word in
        (StateVariables.returnAutomata())#accept3 w

    let wordAsList = ref [];;

    Listeners.resultCountListener := 
      fun () -> 
        if StateVariables.returnResultTree()  then 
          HtmlPageClient.putTreeResult (Lang.defineWordAccepted !Lang.lang)
        else 
          (HtmlPageClient.putTreeResult (Lang.defineWordNotAccepted !Lang.lang));
          let (right, wrong) = (StateVariables.returnRe())#countRightTrees !wordAsList in 
            let textt = (Lang.defineExists !Lang.lang) ^ (string_of_int (right)) ^ (Lang.defineGoodDerivations !Lang.lang) in
              HtmlPageClient.putTreeGoodDerivations textt;
            let textt1 = (Lang.defineExists !Lang.lang) ^ (string_of_int (wrong)) ^ (Lang.defineBadDerivations !Lang.lang) in
              HtmlPageClient.putTreeBadDerivations textt1;;
        
    Listeners.defineNumberTreesListener :=
      fun () -> 
        let pos = (StateVariables.returnRe())#position in 
        let leng = (StateVariables.returnRe())#length in 
        let textt = (string_of_int (pos)) ^ (Lang.defineBy !Lang.lang) ^ (string_of_int (leng)) in 
          HtmlPageClient.putTreeResult textt;;
    
    Listeners.previousTreeListener :=
      fun () -> HtmlPageClient.twoBoxes ();
      Graphics.destroyGraph1();
      HtmlPageClient.putCy2Buttons ();
      let back = (StateVariables.returnRe())#back  in
      let text = (StateVariables.returnRe())#printTree back in 
        !Listeners.resultCountListener();
        !Listeners.defineNumberTreesListener ();
      HtmlPageClient.defineTreeButtons ();
      Graphics.startGraph3 (text);;
    
    Listeners.nextTreeListener := 
      fun () -> HtmlPageClient.twoBoxes ();
      HtmlPageClient.clearBox2();
      HtmlPageClient.putCy2Buttons();
      let next = (StateVariables.returnRe())#next  in
      let text = (StateVariables.returnRe())#printTree next in 
        !Listeners.resultCountListener ();
        !Listeners.defineNumberTreesListener ();
      HtmlPageClient.defineTreeButtons ();  
      Graphics.startGraph3 (text);;


    let acceptRegularExpression word = 
      StateVariables.changeCy2ToVerify ();
      let w = (StateVariables.returnRe())#stringAsList1 word in
        wordAsList := w;
      HtmlPageClient.clearBox2();
      HtmlPageClient.twoBoxes ();
      HtmlPageClient.putCy2Buttons ();
      (StateVariables.returnRe())#startAllTrees w;
      StateVariables.changeResultTree ((StateVariables.returnRe())#accept w);
      if (StateVariables.returnResultTree()) then
        (!Listeners.resultCountListener ();
        let right = (StateVariables.returnRe())#getRightTrees in 
        let text = (StateVariables.returnRe())#printTree right in 
        !Listeners.defineNumberTreesListener ();
        HtmlPageClient.defineTreeButtons ();
        Graphics.startGraph3 (text))
      else 
        (!Listeners.resultCountListener();
        let wrong = (StateVariables.returnRe())#getWrongTrees in 
        let text = (StateVariables.returnRe())#printTree wrong in 
        !Listeners.defineNumberTreesListener ();
        HtmlPageClient.defineTreeButtons ();
        Graphics.startGraph3 (text))

	let updateTable stack =
		HtmlPageClient.deleteTable "stack" "buttonBox";
		let table = HtmlPageClient.createTable "stack" "Current Stack" in
		HtmlPageClient.putTable table "buttonBox";
		updateTableAux stack table

	let accept word =
		if StateVariables.getCy1Type() = StateVariables.getAutomatonType() then
			ignore (acceptAutomaton word)
		else if StateVariables.getCy1Type() = StateVariables.getPDAType() then (
			let pda = StateVariables.returnPDA() in
			if pda#isDeterministic then (
				updateTable [];
				HtmlPageClient.replaceRE (word);
				StateVariables.changeSentence word
			);
			pda#autoAccept word
		)
		else acceptRegularExpression word

    let getNewSentence () = (* had to change sig type here, will later to avoid code rep in calling *)
      Js.string (StateVariables.returnAutomata())#newSentence1
		
    let startStep word =
		if StateVariables.getCy1Type() = StateVariables.getAutomatonType() then
			let pda = StateVariables.returnAutomata() in
			pda#changeTheTestingSentence word;
			pda#startAccept
		else ( (* its pda *)
			let pda = StateVariables.returnPDA() in
			pda#startAccept word;
			if pda#isDeterministic then 
				updateTable [pda#representation.initialStackSymbol];
				HtmlPageClient.replaceRE ("|" ^ word);
				StateVariables.changeSentence word
		)
				
	let rec subList list l =
		match l with
		| 0 -> []
		| _ -> (List.hd list)::(subList (List.tl list) (l-1))
				
	let changeRE stack word =
		updateTable stack;
		let currRE = StateVariables.getSentence() in
		let strSplit = Util.word2str (subList (Util.str2word currRE) (String.length currRE - List.length word)) in
		let newString = strSplit ^ "|" ^ (Util.word2str word) in
		HtmlPageClient.replaceRE (newString)
						
    let nextStep () =
		if StateVariables.getCy1Type() = StateVariables.getAutomatonType() then
			(StateVariables.returnAutomata())#next
		else ( (* its pda *)
			let pda = StateVariables.returnPDA() in
			let (stack,word) = pda#next in
			if pda#isDeterministic then
				changeRE stack word
			else (* NonDet *)
				HtmlPageClient.putCyPDAButtons()
		)	
      
    let backStep () = 
		if StateVariables.getCy1Type() = StateVariables.getAutomatonType() then
			(StateVariables.returnAutomata())#back
		else ( (* its pda *)
			let pda = StateVariables.returnPDA() in
			let (stack,word) = pda#back in
			if pda#isDeterministic then
				changeRE stack word
			else (* NonDet *)
				HtmlPageClient.putCyPDAButtons()
		)

    let defineRegularExpression1 example =
      HtmlPageClient.twoBoxes ();
      HtmlPageClient.clearBox2 ();
      StateVariables.changeCy2ToRegex ();
      HtmlPageClient.putCy2Buttons();
      let text = RegExpSyntax.toString example#representation in
        HtmlPageClient.putCy2Text text

    let automatonToRegExp() =
      if StateVariables.getCy1Type() = StateVariables.getRegexType() then
        JS.alertStr (Lang.defineAlertRegex !Lang.lang)
      else
        (StateVariables.changeCy2ToRegex ();
        let rep = (StateVariables.returnAutomata())#representation in 
        let auto = new FiniteAutomaton.model (Representation {
          alphabet = rep.alphabet;
          states = rep.states;
          initialState = rep.initialState;
          transitions = rep.transitions;
          acceptStates = rep.acceptStates
        }) in 
        let reg = PolyModel.fa2re (auto) in
          let r = reg#simplify in 
          let rep = r#representation in 
          let re = new RegularExpressionGraphics.model (Representation (rep)) in
            defineRegularExpression1 re)

    let fromExpressionToAutomaton () =
      if StateVariables.getCy1Type() = StateVariables.getAutomatonType() then 
        JS.alertStr (Lang.defineAlertAutomaton !Lang.lang)
      else
        (HtmlPageClient.twoBoxes ();
        HtmlPageClient.clearBox2();
        StateVariables.changeCy2ToAutomaton ();
        HtmlPageClient.putCy2Buttons ();
        Graphics.startGraph1();
        Graphics.fit();
        let rep = (StateVariables.returnRe())#representation in 
        let exp = new RegularExpression.model (Representation rep) in 
        let auto = PolyModel.re2fa exp in 
          let maton = auto#representation in
            StateVariables.changeAutomata1 (new FiniteAutomatonGraphics.model (Representation (maton)));
            (StateVariables.returnAutomata1())#drawExample1)

    let checkHelper isWhat = 
      let result = isWhat#checkExercise (StateVariables.returnEnum()) in 
        HtmlPageClient.defineResult result;
        let (insideErrors, outsideErrors) = isWhat#checkExerciseFailures (StateVariables.returnEnum()) in 
          Set.iter (fun el -> 
            if Set.belongs el insideErrors then HtmlPageClient.createSpanList el "error" "inside" 
            else HtmlPageClient.createSpanList el "right" "inside") (StateVariables.returnEnum())#representation.inside;
          Set.iter (fun el -> 
            if Set.belongs el outsideErrors then HtmlPageClient.createSpanList el "error" "outside" 
            else HtmlPageClient.createSpanList el "right" "outside") (StateVariables.returnEnum())#representation.outside;;
                
    Listeners.checkExerciseListener :=
      fun () -> (if StateVariables.getCy1Type() = StateVariables.getRegexType() && StateVariables.getCy2Type() = StateVariables.getEnumerationType() then
        checkHelper (StateVariables.returnRe())
      else
        checkHelper (StateVariables.returnAutomata()));;

    Listeners.clearExerciseListener :=
      fun () -> (if StateVariables.getCy2Type() = StateVariables.getEnumerationType() then
        StateVariables.cleanCy2Type ();
        HtmlPageClient.oneBox ();
        let element = Dom_html.getElementById "cy2" in
          element##.innerHTML := Js.string "");;


          let printErrors () =
            let errors = (StateVariables.returnAutomata())#errors in
            if errors = [] then 
            ()
            else 
              JS.alertStr (String.concat "\n" errors)

  let feedback () =
    HtmlPageClient.oneBox ();
    StateVariables.changeCy1ToFeedback ();
    HtmlPageClient.disableButtons (StateVariables.getCy1Type());
    Graphics.destroyGraph();
    HtmlPageClient.feedback()

  let about () =
    HtmlPageClient.oneBox ();
    StateVariables.changeCy1ToText();
    Graphics.destroyGraph();
    HtmlPageClient.disableButtons (StateVariables.getCy1Type());
    HtmlPageClient.about();;

  Listeners.changeDirectionListener :=  fun () -> Graphics.changeDirection();;
    

  end
