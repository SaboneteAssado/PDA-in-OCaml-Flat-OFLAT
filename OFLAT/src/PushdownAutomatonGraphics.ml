open Js_of_ocaml
open OCamlFlat
open BasicTypes
open Graphics
open Lang
open JS
open Js.Opt
open Random

(**EZjs_cytoscape*)
  class type position =
    object
      method x : int Js.readonly_prop
      method y : int Js.readonly_prop
    end

  class type ['a] style =
    object
      method selector : Js.js_string Js.t Js.readonly_prop
      method style : 'a Js.readonly_prop
    end

  module DataItem =
  struct

    class type data =
      object
        method id : Js.js_string Js.t Js.prop
        method source : Js.js_string Js.t Js.prop
        method target : Js.js_string Js.t Js.prop
        method nodeType : Js.js_string Js.t Js.prop
        method label : Js.js_string Js.t Js.prop
      end

    class type t =
      object
        method data : data Js.t Js.prop
        method group : Js.js_string Js.t Js.prop
        method classes : Js.js_string Js.t Js.prop
        method position : position Js.t Js.prop
        method renderedPosition : position Js.t Js.prop
        method addClass : Js.js_string Js.t -> unit Js.meth
        method removeClass : Js.js_string Js.t -> unit Js.meth
      end
      
  end

  class type layout_options =
    object
      method name : Js.js_string Js.t Js.readonly_prop
    end

  class type layout =
    object
      method run : unit Js.meth
    end
    
  class type props =
    object
      method container : Dom_html.element Js.t Js.prop
      method elements : DataItem.t Js.t Js.js_array Js.t Js.prop
      method style : Js.Unsafe.any style Js.t Js.js_array Js.t Js.prop
      method layout : layout_options Js.t Js.prop
      method zoom : int Js.prop
      method pan : position Js.t Js.prop
      method minZoom : float Js.prop
      method maxZoom : float Js.prop
      method zoomingEnabled : bool Js.t Js.prop
      method userZoomingEnabled : bool Js.t Js.prop
      method panningEnabled : bool Js.t Js.prop
      method userPanningEnabled :bool Js.t Js.prop
      method boxSelectionEnabled : bool Js.t Js.prop
      method selectionType : Js.js_string Js.t Js.prop
      method touchTapThreshold : int Js.prop
      method desktopTapThreshold : int Js.prop
      method autolock : bool Js.t Js.prop
      method autoungrabify : bool Js.t Js.prop
      method autounselectify : bool Js.t Js.prop
      method headless : bool Js.t Js.prop
      method styleEnabled : bool Js.t Js.prop
      method hideEdgesOnViewport : bool Js.t Js.prop
      method textureOnViewport : bool Js.t Js.prop
      method motionBlur : bool Js.t Js.prop
      method motionBlurOpacity : float Js.prop
      method wheelSensitivity : float Js.prop
      method pixelRatio : Js.js_string Js.t Js.prop
    end

  class type cytoscape =
    object
      method add : DataItem.t Js.t -> unit Js.meth
      method remove : DataItem.t Js.t -> unit Js.meth
      method mount : Dom_html.element Js.t -> unit Js.meth
      method layout : layout_options Js.t -> layout Js.t Js.meth
      method resize : unit Js.meth
      method on : Js.js_string Js.t -> Js.js_string Js.t -> (Dom_html.event Js.t -> unit) -> unit Js.meth
      method getElementById: Js.js_string Js.t -> DataItem.t Js.t Js.meth
    end

  type cytoscape_cs = (props Js.t -> cytoscape Js.t) Js.constr

  let cytoscape_cs : cytoscape_cs = Js.Unsafe.variable "cytoscape"

  let default_style : Js.Unsafe.any style Js.t Js.js_array Js.t =
    let nodeStyle = Js.Unsafe.coerce @@ object%js
        val selector = Js.string "node"
        val style = Js.def (object%js
            val label = Js.string "data(id)"
            val textValign = Js.string "center";
            val textHalign = Js.string "center";
			end)
		end in
	let finalStyle = Js.Unsafe.coerce @@ object%js
        val selector = Js.string ".SUCCESS"
        val style = Js.def (object%js
            val borderWidth = Js.string "7px";
            val borderColor = Js.string "black";
            val borderStyle = Js.string "double";
			end)
		end in
	let hiddenStyle = Js.Unsafe.coerce @@ object%js
        val selector = Js.string ".HIDDEN"
        val style = Js.def (object%js
            val visibility = Js.string "hidden";
			end)
		end in
	let edgeStyle = Js.Unsafe.coerce @@ object%js	
		val selector = Js.string "edge"
        val style = Js.def (object%js
			val textWrap = Js.string "wrap";
            val label = Js.string "data(label)";
            val curveStyle = Js.string "bezier";
            val targetArrowShape = Js.string "triangle";
			end)
		end in
	Js.array [| nodeStyle;edgeStyle;finalStyle;hiddenStyle |]

  let default_layout : layout_options Js.t =
    object%js val name = Js.string "preset" end

  let position x y : position Js.t =
    object%js val x = x val y = y end

  let node id ?pos classes nType label: DataItem.t Js.t =
    let data : DataItem.data Js.t = Js.Unsafe.obj [||] in
    data##.id := Js.string id;
    data##.nodeType := Js.string nType;
    data##.label := Js.string label;
    let node : DataItem.t Js.t = Js.Unsafe.obj [||] in
    node##.data := data;
    (match pos with 
		| None -> 
			node##.position := position (Random.int 1399) (Random.int 299) 
		| Some "i" ->
			node##.position := position 100 200
		| Some "h" ->
			node##.position := position 0 200);
    node##.group := Js.string "nodes";
    node##.classes := Js.string classes;
    node
	
  let edge ?id source target label : DataItem.t Js.t =
    let data : DataItem.data Js.t = Js.Unsafe.obj [||] in
    (match id with None -> () | Some id -> data##.id := Js.string id);
    data##.source := Js.string source;
    data##.target := Js.string target;
    data##.label := Js.string label;
    let edge : DataItem.t Js.t = Js.Unsafe.obj [||] in
    edge##.data := data;
    edge##.group := Js.string "edges";
    edge

  let mk_graph ?(style=default_style) ?(layout=default_layout) ?(props=[]) container_id =
    let container = Dom_html.getElementById container_id in
    let props = Js.array @@ Array.of_list props in
    let g : props Js.t = Js.Unsafe.obj [||] in
    g##.container := container;
    g##.elements := props;
    g##.style := style;
    g##.layout := layout;
    g

  let display props = new%js cytoscape_cs props

  let add_node g id ?pos classes nodeType label  =
    g##add (node id ?pos classes nodeType label )

  let add_edge g source target label =
    g##add (edge source target label )

  let random_layout g : layout Js.t =
    let layout_opt = object%js
      val name = Js.string "random"
    end in
    g##layout layout_opt

  let run_layout (l : layout Js.t) =
    l##run

  let on cy event selector cb =
    cy##on (Js.string event) (Js.string selector) cb
    
module type PushdownAutomatonGraphicsSig = sig

	type transition = PushdownAutomaton.transition
	type transitions = PushdownAutomaton.transitions
	
	type t =  PushdownAutomaton.t
	
	val modelDesignation : unit -> string
	
	class model :
		t Arg.alternatives ->
			object
				method kind : string
				method description : string
				method name : string
				method errors : string list
				method handleErrors : unit
				method validate : unit
				method toJSon: JSon.t

				method nextConfs: state*word -> symbol -> transitions -> (state*word) Set.t
				method accept: word -> bool
				method generate: int -> words
				method tracing: unit
				method checkExercise: Exercise.exercise -> bool
				method checkExerciseFailures: Exercise.exercise -> (words * words)
				
				method getReachableStates: states
				method getProductiveStates: states
				method getUsefulStates: states
				method getUsefulTransitions: transitions
				method getUsefulSymbols: symbols
				method getUsefulStackSymbols: symbols
				method clean: PushdownAutomaton.model
				
				
				method representation: t
				
				method isDeterministic : bool	
				
				(* ------------------------ *)
				method startPDA: string -> cytoscape Js_of_ocaml.Js.t
				method areAllStatesUseful: bool
				method cleanUselessStates: model
				
				method paintProductive: unit
				method paintReachable: unit
				method paintUseful: unit
				
				method numberStates: int
				method numberTransitions: int
				
				method startAccept: string -> unit
				method next: word*word
				method back: word*word
				
				method resetColors: unit
				method getConfs: (state*word*word) Set.t
				method getNextConfs: (state*word*word) -> transitions -> (state*word*word) Set.t
				method autoAccept: string -> unit
				
	end
end

module rec PushdownAutomatonGraphics : PushdownAutomatonGraphicsSig =
	struct
		
	type transition = PushdownAutomaton.transition
	type transitions = PushdownAutomaton.transitions
	
	type t =  PushdownAutomaton.t

	let modelDesignation () = PushdownAutomaton.modelDesignation
	
	let delay n =
		Js_of_ocaml_lwt.Lwt_js.sleep (float_of_int n *. 0.01)
	
	let rec combAux (a,b,c,d,e) l =
		match l with
		| [] -> [((a,d),[(b,c,e)])]
		| ((x,y),z)::xs ->
			if a = x && d = y then
				((a,d),(b,c,e)::z)::xs
			else
				((x,y),z)::combAux (a,b,c,d,e) xs
	
	let rec combAutomaton ts l =
		match ts with
		| [] -> l
		| x::xs ->
			combAutomaton xs (combAux x l)
	
	let rec defineStates cy states aStates criteria iState =
		match states with
		| [] -> ()
		| x::xs ->
			if Set.belongs x aStates && criteria then
				add_node cy x "SUCCESS" "" ""
			else if x = iState then
				add_node cy x ~pos:"i" "" "" ""
			else
				add_node cy x "" "" "";
			defineStates cy xs aStates criteria	iState
	
	let get35AsString c e =
			let cChanged = 
				if c = epsilon then "ε"
				else Util.ch2str c
			in
			let eChanged = 
				if e = [] then "ε"
				else Util.word2str e
			in
			(cChanged,eChanged)
	
	let rec getLabel b =
		match b with
		| [] -> ""
		| (b,c,e)::xs ->
			let (cChanged,eChanged) = get35AsString c e in
			cChanged ^ "; "^ Util.ch2str b ^ "/" ^ eChanged ^ "\n" ^ getLabel xs

	let rec defineTransitions cy ts = 
		match ts with
		| [] -> ()
		| ((a,d),z)::xs ->
			add_edge cy a d (getLabel z);
			defineTransitions cy xs
	
	(* define all states and transitions of the automaton in Cytoscape *)
	let defineStatesTrans cy states trans aStates criteria iState =
					(* TODO depende do criteria *)
					defineStates cy (Set.toList states) aStates criteria iState;
						
					
					let straightenedTrans = combAutomaton (Set.toList trans) [] in
					defineTransitions cy straightenedTrans
	
	(* paints nodes*)
	let iterateList meth list color=
		List.iter (fun el -> (meth el color) ) list
		
	(* color list *)
	let productiveColor = "orange"
	let reachableColor = "yellow"
	let usefulColor = "purple"
	let stepStateColor = "blue"
	let acceptStateColor = "green"
	let resetColor = "gray"
	let wrongFinalState = "red"

	(* paints a state *)
	let paint state color =
		Graphics.paintNode state color
	
	(* resets all colors to gray aka initial color *)
	let resetColor sts = 
		Set.iter( fun x -> paint x resetColor) sts
	
	let paintExplored confs acceptSts criteria =
		let isAccept = ref false in
		let stack = List.flatten (List.map (fun (_,b,_) -> b ) (Set.toList confs)) in
		(* accept states criteria *)
		if criteria then (
			Set.iter( fun (a,_,c) ->
				if Set.belongs a acceptSts && c = [] then (
					isAccept := true;
					paint a acceptStateColor
				)
				else
					paint a stepStateColor
			) confs
		)
		(* empty stack criteria *)
		else ( 
			Set.iter( fun (a,b,c) -> 
				if b = [] && c = [] then (
					isAccept := true;
					paint a acceptStateColor
				)
				else
					paint a stepStateColor
			) confs	
		);
		(stack,!isAccept)
	
	class model  (arg: PushdownAutomaton.t Arg.alternatives) =
			object(self)  inherit PushdownAutomaton.model arg as super	
				
				val mutable steps = [||];
				val mutable lengthSteps = 0;
				val mutable position = 0;
				val mutable ending = false;
				
				(* draws the PDA *)
				method startPDA (name: string): cytoscape Js_of_ocaml.Js.t =
					let graph1 = mk_graph name in
					let cy = display graph1 in
					
					(if name = "cy" then
						Js.Unsafe.global##.cy := cy
					else
						Js.Unsafe.global##.cy2 := cy);
					
					let rep = self#representation in
					let states = rep.states in
					let trans = rep.transitions in
					let aStates = rep.acceptStates in
					let criteria = rep.criteria in
					let initSt = rep.initialState in
					
					defineStatesTrans cy states trans aStates criteria initSt;
					add_node cy "hidden" ~pos:"h" "HIDDEN" "" "";
					add_edge cy "hidden" initSt "";
					cy
				
				(* gets all the configurations from a certain step of the acceptance process *)
				method getConfs =
					steps.(position)
						
				(* returns true if all states are useful, false otherwise *)
				method areAllStatesUseful: bool =
					let states = self#representation.states in
					let uStates = self#getUsefulStates in
					if ( Set.diff states uStates = Set.empty ) then
						true
					else
						false
				
				(* returns a clean representation of the automaton *)
				method cleanUselessStates: PushdownAutomatonGraphics.model =
					let clean = super#clean in
					let rep = clean#representation in 
						new PushdownAutomatonGraphics.model (Representation rep)
					
				(* paints the automaton's productive states*)
				method paintProductive: unit =
					resetColor self#representation.states;
					let l = Set.toList self#getProductiveStates in
					iterateList paint l productiveColor
				
				(* paints the automaton's useful states *)
				method paintUseful: unit =
					resetColor self#representation.states;
					let l = Set.toList self#getUsefulStates in
					iterateList paint l usefulColor
				
				(* paints the automaton's states that are reachable  *)
				method paintReachable =
					resetColor self#representation.states;
					let l = Set.toList self#getReachableStates in
					iterateList paint l reachableColor
				
				(* devolve o número de estados*)
				method numberStates: int =
					Set.size self#representation.states
				
				(* devolve o número de transições *)
				method numberTransitions: int =
					Set.size self#representation.transitions
				
				(* inicia o processo de aceitação sequencial *)
				method startAccept w: unit = 
					resetColor self#representation.states;
				
					steps <- Array.make 1000 Set.empty;
					position <- 0;
					lengthSteps <- 0;
					ending <- false;
					
					let iState = self#representation.initialState in
					let iStack = [self#representation.initialStackSymbol] in
					let word = Util.str2word w in
					
					let startStep = Set.make [(iState,iStack,word)] in 
					Array.set steps position startStep;
					
					paint iState stepStateColor
				
				(* devolve as configurações seguintes duma configuração *)	
				method getNextConfs (a,b,c) ts =
					match c with
						| []->
							let confs1 = self#nextConfs (a,b) epsilon ts in
								Set.map ( fun (a,b) -> (a,b,[]) ) confs1
						| x::xs ->
							let confs1 = self#nextConfs (a,b) epsilon ts in
							let confs2 = Set.map ( fun (a,b) -> (a,b,c) ) confs1 in
							let confs3 = self#nextConfs (a,b) x ts in
							let confs4 = Set.map ( fun (a,b) -> (a,b,xs) ) confs3 in
								Set.union confs2 confs4
				
				(* pinta as configurações seguintes e devolve o conteúdo da stack e o que resta consumir da palavra de entrada *)
				method next: word*word =
					let rep = self#representation in
					let acceptSts = rep.acceptStates in
					let confs = steps.(position) in
					(* check if we explored this before *)
					if ending = true then (
						let stack = List.flatten (List.map (fun (_,b,_) -> b ) (Set.toList confs)) in
						let word = List.flatten (List.map (fun (_,_,c) -> c ) (Set.toList confs)) in
							(stack,word)
					)
					else if position < lengthSteps then (
						resetColor rep.states;
						
						position <- position + 1;
						let confs = steps.(position) in
						
						let (stack,wasAccepted) = paintExplored confs acceptSts rep.criteria in
						ending <- wasAccepted;
						let word = List.flatten (List.map (fun (_,_,c) -> c ) (Set.toList confs)) in
							(stack,word)
					)
					else ( (* begin exploring *)
						resetColor rep.states;
						
						let trans = rep.transitions in
						let nextSteps = Set.flatMap (fun x -> self#getNextConfs x trans) confs in
						
						let interConfs = Set.inter confs nextSteps in
						(* not explicit enough, lets same state with word to consume unexplored *)
						if Set.size interConfs = Set.size nextSteps then (
							let exploredStates = List.map (fun (a,_,_) -> a ) (Set.toList confs) in
							List.iter ( fun x -> paint x wrongFinalState) exploredStates;
							
							ending <- true;
							
							let stack1 = List.flatten (List.map (fun (_,b,_) -> b ) (Set.toList confs)) in
							let wordLeft = List.flatten (List.map (fun (_,_,c) -> c ) (Set.toList confs)) in
								(stack1,wordLeft)
						)
						else (	
							position <- position + 1;
							steps.(position) <- nextSteps;
							lengthSteps <- lengthSteps+1;
						
							let (newStack,wasAccepted) = paintExplored nextSteps acceptSts rep.criteria in
							ending <- wasAccepted; 
							let newWordLeft = List.flatten (List.map (fun (_,_,c) -> c ) (Set.toList nextSteps)) in
								(newStack,newWordLeft)
						)
					)
						
				(* retrocede no processo de aceitação e anima os estados presentes nessas configurações *)
				method back: word*word =
					ending <- false;
					if position > 0 then (
						resetColor self#representation.states;
						position <- position - 1;
						
						let confs = steps.(position) in
						let rep = self#representation in
						
						let (_,_) = paintExplored confs rep.acceptStates rep.criteria in
						()
					);
					let confs = steps.(position) in
					
					let stack = List.flatten (List.map (fun (_,b,_) -> b ) (Set.toList confs)) in
					let wordLeft = List.flatten (List.map (fun (_,_,c) -> c ) (Set.toList confs)) in
						(stack,wordLeft)
				
				(* reinicializa as cores do automato *)
				method resetColors: unit =
					resetColor self#representation.states
				
				(* realiza o processo de aceitação sequencial de forma automática *)
				method autoAccept (word:string): unit =
					let rec autoAcceptX () =
						if ending then
							Lwt.return ()
						else
							Lwt.bind (delay 100)
									(fun () -> ignore self#next; autoAcceptX ())
					in
					self#startAccept word;
					ignore (autoAcceptX ())
	end
end
