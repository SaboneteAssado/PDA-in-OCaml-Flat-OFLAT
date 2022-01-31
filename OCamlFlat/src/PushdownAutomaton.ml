module type PushdownAutomatonSig = sig

	type transition = state * symbol * symbol * state * symbol list
	type transitions = transition set
	
	type t = {
		inputAlphabet : symbols;
		stackAlphabet : symbols;
		states : states;
		initialState : state;
		initialStackSymbol : symbol;
		transitions : transitions;
		acceptStates : states;
		criteria: bool; (* true = acceptStates | false = emptyStack *) 
	}
	val modelDesignation : string
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
				method accept : word -> bool
				method generate : int -> words
				method tracing : unit
				method checkExercise : Exercise.exercise -> bool
				method checkExerciseFailures : Exercise.exercise -> words * words
				
				method getReachableStates: states
				method getProductiveStates: states
				method getUsefulStates: states
				method getUsefulTransitions: transitions
				method getUsefulSymbols: symbols
				method getUsefulStackSymbols: symbols
				method clean: model
				
				method representation: t
				
				method isDeterministic: bool
			
			end
end

module PushdownAutomaton : PushdownAutomatonSig =
struct

	type transition =
		state	(* state *)	
		* symbol	(* current symbol on top of the stack *)
		* symbol	(* consumed input symbol *)
		* state	(* next state *)
		* symbol list	(* new top of stack*)

	type transitions = transition set

	type t = {
		inputAlphabet: symbols;		(* Input Alphabet *)
		stackAlphabet: symbols;		(* Stack Alphabet *)
		states: states;				(* States *)
		initialState: state;		(* Initial state *)
		initialStackSymbol: symbol;	(* Initial Symbol on the Stack *)
		transitions: transitions;	(* Transition relation *)
		acceptStates: states;		(* Accept states *)
		criteria: bool				(* true = acceptStates | false = emptyStack *)
	}

	let modelDesignation = "pushdown automaton"

	(*------Auxiliary functions---------*)

	(* get start state, current stack symbol, input symbol, end state and new stack symbol of all transitions in set  *)
	let transitionGet1 trns = Set.map ( fun (a,_,_,_,_) -> a ) trns
	let transitionGet2 trns = Set.map ( fun (_,b,_,_,_) -> b ) trns
	let transitionGet3 trns = Set.map ( fun (_,_,c,_,_) -> c ) trns
	let transitionGet4 trns = Set.map ( fun (_,_,_,d,_) -> d ) trns
	let transitionGet5 trns = Set.map ( fun (_,_,_,_,e) -> e ) trns
	
	let proj12of5 (a,b,_,_,_) = (a,b)
	let proj3of5 (_,_,c,_,_) = c
	
	let proj2of2 (_,b) = b
	let proj1of2 (a,_) = a
	
		
	let select3 s x y =
		Util.flatMap ( fun (a,b,c,_,_) -> if x = a && y = b then [c] else [] ) (Set.toList s)
							
	(* get all trans with a certain state, top stack symbol and input *)	
	let select123 x y z trans =
		Set.filter ( fun (a,b,c,_,_) -> x = a && y = b && z = c ) trans
		
	(* get all trans with a certain state and top stack symbol *)	
	let select12 x y trans =
		Set.filter ( fun (a,b,_,_,_) -> x = a && y = b) trans
		
	(* get all trans with a certain state and top stack symbol *)	
	let select1 x trans =
		Set.filter ( fun (a,_,_,_,_) -> x = a) trans
			
	(* cuts graph during exploration (partition) *)
	let rec gcut sts ts =
		Set.partition (fun (a,_,_,_,_) -> Set.belongs a sts) ts		
	
	(* what states are reachable from a group of states *)
	let rec reachable sts ts =
		let (tsy,tsn) = gcut sts ts in
		if tsy = Set.empty then
			sts
		else
			Set.union sts (reachable (Set.map ( fun (_,_,_,d,_) -> d ) tsy) tsn)
		
	(* checks if state s reaches any of the states*)		
	let reachStates s sts ts =	
			Set.inter (reachable (Set.make [s]) ts) sts
			
	class model (arg: t Arg.alternatives) =
		object(self) inherit Model.model arg modelDesignation

			val representation: t =
				let j = Arg.fromAlternatives arg in
					if j = JSon.JNull then
						Arg.getRepresentation arg
					else
						let inputAlphabet = JSon.field_char_set j "inputAlphabet" in
						let stackAlphabet = JSon.field_char_set j "stackAlphabet" in
						let states = JSon.field_string_set j "states" in
						let initialState = JSon.field_string j "initialState" in
						let initialStackSymbol = JSon.field_char j "initialStackSymbol" in
						let transitions = JSon.field_quintuplets_set j "transitions" in
						let acceptStates = JSon.field_string_set j "acceptStates" in
						let criteria = JSon.field_string j "criteria" = "true" in
							{	
								inputAlphabet = inputAlphabet;
								stackAlphabet = stackAlphabet;
								states = states;
								initialState = initialState;
								initialStackSymbol = initialStackSymbol;
								transitions = transitions;
								acceptStates = acceptStates;
								criteria = criteria
							}

			initializer self#handleErrors	(* placement is crucial - after representation *)

			method representation =
				representation

			method toJSon: JSon.t = 
			
				let open JSon in
				let rep = representation in
				JAssoc [
					("kind", JString self#kind);
					("description", JString self#description);
					("name", JString self#name);
					("inputAlphabet", JList (List.map (fun c -> JString (Util.ch2str c)) (Set.toList rep.inputAlphabet)));
					("stackAlphabet", JList (List.map (fun c -> JString (Util.ch2str c)) (Set.toList rep.stackAlphabet)));
					("states", JList (List.map (fun s -> JString s) (Set.toList rep.states)));
					("initialState", JString rep.initialState);
					("initialStackSymbol", JString (Util.ch2str rep.initialStackSymbol));
					("transitions", JList (List.map (fun (a,b,c,d,e) ->
						JList [JString a; JString (Util.ch2str b); JString (Util.ch2str c); JString d; JString (Util.word2str e)]) (Set.toList rep.transitions)));
					("acceptStates", JList (List.map (fun s -> JString s) (Set.toList rep.acceptStates)));
					("criteria", JString (Bool.to_string rep.criteria))
				]

			(**
			* This method verifies if the automaton is valid.
			* An automaton is considered valid if:
			*	o its initial and acceptance states belong to the set of all its states
			* 	o all its transitions have states and input and stack symbols belonging to the set of all its states and input and stack alphabet respectively.
			*
			* Desc: If the automaton is invalid, the cause could derive from any combination of the following
			* three options: either the initial state, one of the acceptance states, or one of the transitions does not follow the
			* previously discussed predicate. This method will print to the console stating which combination of these options caused
			* the automaton to be invalid
			*)
			(* TODO add check -> true or false for criteri *)
			method validate: unit = (

				(* does initial state belong to the set of all states *)
				let validInitSt = Set.belongs representation.initialState representation.states in

				(* does initial stack symbol belong to the set of all stack symbols *)
				let validInitStackSy = Set.belongs representation.initialStackSymbol representation.stackAlphabet in
				
				(* is criteria acceptable_*)
				let validAccSts = (representation.criteria = true
									&& Set.subset representation.acceptStates representation.states) 
									|| 
									(representation.criteria = false 
									&& representation.acceptStates = Set.empty) in

				(* get info from the automaton *)
				let fromState = transitionGet1 representation.transitions in
				let stackSymbol = transitionGet2 representation.transitions in
				let inputSymbol = transitionGet3 representation.transitions in
				let toState = transitionGet4 representation.transitions in
				let newStackSymbols = transitionGet5 representation.transitions in
				
				(* add epsilon to both input and stack alphabet *)
				let beta = Set.add epsilon representation.inputAlphabet in

				(* do all transitions have states belonging to all states and symbols belonging to the alphabet *)
				let validTrns =
					Set.subset fromState representation.states && fromState <> Set.empty
					&& Set.subset stackSymbol representation.stackAlphabet && stackSymbol <> Set.empty
					&& Set.subset inputSymbol beta && inputSymbol <> Set.empty
					&& Set.subset toState representation.states && toState <> Set.empty
					&& Set.for_all
						(fun l -> Set.subset (Set.make l) representation.stackAlphabet)
						newStackSymbols in

				if not validInitSt then
					Error.error representation.initialState
						"initial state does not belong to the set of all states" ()
				;
				
				if not validInitStackSy then
					Error.error self#name
						"initial stack symbol is invalid" ()
				;
				
				if not validAccSts then
					Error.error self#name
						"invalid criteria" ()
				;

				if not validTrns then
					Error.error self#name
						"not all transitions are valid" ()
				)

			method tracing : unit = ()
			
			(**
			* This method applies a transition
			*
			* @param (state,stack) -> configuration we want to explore
			* @param sy -> symbol tobe consumed to apply the transition
			* @param trans -> set of transitions to be applied to the configuration
			*
			* @returns (state*word) Set.t -> set of all the resulting configurations after the transitions are applied
			*
			* Desc: returns the resulting configurations of apllying a transition to a configuration
			*)					
			method nextConfs (state,stack) sy trans: (state*word) Set.t =
				if stack = [] then
					Set.make []
				else 
					let ts = select123 state (List.hd stack) sy trans in
						Set.map ( fun (_,_,_,d,e) -> (d,e@(List.tl stack)) ) ts
			
			(**
			* This method verifies if the given word is accepted by the automaton
			*
			* @param w:word -> word to be accepted
			*
			* @returns bool -> true if w is accepted and false otherwise
			*
			* Desc: Checks if the automaton accepts word w using functions over sets of states
			*)
			
			method accept(w: word): bool = 

				let ist = representation.initialState in
				let trans = representation.transitions in 
				let istack = [representation.initialStackSymbol] in
				let iconf = Set.make [(ist,istack,w)] in
				let acceptSts = representation.acceptStates in
				let criteria = representation.criteria in
						
				let applyTransitions conf isy =
					self#nextConfs conf isy trans in
					
				let eApplyTransitions conf =
					self#nextConfs conf epsilon trans in
				
				let nextConfsZ (a,b,c) =
					match c with
						| []->
							let confs1 = eApplyTransitions (a,b) in
								Set.map ( fun (a,b) -> (a,b,[]) ) confs1
						| x::xs ->
							let confs1 = eApplyTransitions (a,b) in
							let confs2 = Set.map ( fun (a,b) -> (a,b,c) ) confs1 in
							let confs3 = applyTransitions (a,b) x in
							let confs4 = Set.map ( fun (a,b) -> (a,b,xs) ) confs3 in
								Set.union confs2 confs4
				in
					
				let finished confs =
					if criteria then
						Set.exists ( fun (a,_,w) -> w = [] && Set.belongs a acceptSts ) confs
					else
						Set.exists ( fun (_,b,w) -> w = [] && b = [] ) confs	
				in
							
				let rec acceptX confs =
					if finished confs then 
						true
					else
						let confs1 = Set.flatMap ( fun c -> nextConfsZ c ) confs in
						let interConfs = Set.inter confs confs1 in
						(* repeated conf detection *)
						if Set.size interConfs = Set.size confs1 then 
							false
						else
							acceptX confs1
				in 
				
				acceptX iconf
				
			(**
			* This method generates all words of the given size which are accepted by the automaton
			*
			* Precondition -> length >= 0
			*
			* @param length:int -> size of all words to be generated
			*
			* @returns words -> the set of all words with size length
			*)
			method generate (length: int): words =
				
				let ist = representation.initialState in
				let trans = representation.transitions in 
				let istack = [representation.initialStackSymbol] in
				let iconf = (ist,istack) in
				let acceptSts = representation.acceptStates in
				
				(* generate all pairs from confs and returns as [(sy,(state,stack));...])] *)	
				let rec explore (state,stack) ts =
					if stack = [] then
						Set.make []
					else (
						let toExplore = select12 state (List.hd stack) ts in 
							Set.flatMap (fun (_,_,c,_,_) -> Set.map (fun conf -> (c,conf)) (self#nextConfs (state,stack) c ts)) toExplore
					)
				in
				
				(* aux fun to do e-transitions *)
				let rec closure visited conf =
					(* filter confs derived by e symbol *)
					let eTrans = Set.filter ( fun (_,_,c,_,_) -> c = '~' ) trans in
					let neighbors = explore conf eTrans in
					let newStates = Set.map (fun x -> proj1of2 (proj2of2 x)) neighbors in
					if Set.subset newStates visited then
						Set.union (Set.make [conf]) (Set.map snd neighbors)
					else
						Set.union (Set.make [conf]) (Set.flatMap ( fun x -> closure (Set.union visited newStates) (proj2of2 x) ) neighbors)
				in
				
				let rec makePairs sy confs =
					Set.map (fun conf -> (sy,conf)) confs
				in
				
				(* generate all possible confs of size l *)
				let rec genX (state,stack) n =
					if n = 0 then
						if representation.criteria then
							if Set.belongs state acceptSts then
								Set.make [[]]
							else
								Set.empty
						else
							if stack = [] then
								Set.make [[]]
							else
								Set.empty
					else
						let fTrans = Set.filter ( fun (_,_,c,_,_) -> c <> '~' ) trans in
						let neighbors = explore (state,stack) fTrans in
						let pairs = Set.flatMap (fun (sy,conf) -> makePairs sy (closure Set.empty conf)) neighbors  in
							Set.flatMap (fun (sy,conf) -> Set.map (fun w -> sy::w) (genX conf (n-1)) ) pairs
				in
			
				let confs = closure Set.empty iconf in
					Set.flatMap ( fun conf -> genX conf length ) confs
			
			(**
			* This method generates all all states from the automaton that are reached from the initial state
			*
			* @returns states -> the set of all states that meet said condition
			*)
			method getReachableStates: states = 
				let ts = representation.transitions in
				let iSt = representation.initialState in
				let sts = representation.states in
					reachStates iSt sts ts
			
			(**
			* This method generates all all states from the automaton that reach an accept state
			*
			* @returns states -> the set of all states that meet said condition
			*)
			method getProductiveStates: states = 
				let ts = representation.transitions in
				let aSts = representation.acceptStates in
				let sts = representation.states in
				let isy = representation.initialStackSymbol in
				if representation.criteria then
					Set.filter ( fun x -> reachStates x aSts ts <> Set.empty ) sts
				else
					let eStackTrans = Set.filter ( fun (_,b,c,_,e) -> b = isy && c = epsilon && e = [] ) ts in
					let eStackSts = Set.flatMap (fun (a,_,_,d,_) -> Set.make ([a]@[d]) ) eStackTrans in
						Set.filter ( fun x -> reachStates x eStackSts ts <> Set.empty ) sts
			
			(**
			* This method generates all states that reach an accept state and can be reached from the initial state
			*
			* @returns states -> the set of the states that meet the condition
			*) 
			method getUsefulStates: states =
				let ts = representation.transitions in
				let is = representation.initialState in
					Set.inter self#getProductiveStates (reachable (Set.make [is]) ts)
				
			(**
			* This method generates all productive transitions. A transition is productive it exists a productive state in the transition
			*
			* @returns states -> the set of all productive states
			*)
			method getUsefulTransitions: transitions =
				let ts = representation.transitions in
				let usfSts = self#getUsefulStates in
					Set.filter( fun (a,_,_,d,_) -> Set.belongs a usfSts && Set.belongs d usfSts) ts
	
			(**
			* This method generates all productive input symbols. A symbol is productive if it exists in a productive transition
			*
			* @returns states -> the set of all productive input symbols
			*)
			method getUsefulSymbols: symbols =
				let trans = self#getUsefulTransitions in
					Set.remove '~' (Set.map ( fun (_,_,c,_,_)-> c) trans)
				
			(**
			* This method generates all productive states. A state is productive if there exists a word that will lead said state
			* to an acceptance state.
			*
			* @returns symbols -> the set of all productive stack symbols
			*)
			method getUsefulStackSymbols: symbols = 
				let trans = self#getUsefulTransitions in
					Set.flatMap ( fun (_,b,_,_,e)-> Set.make (b::e) ) trans
			
			(**
			* This method verifies if the automaton is deterministic
			*
			* @returns bool -> true if automaton is deterministic, false otherwise
			*
			* Desc: For each state s, this method checks if there exists 2 or more transitions with the same symbol from any
			* state belonging to closeempty of s, independently of the state which said transitions will lead to.
			* If there is no state for which this property is true, then the automaton is deterministic
			*)
			method isDeterministic: bool =
				
				let checkDetCriteria l =
					Set.size (Set.make l) = List.length l
					&& (l = [epsilon] || not (List.mem epsilon l) )
				in
				
				let c12 = Set.map ( fun (a,b,_,_,_) -> (a,b) ) representation.transitions in
				
					Set.for_all (fun (a,b) -> checkDetCriteria (select3 representation.transitions a b) ) c12
					
			(**
			* This method creates the equivalent automaton where all states are useful
			*
			* @returns PDA.model -> the new equivalent automaton where all of its definition is productive
			*
			* Desc: The new automaton is created by eliminating from the original automaton all its non useful states, all transitions
			* that have a non useful state, and all symbols of the alphabet that only appear in said transitions
			*)

			method clean: model =
			
				let usfStates = self#getUsefulStates in
				let usfTrans = self#getUsefulTransitions in
				let usfSymbols = self#getUsefulSymbols in
				let usfStackSymbols = self#getUsefulStackSymbols in

				new model (Arg.Representation {
								inputAlphabet = usfSymbols;
								stackAlphabet = usfStackSymbols;
								states = usfStates;
								initialState = representation.initialState;
								initialStackSymbol = representation.initialStackSymbol;
								transitions = usfTrans;
								acceptStates = representation.acceptStates;
								criteria = representation.criteria
						} )
			
	end
end
	
module PushdownAutomatonTests : sig end =
struct
	let active = true

	let pda_loop = {| {
		kind : "pushdown automaton",
		description : "0n1n deterministic, n>0, loop to test cycle",
		name : "dpda-0n1n",
		inputAlphabet : ["0", "1"],
		stackAlphabet : ["A", "Z"],
		states : ["START", "A", "SUCCESS"],
		initialState : "START",
		initialStackSymbol : "Z",
		transitions : [
				["START","Z","~", "START", "AZ"],
				["START","A","~", "START", "AA"]
			],
		acceptStates : ["SUCCESS"],
		criteria : "true"
	} |}

	let pda_eLoop = {| {
		kind : "pushdown automaton",
		description : "eLoop",
		name : "eLoop",
		inputAlphabet : ["0", "1"],
		stackAlphabet : ["A", "Z"],
		states : ["START", "A", "SUCCESS"],
		initialState : "START",
		initialStackSymbol : "Z",
		transitions : [
				["START","Z","~", "SUCCESS", "Z"],
				["START","Z","~", "START", "Z"]
			],
		acceptStates : ["SUCCESS"],
		criteria : "true"
	} |}

	let pda_acceptNotDet = {| {
		kind : "pushdown automaton",
		description : "ww-1 | w pertence {a,b}",
		name : "ndpda-ww-1",
		inputAlphabet : ["a", "b"],
		stackAlphabet : ["a", "b", "z"],
		states : ["i", "p", "q", "t"],
		initialState : "i",
		initialStackSymbol : "z",
		transitions : [
				["i","z","a", "p", "az"],
				["i","z","b", "p", "bz"],
				["p","a","a", "p", "aa"],
				["p","a","a", "q", ""],
				["p","a","b", "p", "ba"],
				["p","b","a", "p", "ab"],
				["p","b","b", "p", "bb"],
				["p","b","b", "q", ""],
				["q","a","a", "q", ""],
				["q","b","b", "q", ""],
				["q","z","~", "t", "z"]
			],
		acceptStates : ["i", "t"],
		criteria : "true"
	} |}
	
	let pda_acceptDet = {| {
		kind : "pushdown automaton",
		description : "0n1n deterministic, n>0",
		name : "dpda-0n1n",
		inputAlphabet : ["0", "1"],
		stackAlphabet : ["A", "Z"],
		states : ["START", "A", "SUCCESS"],
		initialState : "START",
		initialStackSymbol : "Z",
		transitions : [
				["START","Z","0", "START", "AZ"],
				["START","A","0", "START", "AA"],
				["START","A","1", "A", ""],
				["A","A","1", "A", ""],
				["A","Z","~", "SUCCESS", "Z"]
			],
			acceptStates : ["SUCCESS"],
			criteria : "true"
		} |}
		
	let pda_dirty = {| {
		kind : "pushdown automaton",
		description : "0n1n deterministic, n>0, very dirty",
		name : "dpda-0n1n-dirty",
		inputAlphabet : ["0", "1","2"],
		stackAlphabet : ["A", "Z","X"],
		states : ["START", "A", "B", "SUCCESS","TOY1","TOY2","TOY3", "TOY4", "TOY5", "TOY6"],
		initialState : "START",
		initialStackSymbol : "Z",
		transitions : [
				["START","Z","0", "START", "AZ"],
				["START","A","0", "START", "AA"],
				["START","A","1", "A", ""],
				["A","A","1", "A", ""],
				
				["A","Z","~", "B", "Z"],
				["B","Z","~", "TOY5", "Z"],
				["TOY6","Z","~", "B", "Z"],
				
				["A","A","~", "TOY1", "A"],
				["TOY2","A","~", "A", "A"],
				["TOY3", "Z", "~", "SUCCESS", "Z"],
				["TOY4", "Z", "~", "START", "Z"],
				
				["B","Z","~", "SUCCESS", "Z"]
			],
			acceptStates : ["SUCCESS"],
			criteria : "true"
		} |}
		
	let pda_emptyStackCriteria = {| {
		kind : "pushdown automaton",
		description : "a^n,b^n deterministic, n>1",
		name : "dpda-a^n,b^n",
		inputAlphabet : ["a", "b"],
		stackAlphabet : ["a", "z"],
		states : ["p", "q","1","2"],
		initialState : "p",
		initialStackSymbol : "z",
		transitions : [
			["p","z","a", "p", "az"],
			["p","a","a", "p", "aa"],
			["p","a","b", "q", ""],
			["q","a","b", "q", ""],
			["q","z","~", "q", ""],
			
			["p","z","~", "1", "z"],
			["q","a","~", "2", "a"]
		],
	acceptStates : [],
	criteria : "false"
	} |}
	
	(* test aux functions *)
	let test_accept a w =
		let msg = 
			if a w then "Accepted" else "Rejected"
		in Util.println [msg]
	
	let printTransition (a,b,c,d,e) =
		Util.println ["("; a; ", "; Util.ch2str b; ", "; Util.ch2str c; ", "; d; ", "; Util.word2str e;")"]
	
	(* actual tests *)
	let testIsDeterministicFalse () =
		let pda = new PushdownAutomaton.model (Arg.Text pda_acceptNotDet) in
			if pda#isDeterministic then
				print_string "It is deterministic\n"
			else 
				print_string "It is *not* deterministic\n"
				
	let testIsDeterministicTrue () =
		let pda = new PushdownAutomaton.model (Arg.Text pda_acceptDet) in
			if pda#isDeterministic then
				print_string "It is deterministic\n"
			else 
				print_string "It is *not* deterministic\n"
				
	let testAcceptNotDet () =
		let pda = new PushdownAutomaton.model (Arg.Text pda_acceptNotDet) in
			if pda#accept [] then
				print_string "Accepted"
			else 
				print_string "Rejected"
				
	let testAcceptDet() =
		let pda = new PushdownAutomaton.model (Arg.Text pda_acceptDet) in
			test_accept pda#accept [];
			test_accept pda#accept ['0'];
			test_accept pda#accept ['0';'1'];
			test_accept pda#accept ['0';'0';'1';'1'];
			Util.println []
			
	let testGetUsefulStatesDirty() =
		let pda = new PushdownAutomaton.model (Arg.Text pda_dirty) in
			Util.println ["Useful States:"];
			Util.printStates (Set.toList (pda#getUsefulStates) )	
			
	let testGetUsefulStatesNDet() =
		let pda = new PushdownAutomaton.model (Arg.Text pda_acceptNotDet) in
			Util.println ["Useful States:"];
			Util.printStates (Set.toList (pda#getUsefulStates) )
		
	let testClean()=
		let pda = new PushdownAutomaton.model (Arg.Text pda_dirty) in
		let clean = pda#clean in
			Util.println ["----------> TEST CLEAN <----------"];
			
			Util.println ["States of clean automaton:"];
			Util.printStates (Set.toList clean#representation.states );
			
			Util.println ["----------"];
			
			Util.println ["Input symbols:"];
			Util.printAlphabet (Set.toList clean#representation.inputAlphabet );
			
			Util.println ["----------"];
			
			Util.println ["Stack symbols:"];
			Util.printAlphabet (Set.toList clean#representation.stackAlphabet );
			
			Util.println ["----------"];
			
			Util.println ["Transitions:"];
			Set.iter ( fun x -> printTransition x ) clean#representation.transitions
			
	let testGenerate () =
		let pda = new PushdownAutomaton.model (Arg.Text pda_emptyStackCriteria) in
			Util.println ["----------> TEST GENERATE <----------"];
			Util.println ["Generated words size 0: "]; Util.printWords (Set.toList (pda#generate 0) );
			Util.println ["Generated words size 1: "]; Util.printWords (Set.toList (pda#generate 1) );
			Util.println ["Generated words size 2: "]; Util.printWords (Set.toList (pda#generate 2) );
			Util.println ["Generated words size 3: "]; Util.printWords (Set.toList (pda#generate 3) );
			Util.println ["Generated words size 4: "]; Util.printWords (Set.toList (pda#generate 4) );
			Util.println ["Generated words size 5: "]; Util.printWords (Set.toList (pda#generate 5) );
			Util.println ["Generated words size 6: "]; Util.printWords (Set.toList (pda#generate 6) )
	
	let testGetReachableStates() =
		let pda = new PushdownAutomaton.model (Arg.Text pda_dirty) in
			Util.println ["----------> TEST getReachableStates <----------"];
			Util.printStates (Set.toList (pda#getReachableStates))
			
	let testGetProductiveStates() =
		let pda = new PushdownAutomaton.model (Arg.Text pda_dirty) in
			Util.println ["----------> TEST getProductiveStates <----------"];
			Util.printStates (Set.toList (pda#getProductiveStates))
			
	let testAcceptEmpty() =
		let pda = new PushdownAutomaton.model (Arg.Text pda_emptyStackCriteria) in
			Util.println ["vazio :"];
			test_accept pda#accept [];
			Util.println ["a :"];
			test_accept pda#accept ['a'];
			Util.println ["ab :"];
			test_accept pda#accept ['a';'b'];
			Util.println ["aab :"];
			test_accept pda#accept ['a';'a';'b'];
			Util.println ["abb :"];
			test_accept pda#accept ['a';'b';'b'];
			Util.println []
	
	let runAll =
		if active then (
			Util.header "PushdownAutomatonTests starting...";
			Util.println ["----------"];
			Util.println ["-"];
			Util.println ["-"];
			(* testIsDeterministicFalse(); *)
			(* testIsDeterministicTrue(); *)
			(* testAcceptNotDet(); *)
			(* testAcceptDet();*)
			(* testGetUsefulStatesDirty(); *)
			(* testGetUsefulStatesNDet(); *)
			(* testClean(); *)
			(* testGenerate() *)
			(* testGetReachableStates(); *)
			(* testGetProductiveStates(); *)
			(* testAcceptEmpty(); *)
		)
end
		
