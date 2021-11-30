(*
 * TopLevel.ml
 *
 * This file is part of the OCamlFlat library
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
 *  Written by João Gonçalves (jg)
 *)

(*
 * ChangeLog:
 *
 * feb/2021 (amd) - Added some missing functions.
 * feb/2020 (jg) - Initial version.
 *)

(*
 * Description: Set of functions with simple signatures to the used in the
 * ocaml toplevel system. In a sense, this provides a command-line interface
 * to most of the functionalities of the OCamlFlat library.
 *
 * TODO: Improve.
 *)

module TopLevel =
struct
	open Exercise
	open FiniteAutomaton
	open RegularExpression
	open ContextFreeGrammar
	open PushdownAutomaton
	open PolyModel

(* Toplevel types *)

	type finiteAutomaton = {
			alphabet: char list;
			states: state list;
			initialState: state;
			transitions: FiniteAutomaton.transition list;
			acceptStates: state list
		}
		
	type pushdownAutomaton = {
			inputAlphabet: char list;	
			stackAlphabet: char list;		
			states: state list;				
			initialState: state;		
			initialStackSymbol: char;	
			transitions: PushdownAutomaton.transition list;	
			acceptStates: state list		
		}

	type regularExpression = string

	type contextFreeGrammar = {
			alphabet: char list;
			variables: char list;
			initial: char;
			rules: string list
		}

	type exercise = {
			inside: string list ;
			outside: string list
		}


	(* Toplevel convertions *)

	let fa_convertTo (fa: FiniteAutomaton.t ) =
		{
			alphabet = Set.toList fa.alphabet;
			states = Set.toList fa.states;
			initialState = fa.initialState;
			transitions = Set.toList fa.transitions;
			acceptStates = Set.toList fa.acceptStates	
		}

	let fa_convertFrom (fa: finiteAutomaton) : FiniteAutomaton.t =
		{
			alphabet = Set.make fa.alphabet;
			states = Set.make fa.states;
			initialState = fa.initialState;
			transitions = Set.make fa.transitions;
			acceptStates = Set.make fa.acceptStates
		}

	let re_convertTo re : regularExpression = RegExpSyntax.toString re

	let re_convertFrom re = RegExpSyntax.parse re

	let cfg_convertTo (cfg: ContextFreeGrammar.t ) =
		let alpha = Set.toList cfg.alphabet in
		let variables = Set.toList cfg.variables in
		let initial = cfg.initial in
		let rules = CFGSyntax.toStringList cfg.rules in
			{
				alphabet = alpha;
				variables = variables;
				initial = initial;
				rules = rules
			}

	let cfg_convertFrom (cfg: contextFreeGrammar) : ContextFreeGrammar.t =
			{
				alphabet = Set.make cfg.alphabet;
				variables = Set.make cfg.variables;
				initial = cfg.initial;
				rules = CFGSyntax.parse (Set.make cfg.rules)
			}

	let exer_convertTo (exer: Exercise.t) =
		let inws = Set.map (fun w -> Util.word2str w) exer.inside in
		let outws = Set.map (fun w -> Util.word2str w) exer.outside in
			{
				inside = Set.toList inws;
				outside = Set.toList outws
			}

	let exer_convertFrom exer : Exercise.t =
		let inws = List.map (fun s -> Util.str2word s) exer.inside in
		let outws = List.map (fun s -> Util.str2word s) exer.outside in
			{
				problem = "";
				inside = Set.make inws;
				outside = Set.make outws
			}

	let exer_convertFailures ins outs =
		let ins = Set.map (fun w -> Util.word2str w) ins in
		let outs = Set.map (fun w -> Util.word2str w) outs in
			(Set.toList ins, Set.toList outs)


	(* Automaton functions *)

	let fa_load file =
		let a = new FiniteAutomaton.model (Arg.File file) in
			fa_convertTo a#representation

	let fa_build text =
		let a = new FiniteAutomaton.model (Arg.Text text) in
			fa_convertTo a#representation

	let fa_json json =
		let a = new FiniteAutomaton.model (Arg.JSon json) in
			fa_convertTo a#representation

	let fa_predef name =
		fa_build (Examples.example name)

	let fa_accept fa w =
		let fa = fa_convertFrom fa in
		let a = new FiniteAutomaton.model (Arg.Representation fa) in
		let w = Util.str2word w in
			a#accept w

	let fa_traceAccept fa w =
		let fa = fa_convertFrom fa in
		let a = new FiniteAutomaton.model (Arg.Representation fa) in
		let w = Util.str2word w in
			a#acceptWithTracing w

	let fa_generate fa l =
		let fa = fa_convertFrom fa in
		let a = new FiniteAutomaton.model (Arg.Representation fa) in
		let b = Set.map (fun w -> Util.word2str w) (a#generate l) in
			Set.toList b

	let fa_reachable fa =
		let fa = fa_convertFrom fa in
		let a = new FiniteAutomaton.model (Arg.Representation fa) in
		let start = fa.initialState in
			Set.toList (a#reachable start)

	let fa_productive fa =
		let fa = fa_convertFrom fa in
		let a = new FiniteAutomaton.model (Arg.Representation fa) in
			Set.toList (a#productive)

	let fa_clean fa =
		let fa = fa_convertFrom fa in
		let a = new FiniteAutomaton.model (Arg.Representation fa) in
		let b = a#cleanUselessStates in
			fa_convertTo b#representation


	let fa_toDeter fa =
		let fa = fa_convertFrom fa in
		let a = new FiniteAutomaton.model (Arg.Representation fa) in
		let b = a#toDeterministic in
			fa_convertTo b#representation


	let fa_isDeter fa =
		let fa = fa_convertFrom fa in
		let a = new FiniteAutomaton.model (Arg.Representation fa) in
			a#isDeterministic

	let fa_minimize fa =
		let fa = fa_convertFrom fa in
		let a = new FiniteAutomaton.model (Arg.Representation fa) in
		let b = a#minimize in
			fa_convertTo b#representation

	let fa_isMin fa =
		let fa = fa_convertFrom fa in
		let a = new FiniteAutomaton.model (Arg.Representation fa) in
			a#isMinimized

	let fa_toRegex fa =
		let fa = fa_convertFrom fa in
		let a = new FiniteAutomaton.model (Arg.Representation fa) in
		let b = fa2re a in
			re_convertTo b#representation

	(* Regex functions *)

	let re_load file =
		let a = new RegularExpression.model (Arg.File file) in
			re_convertTo a#representation

	let re_build text =
		let a = new RegularExpression.model (Arg.Text text) in
			re_convertTo a#representation

	let re_json json =
		let a = new RegularExpression.model (Arg.JSon json) in
			re_convertTo a#representation

	let re_predef name =
		re_build (Examples.example name)

	let re_alphabet re =
		let re = re_convertFrom re in
		let a = new RegularExpression.model (Arg.Representation re) in
			Set.toList a#alphabet

	let re_accept re w =
		let re = re_convertFrom re in
		let a = new RegularExpression.model (Arg.Representation re) in
		let w = Util.str2word w in
			a#accept w

	let re_trace re w =
		let re = re_convertFrom re in
		let a = new RegularExpression.model (Arg.Representation re) in
		let w = Util.str2word w in
			a#allTrees w

	let re_generate re l =
		let re = re_convertFrom re in
		let a = new RegularExpression.model (Arg.Representation re) in
		let b = Set.map (fun w -> Util.word2str w) (a#generate l) in
			Set.toList b

	let re_simplify re =
		let re = re_convertFrom re in
		let a = new RegularExpression.model (Arg.Representation re) in
		let b =	a#simplify in
			re_convertTo b#representation

	let re_toFA re =
		let re = re_convertFrom re in
		let a = new RegularExpression.model (Arg.Representation re) in
		let b =	re2fa a in
			fa_convertTo b#representation


	(* CFG functions *)

	let cfg_load file =
		let a = new ContextFreeGrammar.model (Arg.File file) in
			cfg_convertTo a#representation

	let cfg_build text =
		let a = new ContextFreeGrammar.model (Arg.Text text) in
			cfg_convertTo a#representation

	let cfg_json json =
		let a = new ContextFreeGrammar.model (Arg.JSon json) in
			cfg_convertTo a#representation

	let cfg_predef name =
		cfg_build (Examples.example name)

	let cfg_accept cfg w =
		let cfg = cfg_convertFrom cfg in
		let a = new ContextFreeGrammar.model (Arg.Representation cfg) in
		let w = Util.str2word w in
			a#accept w

	let cfg_trace cfg w =
		let cfg = cfg_convertFrom cfg in
		let a = new ContextFreeGrammar.model (Arg.Representation cfg) in
		let w = Util.str2word w in
			a#acceptWithTracing w

	let cfg_generate cfg l =
		let cfg = cfg_convertFrom cfg in
		let a = new ContextFreeGrammar.model (Arg.Representation cfg) in
		let b = Set.map (fun w -> Util.word2str w) (a#generate l) in
			Set.toList b

	let cfg_toFA cfg =
		let cfg = cfg_convertFrom cfg in
		let a = new ContextFreeGrammar.model (Arg.Representation cfg) in
		let b =	cfg2fa a in
			fa_convertTo b#representation

	let cfg_toRe cfg =
		let cfg = cfg_convertFrom cfg in
		let a = new ContextFreeGrammar.model (Arg.Representation cfg) in
		let b =	cfg2re a in
			re_convertTo b#representation

	(* Exercise functions *)

	let exer_load file =
		let e = new Exercise.exercise (Arg.File file) in
			exer_convertTo e#representation

	let exer_build text =
		let e = new Exercise.exercise (Arg.Text text) in
			exer_convertTo e#representation

	let exer_json json =
		let e = new Exercise.exercise (Arg.JSon json) in
			exer_convertTo e#representation

	let exer_predef name =
		exer_build (Examples.example name)

	let exer_testFA exer fa =
		let fa = fa_convertFrom fa in
		let a = new FiniteAutomaton.model (Arg.Representation fa) in
		let exer = exer_convertFrom exer in
		let e = new Exercise.exercise (Arg.Representation exer) in
			a#checkExercise e

	let exer_testFAFailures exer fa =
		let fa = fa_convertFrom fa in
		let a = new FiniteAutomaton.model (Arg.Representation fa) in
		let exer = exer_convertFrom exer in
		let e = new Exercise.exercise (Arg.Representation exer) in
		let (ins,outs) = a#checkExerciseFailures e in
			exer_convertFailures ins outs

	let exer_testRe exer re =
		let re = re_convertFrom re in
		let a = new RegularExpression.model (Arg.Representation re) in
		let exer = exer_convertFrom exer in
		let e = new Exercise.exercise (Arg.Representation exer) in
			a#checkExercise e

	let exer_testReFailures exer re =
		let re = re_convertFrom re in
		let a = new RegularExpression.model (Arg.Representation re) in
		let exer = exer_convertFrom exer in
		let e = new Exercise.exercise (Arg.Representation exer) in
		let (ins,outs) = a#checkExerciseFailures e in
			exer_convertFailures ins outs

	let exer_testCfg exer cfg =
		let cfg = cfg_convertFrom cfg in
		let a = new ContextFreeGrammar.model (Arg.Representation cfg) in
		let exer = exer_convertFrom exer in
		let e = new Exercise.exercise (Arg.Representation exer) in
			a#checkExercise e

	let exer_testCfgFailures exer cfg =
		let cfg = cfg_convertFrom cfg in
		let a = new ContextFreeGrammar.model (Arg.Representation cfg) in
		let exer = exer_convertFrom exer in
		let e = new Exercise.exercise (Arg.Representation exer) in
		let (ins,outs) = a#checkExerciseFailures e in
			exer_convertFailures ins outs
end
