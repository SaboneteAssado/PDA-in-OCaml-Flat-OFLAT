(*
 * OCamlFlat.ml
 *
 * This file is part of the OCamlFlat library
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by Artur Miguel Dias (amd)
 *)

(*
 * ChangeLog:
 *
 * feb/2021 (amd) - New module.
 *)

let ocamlFlatVersion = "1.0"
(*
 * Error.ml
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
 *  Written by Artur Miguel Dias (amd)
 *)

(*
 * ChangeLog:
 *
 * jan/2021 (amd) - Module in an independent file.
 * jun/2019 (amd) - Initial version, inside the big file "OCamlFlatSupport.ml".
 *)

(*
 * Description: Supports a log of errors. Probably, a text-based application
 * will use the error log differently from a graphical-based application.
 * The errors are handled in a imperative style to simplify the signature of
 * many functions - this modules implements a kind of error log monad.
 *)

module type ErrorSig =
sig
	val start : unit -> string list ref
	val stop : unit -> unit
	val onlyFirst : unit -> unit
	val error : string -> string -> 'a -> 'a
	val show : string -> string -> unit
end

module Error : ErrorSig =
struct
	type t = string list ref

	let sink: t = ref []
	let err: t ref = ref sink
	let onlyOne: bool ref = ref false

	let start () =	(* setup a log of errors *)
		let r = ref [] in
			err := r;
			r		(* returns a new error log *)

	let stop () = (* disables logging *)
		sink := [];
		err := sink

	let onlyFirst () =
		onlyOne := true

	let error (culprit: string) (str: string) (res: 'a): 'a =
		let r = !err in
		if !r = [] || not !onlyOne then
			r := !r@[culprit ^ ": " ^ str];
		res

	let show (expectedKind: string) (name: string): unit =
		let r = !err in
			if !r = [] then ()
			else (
				print_string (expectedKind^" "^name^ " has errors:\n");
				List.iter (fun m -> print_string ("	"^m^"\n")) !r
			)
end
(*
 * Util.ml
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
 *  Written by Artur Miguel Dias (amd)
 *)

(*
 * ChangeLog:
 *
 * jan/2021 (amd) - Module in an independent file.
 * jun/2019 (amd) - Initial version, inside the big file "OCamlFlatSupport.ml".
 *)

(*
 * Description: Miscellaneous utility functions. 
 *
 * TODO: Check if this is the right place for some of this functions.
 *)

module type UtilSig =
sig
	val ch2str : char -> string
	val str2word : string -> char list
	val word2str : char list -> string
	val strings2words : string list -> char list list
	val words2strings : char list list -> string list
	val stripChars : string -> string -> string
	val stripHead : string -> int -> string
	
	val flatMap:  ('a -> 'b list) -> 'a list -> 'b list
	val concatAll : 'a list -> 'a list list -> 'a list list
	val distrib2 : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
	val indexOf : 'a -> 'a list -> int
	val fixedPoint : ('a -> 'a) -> 'a -> 'a
	
	val load_file : string -> string
	val print : string list -> unit
	val println : string list -> unit
	val header : string -> unit
	val printAlphabet : char list -> unit
	val printStates : string list -> unit
	val printTransition : string -> char -> string -> unit
	val printWords : char list list -> unit
	val printStrings : string list -> unit
end

module Util : UtilSig =
struct
	let ch2str c =
		Char.escaped c

	(* let str2word s =      //only ocaml >= 4.06
		List.init (String.length s) (String.get s) ; *)
	let str2word s =
		let n = String.length s in
		let rec iterStr i =
			if i < n then s.[i]::iterStr (i+1)
			else []
		in
			iterStr 0

	let word2str w =
		let buf = Buffer.create 16 in
		let () = List.iter (Buffer.add_char buf) w in
			Buffer.contents buf

	let strings2words ss =
		List.map str2word ss
		
	let words2strings ws =
		List.map word2str ws

	let stripChars s cs =
		let len = String.length s in
		let j = ref 0 in
		let res = Bytes.create len in
			for i = 0 to len-1 do
				if not (String.contains cs s.[i]) then begin
					Bytes.set res !j s.[i];
					j := !j + 1
				end
			done;
			Bytes.to_string (Bytes.sub res 0 !j)

	let stripHead s n =
		let len = String.length s in
		let j = ref 0 in
		let skip = ref n in
		let res = Bytes.create len in
			for i = 1 to len-1 do
				if !skip > 0 && s.[i] = '\t' then
					skip := !skip - 1
				else begin
					if s.[i] = '\n' then
						skip := n
					else ();
					Bytes.set res !j s.[i];
					j := !j + 1
				end
			done;
			Bytes.to_string (Bytes.sub res 0 !j)

	let flatMap f l =
		List.flatten (List.map f l)

	let addAll symb =
		List.map (fun l -> symb::l)

	let concatAll w =
		List.map (fun l -> w@l)

	let distrib2 f (a,b) =
		f a b

	let indexOf e l =
		let rec index e l n =
			match l with
				[] -> -1
				|x::xs -> if e = x then n else index e xs (n+1)
		in
		index e l 0

	let rec fixedPoint (f: 'a -> 'a) (x: 'a): 'a =
		let next = f x in
			if x = next then x
			else fixedPoint f next
			
			
	let load_file (filename: string): string =
		try
			let ic = open_in filename in
			let n = in_channel_length ic in
			let s = Bytes.create n in
				really_input ic s 0 n;
				close_in ic;
				Bytes.to_string s
		with
			Sys_error str ->
				Error.error "file" str ""


	let rec print (l: string list) =
		match l with
		| [] -> ()
		| x::xs -> print_string x; print xs

	let println (l: string list) =
		print l ;
		print_newline()

	let header (str: string) =
		println ["----------"] ;
		println [str]

	let printAlphabet (alf:char list) =
		List.iter (fun x -> print [ch2str x; ", "]) alf;
		println []

	let printStates (st:string list) =
		List.iter (fun x -> print [x; ", "]) st;
		println []

	let printTransition (a:string) (b:char) (c:string) =
		println ["("; a; ", "; ch2str b; ", "; c; ")"]

	let printWord (w:char list) =
		println ["'"; word2str w; "'"]

	let printWords (l: char list list) =
		List.iter printWord l
		
	let printString (s: string) =
		println ["'"; s; "'"]

	let printStrings (l: string list) =
		List.iter printString l
end

module UtilTests =
struct
	let active = false

	let test0 () =
		Util.println [Util.load_file "examples/fa_abc.json"]

	let test1 () =
		let a = Util.word2str ['e';'r';'t'] in
		let b = Util.word2str ['4';'5';'y'] in
			Util.println [a; b]

	let runAll : unit =
		if active then (
			Util.header "UtilTests";
			test0 ();
			test1 ()
		)
end
(*
 * Set.ml
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
 *  Written by Artur Miguel Dias (amd)
 *)

(*
 * ChangeLog:
 *
 * jan/2021 (amd) - Module in an independent file.
 * jun/2019 (amd) - Initial version, inside the big file "OCamlFlatSupport.ml".
 *)

(*
 * Description: Polymorphic sets. Naive implementation.
 *
 * TODO: Improve the implementation or move to the functorized sets of
 * the ocaml standard library.
 *)

module type SetSig =
sig
	(*type 'a t = 'a list*)
	type 'a t (* opaque *)
	val make : 'a list -> 'a t
	val toList : 'a t -> 'a list
	val empty : 'a t
	val size : 'a t -> int
	val belongs : 'a -> 'a t -> bool
	val union : 'a t -> 'a t -> 'a t
	val nth : 'a t -> int -> 'a
	val add : 'a -> 'a t -> 'a t
	val remove : 'a -> 'a t -> 'a t
	val inter : 'a t -> 'a t -> 'a t
	val diff : 'a t -> 'a t -> 'a t
	val subset : 'a t -> 'a t -> bool
	val map : ('a -> 'b) -> 'a t -> 'b t
	val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
	val filter : ('a -> bool) -> 'a t -> 'a t
	val for_all : ('a -> bool) -> 'a t -> bool
	val exists : ('a -> bool) -> 'a t -> bool
	val flatten : 'a t t -> 'a t
	val flatMap : ('a -> 'b t) -> 'a t -> 'b t
	val iter : ('a -> unit) -> 'a t -> unit
	val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
	val combinations : 'a t -> 'b t -> ('a * 'b) t
	val star : 'a list t -> int -> 'a list t
	val allDistinct : ('a -> 'b) -> 'a t -> bool
	val hasDuplicates : 'a t -> bool
	val validate : 'a list -> string -> 'a t
	val historicalFixedPoint : ('a t -> 'a t) -> ('a t) -> 'a t
	val historicalFixedPointTracing : ('a t -> 'a t) -> ('a t) -> 'a t list
	val test: unit -> int list list
end

module Set : SetSig =
struct
	type 'a t = 'a list
	let make (l: 'a list): 'a t = List.sort_uniq compare l
	let toList (s: 'a t): 'a list = s

	let empty: 'a t = []
	let size (s: 'a t): int = List.length s
	let belongs (v: 'a) (s: 'a t): bool = List.mem v s
	let union (s1: 'a t) (s2: 'a t): 'a t = make (s1 @ s2)
	let nth (s: 'a t) (n: int) : 'a = List.nth s n
	let add (v: 'a) (s: 'a t): 'a t = make (v :: s)
	let remove (v: 'a) (s: 'a t): 'a t = List.filter (fun x -> x <> v) s
	let inter (s1: 'a t) (s2: 'a t): 'a t = List.filter (fun x -> belongs x s2) s1
	let diff (s1: 'a t) (s2: 'a t): 'a t = List.filter (fun x -> not (belongs x s2)) s1
	let subset (s1: 'a t) (s2: 'a t): bool = List.for_all (fun x -> belongs x s2) s1

	let map f s = make (List.map f s)
	let mapi f s = make (List.mapi f s)
	let filter f s = List.filter f s
	let for_all f s = List.for_all f s
	let exists f s = List.exists f s
	let flatten ss = make (List.flatten ss)
	let flatMap f s = flatten (List.map f s)
	let iter f s = List.iter f s
	let partition f s = let (a, b) = List.partition f s in (a, b)	(* <- already ordered *)
	let combinations (s1: 'a t) (s2: 'b t): ('a * 'b) t =
		flatMap (fun x -> List.map (fun y -> (x,y)) s2) s1	(* <- already ordered *)
	let starOne (s: 'a list t) (n: int) (l:'a list): 'a list t = (* private auxiliary *)
		let z = n - (List.length l) in
		let sel = filter (fun k -> List.length k <= z) s in
			map (fun k -> k @ l) sel
	let star (s: 'a list t) (n: int): 'a list t =
		Util.fixedPoint (fun x -> union x (flatMap (starOne x n) s)) [[]]

	let allDistinct f s = size s = size (map f s)
	let hasDuplicates (s: 'a t): bool = size s <> size (make s)
	let validate (l: 'a list) (culprit: string): 'a t =
		if hasDuplicates l
			then Error.error culprit "Repetitions in set" empty
			else make l

	let historicalFixedPoint (f: 'a t -> 'a t) (x: 'a t): 'a t =
		let rec historicalFixedPointX (f: 'a t -> 'a t) (x: 'a t) (acum: 'a t): 'a t =
			let next = f x in
			let newAcum = union x acum in
			if acum = newAcum then x
			else historicalFixedPointX f next newAcum
		in
			historicalFixedPointX f x empty

	let historicalFixedPointTracing (f: 'a t -> 'a t) (x: 'a t): 'a t list =
		let rec historicalFixedPointX (f: 'a t -> 'a t) (x: 'a t) (acum: 'a t) (trace: 'a t list): 'a t list =
			let next = f x in
			let newTrace = trace@[next] in
			let newAcum = union x acum in
			if acum = newAcum then trace
			else historicalFixedPointX f next newAcum newTrace
		in
			historicalFixedPointX f x empty [x]

	let test (): int list list =	(* Set.test ();; *)
		toList (star (make[ [1]; [2;3]]) 4)
end

module type UPSetSig = (* unordered pair set *)
sig
	type 'a t
	val make : ('a * 'a) list -> 'a t
	val toList : 'a t -> ('a * 'a) list
	val empty : 'a t
	val size : 'a t -> int
	val belongs : 'a * 'a -> 'a t -> bool
	val union : 'a t -> 'a t -> 'a t
	val add : 'a * 'a -> 'a t -> 'a t
	val inter : 'a t -> 'a t -> 'a t
	val diff : 'a t -> 'a t -> 'a t
	val subset : 'a t -> 'a t -> bool
	val map : ('a * 'a -> 'b * 'b) -> 'a t -> 'b t
	val filter : ('a * 'a -> bool) -> 'a t -> ('a * 'a) Set.t
	val for_all : ('a * 'a -> bool) -> 'a t -> bool
	val exists : ('a * 'a -> bool) -> 'a t -> bool
	val flatten : 'a t t -> 'a t
	val flatMap : ('a -> 'b t) -> 'a t -> 'b t
	val iter : ('a * 'a -> unit) -> 'a t -> unit
	val partition : ('a * 'a -> bool) -> 'a t -> ('a * 'a) Set.t * ('a * 'a) Set.t
	val combinations : 'a t -> 'b t -> ('a * 'b) t
	val star : 'a list t -> int -> 'a list t
	val allDistinct : ('a * 'a -> 'b) -> 'a t -> bool
	val hasDuplicates : 'a t -> bool
	val validate : ('a * 'a) list -> string -> 'a t
	val test: unit -> (int * int) list
end

module UPSet : UPSetSig =
struct
	type 'a t = ('a*'a) Set.t

	(* invariant: a < b for all pairs (a,b) *)
	let ord (a,b) = if a < b then (a, b)		(* keep *)
					else if a > b then (b, a)	(* swap *)
					else failwith "UPSet.ord"	(* error *)

	let make (l: ('a*'a) list): 'a t =
		let l1 = List.filter (fun (a,b) -> a <> b) l in
		let l2 = List.map ord l1 in
			Set.make l2
	let toList (s: 'a t): ('a*'a) list = Set.toList s

	let empty: 'a t = Set.empty
	let size (s: 'a t): int = Set.size s
	let belongs (v: 'a*'a) (s: 'a t): bool = Set.belongs (ord v) s
	let union (s1: 'a t) (s2: 'a t): 'a t = Set.union s1 s2
	let add (v: 'a*'a) (s: 'a t): 'a t = Set.add (ord v) s
	let inter (s1: 'a t) (s2: 'a t): 'a t = Set.inter s1 s2
	let diff (s1: 'a t) (s2: 'a t): 'a t = Set.diff s1 s2
	let subset (s1: 'a t) (s2: 'a t): bool = Set.subset s1 s2

	let map f (s: 'a t) = make (Set.toList (Set.map f s))
	let filter f (s: 'a t) = Set.filter f s
	let for_all f (s: 'a t) = Set.for_all f s
	let exists f (s: 'a t) = Set.exists f s
	let flatten (ss: 'a t t) = failwith "UPSet.flatten"
	let flatMap f (s: 'a t) = failwith "UPSet.flatMap"
	let iter f (s: 'a t) = Set.iter f s
	let partition f (s: 'a t) = Set.partition f s
	let combinations (s1: 'a t) (s2: 'b t): ('a * 'b) t = failwith "UPSet.combinations"
	let star (s: 'a list t) (n: int): 'a list t = failwith "UPSet.star"

	let allDistinct f (s: 'a t) = Set.allDistinct f s
	let hasDuplicates (s: 'a t): bool = Set.hasDuplicates s
	let validate (l: ('a*'a) list) (culprit: string): 'a t = failwith "UPSet.validate"
	let test () =	(* UPSet.test ();; *)
		toList (make [(1,1);(1,2);(2,2);(3,2);(3,2);(2,3)])
end
(*
 * Scanner.ml
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
 *  Written by Artur Miguel Dias (amd)
 *)

(*
 * ChangeLog:
 *
 * jan/2021 (amd) - Initial version, for the JSon parser.
 *)

(*
 * Description: Simple lexical analyzer that assumes that the tokens are
 * the individual non-blank characters. The function getToken is available
 * to handle the rare cases where we need a multi-char token. This module is
 * suitable for the three parsers defined in the OCamlFlat library.
 * The tokens are handled in a imperative style to simplify the signature of
 * of the client parsing functions.
 *)

module type ScannerSig =
sig
	val start : string -> string -> string list ref
	val skip : unit -> unit
	val curr : unit -> char
	val getToken : (char -> bool) -> string
	val expecting : string -> char -> 'a -> 'a
	val stop : unit -> unit
end

module Scanner : ScannerSig =
struct
	let parserName = ref ""
	let inputString = ref ""
	let inputStringLength = ref 0
	let inputStringPosition = ref 0

	let start name s =
		parserName := name;
		inputString := s;
		inputStringLength := String.length s;
		inputStringPosition := 0;
		let log = Error.start () in
			Error.onlyFirst ();
			log

	let isInside () =
		!inputStringPosition < !inputStringLength

	let getThis () =
		String.get !inputString !inputStringPosition

	let skip () =
		inputStringPosition := !inputStringPosition + 1

	let skipWhile good =
		while isInside () && good (getThis ()) do skip() done

	let curr () =
		skipWhile (fun c -> c = ' ' || c = '\t' || c = '\n');
		if isInside () then
			getThis ()
		else
			' '

	let getToken good =
		let start = !inputStringPosition in
			skipWhile good;
			String.sub !inputString start (!inputStringPosition - start)

	let expecting exp got z =
		let g = if got = ' ' then "'EOF'" else "'" ^ Char.escaped got ^ "'" in
		let mesg ="Expecting " ^ exp ^ ", got " ^ g in
			Error.error !parserName mesg z

	let stop () =
		Error.show !parserName "input";
		Error.stop ()

end
(*
 * JSon.ml
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
 *  Written by Artur Miguel Dias (amd)
 *)

(*
 * ChangeLog:
 *
 * jan/2021 (amd) - Added a very simple recursive descent parser for JSon.
 * jan/2021 (amd) - Module in an independent file.
 * jun/2019 (amd) - Initial version, inside the big file "OCamlFlatSupport.ml".
 *)

(*
 * Description: Very simple JSon parser, plus some JSon handling functions.
 *)

module type JSonSig =
sig
	type t =
		| JNull
		| JString of string
		| JAssoc of (string * t) list
		| JList of t list
		(* | JBool of bool (*dbm*) *)

	val from_string : string -> t
	val from_file : string -> t

	val to_string : t -> string
	val show : t -> unit

	val field_string : t -> string -> string
	val as_string : t -> string -> string
	val field_string_list : t -> string -> string list
	val field_string_set : t -> string -> string Set.t
	val as_char : t -> string -> char
	val field_char : t -> string -> char
	val field_char_list : t -> string -> char list
	val field_char_set : t -> string -> char Set.t
	val as_string_char_string : t -> string -> string * char * string
	val field_triples_list : t -> string -> (string * char * string) list
	val field_triples_set : t -> string -> (string * char * string) Set.t
	val identification : t -> string * string * string
	val makeDummyIdentification : string -> t
	
	(* dbm *)
	val as_string_char_char_string_charlist : t -> string -> string * char * char * string * char list
	val field_quintuplets_list : t -> string -> (string * char * char * string * char list) list
	val field_quintuplets_set : t -> string -> (string * char * char * string * char list) Set.t
	(*
	val as_bool : t -> string -> bool
	val field_bool : t -> string -> bool
	*)
	
end

module JSon : JSonSig =
struct
	open Scanner

	type t =
		| JNull
		| JString of string
		| JAssoc of (string * t) list
		| JList of t list
		(* | JBool of bool (* DBM *) *)

(* PARSER *)
	let parseString () =
		skip();	(* skip quotation mark *)
		let tk = getToken (fun c -> c <> '"') in
			match curr () with
				| '"' -> skip(); tk
				| err -> expecting "closing '\"'" err ""

	let parseWord () =
		getToken (fun c -> 'a' <= c && c <= 'z'
					|| 'A' <= c && c <= 'Z'
					|| '0' <= c && c <= '9'
					|| c = '_')

	let parseLabel () =
		match curr() with
			| '"' -> parseString ()
			| 'a'..'z' -> parseWord ()
			| err -> expecting "'STRING' or '}'" err ""

	let checkEOF () =
		match curr() with
			| ' ' -> ()
			| err -> expecting "'EOF'" err ()


	let rec parsePair () =
		let label = parseLabel () in
			match curr() with
				| ':' -> skip(); (label, parseJSon ())
				| err -> expecting "':'" err ("", JNull)

	and parseAssocCont () =
		let p = parsePair () in
			match curr() with
				| ',' -> skip(); p::parseAssocCont ()
				| '}' -> skip(); [p]
				| err -> expecting "',' or '}'" err []

	and parseAssoc () =
		skip();	(* skip { *)
		match curr() with
			| '}' -> skip(); []
			| ' ' -> expecting "'}' or 'STRING'" ' ' []
			| _ -> parseAssocCont ()

	and parseListCont () =
		let j = parseJSon () in
			match curr() with
				| ',' -> skip(); j::parseListCont ()
				| ']' -> skip(); [j]
				| err -> expecting "',' or ']'" err []

	and parseList () =
		skip();	(* skip [ *)
		match curr() with
			| ']' -> skip(); []
			| ' ' -> expecting "']' or 'JSON'" ' ' []
			| _ -> parseListCont ()

	and parseJSon s =
		match curr() with
			| '"' -> JString (parseString ())
			| '[' -> JList (parseList ())
			| '{' -> JAssoc (parseAssoc ())
			| err -> expecting "'JSON'" err JNull

	let from_string s =
		let log = Scanner.start "JSon" s in
			let j = parseJSon () in
				checkEOF ();
				Scanner.stop ();
				if !log = [] then j else JNull

	let from_file filename =
		from_string (Util.load_file filename)

(* PRETTY PRINT *)
	let tab n =
		String.make n '\t'

	let isComplex j =
		match j with
			| JList l -> true
			| JAssoc l -> true
			| _ -> false

	let rec textual (tab1: int) (tab2: int) (j: t) : string =
		tab tab1
		^
		match j with
			| JNull ->
				"null"
			| JString s ->
					"\"" ^ s ^ "\""
			| JList l when List.exists isComplex l ->
					let elems = List.map (textual (tab2+1) (tab2+1)) l in (
							"[\n"
							^ String.concat (",\n") elems ^ "\n"
							^ tab tab2 ^ "]"
						)
			| JList l ->
					let elems = List.map (textual 0 0) l in
						("[" ^ String.concat ", " elems ^ "]")
			| JAssoc [] ->
					"{}"
			| JAssoc l ->
					let field (s,j) = tab (tab2+1) ^ s ^ " : " ^ textual 0 (tab2+1) j in
						let elems = List.map field l in (
							"{\n"
							^ String.concat ",\n" elems ^ "\n"
							^ tab tab2 ^ "}"
						)
	let to_string j =
		textual 0 0 j

	let show (j: t) =
		Util.println [to_string j]

(* MEMBERSHIP *)
	let isMember name j =
		match j with
			| JAssoc obj -> (
					try
						List.assoc name obj
					with Not_found -> JNull
				)
			| _ ->
				failwith ("Can't get member '" ^ name ^ "' of association type ")

(* MORE *)

	let error = Error.error

	let field_string (j: t) (field: string) =
		match j |> isMember field with
			| JNull -> error field "Missing field" "#"
			| JString s -> s
			| _ -> error field "Expected string" "#"

	let as_string (j: t) (field: string) =
		match j with
			| JString s -> s
			| _ -> error field "Expected string" "#"

	let field_string_list (j: t) (field: string) =
		match j |> isMember field with
			| JNull -> error field "Missing field" []
			| JList l -> List.map (fun j -> as_string j field) l
			| _ -> error field "Expected string list" []

	let field_string_set (j: t) (field: string) =
		Set.validate (field_string_list j field) field

	let as_char (j: t) (field: string) = 
		match j with
			| JString s when String.length s = 1 -> String.get s 0
			| _ -> error field "Expected char" '#'
			
	let as_char_list (j: t) (field: string) =
		match j with
			| JString s -> Util.str2word s
			| _ -> error field "Expected char" []
			
	let field_char (j: t) (field: string) =
		match j |> isMember field with
			| JNull -> error field "Missing field" '#'
			| JString s -> as_char (JString s) field
			| _ -> error field "Expected char" '#'

	let field_char_list (j: t) (field: string) =
		match j |> isMember field with
			| JNull -> error field "Missing field" []
			| JList l -> List.map (fun j -> as_char j field) l
			| _ -> error field "Expected char list" []

	let field_char_set (j: t) (field: string) =
		Set.validate (field_char_list j field) field

	let as_string_char_string (j: t) (field: string) =
		match j with
			| JList [a; b; c] -> (as_string a field, as_char b field, as_string c field)
			| _ -> error field "Malformed triple" ("#",'#',"#")

	let field_triples_list (j: t) (field: string) =
		match j |> isMember field with
			| JList l -> List.map (fun j -> as_string_char_string j field) l
			| _ -> []

	let field_triples_set (j: t) (field: string) =
		Set.validate (field_triples_list j field) field

	let as_string_char_char_string_charlist (j: t) (field: string) =
		match j with
			| JList [a; b; c; d; e] -> (as_string a field,
										as_char b field,
										as_char c field,
										as_string d field,
										as_char_list e field
										)
			| _ -> error field "Malformed quintuplet" ("#",'#','#',"#",[])
	
	let field_quintuplets_list (j: t) (field: string) =
		match j |> isMember field with
			| JList l -> List.map (fun j -> as_string_char_char_string_charlist j field) l
			| _ -> []

	let field_quintuplets_set (j: t) (field: string) =
		Set.validate (field_quintuplets_list j field) field

	(*
	let as_bool (j: t) (field: string) = 
		match j with
			| JString s when s == "true" -> true
			| JString s when s == "false" -> false
			| _ -> error field "Expected bool" true

	let field_bool (j: t) (field: string) =
		match j |> isMember field with
			| JNull -> error field "Missing field" 
			| JString s -> as_bool (JString s) field
			| _ -> error field "Expected bool" true
	*)
	let identification (j: t): string * string * string =
		let kind = field_string j "kind" in
		let description = field_string j "description" in
		let name = field_string j "name" in
			(kind, description, name)

	let makeDummyIdentification (k: string): t =
		JAssoc [("kind", JString k);
				("description", JString "_");
				("name", JString "_")]
end

module JSonTests =
struct
	let active = false

	let jsonSample = {| {
		name: {
			first: "aa",
			last: "22",
			fullname: "33"
		},
		age: "33",
		hobbies: [ "44", "55" ]
	} |};;

	let test0 () =
		let json = JSon.from_string jsonSample in
			JSon.show json

	let test1 () =
		let json = JSon.from_file "fa_abc.json" in
			JSon.show json

	let runAll =
		if active then (
			Util.header "JSonTests";
			test0 ()
		)

end
(*
 * RegExpSyntax.ml
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
 *  Written by Artur Miguel Dias (amd)
 *)

(*
 * ChangeLog:
 *
 * jan/2021 (amd) - Module in an independent file.
 * jun/2019 (amd) - Initial version, inside the big file "OCamlFlatSupport.ml".
 *)

(*
 * Description: Very simple parser for regular expressions.
 *
 * TODO: Register the errors using the Error module. Turn into a client
 * of the Scanner module.
 *)

module type RegExpSyntaxSig =
sig
	type t =
		| Plus of t * t
		| Seq of t * t
		| Star of t
		| Symb of char
		| Empty
		| Zero
 
	val parse : string -> t
	val toString : t -> string
	val show : t -> unit
end

module RegExpSyntax : RegExpSyntaxSig =
struct
		(* Grammar:
				E -> E + E | E E | E* | c | (E) | ()

			Grammar with priorities:
				E -> T | T + E
				T -> F | F T
				F -> A | A*
				A -> P | c
				P -> (E) | ()
		*)
	type t =
		| Plus of t * t
		| Seq of t * t
		| Star of t
		| Symb of char
		| Empty
		| Zero
	;;

	let inputString = ref ""
	let inputStringLength = ref 0
	let inputStringPosition = ref 0

	let skip () =
		inputStringPosition := !inputStringPosition + 1

	let rec curr () =
		if !inputStringPosition >= !inputStringLength then
			' '
		else if String.get !inputString !inputStringPosition = ' ' then
			( skip(); curr() )
		else
			String.get !inputString !inputStringPosition

	let rec parse_exp () =
		let t = parse_term () in
			match curr() with
				| '+' -> skip(); Plus (t, parse_exp ())
				| _ -> t

	and parse_term () =
		let f = parse_factor () in
			match curr() with
				| '+' | ')' | ' ' -> f
				| _ -> Seq (f, parse_term ())

	and parse_factor () =
		let a = parse_atom () in
			match curr() with
				| '*' -> skip(); (Star a)
				| _ -> a

	and parse_atom () =
		match curr() with
			| '~' -> skip(); Empty
			| '!' -> skip(); Zero
			| '(' -> skip(); parse_parentised ()
			| '+' | '*' -> failwith "Invalid use of wildcard\n"
			| ' ' -> failwith "Premature end of expression\n"
			| c -> skip(); (Symb c)

	and parse_parentised () =
		let e = parse_exp () in (
			match curr() with
				| ')' -> skip(); e
				| _ -> failwith "Right-parenthesis expected\n"
		)

	let parse s =
		inputString := s;
		inputStringLength := String.length s;
		inputStringPosition := 0;
		parse_exp ()

	let rec toStringN n re =
		match re with
			| Plus(l, r) ->
					(if n > 0 then "(" else "") ^
					toStringN 0 l ^ "+" ^ toStringN 0 r
					^ (if n > 0 then ")" else "")
			| Seq(l, r) ->
					(if n > 1 then "(" else "") ^
					toStringN 1 l ^ toStringN 1 r
					^ (if n > 1 then ")" else "")
			| Star(r) ->
					toStringN 2 r ^ "*"
			| Symb(c) -> String.make 1 c
			| Empty -> "~"
			| Zero -> "!"

	let toString re =
		toStringN 0 re

	let show re =
		Util.println [toString re]
end

module RegExpSyntaxTests = struct
	let active = false

	let test0 () =
		let re = RegExpSyntax.parse "ab+~*" in
			RegExpSyntax.show re

	let test1 () =
		let re = RegExpSyntax.parse "~((a+b)*(cd)*)*" in
			RegExpSyntax.show re

	let runAll =
		if active then (
			Util.header "RegExpSyntaxTests";
			test0 ();
			test1 ()
	)
end
(*
 * CFGSyntax.ml
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
 *  Written by Artur Miguel Dias (amd)
 *)

(*
 * ChangeLog:
 *
 * jan/2021 (amd) - Module in an independent file.
 * dec/2019 (amd) - Initial version, inside the big file "OCamlFlatSupport.ml".
 *)

(*
 * Description: Very simple parser for regular expressions.
 *
 * TODO: Register the errors using the Error module. Turn into a client
 * of the Scanner module.
 *)

module type CFGSyntaxSig =
sig
	type rule = { head : char; body : char list; }
	type rules = rule Set.t

	val parse : string Set.t -> rule Set.t
	val toStringList : rule Set.t -> string list
	val show : rule Set.t -> unit
end

module CFGSyntax: CFGSyntaxSig =
struct
	type rule = { head : char; body : char list; }
	type rules = rule Set.t

	let inputString = ref ""
	let inputStringLength = ref 0
	let inputStringPosition = ref 0

	let isWhite c =
		List.mem c [' '; '\t']

	let skip () =
		inputStringPosition := !inputStringPosition + 1

	let rec curr () =
		if !inputStringPosition >= !inputStringLength then
			' '
		else if isWhite (String.get !inputString !inputStringPosition) then
			( skip(); curr() )
		else
			String.get !inputString !inputStringPosition

	let rec parseHead () =
		match curr() with
			| ' ' -> failwith "Premature end of expression\n"
			| c -> skip() ; c

	let rec parseNeck () =
		match curr() with
			| ' ' -> failwith "Premature end of expression\n"
			| '-' -> skip();
					if curr() = '>' then skip()
					else failwith "Bad neck\n"
			| _ -> failwith "Bad neck\n"

	let rec parseBody () =
		match curr() with
			| ' ' -> [[]]
			| '|' -> skip(); []::parseBody ()
			| c -> skip();
					match parseBody () with
						| [] -> failwith "never happens"
						| x::xs -> (c::x)::xs

	let parseLine line =
		if String.trim line = "" then Set.empty
		else (
			inputString := line;
			inputStringLength := String.length line;
			inputStringPosition := 0;
			let h = parseHead () in
			let () = parseNeck () in
			let bs = Set.make (parseBody ()) in
				Set.map (fun b -> {head=h; body=b}) bs
		)

	let parse rs =
		Set.flatMap parseLine rs


	let toString1 r =
		let full = [r.head; ' '; '-'; '>' ; ' '] @ r.body in
			String.concat "" (List.map (String.make 1) full)

	let toString rs =
		let rl = Set.toList rs in
		String.concat "\n" (List.map toString1 rl)

	let toStringList rs =
		let rl = Set.toList rs in
			List.map toString1 rl

	let show rs =
		Util.println [toString rs]
end

module CFGSyntaxTests =
struct
	let active = false

	let test0 () =
		let cfg = Set.make [ "S -> aTb | ~"; "T -> aSb" ] in
		let rules = CFGSyntax.parse cfg in
			CFGSyntax.show rules

	let test1 () =
		let cfg = Set.make ["S -> aSb | ~"] in
		let rules = CFGSyntax.parse cfg in
			CFGSyntax.show rules

	let runAll =
		if active then (
			Util.header "CFGSyntaxTests";
			Util.header "test0";
			test0 ();
			Util.header "test1";
			test1 ();
			Util.header ""
	)
end
(*
 * BasicTypes.ml
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
 *  Written by Artur Miguel Dias (amd)
 *)

(*
 * ChangeLog:
 *
 * jan/2021 (amd) - Module in an independent file.
 * jun/2019 (amd) - Initial version, inside the big file "OCamlFlatSupport.ml".
 *)

(*
 * Description: Some types and constants used across all the modules.
 *)

module BasicTypes =
struct
	type 'a set = 'a Set.t

	type symbol = char
	type symbols = symbol set

	type variable = char
	type variables = char set

	let epsilon: symbol = '~' (* used for representing the empty transitions *)

	type word = symbol list
	type words = word set

	type state = string
	type states = state set
end

open BasicTypes
(*
 * Configuration.ml
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
 *  Written by Artur Miguel Dias (amd)
 *)

(*
 * ChangeLog:
 *
 * jan/2021 (amd) - Module in an independent file.
 * jun/2019 (amd) - Initial version, inside the big file "OCamlFlatSupport.ml".
 *)
 
module type ConfigurationSig =
sig
	val diagnosticsOn : unit -> bool
end


module Configuration : ConfigurationSig =
struct
	let automaticDiagnostics = ref true

	let diagnosticsOn () = !automaticDiagnostics
end
(*
 * Examples.ml
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
 * dec/2020 (amd) - Collected the examples in a single module.
 * sep/2019 (jg) - Initial version, each example in an individual file.
 *)

(*
 * Description: A set of good predefined examples.
 *
 * TODO: Check if these examples are really good and improve.
 *)

module type ExamplesSig =
sig
	val examples : string list
	val example : string -> string
	val see : string -> unit
end

module Examples : ExamplesSig =
struct
	(* Entity definitions *)

	let dfa_1 = {| {
			kind : "finite automaton",
			description : "this is an example",
			name : "dfa_1",
			alphabet: ["a", "b"],
			states : ["START", "A", "B", "C"],
			initialState : "START",
			transitions : [
					["START", "a", "A"], ["A", "b", "B"], ["B", "a", "C"], ["C", "b", "B"],
					["C", "a", "A"]
				],
			acceptStates : ["START", "B", "C"]
		} |}

	let dfa_2 = {| {
			kind : "finite automaton",
			description : "this is an example",
			name : "dfa_2",
			alphabet: ["0", "1"],
			states : ["START", "1", "2", "3"],
			initialState : "START",
			transitions : [
				["START", "1", "1"], ["1", "1", "START"], ["1", "0", "2"], ["2", "0", "1"],
				["2", "1", "3"], ["3", "1", "2"], ["3", "0", "START"], ["START", "0", "3"]
			],
			acceptStates : ["1"]
			} |}

	let fa_abc = {| {
			kind : "finite automaton",
			description : "this is an example",
			name : "fa_abc",
			alphabet : ["a", "b", "c", "d"],
			states : ["START", "A", "AB", "SUCCESS"],
			initialState : "START",
			transitions : [
					["START","a","A"], ["START","b","START"], ["START","c","START"], ["START","d","START"],
					["A","a","A"], ["A","b","AB"], ["A","c","START"], ["A","d","START"],
					["AB","a","A"], ["AB","b","START"], ["AB","c","SUCCESS"], ["AB","d","START"],
					["SUCCESS","a","SUCCESS"], ["SUCCESS","b","SUCCESS"], ["SUCCESS","c","SUCCESS"], ["SUCCESS","d","SUCCESS"]
				],
			acceptStates : ["SUCCESS"]
		} |}

	let nfa_1 = {| {
			kind : "finite automaton",
			description : "this is an example",
			name : "nfa_1",
			alphabet: ["a", "b"],
			states : ["START", "A", "B"],
			initialState : "START",
			transitions : [
					["START", "a", "A"], ["A", "b", "B"], ["A", "b", "START"], ["B", "a", "START"]
				],
			acceptStates : ["START"]
			} |}

	let nfa_2 = {| {
			kind : "finite automaton",
			description : "this is an example",
			name : "nfa_2",
			alphabet : ["a", "b", "c", "d", "e"],
			states : ["START", "A", "AB", "SUCCESS", "UNREACHABLE", "UNPRODUCTIVE"],
			initialState : "START",
			transitions : [
					["START","a","A"], ["START","b","START"], ["START","c","START"], ["START","d","START"],
					["A","a","A"], ["A","b","AB"], ["A","c","START"], ["A","d","START"],
					["AB","a","A"], ["AB","b","START"], ["AB","c","SUCCESS"], ["AB","d","START"],
					["SUCCESS","a","SUCCESS"], ["SUCCESS","b","SUCCESS"], ["SUCCESS","c","SUCCESS"], ["SUCCESS","d","SUCCESS"], ["A","a","AB"], ["UNREACHABLE", "a", "SUCCESS"],
					["SUCCESS", "e", "UNPRODUCTIVE"], ["UNPRODUCTIVE", "a", "UNPRODUCTIVE"]
				],
			acceptStates : ["SUCCESS"]
		} |}

	let re_abc = {| {
			kind : "regular expression",
			description : "this is an example",
			name : "re_abc",
			re : "((a+b)*(cd)*)*"
		} |}

	let re_complex = {| {
			kind : "regular expression",
			description : "this is a complex example",
			name : "re_complex",
			re : "(a+(b(c+d)+ea))*f*g"
		} |}

	let re_convoluted = {| {
			kind : "regular expression",
			description : "this is a convoluted example",
			name : "re_convoluted",
			re : "((((a+b)*(cd)*)*+(e(f+gh*i)*jk)*+lmn)op+q)"
		} |}

	let re_simple = {| {
			kind : "regular expression",
			description : "this is a simple example",
			name : "re_simple",
			re : "a+a*+bc*"
		} |}


	let cfg_simple = {| {
			kind : "context free grammar",
			description : "this is an example",
			name : "cfg_simple",
			alphabet : ["0", "1"],
			variables : ["S", "P"],
			initial : "S",
			rules : [	"S -> 1S0 | P",
						"P -> 0P1 | ~" ]
		} |}

	let exer_astar = {| {
			kind : "exercice",
			description : "this is an example",
			name : "exer_astar",
			problem : "Convert the regular expression a* to finite automaton.",
			inside : ["a","aa","aaa","aaaaaaa"],
			outside : ["d","b","ava"]
		} |}

	let exer_abcd = {| {
			kind : "exercice",
			description : "this is an example",
			name : "exer_abcd",
			problem : "Convert the regular expression (a+b)*(c+d) to finite automaton.",
			inside : ["abc","c","ab","b","abac"],
			outside : ["","aba","bab","abba","baab","abcd"]
		} |}

	let exer_ab = {| {
			kind : "exercice",
			description : "this is an example",
			name : "exer_ab",
			problem : "Convert the regular expression ab*+ba* to finite automaton.",
			inside : ["a","ab","abb","abbbbbbbb","b","ba","baa","baaaaaa"],
			outside : ["","aba","bab","abba","baab","c"]
		} |}

	let exer_re2fa = {| {
			kind : "exercice",
			description : "this is an example",
			name : "exer_re2fa",
			problem : "Converta o autómato finito com alfabeto: [x, y, z], estados: [S, T, V], estado inicial: S, transições [[S, x, S], [S, y, T], [S, z, V], [T, x, T], [T, z, T], [T, y, V], [V, x, T]], e estados finais: [V] em expressão regular.",
			inside : ["z", "xz", "yy", "yzy", "xyy", "zxxy"],
			outside : ["x","y","xy", "xyz", "yyx", "xzxz", "xyxz"]
		} |}

	let exer_readwrite = {| {
			kind : "exercice",
			description : "this is an example",
			name : "exer_readwrite",
			problem : "open,close,read,write",
			inside : ["","orc","owc","orwc","owwrwrrc","ocorwc"],
			outside : ["or","oo","o","w","r","c","orw","owrrww","corwc"]
		} |}

	let pda_clean_test = {|
		{
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
					["B","Z","~", "B", "Z"],
					["TOY6","Z","~", "B", "Z"],
					
					["A","A","~", "TOY1", "A"],
					["TOY2","A","~", "A", "A"],
					["TOY2","A","~", "TOY2", "A"],
					["TOY3", "Z", "~", "SUCCESS", "Z"],
					["TOY4", "Z", "~", "START", "Z"],
					
					["B","Z","~", "SUCCESS", "Z"]
				],
			acceptStates : ["SUCCESS"],
			criteria : "true"
		}
	|}
	
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
	
	let pda_emptyStackDet = {| {
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
	
	let pda_acceptStatesDet = {| {
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
	
	let pda_ww1 = {|
		{
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
		}
	|}
	
	(* Examples table *)

	let oflatExamplesTable = [
		("0n1n_n>0_ES", pda_emptyStackDet);
		("0n1n_n>0_AS", pda_acceptStatesDet);
		("pda_clean", pda_clean_test);
		("pda_cycle", pda_loop);
		("pda_ww¹_NDet", pda_ww1);
	
		("dfa_1", dfa_1);
		("dfa_2", dfa_2);
		("fa_abc", fa_abc);
		("nfa_1", nfa_1);
		("nfa_2", nfa_2);

		("re_abc", re_abc);
		("re_complex", re_complex);
		("re_convoluted", re_convoluted);
		("re_simple", re_simple);

		("cfg_simple", cfg_simple);

		("exer_astar", exer_astar);
		("exer_abcd", exer_abcd);
		("exer_ab", exer_ab);
		("exer_re2fa", exer_re2fa);
		("exer_readwrite", exer_readwrite)
		
		
	]

	let examples =
		List.map fst oflatExamplesTable

	let example name =
		List.assoc name oflatExamplesTable

	let see name =
		Util.println [example name]

end
(*
 * Entity.ml
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
 *  Written by Artur Miguel Dias (amd)
 *)

(*
 * ChangeLog:
 *
 * feb/2021 (amd) - Added the alternative Predef.
 * jan/2021 (amd) - Module in an independent file.
 * jun/2019 (amd) - Initial version, inside the big file "OCamlFlat.ml".
 *)

(*
 * Description: An entity is a named instance of a concept. As now, the entities
 * are the exercises and the FLAT models. The type "alternatives" is to allow
 * the constructor to be used with several kind of parameter forms.
 *)

module Arg =
struct
	type 'r alternatives =
		| JSon of JSon.t
		| Text of string
		| File of string
		| Predef of string
		| Representation of 'r

	let fromAlternatives alt =
		match alt with
			| JSon j -> j
			| Text str -> JSon.from_string str
			| File str -> JSon.from_file str
			| Predef str -> JSon.from_string (Examples.example str)
			| Representation r -> JSon.JNull

	let getRepresentation alt =
		match alt with
			| Representation r -> r
			| _ -> failwith "getRepresentation"
end

module Entity =
struct
	class virtual entity (arg: 'r Arg.alternatives) (expectedKind: string) =
		let errors = Error.start () in
		let r = Arg.fromAlternatives arg in
		let j = if r <> JSon.JNull then r else JSon.makeDummyIdentification(expectedKind) in
		let (kind, description, name) = JSon.identification j in
		object(self)
			method kind: string = kind
			method description: string = description
			method name: string = name
			method errors : string list = !errors
			method virtual validate: unit
			method virtual toJSon: JSon.t
			method handleErrors = (
				if self#kind <> expectedKind then
					Error.error self#kind "Wrong kind" ();
				self#validate;
				if Configuration.diagnosticsOn () then
					Error.show expectedKind self#name;
				Error.stop ()
			)
	end
end

(*
 * Exercise.ml
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
 *  Written by Artur Miguel Dias (amd)
 *)

(*
 * ChangeLog:
 *
 * jan/2021 (amd) - Module in an independent file.
 * set/2019 (amd) - Initial version, inside the big file "OCamlFlat.ml"
 *)

(*
 * Description: Support to pedagogical exercises. The solutions are validated
 * using unit tests.
 *
 * TODO: Improve with support for the validation of extra properties,
 * beyond unit testing.
 *)

module type ExerciseSig =
sig
	type t = { problem : string; inside : words; outside : words; }
	class exercise :
		t Arg.alternatives ->
			object
				method description : string
				method errors : string list
				method handleErrors : unit
				method kind : string
				method name : string
				method representation : t
				method toJSon : JSon.t
				method tracing : unit
				method validate : unit
			end
end

module Exercise : ExerciseSig =
struct

	type t = {
		problem : string;
		inside : words;
		outside : words
	}

	class exercise (arg: 'r Arg.alternatives ) =
		object(self) inherit Entity.entity arg "exercice"

			val representation: t =
				let j = Arg.fromAlternatives arg in
					if j = JSon.JNull then
						Arg.getRepresentation arg
					else
						let problem = JSon.field_string j "problem" in
						let inside = JSon.field_string_set j "inside" in
						let outside = JSon.field_string_set j "outside" in
						{
							problem = problem;
							inside = Set.map (fun s -> Util.str2word s) inside;
							outside = Set.map (fun s -> Util.str2word s) outside
						}

		initializer self#handleErrors	(* placement is crucial - after representation *)

		method representation =
				representation
		method validate = ()
		method toJSon: JSon.t =
			let open JSon in
			let rep = representation in
				JAssoc [
					("kind", JString self#kind);
					("description", JString self#description);
					("name", JString self#name);
					("problem", JString rep.problem );
					("inside", JList (List.map
									(fun w -> JString (Util.word2str w))
									(Set.toList rep.inside)));
					("outside", JList (List.map
									(fun w -> JString (Util.word2str w))
									(Set.toList rep.outside)))
				]
		method tracing = ()
	end
end

module ExerciseTests : sig end =
struct
let active = false

	let test0 () =
		let e = new Exercise.exercise (Arg.Predef "exer_re2fa") in
			let j = e#toJSon in
				JSon.show j

	let runAll =
		if active then (
			Util.header "ExercicesTests";
			test0 ()
		)
end


(*
 * Model.ml
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
 *  Written by Artur Miguel Dias (amd)
 *)

(*
 * ChangeLog:
 *
 * jan/2021 (amd) - Module in an independent file.
 * jun/2019 (amd) - Initial version, inside the big file "OCamlFlat.ml".
 *)

(*
 * Description: Abstract FLAT model.
 *
 * TODO: Probably add a new method "canonical" to generate a
 * normalized/simplified version of the FLAT model.
 *)

module type ModelSig =
sig
	class virtual model :
		'r Arg.alternatives -> string ->
			object
				method kind : string
				method description : string
				method name : string
				method errors : string list
				method handleErrors : unit
				method virtual validate : unit
				method virtual toJSon: JSon.t

				method virtual accept : word -> bool
				method virtual generate : int -> words
				method virtual tracing : unit
				method checkExercise : Exercise.exercise -> bool
				method checkExerciseFailures : Exercise.exercise -> (words * words)
			end
end

module Model : ModelSig
 =
struct
open Exercise

	class virtual model (arg: 'r Arg.alternatives) (expectedKind: string) =
		object(self) inherit Entity.entity arg expectedKind
			method virtual validate: unit
			method virtual accept: word -> bool
			method virtual generate: int -> words
			method virtual tracing: unit

			method checkExercise (exercise: exercise) =
						Set.for_all (fun w -> self#accept w) exercise#representation.inside
					&& Set.for_all (fun w -> not (self#accept w)) exercise#representation.outside

			method checkExerciseFailures (exercise: exercise) =
				(Set.filter (fun w -> not (self#accept w)) exercise#representation.inside,
					Set.filter (fun w -> self#accept w) exercise#representation.outside)

	end

end
(*
 * FiniteAutomaton.ml
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
 * jan/2021 (amd) - Module in an independent file and some cleanup.
 * dec/2019 (jg) - Main functionalities.
 * jun/2019 (amd) - Initial skeleton, inside the big file "OCamlFlat.ml".
 *)

(*
 * Description: Finite automata functionality.
 *
 * TODO: More cleanup.
 *)

module type FiniteAutomatonSig = sig

	type transition = state * symbol * state
	type transitions = transition set
	type t = {
		alphabet : symbols;
		states : states;
		initialState : state;
		transitions : transitions;
		acceptStates : states;
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

				method tracing : unit

				method acceptBreadthFirst: word -> bool
				method accept : word -> bool
				method acceptWithTracing : word -> unit
				method generate : int -> words
				method generateUntil : int -> words
				method reachable : state -> states
				method productive : states
				method getUsefulStates : states
				method getUselessStates : states
				method cleanUselessStates: model
				method areAllStatesUseful: bool

				method toDeterministic : model
				method isDeterministic : bool
				method equivalencePartition: states set
				method minimize : model
				method isMinimized : bool

				method representation : t

				method checkExercise : Exercise.exercise -> bool
				method checkExerciseFailures : Exercise.exercise -> (words * words)
			end
end

module FiniteAutomaton : FiniteAutomatonSig =
struct

	type transition =
		state	(* state *)
		* symbol	(* consumed input symbol *)
		* state	(* next state *)

	type transitions = transition set

	type t = {
		alphabet: symbols;			(* Alphabet *)
		states: states;				(* States *)
		initialState: state;		(* Initial state *)
		transitions: transitions;	(* Transition relation *)
		acceptStates: states		(* Accept states *)
	}

	let modelDesignation = "finite automaton"

	(*------Auxiliary functions---------*)

	(* get starting state, symbol, and/or end state of all transitions in set *)
	let transitionGet1 trns = Set.map ( fun (a,_,_) -> a ) trns
	let transitionGet2 trns = Set.map ( fun (_,b,_) -> b ) trns
	let transitionGet3 trns = Set.map ( fun (_,_,c) -> c ) trns
	let transitionGet23 trns = Set.map (fun(_,b,c) -> (b,c)) trns

	(* fuse all states into a new state *)
	let fuseStates sts = String.concat "_" sts


	(* checks if set ts has at least one transition from state st through symbol sy *)
	let hasTrans st sy ts = Set.exists (fun (x,y,_) -> x = st && y = sy) ts

	(* returns the set of state st and all its states reachable by an epsilon transition *)
	let nextEpsilon1 st ts =
		let trns = Set.filter (fun (a,b,c) -> st = a && b = epsilon) ts in
		let nextStates = transitionGet3 trns in
			Set.add st nextStates

	(* returns the set of states sts and all states reachable from sts through epsilon transitions *)
	let rec closeEmpty sts t =
		let ns = Set.flatMap (fun st -> nextEpsilon1 st t) sts in
			if (Set.subset ns sts) then ns else closeEmpty (Set.union sts ns) t

	(* returns states reachable from st through symbol sy *)
	let nextStates st sy t =
		let n = Set.filter (fun (a,b,c) -> st = a && sy = b) t in
			transitionGet3 n

	class model (arg: t Arg.alternatives) =
		object(self) inherit Model.model arg modelDesignation

			val representation: t =
				let j = Arg.fromAlternatives arg in
					if j = JSon.JNull then
						Arg.getRepresentation arg
					else
						let alphabet = JSon.field_char_set j "alphabet" in
						let states = JSon.field_string_set j "states" in
						let initialState = JSon.field_string j "initialState" in
						let transitions = JSon.field_triples_set j "transitions" in
						let acceptStates = JSon.field_string_set j "acceptStates" in
							{	alphabet = alphabet;
								states = states;
								initialState = initialState;
								transitions = transitions;
								acceptStates = acceptStates
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
					("alphabet", JList (List.map (fun c -> JString (Util.ch2str c)) (Set.toList rep.alphabet)));
					("states", JList (List.map (fun s -> JString s) (Set.toList rep.states)));
					("initialState", JString rep.initialState);
					("transitions", JList (List.map (fun (a,b,c) ->
						JList [JString a; JString (Util.ch2str b); JString c]) (Set.toList rep.transitions)));
					("acceptStates", JList (List.map (fun s -> JString s) (Set.toList rep.acceptStates)))
				]


			(**
			* This method verifies if the automaton is valid.
			* An automaton is considered valid if its initial and acceptance states belong to the set of all its states
			* and if all its transitions have states and symbols belonging to the set of all its states and its alphabet respectively.
			*
			* Desc: If the automaton is invalid, the cause could derive from any combination of the following
			* three options: either the initial state, one of the acceptance states, or one of the transitions does not follow the
			* previously discussed predicate. This method will print to the console stating which combination of these options caused
			* the automaton to be invalid
			*)
			method validate: unit = (

				(* does initial state belong to the set of all states *)
				let validInitSt = Set.belongs representation.initialState representation.states in


				(* are all accepted states members of all states *)
				let validAccSts = Set.subset representation.acceptStates representation.states in

				let fromSt = transitionGet1 representation.transitions in
				let sy = transitionGet2 representation.transitions in
				let toSt = transitionGet3 representation.transitions in
				let alpha = Set.add epsilon representation.alphabet in

				(* do all transitions have states belonging to all states and symbols belonging to the alphabet *)
				let validTrns = (Set.subset fromSt representation.states) &&
				(Set.subset sy alpha) && (Set.subset toSt representation.states) in


				if not validInitSt then
					Error.error representation.initialState
						"initial state does not belong to the set of all states" ()
				;

				if not validAccSts then
					Error.error self#name
						"not all accepted states belong to the set of all states" ()
				;

				if not validTrns then
					Error.error self#name
						"not all transitions are valid" ()
				)


			method tracing : unit = ()



			(**
			* This method verifies if the given word is accepted by the automaton
			*
			* @param w:word -> word to be tested for acceptance
			*
			* @returns bool -> true if w is accepted and false otherwise
			*
			* Desc: Checks if the automaton accepts word w using configurations (that is, pairs formed by a state and
			* a remaining word) and a breadth-first approach as to deal with potential non-termination
			*)
			method acceptBreadthFirst(w: word): bool = false
			(*
				let rec acc cf t sta =
					match cf with
						[] -> false
						|(st,[])::ls ->
							let accepts = (Set.inter (closeEmpty (Set.make [st]) t) sta) <> Set.empty in
								accepts || acc ls t sta
						|(st,x::xs)::ls ->
							let n = nextStates st x t in
							let cfn = Set.map (fun c -> (c,xs)) n in
							let n2 = nextStates st epsilon t in
							let cfn2 = Set.map (fun c -> (c,x::xs)) n2 in
								acc (Set.flatten (Set.make [ls;cfn;cfn2])) t sta in
				acc (Set.make [(representation.initialState,w)]) representation.transitions representation.acceptStates
			*)

			(**
			* This method verifies if the given word is accepted by the automaton
			*
			* @param w:word -> word to be accepted
			*
			* @returns bool -> true if w is accepted and false otherwise
			*
			* Desc: Checks if the automaton accepts word w using functions over sets of states
			*)
			method accept (w: word): bool =

				let transition sts sy t =
					let nsts = Set.flatMap (fun st -> nextStates st sy t) sts in
						Set.union nsts (closeEmpty nsts t) in

				let rec acceptX sts w t =
					match w with
						[] -> (Set.inter sts representation.acceptStates) <> Set.empty
						|x::xs -> let nextSts = transition sts x t in
							nextSts <> Set.empty && acceptX nextSts xs t in

				let i = closeEmpty (Set.make [representation.initialState]) representation.transitions in
					acceptX i w representation.transitions



			method acceptWithTracing (w:word): unit =


				let transition sts sy t =
					let nsts = Set.flatMap (fun st -> nextStates st sy t) sts in
						Set.union nsts (closeEmpty nsts t) in

				let rec acceptX sts w t =
					match w with
						[] -> [(w,sts)]
						|x::xs -> let nextSts = transition sts x t in
									let res = acceptX nextSts xs t in
										(w,sts)::res
				in

				let i = closeEmpty (Set.make [representation.initialState]) representation.transitions in

				let res = acceptX i w representation.transitions in

				let printRes w sts =
					Util.print ["('"; Util.word2str w; "',["];
					Set.iter (fun st -> Util.print [st; ";"]) sts;
					Util.print ["])"];

				in

				List.iter (fun (w,sts) -> printRes w sts; Util.print [";"]) res; Util.println []




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

				(* adds symbol to the left of all words *)
				let addSyToRWords symb ws = Set.map (fun l -> symb::l) ws in

				let hasAcceptState sts accSts = Set.exists (fun st -> Set.belongs st accSts) sts in
				let nxtNonEmptyTrns st ts = Set.filter (fun (a,b,_) -> a = st && b <> epsilon) ts in

				let rec gen n state transitions accSts =

					let clsEmpty = (closeEmpty (Set.make [state]) transitions) in
					if n = 0 then
						if hasAcceptState clsEmpty accSts then Set.make [[]] else Set.empty

					else
						let trnsSet = Set.flatMap (fun st -> nxtNonEmptyTrns st transitions ) clsEmpty in

						let rwords st1 l1 = gen (l1-1) st1 transitions accSts in
						let genX sy st l = addSyToRWords sy (rwords st l) in

								Set.flatMap (fun (_,sy,st) -> genX sy st n) trnsSet
				in
				gen length representation.initialState representation.transitions representation.acceptStates


			(**
			* This method generates all words up to a given size which are accepted by the automaton
			*
			* Precondition -> length >= 0
			*
			* @param length:int -> maximum size of all words to be generated
			*
			* @returns words -> the set of all words with size length or less
			*)
			method generateUntil (length: int): words =

				(* adds symbol to the left of all words *)
				let addSyToRWords symb ws = Set.map (fun l -> symb::l) ws in

				let hasAcceptState sts accSts = Set.exists (fun st -> Set.belongs st accSts) sts in
				let nxtNonEmptyTrns st ts = Set.filter (fun (a,b,_) -> a = st && b <> epsilon) ts in

				let rec gen n state transitions accSts =

					let clsEmpty = (closeEmpty (Set.make [state]) transitions) in
					if n = 0 then
						if hasAcceptState clsEmpty accSts then Set.make [[]] else Set.empty

					else
						let trnsSet = Set.flatMap (fun st -> nxtNonEmptyTrns st transitions ) clsEmpty in
						let genX sy st l = addSyToRWords sy (gen (l-1) st transitions accSts) in
						let lenOneOrMore = Set.flatMap (fun (_,sy,st) -> genX sy st n) trnsSet in
						let lenZero = if hasAcceptState clsEmpty accSts then Set.make [[]] else Set.empty in
							Set.union lenOneOrMore lenZero
				in
				gen length representation.initialState representation.transitions representation.acceptStates


			(**
			* This method generates all states that are reachable from the given state. A state is reachable from s if there
			* exists a word that starting on s will lead to that state
			*
			* @param s:state -> the given state
			*
			* @returns states -> the set of all states reachable from s.
			*)
			method reachable (s:state): states =

				let neighbourSts st t = transitionGet3 (Set.filter (fun (a,_,_) -> a = st) t) in
				let nextStates sts t = Set.flatMap (fun st -> neighbourSts st t) sts in
				let remain s t = Set.filter (fun (a,_,_) -> not (Set.belongs a s)) t in

				let rec reach visited s t = if visited = s then Set.empty else Set.union s ( reach s (nextStates s t) (remain s t) ) in
					reach Set.empty (Set.make [s]) representation.transitions



			(**
			* This method generates all productive states. A state is productive if there exists a word that will lead said state
			* to an acceptance state
			*
			* @returns states -> the set of all productive states
			*
			* Desc: For each state of the automaton, this method applies the reachable method and checks if any of the resulting
			* states is an acceptance state, and if it is then that state will belong to the resulting set of productive states
			*)
			method productive: states =

				let reachsAccSt st = Set.exists (fun s -> Set.belongs s representation.acceptStates ) (self#reachable st) in
					Set.filter (fun st -> reachsAccSt st) representation.states

			(**
			* This method generates the set of all useful states
			*
			* @returns states -> the set of all useful states
			*)
			method getUsefulStates: states =
				Set.inter self#productive (self#reachable representation.initialState)


			(**
			* This method generates the set of all non useful states
			*
			* @returns states -> the set of all non useful states
			*)
			method getUselessStates: states =
				Set.diff representation.states self#getUsefulStates


			(**
			* This method creates the equivalent automaton where all states are useful
			*
			* @returns FiniteAutomaton.model -> the new equivalent automaton where all states are useful
			*
			* Desc: The new automaton is created by eliminating from the original automaton all its non useful states, all transitions
			* that have a non useful state, and all symbols of the alphabet that only appear in said transitions
			*)
			method cleanUselessStates: model =

				let usfSts = self#getUsefulStates in
				let usfTrs = Set.filter (fun (a,_,c) -> Set.belongs a usfSts &&
														Set.belongs c usfSts)
								representation.transitions in

				let alf = transitionGet2 usfTrs in
				let usfAlf = Set.diff alf (Set.make [epsilon]) in
				let newAccSts = Set.inter representation.acceptStates usfSts in
				let usfSts = Set.add representation.initialState usfSts in

				new model (Arg.Representation {
								alphabet = usfAlf;
								states = usfSts;
								initialState = representation.initialState;
								transitions = usfTrs;
								acceptStates = newAccSts
						} )


			(**
			* This method verifies if all the automaton's states are useful
			*
			* @returns bool -> true if all states of the automaton are useful, false otherwise
			*)
			method areAllStatesUseful: bool =

				let usfSts = self#getUsefulStates in
					Set.size representation.states = Set.size usfSts


			(**
			* This method converts the non-deterministic automaton into its deterministic equivalent
			*
			* @returns FiniteAutomaton.model -> the new deterministic automaton
			*
			* Desc: If the automaton to determinize is already deterministic,
			* the resulting automaton will be equal to the original
			*)
			method toDeterministic: model =

				let move sts sy ts = Set.flatMap (fun st -> nextStates st sy ts ) sts in

				(* generates the set of states reachable from the given state set though the given symbol *)
				let newR oneR sy ts =
					let nxtSts = move oneR sy ts in
					let clsempty = closeEmpty nxtSts ts in
					Set.union nxtSts clsempty in

				(* creates all transitions (given state set, a given symbol, states reachable from set through given symbol) *)
				let rToTs r =
					let nxtTrans = Set.map (fun sy -> (r,sy,newR r sy representation.transitions)) representation.alphabet in
						Set.filter (fun (_,_,z) -> not (z = Set.empty)) nxtTrans in

				(* applies previous function to all state sets until no new set is generated *)
				let rec rsToTs stsD rD trnsD alph =
					let nxtTs = Set.flatMap (fun stSet -> rToTs stSet ) rD in
					let nxtRs = Set.map (fun (_,_,z) -> z) nxtTs in
					let newRs = Set.filter (fun r -> not (Set.belongs r stsD)) nxtRs in
					if newRs = Set.empty then (Set.union trnsD nxtTs) else
						rsToTs (Set.union newRs stsD) newRs (Set.union trnsD nxtTs) alph in


				let r1 = closeEmpty (Set.make [representation.initialState]) representation.transitions in

				(* all transitions of the new deterministic automaton *)
				let trnsD = rsToTs (Set.make [r1]) (Set.make [r1]) Set.empty representation.alphabet in

				let tds = Set.map (fun (a,b,c) -> (fuseStates (Set.toList a), b, fuseStates (Set.toList c))) trnsD in

				let newInitialState = fuseStates (Set.toList r1) in

				let stSet1 = Set.map (fun (a,_,_) -> a) trnsD in
				let stSet2 = Set.map (fun (_,_,c) -> c) trnsD in
				let stSet = Set.union stSet1 stSet2 in

				let isAccepState st = Set.belongs st representation.acceptStates in
				let hasAnAccepSt set = Set.exists (fun st -> isAccepState st ) set in
				let newAccStsSet = Set.filter (fun set -> hasAnAccepSt set) stSet in

				let newAllSts = Set.map (fun set -> fuseStates (Set.toList set)) stSet in
				let newAccSts = Set.map (fun set -> fuseStates (Set.toList set)) newAccStsSet in


				new model (Arg.Representation {
								alphabet = representation.alphabet;
								states = newAllSts;
								initialState = newInitialState;
								transitions = tds;
								acceptStates = newAccSts
						} )

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

				let trnsFromSt st ts = Set.filter (fun (st1,sy,_) -> st1 = st && sy <> epsilon) ts in

				let isStDeter st ts =
					let allSts = closeEmpty (Set.make [st]) ts in
					let allTs = Set.flatMap (fun st -> trnsFromSt st ts) allSts in
					let sys = transitionGet2 allTs in
						Set.size allTs = Set.size sys in

				let hasNondeterSt = Set.exists (fun st -> not (isStDeter st representation.transitions) )
										representation.states in
					not hasNondeterSt



			(* partition states by equivalence *)
			method equivalencePartition: states set =

				let fa = self#toDeterministic in

				let fa2 = fa#cleanUselessStates in


				let rep = fa2#representation in

				let (inF, notF) = Set.partition (fun x -> Set.belongs x rep.acceptStates) rep.states in
				let distI1 = Set.combinations inF notF in

				let hasTransMulti sts sy ts = Set.partition (fun st -> hasTrans st sy ts) sts in
				let distI2 = Set.flatMap (fun sy -> Util.distrib2 Set.combinations
													(hasTransMulti rep.states sy rep.transitions))
								rep.alphabet in


				let distI = Set.union distI1 distI2 in

				let stsXSts = Set.combinations rep.states rep.states in

				(* generates all pairs of states that can reach the pair (st1,st2) through a transition with symbol sy *)
				let reachingSts st1 st2 sy p =
					let t1 = Set.filter (fun (_,y,z) -> z = st1 && y = sy) rep.transitions in
					let t2 = Set.filter (fun (_,y,z) -> z = st2 && y = sy) rep.transitions in
					let s1 = transitionGet1 t1 in
					let s2 = transitionGet1 t2 in
						Set.diff (Set.combinations s1 s2) p in

				let findAR p q = Set.flatMap (fun (a,b) -> Set.flatMap (fun sy -> reachingSts a b sy p) rep.alphabet) q in

				let distA = findAR distI distI in

				let rec aped p q = if (q = Set.empty || (Set.union p q) = stsXSts) then Set.union p q
					else aped (Set.union p q) (findAR (Set.union p q) q ) in

				let dist = aped distI distA in


				(* given for example states a b c d generates (a,a) (a,b) (a,c) (a,d) (b,b) (b,c) (b,d) (c,c) (c,d) (d,d) *)
				let rec halfCombs sts =
					match sts with
						[] -> Set.empty
						|x::xs -> Set.union (Set.combinations (Set.make [x]) (Set.make sts)) (halfCombs xs) in

				let halfTriang = halfCombs (Set.toList rep.states) in

				(* given set of equivalent states dicti, substitutes state st for its leftmost equivalent state according to dicti *)
				let rec translate st dicti =
					match dicti with
						[] -> st
						|(eq1,eq2)::xs -> if eq2 = st then eq1 else translate st xs in

				(* the set of equivalent state pairs are those not present in the set of distinct state pairs *)
				let equiv = Set.filter ( fun (a,b) -> not (Set.belongs (a,b) dist) &&
														not (Set.belongs (b,a) dist) ) halfTriang in

				let equivList = Set.toList equiv in
				let hasAny st1 st2 sta stb = (translate st1 equivList) = sta || (translate st2 equivList) = sta
											|| (translate st1 equivList) = stb || (translate st2 equivList) = stb in


				let rec agroup eq =
					match eq with
						| [] -> Set.empty
						| (a,b)::ls ->
							let (part1,part2) = Set.partition (fun (x,y) -> hasAny x y a b) (Set.make eq) in
							let gRemain = Set.flatMap (fun (c,d) -> Set.make [c;d]) part1 in
								Set.add (Set.union (Set.make [a;b]) gRemain) (agroup (Set.toList part2))
				in

				agroup equivList




			(**
			* This method minimizes the automaton
			*
			* @returns FiniteAutomaton.model -> the new minimal equivalent automaton
			*
			* Desc: The given automaton is minimized according to the process described in lecture a15.
			*)
			method minimize: model =

				let fa = self#toDeterministic in

				let fa2 = fa#cleanUselessStates in


				let rep = fa2#representation in

				let (inF, notF) = Set.partition (fun x -> Set.belongs x rep.acceptStates) rep.states in
				let distI1 = Set.combinations inF notF in

				let hasTransMulti sts sy ts = Set.partition (fun st -> hasTrans st sy ts) sts in
				let distI2 = Set.flatMap (fun sy -> Util.distrib2 Set.combinations
													(hasTransMulti rep.states sy rep.transitions))
								rep.alphabet in


				let distI = Set.union distI1 distI2 in

				let stsXSts = Set.combinations rep.states rep.states in

				(* generates all pairs of states that can reach the pair (st1,st2) through a transition with symbol sy *)
				let reachingSts st1 st2 sy p =
					let t1 = Set.filter (fun (_,y,z) -> z = st1 && y = sy) rep.transitions in
					let t2 = Set.filter (fun (_,y,z) -> z = st2 && y = sy) rep.transitions in
					let s1 = transitionGet1 t1 in
					let s2 = transitionGet1 t2 in
						Set.diff (Set.combinations s1 s2) p in

				let findAR p q = Set.flatMap (fun (a,b) -> Set.flatMap (fun sy -> reachingSts a b sy p) rep.alphabet) q in

				let distA = findAR distI distI in

				let rec aped p q = if (q = Set.empty || (Set.union p q) = stsXSts) then Set.union p q
					else aped (Set.union p q) (findAR (Set.union p q) q ) in

				let dist = aped distI distA in


				(* given for example states a b c d generates (a,b) (a,c) (a,d) (b,c) (b,d) (c,d) *)
				let rec halfCombs sts =
					match sts with
						[] -> Set.empty
						|x::xs -> Set.union (Set.combinations (Set.make [x]) (Set.make xs)) (halfCombs xs) in
				let halfTriang = halfCombs (Set.toList rep.states) in

				(* given set of equivalent states dicti, substitutes state st for its leftmost equivalent state according to dicti *)
				let rec translate st dicti =
					match dicti with
						[] -> st
						|(eq1,eq2)::xs -> if eq2 = st then eq1 else translate st xs in

				(* the set of equivalent state pairs are those not present in the set of distinct state pairs *)
				let equiv = Set.filter ( fun (a,b) -> not (Set.belongs (a,b) dist) &&
														not (Set.belongs (b,a) dist) ) halfTriang in

				let equivList = Set.toList equiv in

				let eq = Set.map (fun (a,b) -> b) equiv in
				let newSts = Set.diff rep.states eq in
				let newInitSt = translate rep.initialState equivList in
				let newAccSts = Set.inter rep.acceptStates newSts in
				let newTrans = Set.map (fun (x,y,z) -> (translate x equivList,y,translate z equivList) ) rep.transitions in


				new model (Arg.Representation {
								alphabet = rep.alphabet;
								states = newSts;
								initialState = newInitSt;
								transitions = newTrans;
								acceptStates = newAccSts
						} )


			(**
			* This method verifies if the automaton is minimal
			*
			* @returns boolean -> true if automaton is minimal, false otherwise
			*
			* Desc: The given automaton is considered minimal if the result of minimizing it is an automaton with the same
			* number of states
			*)
			method isMinimized: bool =

				let fa = self#minimize in
				let rep = fa#representation in
					Set.size representation.states = Set.size rep.states


		end

end

module FiniteAutomatonTests : sig end =
struct
	let active = false

	let test0 () =
		let fa = new FiniteAutomaton.model (Arg.Predef "fa_abc") in
			let j = fa#toJSon in
				JSon.show j

	let testBug () =
		let fa = new FiniteAutomaton.model (Arg.Predef "fa_abc") in
		let fa2 = fa#toDeterministic in
			let j = fa2#toJSon in
				JSon.show j;
		let fa3 = fa2#cleanUselessStates in
			let j = fa3#toJSon in
				JSon.show j

	let testBug2 () =
		let fa = new FiniteAutomaton.model (Arg.Predef "fa_abc") in
		let fa2 = fa#toDeterministic in
			Util.println ["productive states:"];
			Util.printStates (Set.toList fa2#productive);
			Util.println []

	let fa_accept = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "ab123",
		alphabet : ["a", "b"],
		states : ["1", "2", "3"],
		initialState : "1",
		transitions : [
				["1","a","2"], ["1","b","3"],
				["2","b","2"],
				["3","a","3"]
			],
		acceptStates : ["2", "3"]
	} |}

	let fa_accept2 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a", "b", "c", "d"],
		states : ["START", "A", "AB", "C", "SUCCESS", "D"],
		initialState : "START",
		transitions : [
				["START","a","A"], ["START","~","AB"],
				["A","~","C"],
				["AB","b","SUCCESS"], ["AB","~","SUCCESS"],
				["C","~","SUCCESS"], ["C","d","C"],
				["SUCCESS","~","START"]
			],
		acceptStates : ["SUCCESS"]
	} |}

	let check f w =
		let msg = 
			if f w then "word was accepted" else "word was not accepted"
		in Util.println [msg]

	let testAcceptBF () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_accept) in
			check fa#acceptBreadthFirst [];
			check fa#acceptBreadthFirst ['a'];
			check fa#acceptBreadthFirst ['a';'b'];
			check fa#acceptBreadthFirst ['b'];
			check fa#acceptBreadthFirst ['b';'a'];
			check fa#acceptBreadthFirst ['a';'b';'b'];
			check fa#acceptBreadthFirst ['a';'b';'a'];
			check fa#acceptBreadthFirst ['b';'a';'a'];
			check fa#acceptBreadthFirst ['b';'a';'b'];
			Util.println []

	let testAcceptBF2 () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_accept2) in
			check fa#acceptBreadthFirst [];
			check fa#acceptBreadthFirst ['a'];
			check fa#acceptBreadthFirst ['a';'d'];
			check fa#acceptBreadthFirst ['a';'b';'a';'d'];
			check fa#acceptBreadthFirst ['c'];
			Util.println []

	let testAccept () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_accept) in
			check fa#accept [];
			check fa#accept ['a'];
			check fa#accept ['a';'b'];
			check fa#accept ['b'];
			check fa#accept ['b';'a'];
			check fa#accept ['a';'b';'b'];
			check fa#accept ['a';'b';'a'];
			check fa#accept ['b';'a';'a'];
			check fa#accept ['b';'a';'b'];
			Util.println []

	let testAccept2 () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_accept2) in
			check fa#accept [];
			check fa#accept ['a'];
			check fa#accept ['a';'d'];
			check fa#accept ['a';'b';'a';'d'];
			check fa#accept ['c'];
			Util.println []

	let testAccTrace () =
		let fa = new FiniteAutomaton.model (Arg.Predef "fa_abc") in
			fa#acceptWithTracing ['a';'b';'e']

	let fa_generate = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a", "b"],
		states : ["S1", "S2", "S3", "S4", "S5"],
		initialState : "S1",
		transitions : [
				["S1","b","S2"], ["S1","a","S3"], ["S1","~","S3"],
				["S2","~","S3"],
				["S3","~","S3"]
			],
		acceptStates : ["S3"]
	} |}

	let fa_generate2 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a", "b"],
		states : ["S1", "S2"],
		initialState : "S1",
		transitions : [
				["S1","a","S1"], ["S1","b","S2"],
				["S2","a","S2"], ["S2","b","S1"]

			],
		acceptStates : ["S2"]
	} |}

	let fa_generate3 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a"],
		states : ["S1"],
		initialState : "S1",
		transitions : [
				["S1","a","S1"]
			],
		acceptStates : ["S1"]
	} |}

	let fa_generate4 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a","b"],
		states : ["S1","S2"],
		initialState : "S1",
		transitions : [
				["S1","a","S1"], ["S1","b","S2"],
				["S2","a","S2"]
			],
		acceptStates : ["S1"]
	} |}

	let testGenerate () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_generate) in
			Util.println ["generated words size 0:"]; Util.printWords (Set.toList (fa#generate 0) );
			Util.println ["generated words size 1:"]; Util.printWords (Set.toList (fa#generate 1) );
			Util.println ["generated words size 2:"]; Util.printWords (Set.toList (fa#generate 2) );
			Util.println ["generated words size 100:"]; Util.printWords (Set.toList (fa#generate 100) );
			Util.println []

	let testGenerate2 () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_generate2) in
			Util.println ["generated words size 0:"]; Util.printWords (Set.toList (fa#generate 0));
			Util.println ["generated words size 1:"]; Util.printWords (Set.toList (fa#generate 1));
			Util.println ["generated words size 2:"]; Util.printWords (Set.toList (fa#generate 2));
			Util.println ["generated words size 3:"]; Util.printWords (Set.toList (fa#generate 3) );
			Util.println ["generated words size 4:"]; Util.printWords (Set.toList (fa#generate 4) );
			Util.println ["generated words size 18:"]; Util.printWords (Set.toList (fa#generate 18) );

			Util.println []

	let testGenerate3 () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_generate3) in
			Util.println ["generated words size 0:"]; Util.printWords (Set.toList (fa#generate 0));
			Util.println ["generated words size 1:"]; Util.printWords (Set.toList (fa#generate 1));
			Util.println ["generated words size 10:"]; Util.printWords (Set.toList (fa#generate 10));
			Util.println ["generated words size 50:"]; Util.printWords (Set.toList (fa#generate 50));
			Util.println ["generated words size 100:"]; Util.printWords (Set.toList (fa#generate 100));
			Util.println ["generated words size 1000:"]; Util.printWords (Set.toList (fa#generate 1000));
			Util.println []

	let testGenerate4 () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_generate4) in
			Util.println ["generated words size 0:"]; Util.printWords (Set.toList (fa#generate 0));
			Util.println ["generated words size 1:"]; Util.printWords (Set.toList (fa#generate 1));
			Util.println ["generated words size 10:"]; Util.printWords (Set.toList (fa#generate 10));
			Util.println ["generated words size 100:"]; Util.printWords (Set.toList (fa#generate 100));
			Util.println []

	let testGenerateUntil () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_generate) in
			Util.println ["generated words size 5:"]; Util.printWords (Set.toList (fa#generateUntil 5));
			Util.println [];
		let fa = new FiniteAutomaton.model (Arg.Text fa_generate2) in
			Util.println ["generated words size 5:"]; Util.printWords (Set.toList (fa#generateUntil 5));
			Util.println [];
		let fa = new FiniteAutomaton.model (Arg.Text fa_generate3) in
			Util.println ["generated words size 5:"]; Util.printWords (Set.toList (fa#generateUntil 5));
			Util.println [];
		let fa = new FiniteAutomaton.model (Arg.Text fa_generate4) in
			Util.println ["generated words size 5:"]; Util.printWords (Set.toList (fa#generateUntil 5));
			Util.println []

	let fa_reach = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a", "b"],
		states : ["S1", "S2", "S3"],
		initialState : "S1",
		transitions : [
			],
		acceptStates : ["S1"]
	} |}

	let fa_reach2 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a","b"],
		states : ["S1","S2","S3","S4","S5","S6"],
		initialState : "S1",
		transitions : [
				["S1","~","S2"], ["S1","a","S3"],
				["S2","a","S2"],
				["S3","~","S4"],
				["S4","~","S5"],
				["S5","~","S3"], ["S5","b","S6"], ["S5","~","S5"]
			],
		acceptStates : ["S1"]
	} |}

	let testReachable () =
			let open FiniteAutomaton in
			let fa = new FiniteAutomaton.model (Arg.Text fa_reach) in
			let fa2 = new FiniteAutomaton.model (Arg.Text fa_reach2) in
			let start = fa#representation.initialState in
			let start2 = fa2#representation.initialState in
				Util.println ["reachable states:"]; Util.printStates (Set.toList (fa#reachable start)); Util.println [];
				Util.println ["reachable states:"]; Util.printStates (Set.toList (fa#reachable start2)); Util.println []

	let fa_productive = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a","b"],
		states : ["S1","S2","S3","S4"],
		initialState : "S1",
		transitions : [
				["S1","a","S2"], ["S1","b","S3"],
				["S4","a","S2"], ["S4","b","S3"], ["S3","a","S3"]
			],
		acceptStates : ["S4"]
	} |}

	let fa_productive2 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a","b"],
		states : ["S1","S2","S3","S4","S5","S6","S7"],
		initialState : "S1",
		transitions : [
				["S1","~","S2"], ["S1","a","S2"], ["S1","~","S3"], ["S1","a","S3"], ["S1","~","S5"], ["S1","a","S5"],
				["S2","~","S1"], ["S2","a","S1"],
				["S4","~","S3"], ["S4","a","S3"],["S4","~","S4"], ["S4","a","S4"],
				["S5","~","S2"], ["S5","a","S2"],["S5","~","S6"], ["S5","a","S6"],
				["S6","~","S6"], ["S6","a","S6"],["S6","~","S7"], ["S6","a","S7"],
				["S7","~","S3"], ["S7","a","S3"],["S7","~","S5"], ["S7","a","S5"]
			],
		acceptStates : ["S2","S4"]
	} |}

	let testProductive () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_productive) in
		let fa2 = new FiniteAutomaton.model (Arg.Text fa_productive2) in
			Util.println ["productive states:"]; Util.printStates (Set.toList (fa#productive)); Util.println [];
			Util.println ["productive states:"]; Util.printStates (Set.toList (fa2#productive)); Util.println []


	let fa_clean = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a","b"],
		states : ["S1","S2","S3","S4"],
		initialState : "S1",
		transitions : [
				["S1","a","S2"], ["S1","b","S3"],
				["S4","a","S2"], ["S4","b","S3"], ["S3","a","S3"]
			],
		acceptStates : ["S4"]
	} |}

	let fa_clean2 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a","b"],
		states : ["S1","S2","S3","S4"],
		initialState : "S1",
		transitions : [
				["S1","a","S2"], ["S1","~","S3"],
				["S3","a","S2"], ["S3","~","S1"], ["S3","a","S4"]
			],
		acceptStates : ["S2"]
	} |}

	let testClean () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_clean) in
		let fa2 = new FiniteAutomaton.model (Arg.Text fa_clean2) in
		let mfa = fa#cleanUselessStates in
		let mfa2 = fa2#cleanUselessStates in
		let j = mfa#toJSon in
		let j2 = mfa2#toJSon in
			JSon.show j; Util.println [];
			JSon.show j2; Util.println []

	let fa_isDeter = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "isDeter",
		alphabet : ["a", "b"],
		states : ["S1", "S2", "S3", "S4", "S5"],
		initialState : "S1",
		transitions : [
				["S1","~","S2"], ["S1","a","S3"],
				["S2","a","S3"], ["S2","b","S2"]
			],
		acceptStates : ["S3"]
	} |}

	let fa_isDeter2 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "isDeter",
		alphabet : ["a", "b"],
		states : ["S1", "S2", "S3", "S4", "S5"],
		initialState : "S1",
		transitions : [
				["S1","~","S2"], ["S1","b","S3"],
				["S2","a","S4"], ["S4","b","S5"],
				["S3","b","S5"]
			],
		acceptStates : ["S5"]
	} |}

	let fa_isDeter3 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "isDeter",
		alphabet : ["a", "b"],
		states : ["S1", "S2", "S3", "S4", "S5"],
		initialState : "S1",
		transitions : [
				["S1","a","S2"], ["S1","a","S3"],
				["S2","b","S4"],
				["S3","b","S4"]
			],
		acceptStates : ["S4"]
	} |}

	let fa_toDeter = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["0", "1"],
		states : ["S1", "S2", "S3", "S4", "S5"],
		initialState : "S1",
		transitions : [
				["S1","1","S2"], ["S1","1","S3"] , ["S1","0","S5"],
				["S2","~","S4"],
				["S4","0","S3"],
				["S5","1","S2"], ["S5","0","S3"], ["S5","0","S4"]
			],
		acceptStates : ["S3"]
	} |}

	let testIsDeterministic () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_isDeter) in
		let fa2 = new FiniteAutomaton.model (Arg.Text fa_isDeter2) in
		let fa3 = new FiniteAutomaton.model (Arg.Text fa_isDeter3) in
			if fa#isDeterministic then
				Util.println ["automata is deterministic"] else Util.println ["automata is non-deterministic"];
			if fa2#isDeterministic then
				Util.println ["automata is deterministic"] else Util.println ["automata is non-deterministic"];
			if fa3#isDeterministic then
				Util.println ["automata is deterministic"] else Util.println ["automata is non-deterministic"]



	let testToDeterministic () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_toDeter) in
		let mfa = fa#toDeterministic in
		let j = mfa#toJSon in
			JSon.show j;
		let fa = new FiniteAutomaton.model (Arg.Text fa_isDeter) in
		let mfa = fa#toDeterministic in
		let j = mfa#toJSon in
			JSon.show j


	let testEquivalence () =
		let fa = new FiniteAutomaton.model (Arg.Predef "fa_abc") in
		let s = fa#equivalencePartition in
			Set.iter (fun s -> Util.print ["set: "]; Util.printStates (Set.toList s)) s


	let fa_minimize = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a", "b"],
		states : ["S1", "S2", "S3", "S4", "S5"],
		initialState : "S1",
		transitions : [
				["S1","a","S2"], ["S1","b","S3"],
				["S2","b","S4"], ["S2","a","S3"],
				["S3","a","S2"], ["S3","b","S4"],
				["S4","b","S3"], ["S4","a","S2"],
				["S4","a","S5"]
			],
		acceptStates : ["S4"]
	} |}

	let fa_minimize2 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "min",
		alphabet : ["i", "c", "1", "2"],
		states : ["S01", "S02", "S03", "S04", "S05",
				"S06", "S07", "S08", "S09", "S10"],
		initialState : "S01",
		transitions : [
				["S01","i","S02"],
				["S02","1","S03"], ["S02","i","S02"],
				["S03","1","S04"], ["S03","i","S04"],
				["S04","1","S03"], ["S04","2","S05"], ["S04","i","S04"],
				["S05","i","S06"], ["S05","c","S07"],
				["S06","i","S06"], ["S06","1","S03"],
				["S07","1","S04"], ["S07","i","S08"],
				["S08","i","S08"], ["S08","1","S03"], ["S08","2","S09"],
				["S09","c","S03"], ["S09","i","S10"],
				["S10","1","S03"], ["S10","i","S10"]
			],
		acceptStates : ["S10"]
	} |}

	let fa_minimize3 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a", "b","c"],
		states : ["S0","S1", "S2", "S3", "S4", "S5"],
		initialState : "S0",
		transitions : [
				["S0","a","S1"], ["S0","b","S2"],
				["S1","b","S0"], ["S1","a","S1"], ["S1","c","S4"],
				["S2","b","S0"], ["S2","a","S2"], ["S2","c","S5"],
				["S3","b","S1"], ["S3","a","S3"], ["S3","c","S4"],
				["S4","b","S5"],
				["S5","b","S4"]
			],
		acceptStates : ["S4","S5"]
	} |}

	let fa_minimize4 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["0", "1"],
		states : ["00","01", "10", "11"],
		initialState : "00",
		transitions : [
				["00","1","01"], ["00","0","10"],
				["01","1","00"], ["01","0","11"],
				["10","0","00"], ["10","1","11"],
				["11","1","10"], ["11","0","01"]
			],
		acceptStates : ["01"]
	} |}

	let testMinimize () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_minimize) in
		let mfa = fa#minimize in
		let j = mfa#toJSon in
			JSon.show j

	let testMinimize2 () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_minimize2) in
		let mfa = fa#minimize in
		let j = mfa#toJSon in
			JSon.show j

	let testMinimize3 () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_minimize3) in
		let mfa = fa#minimize in
		let j = mfa#toJSon in
			JSon.show j

	let testMinimize4 () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_minimize4) in
		let mfa = fa#minimize in
		let j = mfa#toJSon in
			JSon.show j

	let runAll =
		if active then (
			Util.header "FiniteAutomatonTests";
			test0 ();
			testBug ();
			testBug2 ();
			testAcceptBF ();
			testAcceptBF2 ();
			testAccept ();
			testAccept2 ();
			testAccTrace ()
		)
end

(*
 * RegularExpression.ml
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
 * jan/2021 (amd) - Module in an independent file and some cleanup.
 * dec/2019 (jg) - Main functionalities.
 * jun/2019 (amd) - Initial skeleton, inside the big file "OCamlFlat.ml".
 *)

(*
 * Description: Regular expressions functionality.
 *
 * TODO: More cleanup. Improve the regular expression simplifier.
 *)

module type RegularExpressionSig =
sig
	type t = RegExpSyntax.t

	type reTree =
	| Fail
	| Tree of word * t * reTree list

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

				method accept : word -> bool
				method allTrees : word -> unit
				method generate : int -> words
				method tracing : unit

				method alphabet : symbols
				method quasiLanguage : words

				method simplify : model

				method representation : t

				method checkExercise : Exercise.exercise -> bool
				method checkExerciseFailures : Exercise.exercise -> (words * words)

			end
end

module RegularExpression : RegularExpressionSig =
struct
	open RegExpSyntax

	type t = RegExpSyntax.t

	type reTree =
	| Fail
	| Tree of word * t * reTree list

	let modelDesignation = "regular expression"

	let ccc (x : RegExpSyntax.t): t = x

	(* auxiliary functions *)
	let seqConcat aset bset = Set.flatMap (fun s1 -> Set.map (fun s2 -> s1@s2) bset) aset

	class model (arg: t Arg.alternatives) =
		object(self) inherit Model.model arg modelDesignation

			val representation: t =
				let j = Arg.fromAlternatives arg in
					if j = JSon.JNull then
						Arg.getRepresentation arg
					else
						let re = JSon.field_string j "re" in
							RegExpSyntax.parse re

			initializer self#handleErrors	(* placement is crucial - after representation *)

			method toJSon: JSon.t =
				let open JSon in
				let rep = representation in
				JAssoc [
					("kind", JString self#kind);
					("description", JString self#description);
					("name", JString self#name);
					("re", JString (RegExpSyntax.toString rep));
				]

			method representation = representation

			method validate = (

				(*
				let representation = RegExpSyntax.parse "(((xx+ut)+(aaa+dss+ghf)+(xx+uu))ee)+bgc*+(jgg+bgcd)" in

				let rec lang rep =
					match rep with
						| RegExpSyntax.Plus(l, r) -> Util.print "pls: "; Util.print (RegExpSyntax.toString l); Util.print ", ";
						Util.print (RegExpSyntax.toString r); Util.println ""; Set.union (lang l) (lang r)
						| RegExpSyntax.Seq(l, r) -> Util.print "seq: "; Util.print (RegExpSyntax.toString l); Util.print ", ";
						Util.print (RegExpSyntax.toString r); Util.println ""; Set.union (lang l) (lang r)
						| RegExpSyntax.Star(r) -> Util.print "str: "; Util.print (RegExpSyntax.toString r); Util.println ""; (lang r)
						| RegExpSyntax.Symb(c) -> Set.make [c]
						| RegExpSyntax.Empty -> Set.empty
						| RegExpSyntax.Zero -> Set.empty
				in
				let a = lang representation in
				()
				*)
			)


			method tracing: unit = ()


			(**
			* This method generates the alphabet of all symbols in the expression
			*
			* @returns symbols -> the set of all symbols in the expression's alphabet
			*
			*)
			method alphabet: symbols =

				let rec alf rep =
					match rep with
						| Plus(l, r) -> Set.union (alf l) (alf r)
						| Seq(l, r) -> Set.union (alf l) (alf r)
						| Star(r) -> alf r
						| Symb(c) -> Set.make [c]
						| Empty -> Set.empty
						| Zero -> Set.empty
				in
					alf representation


			(**
			* This method generates the language of the regular expression for when klenne is always zero
			*
			* @returns words -> set of generated words
			*
			*)
			method quasiLanguage: words =

				let rec lang rep =
					match rep with
						| Plus(l, r) -> Set.union (lang l) (lang r)
						| Seq(l, r) -> seqConcat (lang l) (lang r)
						| Star(r) -> Set.make [[]]
						| Symb(c) -> Set.make [[c]]
						| Empty -> Set.empty
						| Zero -> Set.empty
				in
					lang representation




			(**
			* This method tests if a given word is accepted by the regular expression
			*
			* @param w:word -> word to be tested for acceptance
			*
			* @returns bool -> true if w is accepted and false otherwise
			*
			*)
			method accept (w: word): bool =

				let partition w =
					let rec partX w pword =
						match w with
							[] -> Set.empty
							| x::xs -> let fwp = pword@[x] in
											Set.add (fwp, xs) (partX xs fwp) in
					Set.add ([],w) (partX w []) in

				let rec acc rep w =
					match rep with
						| Plus(l, r) -> (acc l w) || (acc r w)
						| Seq(l, r) -> let wpl = partition w in
											Set.exists (fun (wp1,wp2) -> (acc l wp1) && (acc r wp2)) wpl
						| Star(re) -> w = [] ||
									(let wpl = Set.remove ([],w) (partition w) in
											Set.exists (fun (wp1,wp2) -> (acc re wp1) && (acc (Star re) wp2)) wpl)
						| Symb(c) -> w = [c]
						| Empty -> w = []
						| Zero -> false
				in

					acc representation w



			(**
			* This method returns the derivation tree for the word acceptance
			*
			* @param w:word -> word to be tested for acceptance
			*
			* @returns reTree list -> list of derivation trees
			*
			*)
			method allTrees w : unit =


				let partition w =
					let rec partX w pword =
						match w with
							[] -> Set.empty
							| x::xs -> let fwp = pword@[x] in
										Set.add (fwp, xs) (partX xs fwp)
					in
					Set.add ([],w) (partX w [])
				in

				let rec acc w rep =
					match rep with
						| Plus(l, r) ->
							let l1 = acc w l in
							let r1 = acc w r in
								List.map (fun t -> Tree (w, rep, [t])) (l1 @ r1)

						| Seq(l, r) ->
							let wps = partition w in
							let wpl = Set.toList wps in
							List.flatten ( List.map (fun (wp1, wp2) ->
								let tl = acc wp1 l in
								let tr = acc wp2 r in
									List.flatten (List.map (fun x -> List.map
										(fun y -> Tree (w, rep, [x; y])) tr)tl)
							) wpl)

						| Star(re) ->
							if w = [] then
								[Tree (['~'], rep, [])]
							else
								(let wps = Set.remove ([],w) (partition w) in
								let wpl = Set.toList wps in
								List.flatten (List.map (fun (wp1, wp2) ->
									let tl = acc wp1 re in
									let tr = acc wp2 (Star re) in
									List.flatten (List.map (fun x -> List.map
										(fun y -> Tree (w, rep, [x; y])) tr) tl)) wpl))

						| Symb(c) ->
							if w = [c] then
								[Tree (w, rep, [])]
							else
								[Tree (w, rep, [Fail])]

						| Empty ->
							if w = [] then
								[Tree (['~'], rep, [])]
							else
								[Tree (w, rep, [Fail])]

						| Zero -> [Tree (w, rep, [Fail])]

				in

				let ac = acc w self#representation in



				let rec isNotFail t =
					match t with
						Fail -> false
						| Tree ([], re, []) -> true
						| Tree (w, re, []) -> true
						| Tree ([], re, x::xs) -> (isNotFail x) && (isNotFail (Tree ([], re, xs)))
						| Tree (w, re, x::xs) -> (isNotFail x) && (isNotFail (Tree (w, re, xs)))
				in

				let ts = List.filter (fun t -> isNotFail t) ac in


				let printTreeX w re n =
					let s = String.make (3*n) ' ' in
					Util.println [s; Util.word2str w; " -> "; RegExpSyntax.toString re]
				in

				let rec printTree t n =
					match t with
						Fail -> Util.println ["Fail"]
						| Tree ([], re, []) -> Util.println ["TREH "]
						| Tree (w, re, []) -> printTreeX w re n
						| Tree ([], re, x::xs) -> printTreeX [] re n; printTree x (n+1); List.iter (fun t -> printTree t (n+1)) xs
						| Tree (w, re, x::xs) -> printTreeX w re n; printTree x (n+1); List.iter (fun t -> printTree t (n+1)) xs
				in

					List.iter (fun t -> printTree t 0) ts



			(**
			* This method generates all words up to the given length that are generated by the regular expression
			*
			* @param length:int -> maximum length of all generated words
			*
			* @returns words -> set of generated words
			*)
			method generate (length: int): words =

				let rec lang rep ln =
					match rep with
						| Plus(l, r) ->
								Set.union (lang l ln) (lang r ln)
						| Seq(l, r) ->
								let left = lang l ln in
								let rigth w = lang r (ln - (List.length w)) in
								let conc w = Util.concatAll w (Set.toList (rigth w)) in
									Set.flatMap (fun lw -> Set.make (conc lw)) left
						| Star r ->
								let exp = lang r ln in
									Set.star exp ln

							(* alternate version of star, leave 4 now

							let rec starX ws sz =
								if sz <= 0 then Set.make [[]]
								else
									let ws = Set.filter (fun x -> sz >= (List.length x)) ws in
									let newLn w = sz - (List.length w) in
									let tail w ws = Set.toList (starX ws (newLn w)) in
									let conc w ws = Util.concatAll w (tail w ws) in
									let track w ws = Set.add w (Set.make (conc w ws)) in
										Set.flatMap (fun w -> if w = [] then Set.make [[]] else track w ws) ws in
							let exp = lang r ln in
								Set.add [] (starX exp ln)*)

						| Symb(c) -> if ln > 0 then Set.make [[c]] else Set.empty
						| Empty -> Set.make [[]]
						| Zero -> Set.empty
				in
					lang representation length


			(**
			* This method simplifies the regular expression
			*
			* @returns RegularExpression.model -> the new simplified, equivalent expression
			*)
			method simplify : model =

				(* various base case simplification rules to apply to the given expressions *)
				let rec simpX re =
					match re with
						(* plus *)
						(* a* + empty *)
						| Plus(Star(l), Empty) -> Star(l)
						| Plus(Empty, Star(r)) -> Star(r)
						(* a* + zero *)
						| Plus(Zero, r) -> r
						| Plus(l, Zero) -> l
						(* ~ + aa* *)
						| Plus(Empty, Seq(l, Star(r))) when l = r -> Star(r)
						| Plus(Empty, Seq(Star(l), r)) when l = r -> Star(l)
						| Plus(Seq(l, Star(r)), Empty) when l = r -> Star(r)
						| Plus(Seq(Star(l), r), Empty) when l = r -> Star(l)
						(* a* + a + empty *)
						| Plus(Star(l), Plus(Empty, r)) when l = r -> Star(l)
						| Plus(Star(l), Plus(r, Empty)) when l = r -> Star(l)
						| Plus(Plus(Empty, l), Star(r)) when l = r -> Star(r)
						| Plus(Plus(l, Empty), Star(r)) when l = r -> Star(r)
						(* a* + a *)
						| Plus(Star(l), r) when l = r -> Star(l)
						| Plus(l, Star(r)) when l = r -> Star(r)
						(* a + b = a||b when a = b *)
						| Plus(l, r) when l = r -> l
						(* seq *)
						| Seq(Empty, Empty) -> Empty
						| Seq(Zero, Zero) -> Zero
						| Seq(Empty, r) -> r
						| Seq(l, Empty) -> l
						| Seq(Zero, r) -> Zero
						| Seq(l, Zero) -> Zero
						(* (~+a)a* *)
						| Seq(Plus(Empty, l),Star(r)) when l = r -> Star(r)
						| Seq(Plus(l, Empty),Star(r)) when l = r -> Star(r)
						| Seq(Star(l),Plus(Empty, r)) when l = r -> Star(l)
						| Seq(Star(l),Plus(r, Empty)) when l = r -> Star(l)
						| Seq(Star(l),Star(r)) when l = r -> Star(l)
						(* star *)
						| Star(Star(r)) -> Star(r)
						| Star(Plus(Empty, r)) -> Star(r)
						| Star(Plus(r, Empty)) -> Star(r)
						| Star(Empty) -> Empty
						| Star(Zero) -> Empty
						| Star(r) -> re
						(* symb *)
						| Symb(c) -> Symb c
						(* empty *)
						| Empty -> Empty
						(* zero *)
						| Zero -> Zero
						| _ -> re
				in

				(* applies various base case simplifications to the various sub-expressions of regular expression re *)
				let rec simplify re =

					match re with
						| Plus(l,r) -> simpX (Plus(simplify l, simplify r))
						| Seq(l,r) -> simpX (Seq(simplify l, simplify r))
						| Star(re) -> simpX (Star(simplify re))
						| Symb(c) -> Symb c
						| Empty -> Empty
						| Zero -> Zero
				in

				let sre = simplify representation in

				new model (Arg.Representation (sre))

		end
end


module RegularExpressionTests: sig end =
struct
	let active = false

	let test0 () =
		let m = new RegularExpression.model (Arg.Predef "re_abc") in
			let j = m#toJSon in
				JSon.show j

	let test1 () =
		let re = new RegularExpression.model (Arg.Predef "re_abc") in
			let j = re#toJSon in
				JSon.show j

	let testAlphabet () =
		let re = new RegularExpression.model (Arg.Predef "re_abc") in
			Util.println ["alphabet: "]; Util.printAlphabet (Set.toList (re#alphabet));
			Util.println []

	let testAlphabet2 () =
		let re = new RegularExpression.model (Arg.Predef "re_simple") in
			Util.println ["alphabet: "]; Util.printAlphabet (Set.toList (re#alphabet));
			Util.println []

	let testAlphabet3 () =
		let re = new RegularExpression.model (Arg.Predef "re_complex") in
			Util.println ["alphabet: "]; Util.printAlphabet (Set.toList (re#alphabet));
			Util.println []

	let testAlphabet4 () =
		let re = new RegularExpression.model (Arg.Predef "re_convoluted") in
			Util.println ["alphabet: "]; Util.printAlphabet (Set.toList (re#alphabet));
			Util.println []

	let testQuasiLang () =
		let re = new RegularExpression.model (Arg.Predef "re_abc") in
			let ws = re#quasiLanguage in
			Util.printWords (Set.toList ws)

	let testQuasiLang2 () =
		let re = new RegularExpression.model (Arg.Predef "re_simple") in
			let ws = re#quasiLanguage in
			Util.printWords (Set.toList ws)

	let testQuasiLang3 () =
		let re = new RegularExpression.model (Arg.Predef "re_complex") in
			let ws = re#quasiLanguage in
			Util.printWords (Set.toList ws)

	let testQuasiLang4 () =
		let re = new RegularExpression.model (Arg.Predef "re_convoluted") in
			let ws = re#quasiLanguage in
			Util.printWords (Set.toList ws)

	let check f w =
		let msg = 
			if f w then "word was accepted" else "word was not accepted"
		in Util.println [msg]

	let testAccept () =
		let m = new RegularExpression.model (Arg.Predef "re_abc") in
			check m#accept ['a';'a']

	let testAccept2 () =
		let m = new RegularExpression.model (Arg.Predef "re_simple") in
			check m#accept ['a';'a']

	let testAccept3 () =
		let m = new RegularExpression.model (Arg.Predef "re_complex") in
			check m#accept ['a';'a']

	let testAccept4 () =
		let m = new RegularExpression.model (Arg.Predef "re_convoluted") in
			check m#accept ['a';'a']

	let testGenerate () =
		let re = new RegularExpression.model (Arg.Predef "re_abc") in
			Util.println ["generated words size 0:"]; Util.printWords (Set.toList (re#generate 0));
			Util.println ["generated words size 1:"]; Util.printWords (Set.toList (re#generate 1));
			Util.println ["generated words size 2:"]; Util.printWords (Set.toList (re#generate 2));
			Util.println ["generated words size 3:"]; Util.printWords (Set.toList (re#generate 3));
			Util.println ["generated words size 4:"]; Util.printWords (Set.toList (re#generate 4));
			Util.println []

	let testGenerate2 () =
		let re = new RegularExpression.model (Arg.Predef "re_simple") in
			Util.println ["generated words size 0:"]; Util.printWords (Set.toList (re#generate 0));
			Util.println ["generated words size 1:"]; Util.printWords (Set.toList (re#generate 1));
			Util.println ["generated words size 2:"]; Util.printWords (Set.toList (re#generate 2));
			Util.println ["generated words size 3:"]; Util.printWords (Set.toList (re#generate 3));
			Util.println ["generated words size 4:"]; Util.printWords (Set.toList (re#generate 4));
			Util.println []

	let testGenerate3 () =
		let re = new RegularExpression.model (Arg.Predef "re_complex") in
			Util.println ["generated words size 0:"]; Util.printWords (Set.toList (re#generate 0));
			Util.println ["generated words size 1:"]; Util.printWords (Set.toList (re#generate 1));
			Util.println ["generated words size 2:"]; Util.printWords (Set.toList (re#generate 2));
			Util.println ["generated words size 3:"]; Util.printWords (Set.toList (re#generate 3));
			Util.println ["generated words size 4:"]; Util.printWords (Set.toList (re#generate 4));
			Util.println []

	let testGenerate4 () =
		let re = new RegularExpression.model (Arg.Predef "re_convoluted") in
			Util.println ["generated words size 0:"]; Util.printWords (Set.toList (re#generate 0));
			Util.println ["generated words size 1:"]; Util.printWords (Set.toList (re#generate 1));
			Util.println ["generated words size 2:"]; Util.printWords (Set.toList (re#generate 2));
			Util.println ["generated words size 3:"]; Util.printWords (Set.toList (re#generate 3));
			Util.println ["generated words size 4:"]; Util.printWords (Set.toList (re#generate 4));
			Util.println []

	let testSimplify2 () =
		let re = new RegularExpression.model (Arg.Predef "re_simple") in
		let res = re#simplify in
			JSon.show res#toJSon

	let testEnum () =
		let e = new Exercise.exercise (Arg.Predef "exer_re2fa") in
		let re = new RegularExpression.model (Arg.Predef "re_simple") in
		let result = re#checkExercise e in
			if result then Util.print ["it works"] else Util.print ["it does not work"]

	let testTrace () =
		let re = new RegularExpression.model (Arg.Predef "re_simple") in
			re#allTrees ['a';'c';'b';'a';'c';'b']

	let runAll =
		if active then (
			Util.header "RegularExpressionTests";
			testTrace ();
			testSimplify2 ()
		)
end

(*
 * ContextFreeGrammar.ml
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
 * jan/2021 (amd) - Module in an independent file and some cleanup.
 * feb/2020 (jg) - Main functionalities.
 * dec/2019 (amd) - Initial skeleton, inside the big file "OCamlFlat.ml".
 *)

(*
 * Description: Context-free grammar functionality.
 *
 * TODO: More cleanup.
 *)

module type ContextFreeGrammarSig =
sig
	type cfgTree = Leaf of char | Root of char * cfgTree list

	type t = {
		alphabet : symbols;
		variables : variables;
		initial : variable;
		rules : CFGSyntax.rules;
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
				method representation: t

				method tracing: unit
				method isRegular: bool
				method accept: word -> bool
				method acceptWithTracing: word -> unit
				method generate: int -> words

				method checkExercise: Exercise.exercise -> bool
				method checkExerciseFailures : Exercise.exercise -> (words * words)
			end
end

module ContextFreeGrammar : ContextFreeGrammarSig =
struct
	type cfgTree = Leaf of char | Root of char * cfgTree list

	open CFGSyntax

	type t = {
		alphabet : symbols;
		variables : variables;
		initial : variable;
		rules : CFGSyntax.rules;
	}

	let modelDesignation = "context free grammar"

	(*------Auxiliary functions---------*)

	(* given a head, returns the set of all its bodies according to the cfg's rules *)
	let bodiesOfHead h rl =
		let rls = Set.filter (fun r -> r.head = h) rl in
			Set.map (fun r -> r.body) rls


	(* given 2 sets of words, to each word of the left set, appends each word of the right set *)
	let concatWords lws rws =
		if lws = Set.empty then rws
		else if rws = Set.empty then lws
		else
			let pairs = Set.combinations lws rws in
				Set.map (fun (x,y) -> x@y) pairs

	(* tests if the number of symbols in the given word exceeds the given lenght *)
	let exceedsMaxLen w l alph =
		let cleanWord = List.filter (fun c -> Set.belongs c alph) w in
			(List.length cleanWord) > l



	let subX h rws rl =
		let bs = bodiesOfHead h rl in
			concatWords bs rws


	(* applies the cfg's rules to the given word *)
	let rec subVar w vs rs =
		match w with
			| [] -> Set.make [[]]
			| x::xs -> if (Set.belongs x vs) then subX x (subVar xs vs rs) rs
				else concatWords (Set.make [[x]]) (subVar xs vs rs)


	(* removes the empty symbol from all non-empty words *)
	let removeEpsi w = List.filter (fun c -> c <> epsilon) w


	(* filters out all words that have variables and cleans any unnecessary epsilon *)
	let cleanNonWords ws vs =
		let hasVar w = List.exists (fun c -> Set.belongs c vs) w in
		let ws = Set.filter (fun w -> not (hasVar w)) ws in
			Set.map (fun w -> removeEpsi w) ws



	class model (arg: t Arg.alternatives) =
		object(self) inherit Model.model arg modelDesignation

			val representation: t =
				let j = Arg.fromAlternatives arg in
					if j = JSon.JNull then
						Arg.getRepresentation arg
					else
						let alphabet = JSon.field_char_set j "alphabet" in
						let variables = JSon.field_char_set j "variables" in
						let initial = JSon.field_string j "initial" in
						let rules = JSon.field_string_list j "rules" in
							{
								alphabet = alphabet;
								variables = variables;
								initial = initial.[0];
								rules = CFGSyntax.parse (Set.make rules);
							}

			initializer self#handleErrors	(* placement is crucial - after representation *)

			method representation =
				representation

			method toJSon: JSon.t =
				let open JSon in
				let ruleToJSon {head=h; body=b} =
					let aa = String.concat "" (List.map (fun x -> Util.ch2str x) b) in
					let hh = Util.ch2str h in
					JString (hh^" -> "^aa) in

				let rep = representation in
				JAssoc [
					("kind", JString self#kind);
					("description", JString self#description);
					("name", JString self#name);
					("alphabet", JList (List.map (fun s -> JString (Util.ch2str s)) (Set.toList rep.alphabet)));
					("variables", JList (List.map (fun s -> JString (Util.ch2str s)) (Set.toList rep.variables)));
					("initial", JString (Util.ch2str rep.initial) );
					("rules", JList (List.map ruleToJSon (Set.toList rep.rules)));
					]

			method validate: unit = (

				let isIntersectionValid = (Set.inter representation.variables representation.alphabet) = Set.empty in

				let isInitialValid = Set.belongs representation.initial representation.variables in

				let areRuleHeadsValid =
					let hs = Set.map (fun r -> r.head) representation.rules in
						Set.subset hs representation.variables
				in

				let areRuleBodiesValid =
					let bs = Set.map (fun r -> Set.make r.body) representation.rules in
					let allValidSymbs = Set.add epsilon (Set.union representation.alphabet representation.variables) in
					let res = Set.exists (fun b -> not (Set.subset b allValidSymbs)) bs in
						not res
				in

				if not isIntersectionValid then
					Error.error self#name
						"intersection between alphabet and variables is not empty" ()
				;

				if not isInitialValid then
					Error.error (Util.ch2str representation.initial)
						"initial does not belong to the set of all variables" ()
				;

				if not areRuleHeadsValid then
					Error.error self#name
						"not all rule heads belong to the set of all variables" ()
				;

				if not areRuleBodiesValid then
					Error.error self#name
						"not all rule bodies have valid characters" ()
				)

			method tracing: unit = ()

			(* This method checks if the grammar is regular
			*
			* @returns bool -> true if regular, false otherwise
			*)
			method isRegular : bool =

				let vs = representation.variables in
				let alp = representation.alphabet in

				let bs = Set.map (fun r -> r.body) representation.rules in

				let isRightLinear bs =
					let isRightLinearX b =
						match b with
							| [a] -> (Set.belongs a alp) || a = epsilon
							| [a; v] -> (Set.belongs a alp) && (Set.belongs v vs)
							| _ -> false
					in
						Set.for_all (fun b -> isRightLinearX b) bs
				in

				let isLeftLinear bs =
					let rec isLeftLinearX b =
						match b with
							| [a] -> (Set.belongs a alp) || a = epsilon
							| [v; a] -> (Set.belongs v vs) && (Set.belongs a alp)
							| _ -> false
					in
						Set.for_all (fun b -> isLeftLinearX b) bs
				in

					isRightLinear bs || isLeftLinear bs


			(* This method checks if the given word is accepted by the grammar
			*
			* @param testWord -> word to be tested
			*
			* @returns bool -> true if it accepts the word, false otherwise
			*)
			method accept (testWord:word) : bool =

				(* any word with a symbol not from the cfg alphabet will not be accepted
				if not (Set.subset (Set.make testWord) representation.alphabet) then false else
				*)

				let vs = representation.variables in


				(* for word wa, get subword to the left of its first variable *)
				let rec getPrefix wa =
					match wa with
						| [] -> []
						| x::xs -> if Set.belongs x vs then [] else x::(getPrefix xs)
				in

				(* for word wa, get subword to the rigth of its last variable *)
				let getSuffix wa =
					let rec getSuffixX wa sfx =
						match wa with
							| [] -> sfx
							| x::xs -> let auxSfx = sfx@[x] in
										if Set.belongs x vs then getSuffixX xs []
											else getSuffixX xs auxSfx
					in
						getSuffixX wa []
				in

				let rec firstNElements w n =
					match w with
						| [] -> []
						| x::xs -> if n > 0 then x::(firstNElements xs (n-1)) else []
				in

				let rec lastNElements w n =
					match w with
						| [] -> []
						| x::xs -> if n < (List.length w) then lastNElements xs n else w
				in

				(* a word can be discarded if its prefix does not match the leftmostmost part of word w *)
				let keepByPrefix genW testW =
					let pgw = getPrefix genW in
					let ptw = firstNElements testW (List.length pgw) in
						pgw = [] || pgw = ptw
				in


				(* a word can be discarded if its suffix does not match the rightmost part of word w *)
				let keepBySufix genW testW =
					let sgw = getSuffix genW in
					let stw = lastNElements testW (List.length sgw) in
						sgw = [] || sgw = stw
				in

				(* the word inst discarded only if it cant be discarded by neither its prefix nor its suffix *)
				let toKeep w tw = (w = [] && tw = []) || (keepByPrefix w tw && keepBySufix w tw) in


				let alph = representation.alphabet in
				let vs = representation.variables in
				let rs = representation.rules in
				let l = List.length testWord in

				let nextGeneration ws =
					let subsWs = Set.flatMap (fun w -> subVar w vs rs) ws in
					let rws = Set.filter (fun w -> not (exceedsMaxLen w l alph)) subsWs in
					let rws = Set.map (fun w -> removeEpsi w) rws in
						Set.filter (fun w -> toKeep w testWord ) rws

				in

				let start = Set.make [[representation.initial]] in

				let res = Set.historicalFixedPoint nextGeneration start in
					Set.exists (fun x -> x = testWord ) res




			method acceptWithTracing (testWord:word) =



				let vs = representation.variables in


				(* for word wa, get subword to the left of its first variable *)
				let rec getPrefix wa =
					match wa with
						| [] -> []
						| x::xs -> if Set.belongs x vs then [] else x::(getPrefix xs)
				in

				(* for word wa, get subword to the rigth of its last variable *)
				let getSuffix wa =
					let rec getSuffixX wa sfx =
						match wa with
							| [] -> sfx
							| x::xs -> let auxSfx = sfx@[x] in
										if Set.belongs x vs then getSuffixX xs []
											else getSuffixX xs auxSfx
					in
						getSuffixX wa []
				in

				let rec firstNElements w n =
					match w with
						| [] -> []
						| x::xs -> if n > 0 then x::(firstNElements xs (n-1)) else []
				in

				let rec lastNElements w n =
					match w with
						| [] -> []
						| x::xs -> if n < (List.length w) then lastNElements xs n else w
				in

				(* a word can be discarded if its prefix does not match the leftmostmost part of word w *)
				let keepByPrefix genW testW =
					let pgw = getPrefix genW in
					let ptw = firstNElements testW (List.length pgw) in
						pgw = [] || pgw = ptw
				in


				(* a word can be discarded if its suffix does not match the rightmost part of word w *)
				let keepBySufix genW testW =
					let sgw = getSuffix genW in
					let stw = lastNElements testW (List.length sgw) in
						sgw = [] || sgw = stw
				in

				(* the word inst discarded only if it cant be discarded by neither its prefix nor its suffix *)
				let toKeep w tw = (w = [] && tw = []) || (keepByPrefix w tw && keepBySufix w tw) in


				let alph = representation.alphabet in
				let vs = representation.variables in
				let rs = representation.rules in
				let l = List.length testWord in

				let nextGeneration ws =
					let subsWs = Set.flatMap (fun w -> subVar w vs rs) ws in
					let rws = Set.filter (fun w -> not (exceedsMaxLen w l alph)) subsWs in
					let rws = Set.map (fun w -> removeEpsi w) rws in
						Set.filter (fun w -> toKeep w testWord ) rws

				in

				let start = Set.make [[representation.initial]] in

				let res = Set.historicalFixedPointTracing nextGeneration start in


				let trimRes l =
					match l with
					| [] -> []
					| x::xs -> if Set.belongs testWord x then xs
								else l
				in

				let res2 = List.rev (trimRes (List.rev res)) in


				let printWset ws =
					Util.print ["["];
					Set.iter (fun w -> Util.print [Util.word2str w; ";"]) ws;
					Util.println ["]"];
				in

					List.iter (fun ws -> printWset ws) res2




			(* This method generates all words up the the given lenght that belong to the grammars language
			*
			* @ param lenght -> the max lenght of generated words
			*
			* @returns words -> the set of generated words
			*)
			method generate (length:int) : words =

				let alph = representation.alphabet in
				let vs = representation.variables in
				let rs = representation.rules in


				let nextGeneration ws =
					let subsWs = Set.flatMap (fun w -> subVar w vs rs) ws in
						Set.filter (fun w -> not (exceedsMaxLen w length alph)) subsWs
				in

				let start = Set.make [[representation.initial]] in

				let res = Set.historicalFixedPoint nextGeneration start in

					cleanNonWords res vs


		end
end

module ContextFreeGrammarTests: sig end =
struct
	let active = false

	let test0 () =
		let m = new ContextFreeGrammar.model (Arg.Predef "cfg_simple") in
		let j = m#toJSon in
			JSon.show j

	let testRegular () =
		let m = new ContextFreeGrammar.model (Arg.Predef "cfg_simple") in
		let ws = m#isRegular in
			if ws then Util.println ["is regular"] else Util.println ["is not regular"]


	let testAcc () =
		let m = new ContextFreeGrammar.model (Arg.Predef "cfg_simple") in
		let ws = m#accept [] in
			if ws then Util.println ["Word was accepted"]
			else Util.println ["Word was not accepted"]


	let testTrace () =
		let m = new ContextFreeGrammar.model (Arg.Predef "cfg_simple") in
			m#acceptWithTracing ['0';'1']

	let testGen () =
		let m = new ContextFreeGrammar.model (Arg.Predef "cfg_simple") in
		let ws = m#generate 4 in
			Util.printWords (Set.toList ws)

	let runAll =
		if active then (
			Util.header "ContextFreeGrammarTests";
			testRegular ();
			testAcc ();
			testTrace ();
			testGen ()
		)
end

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
				
				(*
				(* are all accepted states members of all states *)
				let validAccSts = Set.subset representation.acceptStates representation.states in
				*)
				(* is criteria acceptable_*)
				let validCriteria = (representation.criteria = true 
									&& representation.acceptStates <> Set.empty
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

				(*
				if not validAccSts then
					Error.error self#name
						"not all accepted states belong to the set of all states" ()
				;
				*)
				
				if not validCriteria then
					Error.error self#name
						"invalid criteria" ()
				;

				if not validTrns then
					Error.error self#name
						"not all transitions are valid" ()
				)

			method tracing : unit = ()
			
			(* applies a transition *)						
			method nextConfs (state,stack) sy trans =
				if stack = [] then
					Set.make []
				else (
					let ts = select123 state (List.hd stack) sy trans in
						Set.map ( fun (_,_,_,d,e) -> (d,e@(List.tl stack)) ) ts
			)
			
			(**
			* This method verifies if the given word is accepted by the automaton
			*
			* @param w:word -> word to be accepted
			*
			* @returns bool option-> true if w is accepted and false otherwise, result might be inconclusive
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
				pilha vazia -> reachable
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
			testGenerate()
			(* testGetReachableStates(); *)
			(* testGetProductiveStates(); *)
			(* testAcceptEmpty(); *)
		)
end
		
(*
 * PolyModel.ml
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
 * jan/2021 (amd) - Created this module, collecting all the operation
                    involving two or more kinds of models.
                    This allows to got rid of the mutual recursion between
                    modules, allowing storing each module in a different file.
 * dec/2019 (jg) - Initial code, across several modules in file "OCamlFlat.ml".
 *)

(*
 * Description: Poly-model operations.
 *
 * TODO: Cleanup.
 *)

module type PolyModelSig =
sig
	val loadModel : string -> Model.model
	val re2fa : RegularExpression.model -> FiniteAutomaton.model
	val fa2re : FiniteAutomaton.model -> RegularExpression.model
	val re2cfg : RegularExpression.model -> ContextFreeGrammar.model
	val fa2cfg : FiniteAutomaton.model -> ContextFreeGrammar.model
	val cfg2fa : ContextFreeGrammar.model -> FiniteAutomaton.model
	val cfg2re : ContextFreeGrammar.model -> RegularExpression.model
end

module PolyModel : PolyModelSig =
struct

	let loadModel (filename: string): Model.model =	(* will load any model *)
		let j = JSon.from_file filename in
			let kind = JSon.field_string j "kind" in
				if FiniteAutomaton.modelDesignation = kind then
					(new FiniteAutomaton.model (Arg.JSon j) :> Model.model)
				else if RegularExpression.modelDesignation = kind then
					(new RegularExpression.model (Arg.JSon j) :> Model.model)
				else if ContextFreeGrammar.modelDesignation = kind then
					(new ContextFreeGrammar.model (Arg.JSon j) :> Model.model)
				else
					(new FiniteAutomaton.model (Arg.JSon j) :> Model.model)

(**
	* This method converts the automaton into a regular expression that accepts its language, by
	* using the transitive closure algorithm
	*
	* @returns RegularExpression.model -> the resulting regular expression
	*
	* Desc: The resulting expression is not minimal
	*)

		(* This method converts the regular expression to its equivalent finite automaton
	*
	* @returns FiniteAutomaton.model -> the resulting finite automaton
	*)
	let re2fa re =
		let open FiniteAutomaton in
		let open RegExpSyntax in

		(*auxiliary var for genName function*)
		let k = ref 0 in

		(*for each new state, generates a name that will distinguish it from all the other generated states *)
		let genName () =
			let n = !k in
			let () = k:= n + 1 in
				(*easy way of having all single digit state names have a zero before their actual number*)
				if n > 9 then "new_St" ^ (string_of_int n)
							else "new_St0" ^ (string_of_int n) in


		let rec compile (rep: RegExpSyntax.t) : FiniteAutomaton.t =
			match rep with
				| Plus(l, r) ->
						let fa1 = compile l in
						let fa2 = compile r in
						let newStart = genName () in
						let newSts = Set.add newStart (Set.union fa1.states fa2.states) in
						let newAccSts = Set.union fa1.acceptStates fa2.acceptStates in
						let newTran1 = (newStart, epsilon, fa1.initialState) in
						let newTran2 = (newStart, epsilon, fa2.initialState) in
						let newTrans = Set.add newTran1 (Set.add newTran2
							(Set.union fa1.transitions fa2.transitions)) in
						let newAlf = Set.union fa1.alphabet fa2.alphabet in

							{alphabet = newAlf; states = newSts; initialState = newStart;
								transitions = newTrans; acceptStates = newAccSts}

				| Seq(l, r) ->
						let fa1 = compile l in
						let fa2 = compile r in
						let ist = fa1.initialState in
						let sts = Set.union fa1.states fa2.states in
						let asts = fa2.acceptStates in
						let newTrns = Set.map (fun x -> (x, epsilon, fa2.initialState) ) fa1.acceptStates in
						let trns = Set.union newTrns (Set.union fa1.transitions fa2.transitions) in
						let alf = Set.union fa1.alphabet fa2.alphabet in

							{alphabet = alf; states = sts; initialState = ist;
								transitions = trns; acceptStates = asts}

				| Star(r) ->
						let fa = compile r in
						let newStart = genName () in
						let newSts = Set.add newStart fa.states in
						let newTrns = Set.map (fun st -> (st, epsilon, newStart)) fa.acceptStates in
						let allNewTrns = Set.add (newStart, epsilon, fa.initialState) (Set.union newTrns fa.transitions) in

							{alphabet = fa.alphabet; states = newSts; initialState = newStart;
								transitions = allNewTrns; acceptStates = Set.make [newStart]}

				| Symb(c) ->
						let newStart = genName () in
						let newAcc = genName () in
						let newSts = Set.make [newStart; newAcc] in
						let newTrn = Set.make [(newStart, c, newAcc)] in

							{alphabet = Set.make [c]; states = newSts; initialState = newStart;
								transitions = newTrn; acceptStates = Set.make [newAcc]}

				| Empty ->
						let newStart = genName () in

								{alphabet = Set.empty; states = Set.make [newStart]; initialState = newStart;
									transitions = Set.empty; acceptStates = Set.make [newStart]}

				| Zero ->
						let newStart = genName () in

							{alphabet = Set.empty; states = Set.make [newStart]; initialState = newStart;
									transitions = Set.empty; acceptStates = Set.empty}
		in

			new FiniteAutomaton.model (Arg.Representation (compile re#representation))

	let fa2reMake fa =
		let open FiniteAutomaton in
		let open RegExpSyntax in
		(* Since the algorithm only works for deterministic automaton, we first convert it
			to its deterministic equivalent *)
		let fa = fa#toDeterministic in

		let rep = fa#representation in

		let sts = rep.states in
		let trns = rep.transitions in

		(* transforms the set of expressions into the regex: plus of all expressions of the set *)
		let rec plusSet reSet =
			let rec pls l =
				match l with
					[] -> Zero
					| x::xs -> if xs = [] then x else Plus (x, pls xs)
			in
				pls (Set.toList reSet)
		in

		(* For the given i and j, returns the value of R when k is zero.
			Note that k will always be 0 when called inside this method *)
		let calczerok k i j =
			let ts = Set.filter (fun (a,_,b) -> i = a && j = b) trns in
			if ts <> Set.empty then
				if i <> j then
					let res = Set.map (fun (_,c,_) -> Symb c) ts in
						(k,i,j,plusSet res)
				else
					let res = Set.map (fun (_,c,_) -> Symb c) ts in
					let re = Plus(Empty, (plusSet res)) in
						(k,i,j,re)

			else (k,i,j,Zero)
		in


		(* For the given i and j, returns the value of R when k is not zero. *)
		let calck k i j prvK =
			let getRij i j =
				let r = Set.nth (Set.filter (fun (_,x,y,_) -> x = i && y = j) prvK) 0 in
					(fun (_,_,_,re) -> re) r
			in
			let assembleRe st i j =
				let rik = getRij i st in
				let rkk = Star (getRij st st) in
				let rkj = getRij st j in
					Seq(rik, Seq(rkk,rkj))
			in

			let rij = getRij i j in
			let rikjs = Set.map (fun st -> assembleRe st i j) sts in
			let rikj = plusSet rikjs in
				(k,i,j,Plus(rij,rikj))

		in

		(* Main function that applies previous 2 functions to all possible i and j pairs *)
		let rec rkij k =
			if k < 1 then
				Set.map (fun (i,j) -> calczerok k i j) (Set.combinations sts sts)
			else
				let prvK = rkij (k-1) in
					Set.map (fun(i,j) -> calck k i j prvK) (Set.combinations sts sts)
		in

		let allRks = rkij (Set.size sts) in
		let result = Set.filter (fun (_,i,j,_) -> i = rep.initialState && Set.belongs j rep.acceptStates ) allRks in
		let res = Set.map (fun (_,_,_,re) -> re) result in
		let newRe = plusSet res in
			newRe

	let fa2re fa =
		let re = fa2reMake fa in
			new RegularExpression.model (Arg.Representation re)





		(* This function converts a regular expression to its equivalent regular grammar
		*
		* @returns FiniteAutomaton.model -> the resulting regular grammar
		*)


	let re2cfg re =
		let open ContextFreeGrammar in
		let open CFGSyntax in

		(*auxiliary var for genVar function*)
		let k = ref 0 in

		(* generates new unused variable name for the cfg *)
		let genVar () =
			let n = !k in
			let () = k:= n + 1 in
			let ascii = 65 + n in
			if ascii < 65 || ascii > 90 then 'A'
				else Char.chr ascii
		in

		(*
		let convertPlsRules rl i1 i2 newInit =
			(* swaps the initial variables of both old cfgs for the new initial var *)
			let swapInits c = if c = i1 || c = i2 then newInit else c in

			let newBody b = List.map (fun c -> swapInits c) b in
			let newRule r = {head = swapInits r.head; body = newBody r.body} in

				Set.map (fun r -> newRule r) rl

		in
		*)

		(* create gcf rules for plus expression *)
		let convertPlsRules rl i1 i2 newInit =

			let newRule1 = {head = newInit; body = [i1]} in
			let newRule2 = {head = newInit; body = [i2]} in

				Set.add newRule1 (Set.add newRule2 rl)

		in

		(* create gcf rules for seq expression *)
		let convertSeqRules lcfg rcfg =
			let rl1 = lcfg.rules in
			let rl2 = rcfg.rules in
			let alp1 = lcfg.alphabet in
			let rl = Set.union rl1 rl2 in

			let newBody r =
				let b = r.body in
					match b with
						| [c] when Set.belongs r rl1 && not (Set.belongs c alp1) && c <> epsilon -> b
						| [c] when Set.belongs r rl1 && Set.belongs c alp1 -> [c; rcfg.initial]
						| [epsilon] when Set.belongs r rl1 -> [epsilon; rcfg.initial]
						| b when Set.belongs r rl2 -> b
						| _ -> b
			in
			let newRule r = {head = r.head; body = newBody r} in
				Set.map (fun r -> newRule r) rl
		in

		(* create gcf rules for star expression *)
		let convertStrRules cfg =

			let newBody b =
				match b with
					| [c] when Set.belongs c cfg.alphabet -> [c; cfg.initial]
					| _ -> b
			in
			let r0 = {head = cfg.initial; body = [epsilon]} in

			let newRule r = {head = r.head; body = newBody r.body} in
			let newRules = Set.map (fun r -> newRule r) cfg.rules in
				Set.add r0 newRules
		in



		let rec compile rep =
			let open RegExpSyntax in
			match rep with

				| Plus(l, r) ->
						let cl = compile l in
						let cr = compile r in
						let alp = Set.union cl.alphabet cr.alphabet in
						let init = genVar () in
						let vs = Set.add init (Set.union cl.variables cr.variables) in
						let rl = Set.union cl.rules cr.rules in
						let rl = convertPlsRules rl cl.initial cr.initial init in

							{alphabet = alp; variables = vs;
								initial = init; rules = rl}

				| Seq(l, r) ->
						let cl = compile l in
						let cr = compile r in
						let alp = Set.union cl.alphabet cr.alphabet in
						let init = cl.initial in
						let vs = Set.union cl.variables cr.variables in
						let rl = convertSeqRules cl cr in

							{alphabet = alp; variables = vs;
								initial = init; rules = rl}

				| Star(re) ->
						let cre = compile re in
						let alp = cre.alphabet in
						let init = cre.initial in
						let vs = cre.variables in
						let rl = convertStrRules cre in

							{alphabet = alp; variables = vs;
								initial = init; rules = rl}

				| Symb(c) ->
						let alp = Set.make [c] in
						let init = genVar () in
						let vars = Set.make [init] in
						let rules = Set.make [{head = init; body = [c]}] in

							{alphabet = alp; variables = vars;
								initial = init; rules = rules}

				| Empty ->
						let alp = Set.empty in
						let init = genVar () in
						let vars = Set.make [init] in
						let rules = Set.make [{head = init; body = [epsilon]}] in
							{alphabet = alp; variables = vars;
								initial = init; rules = rules}

				| Zero ->
						let alp = Set.empty in
						let init = genVar () in
						let var2 = genVar () in
						let vars = Set.make [init; var2] in
						let r1 = {head = init; body = [var2]} in
						let r2 = {head = var2; body = [init]} in
						let rules = Set.make [r1; r2] in

							{alphabet = alp; variables = vars;
								initial = init; rules = rules}
		in


		let cfg = compile re#representation in

			new ContextFreeGrammar.model (Arg.Representation (cfg))


	(**
		* This method converts the automaton into its equivalent regular grammar
		*
		* @returns ContextFreeGrammar.model -> the resulting regular grammar
		*)

	let fa2cfg fa =
			let re = fa2re fa in
				re2cfg re





	(* This method converts the right-linear grammar to its automaton equivalent
	*
	* @pre - the grammar needs to be regular
	*
	* @returns FiniteAutomaton.model -> the equivalent finite automaton
	*)
	let cfg2fa cfg =
		let open ContextFreeGrammar in
		let open CFGSyntax in

		let rep = cfg#representation in

		let alp = rep.alphabet in
		let vrs = rep.variables in
		let toStr = Util.ch2str in

		(* This name will always be unique in the generated automaton *)
		let accSt = "AccSt" in

		let alphabet = alp in
		let states = Set.map (fun v -> toStr v) rep.variables in
		let states = Set.add accSt states in
		let initialState = toStr rep.initial in
		let acceptStates = Set.make [accSt] in



		let ruleToTrans rh rb =
			match rb with
				| [s;v] when Set.belongs s alp && Set.belongs v vrs	-> Set.make [(toStr rh, s, toStr v)]

				| [v] when Set.belongs v vrs -> Set.make [(toStr rh, epsilon, toStr v)]

				| [s] when Set.belongs s alp -> Set.make [(toStr rh, s, accSt)]

				| [e] when e = epsilon -> Set.make [(toStr rh, epsilon, accSt)]

				| _ -> Set.empty
		in

		let transitions = Set.flatMap (fun r -> ruleToTrans r.head r.body) rep.rules in

		let open FiniteAutomaton in
		let fa = {alphabet = alphabet; states = states; initialState = initialState;
					transitions = transitions; acceptStates = acceptStates} in

			new FiniteAutomaton.model (Arg.Representation (fa))


	(* This method converts the right-linear grammar to its equivalent regular expression
	*
	* @pre - the grammar needs to be regular
	*
	* @returns FiniteAutomaton.model -> the equivalent regular expression
	*)

	let cfg2re cfg =
		let fa = cfg2fa cfg in
			fa2re fa
end

module PolyModelTests: sig end =
struct
	open PolyModel

	let active = false

	let testToFA () =
		let re = new RegularExpression.model (Arg.Predef "re_abc") in
		let fa = re2fa re in
			JSon.show fa#toJSon

	let testToFA2 () =
		let re = new RegularExpression.model (Arg.Predef "re_simple") in
		let fa = re2fa re in
			JSon.show fa#toJSon

	let testToFA3 () =
		let re = new RegularExpression.model (Arg.Predef "re_complex") in
		let fa = re2fa re in
			JSon.show fa#toJSon

	let testToFA4 () =
		let re = new RegularExpression.model (Arg.Predef "re_convoluted") in
		let fa = re2fa re in
			JSon.show fa#toJSon

	let fa_toRe = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "fa_toRe",
		alphabet : ["a","b"],
		states : ["1", "2"],
		initialState : "1",
		transitions : [
				["1","a","2"],["2","b","2"]
			],
		acceptStates : ["2"]
	} |}

	let testSimplify () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_toRe) in
		let r = fa2re fa in
		JSon.show r#toJSon;
		let rs = r#simplify in
		let j = rs#toJSon in
			JSon.show j

	let testToRe () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_toRe) in
		let r = fa2re fa in
		let j = r#toJSon in
			JSon.show j

	let testToGrammar () =
		let re = new RegularExpression.model (Arg.Predef "re_simple") in
		let res = re2cfg re in
			JSon.show res#toJSon

	let testToAutomaton () =
		let m = new ContextFreeGrammar.model (Arg.Predef "cfg_abc") in
		let res = cfg2fa m in
			JSon.show res#toJSon

	let testToRe () =
		let m = new ContextFreeGrammar.model (Arg.Predef "cfg_abc") in
		let res = cfg2re m in
			JSon.show res#toJSon


	let runAll =
		if active then (
			Util.header "PolyModelTests";
			testSimplify ()
		)
end
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
(*
 * Tests.ml
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
 *  Written by Artur Miguel Dias (amd)
 *)

(*
 * ChangeLog:
 *
 * jan/2021 (amd) - Initial version.
 *)

(*
 * Description: Set of unit tests for checking the library from time to time.
 *
 * TODO: This is only a starting point. There are already many (disabled) unit
 * tests in several modules and this stuff needs to be reviewed.
 *)

module type TestsSig =
sig
	val tests: unit -> unit
end

module Tests : TestsSig =
struct
	open TopLevel
	open Examples

	let test1 () =
		let a = fa_predef "dfa_1" in
			Util.println [if fa_accept a "ab" then "OK" else "ERROR"]

	let tests () =
		test1 ()
end
open TopLevel
open Examples
open Tests
