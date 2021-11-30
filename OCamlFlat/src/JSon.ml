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
