(*
 * HtmlPageClient.ml
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
 * Description: Visualizer component of the application.
 *)

open OCamlFlat
open Js_of_ocaml
open Graphics
open Lang
open Listeners
open StateVariables
open JS

module HtmlPageClient: sig
  open BasicTypes
  val oneBox: unit -> unit
  val twoBoxes: unit -> unit
  val clearBox1: unit -> unit
  val clearBox2: unit -> unit

  val putCyAutomataButtons: unit -> unit
  val closeInfo: unit -> unit
  val putCy2Buttons: unit -> unit
  val putCyREButtons: unit -> unit
  
  val putButton: String.t -> unit
  val createSpanList: symbol list -> string -> string -> unit

  val getDeterminim: bool -> unit
  val getMinimism: bool -> unit
  val getHasUselessStates: bool -> states -> unit
  val getNumberStates: int -> unit
  val getNumberTransitions: int -> unit
  val defineInformationBoxAutomaton: unit -> unit
  val defineRE: string -> unit

  val putWords: words -> unit
  val putCy2Text: string -> unit

  val putEnumButton: unit -> unit
  val addEnumTitle: unit -> unit
  val defineEnumProblem: string -> unit
  val addAcceptedTitle: unit -> unit
  val addNonAcceptTitle: unit -> unit
  val addEnumCheckButton: unit -> unit
  val defineResult: bool -> unit
  val defineTreeButtons: unit -> unit

  val changeLang: unit -> unit 

  val disableButtons: string -> unit
  val enableAllButtons: string -> unit

  val defineMainTitle: string -> unit

  val about: unit -> unit 
  val feedback: unit -> unit

  val putInfoAutomata: string -> unit
  val putTreeResult: string -> unit
  val putTreeGoodDerivations: string -> unit
  val putTreeBadDerivations: string -> unit
  val putTreeResult: string -> unit

	val deleteTable: string -> string -> unit
	val createTable: string -> string -> Dom_html.tableElement Js.t
	val putTable: Dom_html.tableElement Js.t -> string ->unit
	val addTableRow: Dom_html.tableElement Js.t -> string -> string -> unit
	val deleteTableRow: Dom_html.tableElement Js.t -> string -> unit
	val getMinimalPDA: unit -> unit
	val putUsefulStatesInfoPDA: states -> unit
	val putCyPDAButtons: unit -> unit
	val replaceRE: string -> unit
	
end 
= 

struct
  let button_type = Js.string "button"
let div_type = Js.string "div"
let text_type = Js.string "textarea"
let input_type = Js.string "file"
let doc = Dom_html.document


let button txt idtxt action =
let b = Dom_html.createButton ~_type:button_type doc in
b##.id := Js.string idtxt;
b##.innerHTML := Js.string txt;
b##.onclick := Dom_html.handler (fun _ -> action (); Js._true);
b

let button1 txt idtxt classtxt action =
let b = Dom_html.createButton ~_type:button_type doc in
b##.id := Js.string idtxt;
b##.innerHTML := Js.string txt;
b##.className := Js.string classtxt;
b##.onclick := Dom_html.handler (fun _ -> action (); Js._true);
b

let input idtxt action = 
let b = Dom_html.createInput ~_type:input_type doc in
b##.id := Js.string idtxt;
b##.onchange := Dom_html.handler (fun _ -> action (); Js._true);
b

let div idtxt =
let d = Dom_html.createDiv doc in
d##.id := Js.string idtxt;
d

let div1 idtxt txt =
let d = Dom_html.createDiv doc in
  d##.id := Js.string idtxt;
  d##.innerHTML := Js.string txt;
d

let div2 idtxt classtxt txt =
let d = Dom_html.createDiv doc in
d##.id := Js.string idtxt;
d##.className := Js.string classtxt;
d##.innerHTML := Js.string txt;
d

let textarea idtxt rows cols value =
let t = Dom_html.createTextarea doc in
  t##.id := Js.string idtxt;
  t##.rows := rows;
  t##.cols := cols;
  t##.value := value;
t

let h2 idtxt txt =
let h = Dom_html.createH2 doc in
    h##.id := Js.string idtxt;
    h##.innerHTML := Js.string txt;
  h

let h1 idtxt txt =
let h = Dom_html.createH1 doc in
  h##.id := Js.string idtxt;
  h##.innerHTML := Js.string txt;
h

let span idtxt txt =
let s = Dom_html.createSpan doc in
  s##.id := Js.string idtxt;
  s##.innerHTML := Js.string txt;
s

let p idtxt txt =
let p = Dom_html.createP doc in
  p##.id := Js.string idtxt;
  p##.innerHTML := Js.string txt;
p

let br idtxt =
let b = Dom_html.createBr doc in
    b##.id := Js.string idtxt;
b

let a ref1 txt = 
let c = Dom_html.createA doc in
  c##.href := Js.string ref1;
  c##.innerHTML := Js.string txt;
c

let pre idtxt txt =
let p = Dom_html.createPre doc in
  p##.id := Js.string idtxt;
  p##.innerHTML := Js.string txt;
p

let putInnerHtml idtxt txt =
  let element = Dom_html.getElementById idtxt in
  element##.innerHTML := Js.string txt

let putInnerHtmlButtons idtxt txt idtool classTool txt1 =
  let element = Dom_html.getElementById idtxt in
    element##.innerHTML := (Js.string txt);
    let tool = div2 idtool classTool txt1 in
      Dom.appendChild element tool

  let listOnlyAutomataButtons = ["backwards"; "start"; "forward"; "clearAuto"; "selectRegex"]
  let listOnlyExpressionButtons = ["selectAutomaton"]
  let listOtherButtons = ["testing"; "generate"; "fitGraph"]

  let defineMainTitle type1 =
    let title = Dom_html.getElementById "mainTitle" in
      title##.innerHTML := Js.string "";
    if type1 = "finite automaton" then
      title##.innerHTML := Js.string (Lang.defineMainTitle1 !Lang.lang)
    else if type1 = "regular expression" then
      title##.innerHTML := Js.string (Lang.defineMainTitle2 !Lang.lang)
    else if type1 = PushdownAutomaton.modelDesignation then
      title##.innerHTML := Js.string (Lang.defineMainTitlePDA !Lang.lang)
    else 
      title##.innerHTML := Js.string (Lang.defineMainTitle3 !Lang.lang)

  let disableButton buttonName =
    let buttonTo = Dom_html.getElementById buttonName in
      buttonTo##setAttribute (Js.string "disabled") (Js.string "disabled")
  
  let enableButton buttonName =
    let buttonTo = Dom_html.getElementById buttonName in
      buttonTo##removeAttribute (Js.string "disabled")

  let disableButtons type1 =
    if type1 = "finite automaton" then
      (List.iter (fun el -> disableButton el) listOnlyExpressionButtons;
      List.iter (fun el -> enableButton el) listOnlyAutomataButtons)
    else if type1 = "regular expression" then
      (List.iter (fun el -> disableButton el) listOnlyAutomataButtons;
      List.iter (fun el -> enableButton el) listOnlyExpressionButtons)
      
	else if type1 = PushdownAutomaton.modelDesignation then
      (List.iter (fun el -> disableButton el) listOnlyExpressionButtons;
      List.iter (fun el -> enableButton el) listOnlyAutomataButtons)
      
    else
      (List.iter (fun el -> disableButton el) listOnlyExpressionButtons;
      List.iter (fun el -> disableButton el) listOnlyAutomataButtons;
      List.iter (fun el -> disableButton el) listOtherButtons)

    let enableAllButtons type1 =
      List.iter (fun el -> enableButton el) listOtherButtons;
      if type1 = "finite automaton" then
        (List.iter (fun el -> enableButton el) listOnlyAutomataButtons)

  let createSpanList word acceptance list =
    let element = Dom_html.getElementById list in
    let string_of_word = String.concat "" (List.map (String.make 1) word) in
      let ac = span acceptance ("' " ^ string_of_word ^ " '") in 
        Dom.appendChild element ac;
      let space = br "br" in 
        Dom.appendChild element space

  let clearBox1 () = 
    putInnerHtml "regExp" ""

  let clearBox2 () = 
    putInnerHtml "buttonBox1" "";
    putInnerHtml "textBox" "";
    Graphics.destroyGraph1 ()

  let oneBox () = 
    let box1 = Dom_html.getElementById "Box1" in
      box1##.style##.width:= Js.string "99%";
    let box2 = Dom_html.getElementById "Box2" in
      box2##.style##.width:= Js.string "0%";
    putInnerHtml "buttonBox1" "";
    StateVariables.completeSize();
    Graphics.destroyGraph1 ();
    Graphics.fit ()

  let twoBoxes () = 
    clearBox2();
    let box1 = Dom_html.getElementById "Box1" in
      box1##.style##.width:= Js.string "49.5%";
    let box2 = Dom_html.getElementById "Box2" in
      box2##.style##.width:= Js.string "49.5%";
      StateVariables.halfSize();
      Graphics.fit ()

  let closeButton () =
    let c = button1 "X" "closeLeft" "tooltip1" !Listeners.changeListener in             
    let tool = div2 "tooltipCloseLeft" "tooltiptext1" (Lang.tooltipCloseLeft !Lang.lang) in
      Dom.appendChild c tool;
    c

  let putInfoAutomata info = 
    let textBox = Dom_html.getElementById "textBox" in
      let test = pre "infoAutomata" info in 
        Dom.appendChild textBox test
	
  let putCyAutomataButtons () =
    let buttonBox = Dom_html.getElementById "buttonBox" in
      buttonBox##.innerHTML := Js.string "";
    let divButtons2 = div "close" in
      Dom.appendChild buttonBox divButtons2;
    let di = Dom_html.getElementById "close" in
      Dom.appendChild di (closeButton());
    let info = button1 (Lang.defineFormatting !Lang.lang) "formatting" "tooltip2" !Listeners.getCompleteAutomatonListener in 
      Dom.appendChild di info;
    let tool = div2 "tooltipSpecification" "tooltiptext2" (Lang.tooltipSpecification !Lang.lang) in
      Dom.appendChild info tool;
    let divButtons1 = div "min" in
      Dom.appendChild buttonBox divButtons1;
    let c = button1 (Lang.defineClean !Lang.lang) "clean" "tooltip3" !Listeners.cleanUselessListener in 
      Dom.appendChild divButtons1 c;
      let tool = div2 "tooltipClean" "tooltiptext3" (Lang.tooltipClean !Lang.lang) in
        Dom.appendChild c tool;
    let de = button1 (Lang.defineDeterministic !Lang.lang) "deterministic" "tooltip3" !Listeners.getDeterministicListener in 
      Dom.appendChild divButtons1 de;
      let tool = div2 "tooltipDeterministic" "tooltiptext3" (Lang.tooltipDeterministic !Lang.lang) in
        Dom.appendChild de tool;
    let de = button1 (Lang.defineMinimize !Lang.lang) "minimize" "tooltip3" !Listeners.defineMinimizedListener in 
      Dom.appendChild divButtons1 de;
      let tool = div2 "tooltipMinimize" "tooltiptext3" (Lang.tooltipMinimize !Lang.lang) in
        Dom.appendChild de tool;
    let divButtons = div "prod" in
      Dom.appendChild buttonBox divButtons;
    let b = button1 (Lang.defineProductive !Lang.lang) "productive" "tooltip3" !Listeners.paintAllProductivesListener in 
      Dom.appendChild divButtons b;
      let tool = div2 "tooltipProductive" "tooltiptext3" (Lang.tooltipProductive !Lang.lang) in
        Dom.appendChild b tool;
    let a = button1 (Lang.defineAccessible !Lang.lang) "accessible" "tooltip3" !Listeners.paintAllReachableListener in
      Dom.appendChild divButtons a;
      let tool = div2 "tooltipAccessible" "tooltiptext3" (Lang.tooltipAccessible !Lang.lang) in
        Dom.appendChild a tool;
    let u = button1 (Lang.defineUseful !Lang.lang) "useful" "tooltip3" !Listeners.paintAllUsefulListener in
      Dom.appendChild divButtons u;
      let tool = div2 "tooltipUseful" "tooltiptext3" (Lang.tooltipUseful !Lang.lang) in
        Dom.appendChild u tool

  let putCy2Buttons () = 
    let buttonBox = Dom_html.getElementById "buttonBox1" in
      let test1 = button1 "X" "closeRight" "tooltip1" !Listeners.closeRightListener in 
        Dom.appendChild buttonBox test1;
      let tool = div2 "tooltipCloseRight" "tooltiptext1" (Lang.tooltipCloseRight !Lang.lang) in
        Dom.appendChild test1 tool

  let closeInfo () =
    let buttonBox = Dom_html.getElementById "buttonBox1" in
      let test = button1 "X" "closeRight" "tooltip1" !Listeners.closeCompleteAutomatonListener in 
        Dom.appendChild buttonBox test;
        let tool = div2 "tooltipCloseRight" "tooltiptext1" (Lang.tooltipCloseRight !Lang.lang) in
          Dom.appendChild test tool
    
  let putCyREButtons() =
    let cy = Dom_html.getElementById "buttonBox" in
      cy##.innerHTML := Js.string "";
    let divButtons2 = div "close" in
      Dom.appendChild cy divButtons2;
        Dom.appendChild divButtons2 (closeButton());
    let direction = button1 (Lang.defineDirection !Lang.lang) "changeDirection" "tooltip2" !Listeners.changeDirectionListener in 
      Dom.appendChild divButtons2 direction;
    let tool = div2 "tooltipDirection" "tooltiptext2" (Lang.tooltipDirection !Lang.lang) in
      Dom.appendChild direction tool

  let putTreeResult text =
    let textBox = Dom_html.getElementById "textBox" in
      let en = div1 "treeResult" text in 
        Dom.appendChild textBox en

  let getDeterminim deter = 
    let infoBox = Dom_html.getElementById "infoBox" in
      if deter then 
        let deterministic = span "isdeterministic" (Lang.defineIsDeterministic !Lang.lang) in 
          Dom.appendChild infoBox deterministic
      else 
        let deterministic = span "isdeterministic" (Lang.defineNotDeterministic !Lang.lang) in 
          Dom.appendChild infoBox deterministic
    
  let getMinimism min = 
    let infoBox = Dom_html.getElementById "infoBox" in
      if min then 
        let minimal = span "isminimal" (Lang.defineIsMinimal !Lang.lang) in 
          Dom.appendChild infoBox minimal 
      else 
        let minimal = span "isminimal" (Lang.defineNotMinimal !Lang.lang) in 
          Dom.appendChild infoBox minimal
    
  let getHasUselessStates use uStates = 
    let infoBox = Dom_html.getElementById "infoBox" in
      if use then 
        (let useful = span "areuseful" (Lang.defineNotUseless !Lang.lang) in
          Dom.appendChild infoBox useful)
      else 
        (let useless = Set.toList uStates in
          let number = List.length useless in 
            let useful = (Lang.defineHas !Lang.lang) ^ (string_of_int number) ^ (Lang.defineUselessStates !Lang.lang) in 
              let use = span "areuseful" useful in
                Dom.appendChild infoBox use)

  let getNumberStates nStates = 
    let number = string_of_int nStates in 
      let infoBox = Dom_html.getElementById "infoBox" in
        let sentence = (Lang.defineNumberStates !Lang.lang) ^ number ^ ". " in 
          let sentence1 = span "numberstates" sentence in
            Dom.appendChild infoBox sentence1
    
  let getNumberTransitions nTransitions = 
    let number = string_of_int nTransitions in 
      let infoBox = Dom_html.getElementById "infoBox" in
        let sentence = (Lang.defineNumberTransitions !Lang.lang) ^ number ^ ". " in 
          let sentence1 = span "numbertransitions" sentence in
            Dom.appendChild infoBox sentence1
    
  let defineInformationBoxAutomaton () =
    putInnerHtml "infoBox" ""

  let createServerExampleButton name =
      button name "exampleButton" (fun () -> (Listeners.text := (Examples.example name); !Listeners.createTextListener ()));;

  let putButton name = 
    let examples = Dom_html.getElementById "examplesServer" in
      let title = name in 
        let example = createServerExampleButton title in 
          Dom.appendChild examples example

  let defineRE def =
    let regExp = Dom_html.getElementById "regExp" in
      let expr = div1 "reg" def in
        Dom.appendChild regExp expr;
      putInnerHtml "infoBox" ""

  let putWords listWords =
    putInnerHtml "textBox" "";
    let textBox = Dom_html.getElementById "textBox" in
    let title = div1 "generateWords" (Lang.defineGenerateWords !Lang.lang) in 
      Dom.appendChild textBox title;
    let test = Set.toList listWords in 
      let string_of_word w = "' " ^ String.concat "" (List.map (String.make 1) w) ^ " '" in
        let string_of_words l = String.concat ", " (List.map string_of_word l) in 
          let res = string_of_words test in
            let zz = textarea "textarea" 2 20 (Js.string res) in 
              Dom.appendChild textBox zz

  let putCy2Text text = 
    let textBox = Dom_html.getElementById "textBox" in
      let expr = div1 "text" text in
        Dom.appendChild textBox expr

  let putEnumButton () = 
    let buttonBox = Dom_html.getElementById "buttonBox1" in
      let clearButton = button "X" "clearEnum" !Listeners.clearExerciseListener in      
        Dom.appendChild buttonBox clearButton

  let addEnumTitle () =
    let textBox = Dom_html.getElementById "textBox" in
      let en = h2 "enum" (Lang.defineEnumTitle !Lang.lang) in 
        Dom.appendChild textBox en

  let defineEnumProblem prob =
    let textBox = Dom_html.getElementById "textBox" in
      let test = (Lang.defineProblem !Lang.lang) ^ prob in
        let en = div1 "prob" test in 
          Dom.appendChild textBox en;
    let resultBox = div "resultBox" in 
      Dom.appendChild textBox resultBox

  let addAcceptedTitle () =
    let textBox = Dom_html.getElementById "textBox" in
      let tac = div1 "accept" (Lang.defineAcceptedWords !Lang.lang) in 
        Dom.appendChild textBox tac;
      let ac = div "inside" in 
        Dom.appendChild textBox ac    

  let addNonAcceptTitle () =
    let textBox = Dom_html.getElementById "textBox" in
      let twr = div1 "wrong" (Lang.defineNonAccepted !Lang.lang) in 
        Dom.appendChild textBox twr;
      let wr = div "outside" in 
        Dom.appendChild textBox wr
    
  let addEnumCheckButton () =
    let textBox = Dom_html.getElementById "textBox" in
      let checkButton = button (Lang.defineVerify !Lang.lang) "enumVerify" !Listeners.checkExerciseListener in
        Dom.appendChild textBox checkButton
  
  let defineResult result =
    let resultBox = Dom_html.getElementById "resultBox" in
      resultBox##.innerHTML := Js.string "";
    if result then 
      let res = div1 "right" (Lang.defineRight !Lang.lang) in
        Dom.appendChild resultBox res
    else
      (let res = div1 "error" (Lang.defineWrong !Lang.lang) in 
        Dom.appendChild resultBox res);
      putInnerHtml "inside" "";
      putInnerHtml "outside" ""

  let defineTreeButtons () =
    let textBox = Dom_html.getElementById "textBox" in
      let en = div "treeButtons" in 
        Dom.appendChild textBox en;
      let buttonBox = Dom_html.getElementById "treeButtons" in 
        let previous = button (Lang.definePrevious !Lang.lang) "previousTree" !Listeners.previousTreeListener in
          Dom.appendChild buttonBox previous;
        let next = button (Lang.defineNext !Lang.lang) "nextTree" !Listeners.nextTreeListener in
          Dom.appendChild buttonBox next

  let putTreeGoodDerivations text =
    let textBox = Dom_html.getElementById "textBox" in
      let en = div1 "treeGoodDerivations" text in 
        Dom.appendChild textBox en

  let putTreeBadDerivations text =
    let textBox = Dom_html.getElementById "textBox" in
      let en1 = div1 "treeBadDerivations" text in 
        Dom.appendChild textBox en1

  let putTreeResult text =
    let textBox = Dom_html.getElementById "textBox" in
      let en = div1 "treeResult" text in 
          Dom.appendChild textBox en

  let about () =
    putInnerHtml "regExp" "";
    putInnerHtml "infoBox" "";
    let cy = Dom_html.getElementById "buttonBox" in
      cy##.innerHTML := Js.string "";
        Dom.appendChild cy (closeButton());
    putInnerHtml "mainTitle" (Lang.defineAboutTitle !Lang.lang);
    let info = div "aboutBox" in 
      Dom.appendChild cy info;
    let aboutBox = Dom_html.getElementById "aboutBox" in
      let subtitle = h2 "aboutSubtitle" (Lang.defineAboutSubtitle !Lang.lang) in
        Dom.appendChild aboutBox subtitle;
      let aboutTex = p "aboutText" "" in 
        Dom.appendChild aboutBox aboutTex;
      let text = span "aboutText1" (Lang.defineAboutText1 !Lang.lang) in 
        Dom.appendChild aboutTex text;
      let text1 = a "http://nova-lincs.di.fct.unl.pt/" "NOVA-LINCS" in
        Dom.appendChild aboutTex text1;
      let text2 = span "aboutText2" (Lang.defineAboutText2 !Lang.lang) in 
        Dom.appendChild aboutTex text2;
      let text3 = a "https://release.di.ubi.pt/factor/index.html" "Factor" in
        Dom.appendChild aboutTex text3;
      let text = (Lang.defineAboutText16 !Lang.lang) ^ (Lang.defineAboutText17 !Lang.lang) ^ (Lang.defineAboutText3 !Lang.lang) in
      let text4 = span "aboutText3" text in
        Dom.appendChild aboutTex text4;
      let text5 = a "https://tezos.com/" "Factor" in
        Dom.appendChild aboutTex text5;
      let text6 = span "aboutText16" (Lang.defineAboutText16 !Lang.lang) in
        Dom.appendChild aboutTex text6;
      let text7 = a "https://www.inria.fr/" (Lang.defineFooter1 !Lang.lang) in
        Dom.appendChild aboutTex text7;
      let text8 = span "aboutText16" "." in
        Dom.appendChild aboutTex text8;
      let aboutTex1 = p "aboutText17" "" in 
        Dom.appendChild aboutBox aboutTex1;
      let text9 = span "aboutText4" (Lang.defineAboutText4 !Lang.lang) in
        Dom.appendChild aboutTex text9;
      let text10 = a "https://github.com/git-amd/OFLAT" "GitHub" in
        Dom.appendChild aboutTex text10;
      let text11 = span "aboutText18" "." in
        Dom.appendChild aboutTex text11;
      let sub = h2 "aboutSubtitle2" (Lang.defineAboutSubtitle2 !Lang.lang) in
        Dom.appendChild aboutBox sub;
      let text12 = p "aboutText5" (Lang.defineAboutText5 !Lang.lang) in 
        Dom.appendChild aboutBox text12;
      let text13 = p "aboutText6" (Lang.defineAboutText6 !Lang.lang) in 
        Dom.appendChild aboutBox text13;
      let text14 = pre "aboutText7" (Lang.defineAboutText7 !Lang.lang) in 
        Dom.appendChild aboutBox text14;
      let text15 = p "aboutText8" (Lang.defineAboutText8 !Lang.lang) in 
        Dom.appendChild aboutBox text15;
      let text16 = pre "aboutText9" (Lang.defineAboutText9 !Lang.lang) in 
        Dom.appendChild aboutBox text16;
      let text17 = p "aboutText10" (Lang.defineAboutText10 !Lang.lang) in 
        Dom.appendChild aboutBox text17;
      let text18 = pre "aboutText11" (Lang.defineAboutText11 !Lang.lang) in 
        Dom.appendChild aboutBox text18;
      let text19 = p "aboutText12" (Lang.defineAboutText12 !Lang.lang) in 
        Dom.appendChild aboutBox text19;
      let text20 = p "aboutText13" (Lang.defineAboutText13 !Lang.lang) in 
        Dom.appendChild aboutBox text20;
      let text21 = p "aboutText14" (Lang.defineAboutText14 !Lang.lang) in 
        Dom.appendChild aboutBox text21;
      let text22 = p "aboutText15" (Lang.defineAboutText15 !Lang.lang) in 
        Dom.appendChild aboutBox text22

    let feedback () = 
      let cy = Dom_html.getElementById "regExp" in
        cy##.innerHTML := Js.string "";
      let cy = Dom_html.getElementById "infoBox" in
        cy##.innerHTML := Js.string "";
      let cy = Dom_html.getElementById "buttonBox" in
        cy##.innerHTML := Js.string "";
          Dom.appendChild cy (closeButton());
      putInnerHtml "mainTitle" (Lang.defineFeedbackTitle !Lang.lang);
      let test1 = div (Lang.defineFeedbackTitle !Lang.lang) in
        Dom.appendChild cy test1;
      let text = Dom_html.getElementById "Feedback" in
        let text1 = p "feedbackText" (Lang.defineFeedbackText !Lang.lang) in
          Dom.appendChild text text1;
        let spanBox = p "feedbackText1" "" in
          Dom.appendChild text spanBox;
        let spanBox1 = Dom_html.getElementById "feedbackText1" in
        let span1 = span "feedbackText2" (Lang.defineFeedbackText2 !Lang.lang) in
          Dom.appendChild spanBox1 span1;
        let link = a "mailto:rp.macedo@campus.fct.unl.pt" "Rita Macedo" in
          Dom.appendChild spanBox1 link;
        let span2 = span "feedbackText3" "." in
          Dom.appendChild spanBox1 span2;
        let text3 = p "feedbackThankYou" (Lang.defineFeedbackThankYou !Lang.lang) in
          Dom.appendChild text text3
    
  let changeLang () =
    Graphics.changeLang !Lang.lang;
    putInnerHtml "title" (Lang.defineTitle !Lang.lang);
    putInnerHtml "version" (Lang.defineVersion !Lang.lang);
    putInnerHtml "define" (Lang.defineDefine !Lang.lang);
    putInnerHtml "startGraph" (Lang.defineStartGraph !Lang.lang);
    putInnerHtml "fitGraph" (Lang.defineFitGraph !Lang.lang);
    putInnerHtml "generate" (Lang.defineGenerate !Lang.lang);
    putInnerHtml "testing" (Lang.defineTesting !Lang.lang);
    putInnerHtml "step" (Lang.defineStep !Lang.lang);
    putInnerHtml "start" (Lang.defineStart !Lang.lang);
    putInnerHtml "clearAuto" (Lang.defineClearAuto !Lang.lang);
        
    putInnerHtml "selectRegex" (Lang.defineSelectRegex !Lang.lang);
    putInnerHtml "selectAutomaton" (Lang.defineSelectAutomaton !Lang.lang);
    putInnerHtml "selectConv" (Lang.defineSelectConv !Lang.lang);

    putInnerHtml "importTitle" (Lang.defineImportTitle !Lang.lang);
    putInnerHtml "server" (Lang.defineServer !Lang.lang);     

    putInnerHtml "selectedL" (Lang.defineSelectedL !Lang.lang);   
    putInnerHtml "selectPT" (Lang.defineSelectPT !Lang.lang);
    putInnerHtml "selectEN" (Lang.defineSelectEN !Lang.lang);

    putInnerHtml "about" (Lang.defineAbout !Lang.lang);
    putInnerHtml "feedback" (Lang.defineFeedback !Lang.lang);
        
    putInnerHtml "developed" (Lang.defineDeveloped !Lang.lang);
    putInnerHtml "project" (Lang.defineProject !Lang.lang);
    putInnerHtml "financing" (Lang.defineFinancing !Lang.lang);
    putInnerHtml "and" (Lang.defineAboutText16 !Lang.lang);

    if (StateVariables.getCy1Type() = StateVariables.getAutomatonType()) then
      (putInnerHtml "tooltipCloseLeft" (Lang.tooltipCloseLeft !Lang.lang);
      putInnerHtmlButtons "formatting" (Lang.defineFormatting !Lang.lang) "tooltipSpecification" "tooltiptext2" (Lang.tooltipSpecification !Lang.lang);
      putInnerHtmlButtons "clean" (Lang.defineClean !Lang.lang) "tooltipClean" "tooltiptext3" (Lang.tooltipClean !Lang.lang);
      putInnerHtmlButtons "deterministic" (Lang.defineDeterministic !Lang.lang) "tooltipDeterministic" "tooltiptext3" (Lang.tooltipDeterministic !Lang.lang);
      putInnerHtmlButtons "minimize" (Lang.defineMinimize !Lang.lang) "tooltipMinimize" "tooltiptext3" (Lang.tooltipMinimize !Lang.lang);
      putInnerHtmlButtons "productive" (Lang.defineProductive !Lang.lang) "tooltipProductive" "tooltiptext3" (Lang.tooltipProductive !Lang.lang);
      putInnerHtmlButtons "accessible" (Lang.defineAccessible !Lang.lang) "tooltipAccessible" "tooltiptext3" (Lang.tooltipAccessible !Lang.lang);
      putInnerHtmlButtons "useful" (Lang.defineUseful !Lang.lang) "tooltipUseful" "tooltiptext3" (Lang.tooltipUseful !Lang.lang);
      putInnerHtml "infoBox" "";
      putInnerHtml "mainTitle" (Lang.defineMainTitle1 !Lang.lang);
      !Listeners.defineInformationBoxListener());

    if (StateVariables.getCy1Type() = StateVariables.getRegexType()) then
      (putInnerHtml "tooltipCloseLeft" (Lang.tooltipCloseLeft !Lang.lang);
      putInnerHtmlButtons "changeDirection" (Lang.defineDirection !Lang.lang) "tooltipDirection" "tooltiptext2" (Lang.tooltipDirection !Lang.lang);
      putInnerHtml "mainTitle" (Lang.defineMainTitle2 !Lang.lang));

	if (StateVariables.getCy1Type() = StateVariables.getPDAType()) then
      (putInnerHtml "tooltipCloseLeft" (Lang.tooltipCloseLeft !Lang.lang);
      putInnerHtmlButtons "formatting" (Lang.defineFormatting !Lang.lang) "tooltipSpecification" "tooltiptext2" (Lang.tooltipSpecification !Lang.lang);
      putInnerHtmlButtons "clean" (Lang.defineClean !Lang.lang) "tooltipClean" "tooltiptext3" (Lang.tooltipClean !Lang.lang);  
      putInnerHtmlButtons "productive" (Lang.defineProductive !Lang.lang) "tooltipProductive" "tooltiptext3" (Lang.tooltipProductive !Lang.lang);
      putInnerHtmlButtons "accessible" (Lang.defineAccessible !Lang.lang) "tooltipAccessible" "tooltiptext3" (Lang.tooltipAccessible !Lang.lang);
      putInnerHtmlButtons "useful" (Lang.defineUseful !Lang.lang) "tooltipUseful" "tooltiptext3" (Lang.tooltipUseful !Lang.lang);
      putInnerHtml "infoBox" "";
      putInnerHtml "mainTitle" (Lang.defineMainTitlePDA !Lang.lang);
      !Listeners.defineInformationBoxListener());

    if (StateVariables.getCy2Type() = StateVariables.getEnumerationType()) then
      (putInnerHtml "enumVerify" (Lang.defineVerify !Lang.lang);
      let prob = (StateVariables.returnEnum())#representation.problem in
        let prob1 = (Lang.defineProblem !Lang.lang) ^ prob in
        putInnerHtml "prob" prob1;
      putInnerHtml "enum" (Lang.defineEnumTitle !Lang.lang);
      putInnerHtml "accept" (Lang.defineAcceptedWords !Lang.lang);
      putInnerHtml "wrong" (Lang.defineNonAccepted !Lang.lang);
      putInnerHtml "right" (Lang.defineRight !Lang.lang);
      putInnerHtml "error" (Lang.defineWrong !Lang.lang);
      putInnerHtml "mainTitle" (Lang.defineMainTitle3 !Lang.lang);
      );

    if (StateVariables.getCy2Type() = StateVariables.getInfoType()) then
      (putInnerHtml "generateWords" (Lang.defineGenerateWords !Lang.lang);
      putInnerHtml "tooltipCloseRight" (Lang.tooltipCloseRight !Lang.lang);
      );

    if (StateVariables.getCy2Type() = StateVariables.getVerifyType()) then
      (putInnerHtml "textBox" "";
      !Listeners.resultCountListener ();
      !Listeners.defineNumberTreesListener ();
      defineTreeButtons ();
      putInnerHtml "tooltipCloseRight" (Lang.tooltipCloseRight !Lang.lang);
      );

      if (StateVariables.getCy1Type() = StateVariables.getFeedbackType()) then
      (putInnerHtml "mainTitle" (Lang.defineFeedbackTitle !Lang.lang);
       putInnerHtml "feedbackText" (Lang.defineFeedbackText !Lang.lang);
       putInnerHtml "feedbackText2" (Lang.defineFeedbackText2 !Lang.lang);
       putInnerHtml "feedbackThankYou" (Lang.defineFeedbackThankYou !Lang.lang);
      );

   if (StateVariables.getCy1Type() = StateVariables.getInfoType ()) then
       (putInnerHtml "mainTitle" (Lang.defineAboutTitle !Lang.lang);
        putInnerHtml "aboutSubtitle" (Lang.defineAboutSubtitle !Lang.lang);
        putInnerHtml "aboutSubtitle2" (Lang.defineAboutSubtitle2 !Lang.lang);
        putInnerHtml "aboutText1" (Lang.defineAboutText1 !Lang.lang);
        putInnerHtml "aboutText2" (Lang.defineAboutText2 !Lang.lang);
        putInnerHtml "aboutText3" (Lang.defineAboutText3 !Lang.lang);
        putInnerHtml "aboutText4" (Lang.defineAboutText4 !Lang.lang);
        putInnerHtml "aboutText5" (Lang.defineAboutText5 !Lang.lang);
        putInnerHtml "aboutText6" (Lang.defineAboutText6 !Lang.lang);
        putInnerHtml "aboutText7" (Lang.defineAboutText7 !Lang.lang);
        putInnerHtml "aboutText8" (Lang.defineAboutText8 !Lang.lang);
        putInnerHtml "aboutText9" (Lang.defineAboutText9 !Lang.lang);
        putInnerHtml "aboutText10" (Lang.defineAboutText10 !Lang.lang);
        putInnerHtml "aboutText11" (Lang.defineAboutText11 !Lang.lang);
        putInnerHtml "aboutText12" (Lang.defineAboutText12 !Lang.lang);
        putInnerHtml "aboutText13" (Lang.defineAboutText13 !Lang.lang);
        putInnerHtml "aboutText14" (Lang.defineAboutText14 !Lang.lang);
        putInnerHtml "aboutText15" (Lang.defineAboutText15 !Lang.lang);
       )

	(* ----- *)
	let replaceRE str =
		let regExp = Dom_html.getElementById "regExp" in
		  regExp##.innerHTML := JS.string str
	
	let createTable idtxt txt =
		let t = Dom_html.createTable doc in
			t##.id := Js.string idtxt;
			t##.innerHTML := Js.string ("<b>" ^ txt ^ "</b>");
			t##.border := Js.string "1";
			t##.rules := Js.string "rows";
			t
			
	let deleteTable tableId divId =
		let table = Dom_html.getElementById tableId in
		let div = Dom_html.getElementById divId in
			Dom.removeChild div table
		
	let putTable table id = 
		let div = Dom_html.getElementById id in
			Dom.appendChild div table
			
	let addTableRow table value id =
		let div = Dom_html.getElementById id in
			Dom.removeChild div table;
		let row = table##insertRow 0 in
			row##.innerHTML := Js.string value;
			row##.align := Js.string "center";
			Dom.appendChild div table
		
	let deleteTableRow table id =
		let div = Dom_html.getElementById id in
			Dom.removeChild div table;
			table##deleteRow (-1);
			Dom.appendChild div table
			
	let getMinimalPDA() =
		let infoBox = Dom_html.getElementById "infoBox" in 
		let txt = span "isminimal" (Lang.defineIsMinimalPDA !Lang.lang) in
		Dom.appendChild infoBox txt
	
	let putUsefulStatesInfoPDA uselessStates = 
		let infoBox = Dom_html.getElementById "infoBox" in
      if Set.size uselessStates = 0 then 
        (let useful = span "areuseful" (Lang.defineNotUseless !Lang.lang) in
          Dom.appendChild infoBox useful)
      else (
		let number = Set.size uselessStates in 
		let txt = (Lang.defineHas !Lang.lang) ^ (string_of_int number) ^ (Lang.defineUselessStates !Lang.lang) in 
		let use = span "areuseful" txt in
		Dom.appendChild infoBox use)
		
	let putCyPDAButtons() =
	
		let buttonBox = Dom_html.getElementById "buttonBox" in
		  buttonBox##.innerHTML := Js.string "";
		  
		let divButtons2 = div "close" in
		  Dom.appendChild buttonBox divButtons2;
		let di = Dom_html.getElementById "close" in
		  Dom.appendChild di (closeButton());
		  
		let info = button1 (Lang.defineFormatting !Lang.lang) "formatting" "tooltip2" !Listeners.getCompleteAutomatonListener in 
		  Dom.appendChild di info;
		let tool = div2 "tooltipSpecification" "tooltiptext2" (Lang.tooltipSpecification !Lang.lang) in
		  Dom.appendChild info tool;
		
		(* top buttons *)
		let divButtons1 = div "min" in
		  Dom.appendChild buttonBox divButtons1;
		  
		let c = button1 (Lang.defineClean !Lang.lang) "clean" "tooltip3" !Listeners.cleanUselessListener in 
			Dom.appendChild divButtons1 c;
			let tool = div2 "tooltipClean" "tooltiptext3" (Lang.tooltipClean !Lang.lang) in
			Dom.appendChild c tool;
			
		let u = button1 (Lang.defineUseful !Lang.lang) "useful" "tooltip3" !Listeners.paintAllUsefulListener in
			Dom.appendChild divButtons1 u;
			let tool = div2 "tooltipUseful" "tooltiptext3" (Lang.tooltipUseful !Lang.lang) in
			Dom.appendChild u tool;
		
		(* lower buttons  *)	
		let divButtons = div "prod" in
		  Dom.appendChild buttonBox divButtons;
		  
		let b = button1 (Lang.defineProductive !Lang.lang) "productive" "tooltip3" !Listeners.paintAllProductivesListener in 
			Dom.appendChild divButtons b;
			let tool = div2 "tooltipProductive" "tooltiptext3" (Lang.tooltipProductive !Lang.lang) in
			Dom.appendChild b tool;
			
		let a = button1 (Lang.defineAccessible !Lang.lang) "accessible" "tooltip3" !Listeners.paintAllReachableListener in
			Dom.appendChild divButtons a;
			let tool = div2 "tooltipAccessible" "tooltiptext3" (Lang.tooltipAccessible !Lang.lang) in
			Dom.appendChild a tool
end 
