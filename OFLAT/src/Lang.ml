(*
 * Lang.ml
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
 * Description: support for internationalization.
 *)

open Js_of_ocaml
open CSS

module Lang =
  struct
    let default_language () =
      (Js.Optdef.get
        Dom_html.window##.navigator##.language
          (fun () ->
            Js.Optdef.get Dom_html.window##.navigator##.userLanguage (fun () -> Js.string "en")))
        ##substring
        0
        2
      
    let lang =
      ref
      (Js.to_string (Js.Optdef.case Dom_html.window##.localStorage default_language (fun st ->
          Js.Opt.get (st##getItem (Js.string "hyp_lang")) default_language)))

    let set_language language =
        lang := Js.to_string (language)
    

  
  (** ---------------- Barra lateral primeiro bloco ----------------- **)

    let defineTitle lang =
      match lang with 
        "" -> ""
        | "en" -> "OFLAT"
        | "pt" -> "OFLAT"

    let defineVersion lang =
      match lang with 
        "" -> ""
        | "en" -> "Version 1.3"
        | "pt" -> "Versão 1.3"

    let defineDefine lang =
      match lang with 
      "" -> ""
        | "en" -> "Define regular expression"
        | "pt" -> "Definir expressão regular"
    
    let defineStartGraph lang =
      match lang with 
      "" -> ""
        | "en" -> "New automaton"
        | "pt" -> "Novo autómato"

    let defineFitGraph lang =
      match lang with 
      "" -> ""
        | "en" -> "Fit automaton to box"
        | "pt" -> "Ajustar autómato à caixa"


(** ---------------- Barra lateral segundo bloco ----------------- **)

    let defineGenerate lang =
      match lang with 
      "" -> ""
        | "en" -> "Generate words with size X"
        | "pt" -> "Gerar palavras de tamanho X"
    
    let defineTesting lang =
      match lang with 
      "" -> ""
        | "en" -> "Test word"
        | "pt" -> "Testar palavra"

    let defineStep lang =
      match lang with 
      "" -> ""
        | "en" -> "Step-by-step word acceptance"
        | "pt" -> "Aceitação passo-a-passo da palavra"

    let defineStart lang =
      match lang with 
      "" -> ""
        | "en" -> "Start"
        | "pt" -> "Início"
    
    let defineClearAuto lang =
      match lang with 
      "" -> ""
        | "en" -> "Clean Automaton"
        | "pt" -> "Limpar Autómato"

  (** ---------------- Barra lateral terceiro bloco ----------------- **)

    let defineSelectConv lang =
      match lang with 
      "" -> ""
        | "en" -> "Convert to"
        | "pt" -> "Converter em"

    let defineSelectRegex lang =
      match lang with 
      "" -> ""
        | "en" -> "Regular Expression"
        | "pt" -> "Expressão Regular"
    
    let defineSelectAutomaton lang =
      match lang with 
      "" -> ""
        | "en" -> "Automaton"
        | "pt" -> "Autómato"

  (** ---------------- Barra lateral quarto bloco ----------------- **)

    let defineImportTitle lang =
      match lang with 
      "" -> ""
        | "en" -> "Import example:"
        | "pt" -> "Importar exemplo:"

    let defineServer lang =
      match lang with 
      "" -> ""
        | "en" -> "Pre-defined examples"
        | "pt" -> "Exemplos pré-definidos"

  (** ---------------- Barra lateral quinto bloco ----------------- **)

    let defineSelectedL lang =
      match lang with 
        "" -> ""
        | "en" -> "Language"
        | "pt" -> "Língua"

    let defineSelectPT lang =
      match lang with 
        "" -> ""
        | "en" -> "Portuguese"
        | "pt" -> "Português"

    let defineSelectEN lang =
      match lang with 
        "" -> ""
        | "en" -> "English"
        | "pt" -> "Inglês"

    let defineAbout lang =
      match lang with 
        "" -> ""
        | "en" -> "About"
        | "pt" -> "Sobre"

    let defineFeedback lang =
      match lang with 
      "" -> ""
        | "en" -> "Feedback"
        | "pt" -> "Feedback"

    (** ---------------- Barra de topo ----------------- **)

    let defineMainTitle1 lang =
      match lang with 
      "" -> ""
        | "en" -> "Animated Automata"
        | "pt" -> "Autómatos Animados"

    let defineMainTitle2 lang =
      match lang with 
      "" -> ""
        | "en" -> "Regular Expressions"
        | "pt" -> "Expressões Regulares"

    let defineMainTitle3 lang =
      match lang with 
      "" -> ""
        | "en" -> "Exercises"
        | "pt" -> "Exercícios"

	let defineMainTitlePDA lang =
      match lang with 
      "" -> ""
        | "en" -> "Pushdown Automata"
        | "pt" -> "Autómatos de Pilha"

    (** ---------------- Footer ----------------- **)

    let defineDeveloped lang =
      match lang with 
      "" -> ""
        | "en" -> "Developed in "
        | "pt" -> "Desenvolvido em "

    let defineProject lang =
      match lang with 
      "" -> ""
        | "en" -> " by the projects "
        | "pt" -> " pelo projecto "

    let defineFinancing lang =
      match lang with 
      "" -> ""
        | "en" -> " / Co-financed by "
        | "pt" -> " / Co-financiado por "

    let defineFooter lang =
      match lang with 
      "" -> ""
        | "en" -> "Tezos Foundation"
        | "pt" -> "Fundação Tezos" 

      let defineFooter1 lang =
          match lang with 
          "" -> ""
            | "en" -> "INRIA Foundation"
            | "pt" -> "Fundação INRIA" 

    (** ---------------- Automatos ----------------- **)

    let defineFormatting lang =
      match lang with 
      "" -> ""
        | "en" -> "See automaton specification"
        | "pt" -> "Ver especificação do Autómato"

    let defineClean lang =
      match lang with 
      "" -> ""
        | "en" -> "Clean"
        | "pt" -> "Limpar"
    
    let defineDeterministic lang =
      match lang with 
      "" -> ""
        | "en" -> "Make deterministic"
        | "pt" -> "Tornar determinista"

    let defineMinimize lang =
      match lang with 
      "" -> ""
        | "en" -> "Minimize automaton"
        | "pt" -> "Minimizar autómato"

    let defineProductive lang =
      match lang with 
      "" -> ""
        | "en" -> "Productive states"
        | "pt" -> "Estados produtivos"

    let defineAccessible lang =
      match lang with 
      "" -> ""
        | "en" -> "Accessible states"
        | "pt" -> "Estados acessiveis"

    let defineUseful lang =
      match lang with 
      "" -> ""
        | "en" -> "Useful states"
        | "pt" -> "Estados úteis"
    
    let defineInfoBox lang =
      match lang with 
      "" -> ""
        | "en" -> "Data about the Automaton"
        | "pt" -> "Dados sobre o Autómato"

    let defineIsDeterministic lang =
      match lang with 
      "" -> ""
        | "en" -> "The automaton is deterministic. "
        | "pt" -> "O autómato é determinista. "

    let defineNotDeterministic lang =
      match lang with 
      "" -> ""
        | "en" -> "The Automaton is not deterministic. "
        | "pt" -> "O Autómato não é determinista. "

    let defineIsMinimal lang =
      match lang with 
      "" -> ""
        | "en" -> "The Automaton is minimal. "
        | "pt" -> "O Autómato é mínimo. "
        
    let defineIsMinimalPDA lang =
      match lang with 
      "" -> ""
        | "en" -> "Pushdown Automatons are not minimizable. "
        | "pt" -> "Os autómatos de pilha não são minimizáveis. "

    let defineNotMinimal lang =
      match lang with 
      "" -> ""
        | "en" -> "The automaton is not minimal. "
        | "pt" -> "O autómato não é mínimo. "
    
    let defineNotUseless lang =
      match lang with 
      "" -> ""
        | "en" -> "The automaton has no useless states. "
        | "pt" -> "O autómato não tem estados inúteis. "

    let defineHas lang =
      match lang with 
      "" -> ""
        | "en" -> "The automaton has "
        | "pt" -> "O autómato tem "
    
    let defineUselessStates lang =
      match lang with 
      "" -> ""
        | "en" -> " useless states. "
        | "pt" -> " estados inúteis. "

    let defineNumberStates lang =
      match lang with 
      "" -> ""
        | "en" -> "Number of states: "
        | "pt" -> "Número de Estados: "
    
    let defineNumberTransitions lang =
      match lang with 
      "" -> ""
        | "en" -> "Number of transitions: "
        | "pt" -> "Número de Transições: "

    let defineDirection lang =
      match lang with 
      "" -> ""
        | "en" -> "Change tree direction"
        | "pt" -> "Mudar direção da árvore"

    let defineVerify lang =
      match lang with 
      "" -> ""
        | "en" -> "Verify"
        | "pt" -> "Verificar"

    let defineNonAccepted lang =
      match lang with 
      "" -> ""
        | "en" -> "Words that should not be accepted: "
        | "pt" -> "Palavras que não devem ser aceites: "

    let defineAcceptedWords lang =
      match lang with 
      "" -> ""
        | "en" -> "Words that should be accepted: "
        | "pt" -> "Palavras que devem ser aceites: "

    let defineProblem lang =
      match lang with 
      "" -> ""
        | "en" -> "Problem: "
        | "pt" -> "Problema: "

    let defineEnumTitle lang =
      match lang with 
      "" -> ""
        | "en" -> "Exercise"
        | "pt" -> "Exercício"
    
    let defineRight lang =
      match lang with 
      "" -> ""
        | "en" -> "Correct answer"
        | "pt" -> "Resposta correcta"

    let defineWrong lang =
      match lang with 
      "" -> ""
        | "en" -> "Wrong answer"
        | "pt" -> "Resposta errada"

    let defineGenerateWords lang =
      match lang with 
      "" -> ""
        | "en" -> "Generated Words:"
        | "pt" -> "Palavras Geradas:"

    let defineWordAccepted lang =
      match lang with 
      "" -> ""
        | "en" -> "The word is accepted"
        | "pt" -> "A palavra é aceite"

    let defineWordNotAccepted lang =
      match lang with 
      "" -> ""
        | "en" -> "The word is not accepted"
        | "pt" -> "A palavra não é aceite"

    let defineExists lang =
      match lang with 
      "" -> ""
        | "en" -> "There are "
        | "pt" -> "Existem "
    
    let defineGoodDerivations lang =
      match lang with 
      "" -> ""
        | "en" -> " successful derivations: "
        | "pt" -> " derivações bem sucedidas: "

    let defineBadDerivations lang =
      match lang with 
      "" -> ""
        | "en" -> " failed derivations: "
        | "pt" -> " derivações falhadas: "

    let defineBy lang =
      match lang with 
      "" -> ""
        | "en" -> " by "
        | "pt" -> " por "
    
    let defineNext lang =
      match lang with 
      "" -> ""
        | "en" -> "Next"
        | "pt" -> "Próxima" 

    let definePrevious lang =
      match lang with 
      "" -> ""
        | "en" -> "Previous"
        | "pt" -> "Anterior"

    let defineAlertMinimum lang =
      match lang with 
      "" -> ""
        | "en" -> "The automaton is already minimal"
        | "pt" -> "O Autómato já é mínimo"

    let defineAlertNeedsDeterministic lang =
      match lang with 
      "" -> ""
        | "en" -> "The Automaton must be deterministic in order to be minimized"
        | "pt" -> "O Autómato tem de ser determinista para poder ser minimizado"
    
    let defineAlertDelete lang =
      match lang with 
      "" -> ""
        | "en" -> "It is not possible to delete the initial state, change the initial state to another one and then delete the desired one!"
        | "pt" -> "Não é possível eliminar estado inicial, troque o estado inicial para outro e depois elimine o desejado!"

    let defineAlertUnexistentState lang =
      match lang with 
      "" -> ""
        | "en" -> "The indicated state does not exist!"
        | "pt" -> "O estado indicado não existe!"

    let defineAlertInitialFirst lang =
      match lang with 
      "" -> ""
        | "en" -> "It is necessary to create an initial state first"
        | "pt" -> "É necessário criar primeiro um estado inicial"

    let defineAlertTheTransition lang =
      match lang with 
      "" -> ""
        | "en" -> "The transition "
        | "pt" -> "A transição "

    let defineAlertAlreadyExists lang =
      match lang with 
      "" -> ""
        | "en" -> " already exists!"
        | "pt" -> " já existe!"

    let defineAlertDoNotExists lang =
      match lang with 
      "" -> ""
        | "en" -> " doesn't exists!"
        | "pt" -> " não existe!"

    let defineAlertStartState lang =
      match lang with 
      "" -> ""
        | "en" -> "The starting state does not exist!"
        | "pt" -> "O estado de partida não existe!"

    let defineAlertArrivalState lang =
      match lang with 
      "" -> ""
        | "en" -> "The state of arrival does not exist!"
        | "pt" -> "O estado de chegada não existe!"
    
    let defineAlertAlreadyFinal lang =
      match lang with 
        "" -> ""
        | "en" -> "The state is already final"
        | "pt" -> "O estado já é final"


    let defineAlertWorkingWithAutomata lang = 
      match lang with 
        "" -> ""
        | "en" -> "You can't add state because you are not working with an automaton"
        | "pt" -> "Não pode adicionar estado porque não está a trabalhar com um autómato"

    let defineAlertDeterministic lang =
      match lang with 
      "" -> ""
        | "en" -> "The automaton is already deterministic"
        | "pt" -> "O Autómato já é determinista"

    let defineAlertClean lang =
      match lang with 
      "" -> ""
        | "en" -> "The automaton has no states to clean, there are no useless states!"
        | "pt" -> "O Autómato não tem estados para limpar, não existem estados inúteis!"

    let defineAlertRegex lang =
      match lang with 
      "" -> ""
        | "en" -> "You are already working with a regular expression"
        | "pt" -> "Já está a trabalhar com uma expressão regular"

    let defineAlertAutomaton lang =
      match lang with 
      "" -> ""
        | "en" -> "You are already working with a finite automaton"
        | "pt" -> "Já está a trabalhar com um autómato finito"
    
    let defineAlertExists lang =
      match lang with 
      "" -> ""
        | "en" -> "The state already exists"
        | "pt" -> "O estado já existe"

    let defineAlertAddStateTransitions lang =
      match lang with 
      "" -> ""
        | "en" -> "Add states and transitions in the text box"
        | "pt" -> "Adicione os estados e transições na caixa de texto"

    let defineAlertFormat lang =
      match lang with 
      "" -> ""
        | "en" -> "The format of a transition is: State Transition State"
        | "pt" -> "O formato de uma transição é: Estado Transição Estado"

    let defineAlertName lang =
      match lang with 
      "" -> ""
        | "en" -> "You must name the state in the text box"
        | "pt" -> "Tem de dar um nome ao estado na caixa de texto"

    let defineAlertNoTransitions lang =
      match lang with 
      "" -> ""
        | "en" -> "There is no transition with the symbol given!"
        | "pt" -> "Não há transições com o simbolo indicado!"

    let defineAlertNoMoreStates lang =
      match lang with 
      "" -> ""
        | "en" -> "The word is over. There are no more states to follow."
        | "pt" -> "A palavra terminou. Não existem mais estados seguintes."

    let defineAlertArrivedInitial lang =
      match lang with 
      "" -> ""
        | "en" -> "You are in the initial state. It is not possible to go backwards from this point"
        | "pt" -> "Não é possível andar para trás do estado inicial"

    let defineFeedbackTitle lang =
      match lang with 
      "" -> ""
        | "en" -> "Feedback"
        | "pt" -> "Feedback"

    let defineFeedbackText lang =
      match lang with 
      "" -> ""
        | "en" -> "In order to create an increasingly better page, we appreciate any kind of feedback."
        | "pt" -> "De forma a criarmos uma página cada vez melhor agradecemos qualquer tipo de feedback."

    let defineFeedbackText2 lang =
      match lang with 
      "" -> ""
        | "en" -> "If you have any questions or want to give an opinion, please send an email to "
        | "pt" -> "Se tiver alguma dúvida ou queira dar opinião, envie, por favor, um email para "

    let defineFeedbackThankYou lang =
      match lang with 
      "" -> ""
        | "en" -> "Thank You!"
        | "pt" -> "Obrigada!"

    let defineAboutTitle lang =
      match lang with 
      "" -> ""
        | "en" -> "About"
        | "pt" -> "Sobre"

    let defineAboutSubtitle lang =
      match lang with 
      "" -> ""
        | "en" -> "About OFLAT"
        | "pt" -> "Sobre OFLAT"

    let defineAboutSubtitle2 lang =
      match lang with 
      "" -> ""
        | "en" -> "Instructions"
        | "pt" -> "Instruções"

    let defineAboutText1 lang =
      match lang with 
      "" -> ""
        | "en" -> "This tool is being developed at "
        | "pt" -> "Esta ferramenta está a ser desenvolvida na "
    
    let defineAboutText2 lang =
      match lang with 
      "" -> ""
        | "en" -> " (at the Computer Science Department of FCT-UNL) by the projects "
        | "pt" -> " (no Departamento de Informática da FCT- UNL) pelo projecto "

    let defineAboutText16 lang =
      match lang with 
      "" -> ""
      | "en" -> " and "
      | "pt" -> " and "

    let defineAboutText17 lang =
      match lang with 
      "" -> ""
        | "en" -> " LEAFS"
        | "pt" -> " LEAFS"
    
    let defineAboutText3 lang =
      match lang with 
      "" -> ""
        | "en" -> " and co-financed by "
        | "pt" -> " e Co-financiado pela "

    let defineAboutText4 lang =
      match lang with 
      "" -> ""
        | "en" -> "It is being developed in OCaml through the Ocsigen framework, the source code being available in "
        | "pt" -> "It is being developed in OCaml through the Ocsigen framework, the source code being available in "
    
    let defineAboutText5 lang =
      match lang with 
      "" -> ""
        | "en" -> "At this moment it is possible to work with finite automata, regular expressions and perform exercises."
        | "pt" -> "Neste momento é possível trabalhar com autómatos finitos, expressões regulares e realizar exercícios."
    
    let defineAboutText6 lang =
      match lang with 
      "" -> ""
        | "en" -> "To import a file it must be in txt or JSON. The format of an automaton is:"
        | "pt" -> "Para se importar um ficheiro este deve estar em txt ou JSON. O formato de um autómato é:"

    let defineAboutText7 lang =
      match lang with 
      "" -> ""
        | "en" -> "
                    {
                      kind: finite automaton,
                      description: description of the automaton,
                      name: name of the automaton,
                      alphabet: [list of elements],
                      states: [list of states],
                      initialState: an initial state,
                      transitions: [List of transitions in the format [departure status, alphabet element, arrival status]],
                      acceptStates: [list of states]
                    } "     
        | "pt" -> "
                    {
	                    kind : finite automaton,
	                    description : descrição do autómato,
	                    name : nome do autómato,
	                    alphabet : [lista de elementos],
	                    states : [lista de estados],
	                    initialState : um estado inicial,
                      transitions : [Lista de transições com formato [estado de partida, elemento do alfabeto, estado de chegada]],
                      acceptStates : [lista de estados]
                    }"
    
    let defineAboutText8 lang =
      match lang with 
      "" -> ""
        | "en" -> "The format of a regular expression is:"
        | "pt" -> "O formato de uma expressão regular é:"

    let defineAboutText9 lang =
      match lang with 
      "" -> ""
        | "en" -> "
              {
                kind: regular expression,
                description: description of the regular expression,
                name: name of the regular expression,
                re: regular expression
              } "   
        | "pt" ->  "
              {
                kind: regular expression,
                description: descrição da expressão regular,
                name: nome da expressão regular,
                re: expressão regular
              } " 

    let defineAboutText10 lang =
      match lang with 
      "" -> ""
        | "en" -> "The format of an exercise is:"
        | "pt" -> "O formato de um exercício é:"

    let defineAboutText11 lang =
      match lang with 
      "" -> ""
        | "en" -> "
                {
                  kind: enumeration,
                  description: description of the automaton,
                  name: exercise name,
                  problem: exercise presented to the student,
                  inside: [list of words to be accepted],
                  outside: [list of words that should not be accepted]
                } "
        | "pt" -> "
                {
	                kind : enumeration,
	                description : descrição do autómato,
	                name : nome do exercício,
	                problem : exercício apresentado ao aluno,
	                inside : [lista de palavras que devem ser aceites],
	                outside : [lista de palavras que não devem ser aceites]
                }"

    let defineAboutText12 lang =
      match lang with 
      "" -> ""
        | "en" -> "All elements on the right side of the colon must be enclosed in quotation marks."
        | "pt" -> "Todos os elementos do lado direito dos dois pontos devem ser colocados entre aspas."
    
    let defineAboutText13 lang =
      match lang with 
      "" -> ""
        | "en" -> "It is also possible to load predefined examples from the server."
        | "pt" -> "É possivel também carregar exemplos pré-definidos do servidor."
    
    let defineAboutText14 lang =
      match lang with 
      "" -> ""
        | "en" -> "The regular expression can also be created by typing it in the text box and the automata can be created step by step using the State and Transition select boxes. To create or edit a state it is necessary to indicate its name in the text box and to create or edit a transition put in the text box start status, alphabet element, arrival status (example: A a B). "
        | "pt" -> "A expressão regular pode também ser criada escrevendo a mesma na caixa de texto e os autómatos pode ser criados passo a passo usando as caixas de seleção Estado e Transição. Para se criar ou editar um estado é necessário indicar o nome do mesmo na caixa de texto e para se criar ou editar uma transição coloca-se na caixa de texto estado de partida, elemento do alfabeto, estado de chegada (exemplo: A a B)."

    let defineAboutText15 lang =
      match lang with 
      "" -> ""
        | "en" -> "In the automata the epsilon transition is represented by ~."
        | "pt" -> "Nos autómatos a transição épsilon é representada por ~."

    (** ---------------- Antigos (fora de uso) ----------------- **)

    let defineInputTitle lang =
      match lang with 
      "" -> ""
        | "en" -> "Input"
        | "pt" -> "Input"

    let defineSelectState lang =
      match lang with 
      "" -> ""
        | "en" -> "State"
        | "pt" -> "Estado"

    let defineSelectAdd lang =
      match lang with 
      "" -> ""
        | "en" -> "Add"
        | "pt" -> "Adicionar"

    let defineSelectInitial lang =
      match lang with 
      "" -> ""
        | "en" -> "Initial"
        | "pt" -> "Inicial"
    
    let defineSelectFinal lang =
      match lang with 
      "" -> ""
        | "en" -> "Final"
        | "pt" -> "Final"

    let defineSelectErase lang =
      match lang with 
      "" -> ""
        | "en" -> "Erase"
        | "pt" -> "Apagar"

    let defineSelectTransition lang =
      match lang with 
      "" -> ""
        | "en" -> "Transition"
        | "pt" -> "Transição"

    let tooltipDefine lang =
      match lang with 
      "" -> ""
      | "en" -> "Allows to create a regular expression by inputting the corresponding string"
      | "pt" -> "Permite criar uma expressão regular através da colocação do texto que a define"
      
    let tooltipStartGraph lang =
      match lang with 
      "" -> ""
      | "en" -> "Allows to start a new automaton to create it step-by-step"
      | "pt" -> "Permite começar um novo autómato de forma a criá-lo passo-a-passo"

    let tooltipFitGraph lang =
        match lang with 
        "" -> ""
        | "en" -> "Fits the automata or tree to the box"
        | "pt" -> "Ajusta o automato ou árvore à caixa"

    let tooltipGenerate lang =
      match lang with 
      "" -> ""
      | "en" -> "Gives a list of words, of size smaller or equal a given size, accepted by the automaton"
      | "pt" -> "Dá uma lista de palavras aceites, até um determinado tamanho dado, pelo autómato"

    let tooltipComplete lang =
      match lang with 
      "" -> ""
      | "en" -> "Shows, with an animation, how the automaton accepts or rejects a given word; or shows, with a tree, the acceptance process of a regular expression; the input is asked through a box on the screen"
      | "pt" -> "Mostra-se, com uma animação, como o autómato aceita ou rejeita uma dada palavra; ou mostra, com uma árvore, o processo de aceitação de uma expressão regular; a palavra é solicitada através de uma caixa no ecrã"

    let tooltipStep lang =
        match lang with 
        "" -> ""
        | "en" -> "Allows the user to see the acceptance process step-by-step; the input is asked through a box on the screen"
        | "pt" -> "Permite que o utilizador veja o processo de aceitação passo a passo; a palavra é solicitada através de uma caixa no ecrã"

    let tooltipClear lang =
      match lang with 
      "" -> ""
      | "en" -> "Removes all colors of the automaton"
      | "pt" -> "Limpa as cores do autómato"
    
    let tooltipConvert lang =
      match lang with 
      "" -> ""
      | "en" -> "In a new box converts the automaton in a regular expression or a regular expression in an automaton"
      | "pt" -> "Numa nova caixa converte um autómato em expressão regular e uma expressão regular num autómato"

    let tooltipFile lang =
      match lang with 
      "" -> ""
      | "en" -> "Allows the user to import a file from filesystem. Allowed formats txt or JSON"
      | "pt" -> "Permite ao utilizador importar um ficheiro do sistema de ficheiros. Formatos permitidos txt ou JSON"

    let tooltipAbout lang =
          match lang with 
          "" -> ""
          | "en" -> "More about the page and some instructions"
          | "pt" -> "Mais sobre a página e algumas instruções"
      
    let tooltipFeedback lang =
            match lang with 
            "" -> ""
            | "en" -> "Information page if you want to give feedback"
            | "pt" -> "Página sobre como dar feedback"

    let tooltipLang lang =
      match lang with 
        "" -> ""
        | "en" -> "The user can choose the language of the page"
        | "pt" -> "Permite ao utilizador escolher o idioma da página"

    let tooltipCloseLeft lang =
      match lang with 
        "" -> ""
        | "en" -> "If there is a second figure on the screen, closes the left and keeps the right one; otherwise cleans the screen"
        | "pt" -> "Se existir uma segunda figura no ecrã, fecha a da esquerda e mantém a da direita; senão limpa tudo"

    let tooltipCloseRight lang =
      match lang with 
        "" -> ""
        | "en" -> "Closes the right box on the screen"
        | "pt" -> "Fecha a caixa direita"
        
    let tooltipDirection lang =
      match lang with 
        "" -> ""
        | "en" -> "If the tree is horizontal becomes vertical and vice-versa"
        | "pt" -> "Se a árvore é horizontal torna-se vertical e vice-versa"
          
    let tooltipVerify lang =
      match lang with 
        "" -> ""
        | "en" -> "Verifies if the answer is correct or not, and if it is not, says where it fails"
        | "pt" -> "Verifica se a resposta está correcta ou errada; se estiver errada diz onde falha"

    let tooltipClean lang =
      match lang with 
        "" -> ""
        | "en" -> "Creates a new automaton removing all useless states"
        | "pt" -> "Cria um novo autómato removendo os estados inúteis"

    let tooltipDeterministic lang =
      match lang with 
        "" -> ""
        | "en" -> "Creates on a new box a deterministic version of the automaton"
        | "pt" -> "Cria numa nova caixa uma versão determinística do autómato"

    let tooltipMinimize lang =
      match lang with 
        "" -> ""
        | "en" -> "Creates on a new box a minimized version of the automaton"
        | "pt" -> "Cria numa nova caixa uma versão minimizada do autómato"

    let tooltipProductive lang =
      match lang with 
        "" -> ""
        | "en" -> "Paints all the productive states of the automaton"
        | "pt" -> "Pinta todos os estados produtivos do autómato"

    let tooltipAccessible lang =
      match lang with 
        "" -> ""
        | "en" -> "Paints all the accessible states of the automaton"
        | "pt" -> "Pinta todos os estados acessíveis do autómato"

    let tooltipUseful lang =
      match lang with 
        "" -> ""
        | "en" -> "Paints all the useful states of the automaton"
        | "pt" -> "Pinta todos os estados úteis do autómato"

    let tooltipSpecification lang =
      match lang with 
        "" -> ""
        | "en" -> "Show the specification of the automaton in a new box"
        | "pt" -> "Mostra a especificação do autómato numa nova caixa"
 
  end
