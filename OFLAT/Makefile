APP = OFLAT
VERSION = 1.3
COMP = ocamlc
SRCDIR = src
APPDIR = $(APP)
OUTFILE = $(APP)
FLAGS = -w -8
PACKAGES = js_of_ocaml,js_of_ocaml-ppx,js_of_ocaml-lwt,js_of_ocaml-tyxml
OCAMLFLATLIB = OCamlFlat
OFLATFILES = 	Lang.ml JS.ml Graphics.ml FiniteAutomatonGraphics.ml PushdownAutomatonGraphics.ml \
				RegularExpressionGraphics.ml Listeners.ml				\
				StateVariables.ml HtmlPageClient.ml Controller.ml 	\
				Calling.ml Start.ml

OS = $(shell uname -s)
ifeq ($(OS),Linux)
	OPEN = "x-www-browser"
else
	OPEN = "open"
endif

$(APPDIR)/$(OUTFILE).js: $(SRCDIR)/OCamlFlat.cmo $(addprefix src/, $(OFLATFILES))
	cd $(SRCDIR);															\
	ocamlfind $(COMP) $(FLAGS) -I $(SRCDIR) -package $(PACKAGES) -linkpkg	\
			-o $(OUTFILE).byte $(OCAMLFLATLIB).cmo $(OFLATFILES)
	js_of_ocaml $(APPDIR)/GraphLibrary.js $(SRCDIR)/$(OUTFILE).byte -o $(APPDIR)/$(OUTFILE).js

$(SRCDIR)/$(OCAMLFLATLIB).cmo: $(SRCDIR)/$(OCAMLFLATLIB).ml
	$(COMP) $(FLAGS) -c $(SRCDIR)/$(OCAMLFLATLIB).ml

.PHONY: run
run: $(APPDIR)/$(OUTFILE).js
	cd $(APPDIR) ; $(OPEN) index.html

.PHONY: clean
clean:
	cd $(SRCDIR); rm -f *.cmi *.cmo *.byte

.PHONY: cleandist
cleandist: clean
	rm -f $(APPDIR)/$(OUTFILE).js
