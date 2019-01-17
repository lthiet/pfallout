EXE = trpg.byte
SRC = asset/src
OCB_FLAG = -I $(SRC) -tag debug -use-ocamlfind -package tsdl,tsdl_mixer,tsdl_image,tsdl_ttf
DOC = docs
DIAGRAM = $(DOC)/diagrams/src/classes.wsd
PLANTUML = $(DOC)/diagrams/src/plantuml.jar

all: 
	ocamlbuild $(OCB_FLAG) $(EXE)

run:
	make all
	./$(EXE)

uml: $(DIAGRAM)
	java -jar $(PLANTUML) $(DIAGRAM)

debug:
	make all
	ocamldebug $(EXE)

clean:
	ocamlbuild -clean
	rm -rf $(DOC)/diagrams/out/*