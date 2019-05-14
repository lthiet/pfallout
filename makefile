EXE = trpg.native
SRC = asset/src
OCB_FLAG = -I $(SRC) -r -tag debug -use-ocamlfind -package tsdl,tsdl_mixer,tsdl_image,tsdl_ttf
DOC = docs
DIAGRAM_SRC = $(DOC)/diagrams/src/classes.wsd
DIAGRAM_OUT = $(DOC)/diagrams/src/classes.png
PLANTUML = $(DOC)/diagrams/src/plantuml.jar
UML_FLAG = -DPLANTUML_LIMIT_SIZE=15000 -jar

all: 
	ocamlbuild $(OCB_FLAG) $(EXE)

run:
	make all
	./$(EXE)

uml: $(DIAGRAM_SRC)
	java $(UML_FLAG) $(PLANTUML) $(DIAGRAM_SRC)
	sudo rm -rf /var/www/html/*
	sudo cp $(DIAGRAM_OUT) /var/www/html/

debug:
	make all
	ocamldebug $(EXE)

clean:
	ocamlbuild -clean
	rm -rf $(DOC)/diagrams/src/classes.png
	rm -rf *.aux
	rm -rf *.log
	rm -rf *.out
	rm -rf *.pyg
	rm -rf *.toc
	rm -rf _minted-rapport/
	rm -rf $(EXE)

report:
	pdflatex -shell-escape rapport.tex 