EXE = trpg.byte

all: 
	ocamlbuild -tag debug -use-ocamlfind -package tsdl,tsdl_mixer,tsdl_image,tsdl_ttf $(EXE)

run:
	make all
	./$(EXE)

debug:
	make all
	ocamldebug $(EXE)

clean:
	ocamlbuild -clean