EXE = trpg.byte

all:
	ocamlbuild -use-ocamlfind -package tsdl,tsdl_mixer,tsdl_image,tsdl_ttf $(EXE)

run:
	make all
	./$(EXE)

clean:
	ocamlbuild -clean

123