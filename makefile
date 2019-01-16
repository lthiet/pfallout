EXE = trpg.byte
SRC = asset/src
OCB_FLAG = -I $(SRC) -tag debug -use-ocamlfind -package tsdl,tsdl_mixer,tsdl_image,tsdl_ttf

all: 
	ocamlbuild $(OCB_FLAG) $(EXE)

run:
	make all
	./$(EXE)

debug:
	make all
	ocamldebug $(EXE)

clean:
	ocamlbuild -clean