all:
	ocamlbuild -use-ocamlfind torrent_parser.native

clean: 
	ocamlbuild -clean

.PHONY: all clean
