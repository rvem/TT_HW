ocamllex lexer.mll 
ocamlyacc parser.mly
ocamlc -o Hw1 lambda.ml parser.mli parser.ml lexer.ml hw1.mli hw1.ml
ocamlc -o Hw1reduction lambda.ml hw1_reduction.mli hw1_reduction.ml
rm *.cmo *.cmi lexer.ml parser.ml parser.mli
