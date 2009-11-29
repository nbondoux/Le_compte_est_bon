all2: Le\ compte\ est\ bon Le\ compte\ est\ bon2 Le\ compte\ est\ bon2_2 Le\ compte\ est\ bon3_byte Le\ compte\ est\ bon4 Le\ compte\ est\ bon4_2 Le\ compte\ est\ bon4_3 Le\ compte\ est\ bon4_4 Le\ compte\ est\ bon4_4_c Le\ compte\ est\ bon4_4_c_rapide Le\ compte\ est\ bon4_4_hs Le\ compte\ est\ bon2_2_hs Le\ compte\ est\ bon2_2_2_hs Le\ compte\ est\ bon4_4_hsIO Le\ compte\ est\ bon4_4_c_rapide_2
all: all2 Le\ compte\ est\ bon_2 Le\ compte\ est\ bon4_4_gmp


Le\ compte\ est\ bon:Le\ compte\ est\ bon.ml
	ocamlopt Le\ compte\ est\ bon.ml -o Le\ compte\ est\ bon
Le\ compte\ est\ bon2:Le\ compte\ est\ bon2.ml
	ocamlopt Le\ compte\ est\ bon2.ml -o Le\ compte\ est\ bon2
Le\ compte\ est\ bon2_2:Le\ compte\ est\ bon2_2.ml
	ocamlopt Le\ compte\ est\ bon2_2.ml -o Le\ compte\ est\ bon2_2
Le\ compte\ est\ bon3_byte:Le\ compte\ est\ bon3.ml
	ocamlc Le\ compte\ est\ bon3.ml -o Le\ compte\ est\ bon3_byte
Le\ compte\ est\ bon_2:Le\ compte\ est\ bon_2.ml
	ocamlfind ocamlopt -package extlib -linkpkg Le\ compte\ est\ bon_2.ml -o Le\ compte\ est\ bon_2


Le\ compte\ est\ bon4:Le\ compte\ est\ bon4.ml
	ocamlopt Le\ compte\ est\ bon4.ml -o Le\ compte\ est\ bon4

Le\ compte\ est\ bon4_2:Le\ compte\ est\ bon4_2.ml
	ocamlopt Le\ compte\ est\ bon4_2.ml -o Le\ compte\ est\ bon4_2

Le\ compte\ est\ bon4_3:Le\ compte\ est\ bon4_3.ml
	ocamlopt Le\ compte\ est\ bon4_3.ml -o Le\ compte\ est\ bon4_3

Le\ compte\ est\ bon4_4:Le\ compte\ est\ bon4_4.ml
	ocamlopt Le\ compte\ est\ bon4_4.ml -o Le\ compte\ est\ bon4_4

Le\ compte\ est\ bon4_4_c:Le\ compte\ est\ bon4_4.c
	gcc -O3 -Wall -o  Le\ compte\ est\ bon4_4_c  Le\ compte\ est\ bon4_4.c

Le\ compte\ est\ bon4_4_c_rapide:Le\ compte\ est\ bon4_4_rapide.c
	gcc -O3 -Wall -o Le\ compte\ est\ bon4_4_c_rapide Le\ compte\ est\ bon4_4_rapide.c
Le\ compte\ est\ bon4_4_c_rapide_2:Le\ compte\ est\ bon4_4_rapide_2.c
	gcc -O3 -Wall -o Le\ compte\ est\ bon4_4_c_rapide_2 Le\ compte\ est\ bon4_4_rapide_2.c

Le\ compte\ est\ bon4_4_gmp:Le\ compte\ est\ bon4_4_gmp.ml
	ocamlfind ocamlopt -package gmp -linkpkg Le\ compte\ est\ bon4_4_gmp.ml -o Le\ compte\ est\ bon4_4_gmp

Le\ compte\ est\ bon2_2_hs:Le\ compte\ est\ bon2_2.hs
	ghc Le\ compte\ est\ bon2_2.hs -O9 -o Le\ compte\ est\ bon2_2_hs

Le\ compte\ est\ bon2_2_2_hs:Le\ compte\ est\ bon2_2_2.hs
	ghc Le\ compte\ est\ bon2_2_2.hs -O9 -o Le\ compte\ est\ bon2_2_2_hs

Le\ compte\ est\ bon4_4_hs:Le\ compte\ est\ bon4_4.hs
	ghc Le\ compte\ est\ bon4_4.hs -O9 -o Le\ compte\ est\ bon4_4_hs

Le\ compte\ est\ bon4_4_hsIO:Le\ compte\ est\ bon4_4IO.hs
	ghc Le\ compte\ est\ bon4_4IO.hs -O9 -o Le\ compte\ est\ bon4_4_hsIO

clean:
	rm -f *.cm* *.o *.hi 
cleanall:
	rm -f *.cm* *.o *.hi Le\ compte\ est\ bon Le\ compte\ est\ bon2 Le\ compte\ est\ bon2_2 Le\ compte\ est\ bon3_byte Le\ compte\ est\ bon_2 Le\ compte\ est\ bon4 Le\ compte\ est\ bon4_2 Le\ compte\ est\ bon4_3 Le\ compte\ est\ bon4_4  Le\ compte\ est\ bon4_4_c Le\ compte\ est\ bon4_4_hs Le\ compte\ est\ bon4_4_hsIO
