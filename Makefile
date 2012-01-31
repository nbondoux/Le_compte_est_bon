all2: Le\ compte\ est\ bon Le\ compte\ est\ bon2 Le\ compte\ est\ bon2_2 Le\ compte\ est\ bon3_byte Le\ compte\ est\ bon4 Le\ compte\ est\ bon4_2 Le\ compte\ est\ bon4_3 Le\ compte\ est\ bon4_4 Le\ compte\ est\ bon4_4_c Le\ compte\ est\ bon4_4_c_rapide Le\ compte\ est\ bon4_4_hs Le\ compte\ est\ bon2_2_hs Le\ compte\ est\ bon2_2_2_hs Le\ compte\ est\ bon4_4_hsIO Le\ compte\ est\ bon4_4_2_hs Le\ compte\ est\ bon4_4_3_hs Le\ compte\ est\ bon4_4_c_rapide_2 Le_compte_est_bon4_5 Le_compte_est_bon5 Le_compte_est_bon5_2 Le_compte_est_bon5_ml Le_compte_est_bon5_hs Le_compte_est_bon5_2_hs
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

Le_compte_est_bon5_ml:Le_compte_est_bon5.ml
	ocamlopt Le_compte_est_bon5.ml -o ./Le_compte_est_bon5_ml

Le\ compte\ est\ bon4_4_c:Le\ compte\ est\ bon4_4.c
	gcc -O3 -march=native -Wall -o  Le\ compte\ est\ bon4_4_c  Le\ compte\ est\ bon4_4.c

Le\ compte\ est\ bon4_4_c_rapide:Le\ compte\ est\ bon4_4_rapide.c
	gcc -O3 -march=native -Wall -o Le\ compte\ est\ bon4_4_c_rapide Le\ compte\ est\ bon4_4_rapide.c
Le\ compte\ est\ bon4_4_c_rapide_2:Le\ compte\ est\ bon4_4_rapide_2.c
	gcc -std=c99 -O3 -march=native -Wall -o Le\ compte\ est\ bon4_4_c_rapide_2 Le\ compte\ est\ bon4_4_rapide_2.c
Le_compte_est_bon4_5:Le_compte_est_bon4_5.c
	gcc -std=c99 -O3 -march=native -Wall -o Le_compte_est_bon4_5 Le_compte_est_bon4_5.c
Le_compte_est_bon5:Le_compte_est_bon5.c
	gcc -std=gnu99 -O3 -march=native -Wall -o Le_compte_est_bon5 Le_compte_est_bon5.c
Le_compte_est_bon5_2:Le_compte_est_bon5_2.c
	gcc -std=c99 -O3 -march=native -Wall -o Le_compte_est_bon5_2 Le_compte_est_bon5_2.c


Le\ compte\ est\ bon4_4_gmp:Le\ compte\ est\ bon4_4_gmp.ml
	ocamlfind ocamlopt -package gmp -linkpkg Le\ compte\ est\ bon4_4_gmp.ml -o Le\ compte\ est\ bon4_4_gmp

Le\ compte\ est\ bon2_2_hs:Le\ compte\ est\ bon2_2.hs
	ghc Le\ compte\ est\ bon2_2.hs -O9 -o Le\ compte\ est\ bon2_2_hs

Le\ compte\ est\ bon2_2_2_hs:Le\ compte\ est\ bon2_2_2.hs
	ghc Le\ compte\ est\ bon2_2_2.hs -O9 -o Le\ compte\ est\ bon2_2_2_hs

Le\ compte\ est\ bon4_4_hs:Le\ compte\ est\ bon4_4.hs
	ghc Le\ compte\ est\ bon4_4.hs -O9 -o Le\ compte\ est\ bon4_4_hs

Le\ compte\ est\ bon4_4_2_hs:Le\ compte\ est\ bon4_4_2.hs
	ghc Le\ compte\ est\ bon4_4_2.hs -O9 -o Le\ compte\ est\ bon4_4_2_hs

Le\ compte\ est\ bon4_4_3_hs:Le\ compte\ est\ bon4_4_3.hs
	ghc Le\ compte\ est\ bon4_4_3.hs -O9 -package mtl -o Le\ compte\ est\ bon4_4_3_hs

Le\ compte\ est\ bon4_4_hsIO:Le\ compte\ est\ bon4_4IO.hs
	ghc Le\ compte\ est\ bon4_4IO.hs -O9 -o Le\ compte\ est\ bon4_4_hsIO

Le_compte_est_bon5_hs:Le_compte_est_bon5.hs
	ghc Le_compte_est_bon5.hs -O9 -o Le_compte_est_bon5_hs

Le_compte_est_bon5_2_hs:Le_compte_est_bon5_2.hs
	ghc Le_compte_est_bon5_2.hs -O9 -o Le_compte_est_bon5_2_hs

clean:
	rm -f *.cm* *.o *.hi 
cleanall:
	rm -f *.cm* *.o *.hi Le\ compte\ est\ bon Le\ compte\ est\ bon2 Le\ compte\ est\ bon2_2 Le\ compte\ est\ bon3_byte Le\ compte\ est\ bon4 Le\ compte\ est\ bon4_2 Le\ compte\ est\ bon4_3 Le\ compte\ est\ bon4_4 Le\ compte\ est\ bon4_4_c Le\ compte\ est\ bon4_4_c_rapide Le\ compte\ est\ bon4_4_hs Le\ compte\ est\ bon2_2_hs Le\ compte\ est\ bon2_2_2_hs Le\ compte\ est\ bon4_4_hsIO Le\ compte\ est\ bon4_4_2_hs Le\ compte\ est\ bon4_4_3_hs Le\ compte\ est\ bon4_4_c_rapide_2 Le\ compte\ est\ bon_2 Le\ compte\ est\ bon4_4_gmp Le_compte_est_bon4_5 Le_compte_est_bon5 Le_compte_est_bon5_2 Le_compte_est_bon5_ml Le_compte_est_bon5_hs Le_compte_est_bon5_2_hs
