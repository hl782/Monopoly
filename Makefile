# Makefile instructions: "make gui" creates an executable file named
# "GUI", and runs the file. In order to run "make gui", you need to 
# install gtk with a command analogous to "sudo apt-get install gtk2.0",
# and install lablgtk with a command analogous to "opam install lablgtk".

gui:
	ocamlbuild -use-ocamlfind GUI.byte && ./GUI.byte

play:
	ocamlbuild -use-ocamlfind main.byte && ./main.byte

test:
	ocamlbuild -use-ocamlfind state_test.byte && ./state_test.byte

clean:
	ocamlbuild -clean
