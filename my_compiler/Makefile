%:

	echo '(load "project/compiler.scm") (compile-scheme-file "$(MAKECMDGOALS).scm" "$(MAKECMDGOALS).s")' | scheme -q
	nasm -f elf64 $(MAKECMDGOALS).s -o $(MAKECMDGOALS).o
	gcc -m64 -Wall -o $(MAKECMDGOALS) $(MAKECMDGOALS).o
