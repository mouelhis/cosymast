# COSYMA Makefile (based on OCamlMakefile)
# To clean the project, use "make clean"
TRASH = cosyma *~ *.cmo *.cmi *.mli *.o *.cmx *.annot *.dump *.output *.pdf *.tikz
# Source files ordered according their dependencies
SOURCES = toolkit.ml ode.ml symbolic.ml lts.ml plot.ml control.ml safety.ml reachability.ml ocamldec.ml typecheck.ml parser.mly lexer.mll main.ml
# Binary file to run
RESULT = cosyma
# Basic libraries
LIBS = unix str
# Extra packages
PACKS = extlib  
#Generate (in a .output file) a description of the parsing tables and a report on conflicts resulting from ambiguities in the grammar. 
#YFLAGS=-v
# Compilation mode : native-code, byte-code, debug-code, profiling-native-code
all:native-code  
-include OCamlMakefile
