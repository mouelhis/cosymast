# CoSyMA: A Tool for Controller Synthesis Using Multi-scale Abstractions

CoSyMA is a tool for automatic controller synthesis for incrementally stable switched systems based on multi-scale discrete abstractions. The tool ensures significant performance gains during the controller synthesis.

## Input

A description of a switched system represented by a set of differential equations, the sampling parameters used to define an approximation of the state-space on which discrete abstractions are computed.

## Output

A controller -- if it exists -- for the system that enforces a given safety or time-bounded reachability specification. 

## Installation process

### Requirements

1. O'Caml version 3.12.1 or higher.
2. Native code compilers of the OCaml suite (packages ocaml-native-compilers)
3. Extlib library 1.5.2 or higher (http://code.google.com/p/ocaml-extlib/downloads/list)
4. LaTeX and PGF/TikZ packages for producing vector graphics.
 
### Compilation 

To compile the project, make sure that Makefile is set correctly depending on your preferences:

1. 
```
all: native-code           # native code compilation
all: debug-code            # debugging
all: byte-code             # byte code compilation
all: profiling-native-code # profiling
```
2. The variable `RESULT` is set to the project executable: `RESULT = cosyma`
3. The variable PACKS is set to the list of external packages: `PACKS = extlib`
4. The variable LIBS is set to standard libraries: `LIBS = str unix`
5. The variable TRASH is set to clean the project: `TRASH = *~ #*# *.cmo *.cmi *.mli *.o *.cmx ...` 
 
 Compilation: `make` 
 
 Clean: `make clean`
