# CoSyMA: A Tool for Controller Synthesis Using Multi-scale Abstractions

[CoSyMA](http://multiscale-dcs.gforge.inria.fr/) is a tool for automatic controller synthesis for incrementally stable switched systems based on multi-scale discrete abstractions. The tool ensures significant performance gains during the controller synthesis.

## Input

A description of a switched system represented by a set of differential equations, the sampling parameters used to define an approximation of the state-space on which discrete abstractions are computed.

## Output

A controller -- if it exists -- for the system that enforces a given safety or time-bounded reachability specification. 

## Documentation

Consult the tool's [manual](https://hal.archives-ouvertes.fr/hal-00743982).

## Linux installation/usage process

### Requirements

1. OCaml version 3.12.1 or higher
2. The FindLib OCaml library 1.7.3 or higer (package ocaml-findlib)
3. Extlib library 1.5.2 or higher (packages libextlib-ocaml and libextlib-ocaml-dev)
4. LaTeX & PGF/TikZ packages
 
### Compilation 

To compile the project, make sure that `Makefile` is set correctly depending on your preferences:

1. Compilation options:
```
all: native-code           # native code compilation
all: debug-code            # debugging
all: byte-code             # byte code compilation
all: profiling-native-code # profiling
```
2. Variable `RESULT` is set to the project executable: `RESULT = cosyma`
3. Variable `PACKS` is set to the list of external packages: `PACKS = extlib`
4. Variable `LIBS̀̀` is set to standard libraries: `LIBS = str unix`
5. Variable `TRASH` is set to clean the project: `TRASH = *~ *.cmo *.cmi *.mli *.o *.cmx ...` 
 
Compilation: `make` 
 
Clean: `make clean`
 
### Execution

1. `<path>/cosyma parse <file>.conf` (some examples are provided)
2. `<path>/cosyma synthesis` 
  
The script `run` launches both of them by passing `<file>.conf` as argument.
 
### Plot  

A file `plot.tikz` (representation of the controller abstraction) is generated after the synthesis process. 

Run `<path>/tikz2pdf plot.tikz` to generate the PDF plot (this may take some time if the abstraction is big).

## Winodws/Mac process 

Not provided.
