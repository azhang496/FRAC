OBJS = scanner.cmo ast.cmo parser.cmo semantic.cmo compile.cmo frac.cmo
LIBS=str.cma
YACC = ocamlyacc

default: frac

frac: $(OBJS)
	ocamlc -g -o frac $(LIBS) $(OBJS)

scanner.ml: scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	$(YACC) parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

.PHONY : clean
clean :
	rm -f frac parser.ml parser.mli scanner.ml *.cmo *.cmi *.c *.out *.o

# Generated by ocamldep *.ml *.mli
ast.cmo:
ast.cmx:
parser.cmo: ast.cmo parser.cmi
parser.cmx: ast.cmx parser.cmi
semantic.cmo: ast.cmo sast.cmo
semantic.cmx: ast.cmx sast.cmx
sast.cmo: ast.cmo
sast.cmx: ast.cmx
scanner.cmo: parser.cmi
scanner.cmx: parser.cmx
parser.cmi: ast.cmo
