OBJS = ast.cmo parser.cmo scanner.cmo compile.cmo frac.cmo

YACC = menhir --explain

frac: $(OBJS)
	ocamlc -o frac $(OBJS)

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
	rm -f frac parser.ml parser.mli scanner.ml testall.log \
	*.cmo *.cmi *.out *.diff

# Generated by ocamldep *.ml *.mli
ast.cmo: 
ast.cmx: 
parser.cmo: ast.cmo parser.cmi 
parser.cmx: ast.cmx parser.cmi 
scanner.cmo: parser.cmi 
scanner.cmx: parser.cmx 
parser.cmi: ast.cmo 
