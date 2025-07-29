# ------------------------------------------
# Parser part
#
# uses exported CONDA_ACTIVATE,  CONDA_ENV, and CONDA_ENV_DIR
# ------------------------------------------

SHELL = /bin/bash

ifndef CONDA_ACTIVATE
$(error "No CONDA_ACTIVATE")
endif

ifndef CONDA_ENV
$(error "No CONDA_ENV")
endif

ifndef CONDA_ENV_DIR
$(error "No CONDA_ENV_DIR")
endif


FILES = cnf fof fof_parse fof_lexer features args dtree logic circuit premsel_parse cnfpremsel_parse fcop
FILES_CMX = $(addprefix _build/,$(addsuffix .cmx,$(FILES)))
SOURCES = *.ml *.mll *.mly
PYTHON_INCLUDE = `source $(CONDA_ACTIVATE) &> /dev/null && conda activate "$(CONDA_ENV)" &> /dev/null && python -c "from sysconfig import get_paths as gp; print(gp()['include'])"`
PYTHON_INCLUDE_BASE = `source $(CONDA_ACTIVATE) &> /dev/null && conda activate "$(CONDA_ENV)" &> /dev/null && python -c "from sysconfig import get_paths as gp; import os; print(os.path.basename(gp()['include']))"`
LIB = -L`ocamlopt -where` -lunix -lcamlstr -lasmrun_pic -lm -ldl -L $(CONDA_ENV_DIR)/lib -l$(PYTHON_INCLUDE_BASE)
OBJ = cint.o oo.o graph_indices.o module.o

all: fcoplib.so

fcoplib.so: $(OBJ)
	source $(CONDA_ACTIVATE) && \
	conda activate "$(CONDA_ENV)" && \
	gcc -shared -Wl,-soname,fcoplib -o fcoplib.so -fPIC $(OBJ) $(LIB)

oo.o: $(SOURCES)
	source $(CONDA_ACTIVATE) && \
	conda activate "$(CONDA_ENV)" && \
	ocamlbuild -no-hygiene -libs str,unix,nums fcop.o
	source $(CONDA_ACTIVATE) && \
	conda activate "$(CONDA_ENV)" && \
	ocamlopt -output-obj -o oo.o str.cmxa unix.cmxa $(FILES_CMX)

%.o : %.c
	source $(CONDA_ACTIVATE) && \
	conda activate "$(CONDA_ENV)" && \
	gcc -c -fPIC -I $(PYTHON_INCLUDE) -I $(CONDA_ENV_DIR)/lib/ocaml -o $*.o $<

clean:
	source $(CONDA_ACTIVATE) && \
	conda activate "$(CONDA_ENV)" && \
	ocamlbuild -clean
	rm -f fcoplib.so *o *~ *cmi *cmx
