# A Makefile for iProver server.
#
SHELL = /bin/bash

.PHONY: conda_env conda_env_create conda_env_update clean clean_conda clean_parser opam_switch

# Created by ./configure - contains conda vars!
include Makefile.configure

CONDA_ACTIVATE = "$(CONDA_DIR)/bin/activate"
CONDA_ENV_DIR = "$(CONDA_DIR)/envs/$(CONDA_ENV)"
CONDA_ENV_DIR_PYTHON = "$(CONDA_ENV_DIR)/bin/python" 
CONDA_ENGINE = $(shell if [ -f "$(CONDA_DIR)/bin/mamba" ]; then echo "mamba" ;	else echo "conda" ; fi)


export CONDA_ACTIVATE
export CONDA_ENV
export CONDA_ENV_DIR

all: conda_env parser/fcoplib.so iprover train_gnn/parser/fcoplib.so

conda_env: .conda_env

.conda_env: environment.yaml
	@if [ $(NEW_CONDA) = true ] ; then \
		$(MAKE) conda_env_create ; \
	elif [ $(UPDATE_CONDA) = true ] ; then \
		$(MAKE) conda_env_update ; \
	fi && \
	touch .conda_env

conda_env_create: environment.yaml
	@source $(CONDA_ACTIVATE) && \
	"$(CONDA_ENGINE)" env create -n "$(CONDA_ENV)" -f $< && \
	conda activate "$(CONDA_ENV)" && \
	conda env config vars set LD_LIBRARY_PATH=$(CONDA_ENV_DIR)/lib/ && \
	conda deactivate

conda_env_update: environment.yaml
	@source $(CONDA_ACTIVATE) && \
	"$(CONDA_ENGINE)" env update -n "$(CONDA_ENV)" -f $< --prune && \
	conda activate "$(CONDA_ENV)" && \
	conda env config vars set LD_LIBRARY_PATH=$(CONDA_ENV_DIR)/lib/ && \
	conda deactivate


clean_conda:
	@rm .conda_env && \
	source $(CONDA_ACTIVATE) && \
	echo "Cleaning conda..." && \
	conda clean --all && \
	echo "Purging pip cache..." && \
	pip cache purge && \
	conda deactivate && \
	if [ $(NEW_CONDA) = true ] ; then \
	    echo "Removing $(NEW_CONDA)" && \
	    rm -r $(CONDA_DIR) ; \
	elif [ $(UPDATE_CONDA) = false ] ; then \
	    echo "Conda remove $(CONDA_ENV)" && \
	    source $(CONDA_ACTIVATE) && \
	    conda env remove --name $(CONDA_ENV) ; \
	fi

# ------------------------------------------
# Parser part
#
# uses exported CONDA_ACTIVATE and CONDA_ENV
# ------------------------------------------

parser/fcoplib.so: conda_env
	cd parser && \
	$(MAKE) && \
	cd .. && \
	ln -s ../parser/fcoplib.so server/fcoplib.so

clean_parser:
	cd parser && \
	$(MAKE) clean

# ------------------------------------------
# iProver part
#
# 
# ------------------------------------------

opam_switch: .opam_switch

.opam_switch:
	@if [ $(NEW_OPAM) = true ] ; then \
		eval $(opam env) && \
		opam update && \
		opam switch create $(OPAM_SWITCH) --package=ocaml-variants.4.14.1+options,ocaml-option-flambda && \
		opam install --yes ocamlfind ocamlgraph zarith yojson z3 ; \
	fi && \
	touch .opam_switch


iprover: opam_switch
	git clone https://gitlab.com/korovin/iprover.git && \
	cd iprover && \
	git checkout 2022_sockets && \
	sed -i 's/let dbg_global_flag = false/let dbg_global_flag = true/' src/lib.ml && \
	eval $$(opam env --switch="$(OPAM_SWITCH)") && \
	bash configure && \
	$(MAKE) -j 1 STATIC=true z3=false


clean: clean_conda clean_parser


# ------------------------------------------
# train_gnn part
#
# uses exported CONDA_ACTIVATE and CONDA_ENV
# ------------------------------------------

train_gnn/parser/fcoplib.so: conda_env
	cd train_gnn/parser && \
	$(MAKE) && \
	cd ../.. && \
	ln -s ../train_gnn/parser/fcoplib.so train_gnn/fcoplib.so

