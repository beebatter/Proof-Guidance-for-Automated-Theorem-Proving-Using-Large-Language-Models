# Installation

    ./configure
	make

If you use a new conda environment then

	source conda/bin/activate
	
and then

	conda activate iserver

# Server example

It is possible to run the server with a model from the paper using

	cd server
	python iserver_mp.py --start_dims 32 32 32 --next_dims 32 32 32 --layers 11 --model models/dynamic10_d32_l11.pt
	
and test iProver against it using, for example,

	cd experiments
	./iprover_solo.sh 15 1000 matrix_8__t32_matrix_8 result
