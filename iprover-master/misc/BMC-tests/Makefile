# create k-induction tests

.PHONY : clean all

all: bv_loop_k-ind_2-neg.cnf

bv_loop_k-ind_2-neg.cnf: CreateKInductionShortTest.java
	javac CreateKInductionShortTest.java
	java CreateKInductionShortTest

clean:
	rm -f *.class *.cnf
