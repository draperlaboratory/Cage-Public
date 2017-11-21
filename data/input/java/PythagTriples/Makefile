## Define the two arguments that the 'template' expects
AS_JARS := PythagTriples.jar PythagTriplesSlow.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

PythagTriples.jar: PythagTriples.class Triple.class
	jar -cvfe PythagTriples.jar PythagTriples $^

PythagTriplesSlow.jar: PythagTriplesSlow.class Triple.class
	jar -cvfe PythagTriplesSlow.jar PythagTriplesSlow $^
