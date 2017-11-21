## Define the two arguments that the 'template' expects
AS_JARS := MatrixOrderOptimization.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

MatrixOrderOptimization.jar: MatrixOrderOptimization.class Matrix.class
	jar -cvfe MatrixOrderOptimization.jar MatrixOrderOptimization $^
