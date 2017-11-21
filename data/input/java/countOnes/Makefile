## Define the two arguments that the 'template' expects
AS_JARS := CountOnes.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

include ../Makefile.JavaTemplate

CountOnes.jar: CountOnes.class
	jar -cvfe CountOnes.jar CountOnes $^
