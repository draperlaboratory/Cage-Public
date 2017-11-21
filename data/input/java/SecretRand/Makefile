## Define the two arguments that the 'template' expects
AS_JARS := SecretRand.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

SecretRand.jar: SecretRand.class
	jar -cvfe SecretRand.jar SecretRand $^
