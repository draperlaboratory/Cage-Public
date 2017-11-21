## Define the two arguments that the 'template' expects
AS_JARS := Fib.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

Fib.jar: Fib.class
	jar -cvfe Fib.jar Fib $^
