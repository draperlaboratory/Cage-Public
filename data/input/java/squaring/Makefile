## Define the two arguments that the 'template' expects
AS_JARS := Squaring.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

Squaring.jar: Squaring.class
	jar -cvfe Squaring.jar Account A.class Account.class List.class \
	Node.class
