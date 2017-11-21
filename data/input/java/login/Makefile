## Define the two arguments that the 'template' expects
AS_JARS := Login.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

Login.jar: Login.class Account.class Node.class Nil.class List.class
	jar -cvfe Login.jar Login $^
