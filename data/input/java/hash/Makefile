## Define the two arguments that the 'template' expects
AS_JARS := Hash.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

Hash.jar: Hash.class HashTbl.class Node.class Nil.class List.class
	jar -cvfe Hash.jar Hash $^
