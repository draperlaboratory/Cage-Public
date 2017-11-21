## Define the two argements the template expects
AS_JARS := Account.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

Account.jar: Account.class
	jar -cvfe Account.jar Login A.class List.class Nil.class \
	Account.class Login.class Node.class
