## Define the two arguments that the 'template' expects
AS_JARS := InfLeakIter.jar InfLeakIter2.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

InfLeakIter2.jar: InfLeakIter2.java
	javac $^
	jar -cvfe InfLeakIter2.jar Login Account.class Login.class

InfLeakIter.jar: InfLeakIter.java
	javac $^
	jar -cvfe InfLeakIter.jar Login Account.class Login.class
