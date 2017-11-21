## Define the two arguments that the 'template' expects
AS_JARS := InfLeak.jar InfLeak2.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

InfLeak2.jar: InfLeak2.java
	javac $^
	jar -cvfe InfLeak2.jar Login Account.class Login.class List.class

InfLeak.jar: InfLeak.java
	javac $^
	jar -cvfe InfLeak.jar Login Account.class Login.class List.class
