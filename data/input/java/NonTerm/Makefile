## Define the two arguments that the 'template' expects
AS_JARS := NonTerm.jar NonTermSingle.jar WhileTerm.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

NonTerm.jar: NonTerm.class
	jar -cvfe NonTerm.jar NonTerm $^

NonTermSingle.jar: NonTermSingle.class
	jar -cvfe NonTermSingle.jar NonTermSingle $^

WhileTerm.jar: WhileTerm.class
	jar -cvfe WhileTerm.jar WhileTerm $^
