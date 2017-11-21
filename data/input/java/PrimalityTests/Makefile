## Define the two arguments that the 'template' expects
AS_JARS := Eratosthenes.jar TrialDivision.jar TrialDivisionLoops.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

Eratosthenes.jar: Eratosthenes.class
	jar -cvfe Eratosthenes.jar Eratosthenes $^

TrialDivision.jar: TrialDivision.class
	jar -cvfe TrialDivision.jar TrialDivision $^

TrialDivisionLoops.jar: TrialDivisionLoops.class
	jar -cvfe TrialDivisionLoops.jar TrialDivisionLoops $^
