## Define the two arguments that the 'template' expects
AS_JARS := RodCuttingRec.jar RodCuttingDyn.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

RodCuttingRec.jar: RodCutting.class RodCuttingRec.class
	jar -cvfe RodCuttingRec.jar RodCuttingRec $^

RodCuttingDyn.jar: RodCutting.class RodCuttingDyn.class
	jar -cvfe RodCuttingDyn.jar RodCuttingDyn $^
