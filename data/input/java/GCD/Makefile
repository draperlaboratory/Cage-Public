## Define the two arguments that the 'template' expects
AS_JARS := GCD.jar GCDError.jar StaticGCD.jar GCDLoop.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

GCD.jar: GCD.class
	jar -cvfe GCD.jar GCD $^

GCDError.jar: GCDError.class NegativeInts.class
	jar -cvfe GCDError.jar GCDError $^

StaticGCD.jar: StaticGCD.class
	jar -cvfe StaticGCD.jar StaticGCD $^

GCDLoop.jar: GCDLoop.class
	jar -cvfe GCDLoop.jar GCDLoop $^
