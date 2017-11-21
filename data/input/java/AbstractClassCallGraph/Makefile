## Define the two arguments that the template expects
AS_JARS := AbstractClassCallGraph.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

include ../Makefile.JavaTemplate

## Here's where you fill in rules for building each jar.
## An example is left in commented out.

AbstractClassCallGraph.jar : AbstractMethodClass.class ClassA.class ClassB.class CG_Test.class
	jar -cvfe AbstractClassCallGraph.jar Main $^
