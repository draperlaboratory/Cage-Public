## Define the two arguments that the 'template' expects
AS_JARS := TestPrint.jar TestStaticPrint.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

TestPrint.jar: TestPrint.class
	jar -cvfe TestPrint.jar TestPrint $^

TestStaticPrint.jar: TestStaticPrint.class
	jar -cvfe TestStaticPrint.jar TestStaticPrint $^
