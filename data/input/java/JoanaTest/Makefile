## Define the two arguments that the 'template' expects
AS_JARS := JoanaTest.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

JoanaTest.jar: JoanaTest.class A.class
	jar -cvfe JoanaTest.jar JoanaTest $^
