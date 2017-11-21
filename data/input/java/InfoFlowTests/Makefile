## Define the two arguments that the 'template' expects
AS_JARS := CallAndReturn.jar CallAndCount.jar CountDirectly.jar DoubleCount.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

CallAndReturn.jar: CallAndReturn.class
	jar -cvfe $@ CallAndReturn $^

CallAndCount.jar: CallAndCount.class
	jar -cvfe $@ CallAndCount $^

CountDirectly.jar: CountDirectly.class
	jar -cvfe $@ CountDirectly $^

DoubleCount.jar: DoubleCount.class
	jar -cvfe $@ DoubleCount $^
