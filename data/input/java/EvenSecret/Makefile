## Define the two arguments that the 'template' expects
AS_JARS := EvenSecret.jar EvenSecret2.jar StaticEvenSecret.jar StaticEvenSecret2.jar EvenSecretAsArgument.jar StaticEvenSecretAsArgument.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

StaticEvenSecret.jar: StaticEvenSecret.class
	jar -cvfe $@ StaticEvenSecret $^

StaticEvenSecret2.jar: StaticEvenSecret2.class StaticEvenSecret.class
	jar -cvfe $@ StaticEvenSecret2 $^

EvenSecret.jar: EvenSecret.class
	jar -cvfe $@ EvenSecret $^

EvenSecret2.jar: EvenSecret2.class EvenSecret.class
	jar -cvfe $@ EvenSecret2 $^

EvenSecretAsArgument.jar: EvenSecretAsArgument.class
	jar -cvfe $@ EvenSecretAsArgument $^

StaticEvenSecretAsArgument.jar: StaticEvenSecretAsArgument.class
	jar -cvfe $@ StaticEvenSecretAsArgument $^
