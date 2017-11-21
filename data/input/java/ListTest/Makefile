## Define the two arguments that the 'template' expects
AS_JARS := SingleEl.jar MultipleInserts.jar MultipleInsertsLinear.jar \
	   CircularList.jar ListTest.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

SingleEl.jar :  SingleEl.class LinkedList.class List.class
	jar -cvfe SingleEl.jar SingleEl $^

MultipleInserts.jar : MultipleInserts.class LinkedList.class List.class
	jar -cvfe MultipleInserts.jar MultipleInserts $^

MultipleInsertsLinear.jar : MultipleInsertsLinear.class LinkedList.class \
		            List.class
	jar -cvfe MultipleInsertsLinear.jar MultipleInsertsLinear $^

CircularList.jar : CircularList.class LinkedList.class List.class
	jar -cvfe CircularList.jar CircularList $^

ListTest.jar : SingleEl.class MultipleInserts.class MultipleInsertsLinear.class CircularList.class LinkedList.class List.class
	jar -cvfe ListTest.jar MultipleInserts $^