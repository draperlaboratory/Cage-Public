## Define the two arguments that the 'template' expects
AS_JARS := DoublyLinkedList.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

DoublyLinkedList.jar :  DoublyLinkedList.class
	jar -cvfe DoublyLinkedList.jar DoublyLinkedList $^
