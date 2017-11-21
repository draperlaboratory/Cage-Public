## Define the two arguments that the 'template' expects
AS_JARS := Traversals.jar InOrder.jar PreOrder.jar PostOrder.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

Traversals.jar: Node.class Traversals.class
	jar -cvfe Traversals.jar Traversals $^

InOrder.jar: Node.class Traversals.class InOrder.class
	jar -cvfe InOrder.jar InOrder $^

PreOrder.jar: Node.class Traversals.class PreOrder.class
	jar -cvfe PreOrder.jar PreOrder $^

PostOrder.jar: Node.class Traversals.class PostOrder.class
	jar -cvfe PostOrder.jar PostOrder $^
