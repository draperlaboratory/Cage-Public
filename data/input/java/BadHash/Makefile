## Define the two arguments that the template expects
AS_JARS := BadHash.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

include ../Makefile.JavaTemplate

## Here's where you fill in rules for building each jar.
## An example is left in commented out.

BadHash.jar : HashTable.class Hash.class Bucket.class List.class Main.class NotFound.class
	jar -cvfe BadHash.jar Main $^
