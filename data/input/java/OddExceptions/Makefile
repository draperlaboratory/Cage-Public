## Define the two arguments that the template expects
AS_JARS := OddExceptions.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

include ../Makefile.JavaTemplate

## Here's where you fill in rules for building each jar.
## An example is left in commented out.

OddExceptions.jar : OddExceptions.class OddException.class
	jar -cvfe OddExceptions.jar OddExceptions $^
