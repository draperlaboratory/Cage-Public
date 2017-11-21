## Define the two arguments that the 'template' expects
AS_JARS := BoundaryPositions.jar BoundaryPositionsSlice.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

BoundaryPositions.jar: BoundaryPositions.class
	jar -cvfe $@ BoundaryPositions $^

BoundaryPositionsSlice.jar: BoundaryPositionsSlice.class
	jar -cvfe $@ BoundaryPositionsSlice $^
