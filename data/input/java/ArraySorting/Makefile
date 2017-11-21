## Define the two arguments that the 'template' expects
AS_JARS := IntArray.jar ForBubbleSort.jar WhileBubbleSort.jar BucketSort.jar InsertionSort.jar \
	   SelectionSort.jar CombSort.jar ShellSort.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

IntArray.jar: IntArray.class
	jar -cvfe IntArray.jar IntArray $^

ForBubbleSort.jar : IntArray.class ForBubbleSort.class
	jar -cvfe ForBubbleSort.jar ForBubbleSort $^

WhileBubbleSort.jar : IntArray.class WhileBubbleSort.class
	jar -cvfe WhileBubbleSort.jar WhileBubbleSort $^

BucketSort.jar : IntArray.class BucketSort.class
	jar -cvfe BucketSort.jar BucketSort $^

InsertionSort.jar : IntArray.class InsertionSort.class
	jar -cvfe InsertionSort.jar InsertionSort $^

SelectionSort.jar : IntArray.class SelectionSort.class
	jar -cvfe SelectionSort.jar SelectionSort $^

CombSort.jar : IntArray.class CombSort.class
	jar -cvfe CombSort.jar CombSort $^

ShellSort.jar : IntArray.class ShellSort.class
	jar -cvfe ShellSort.jar ShellSort $^
