## Define the two arguments that the 'template' expects
AS_JARS := IntArrayBubble.jar CharArrayBubble.jar IntListBubble.jar \
	   CharListBubble.jar IntArrayComb.jar CharArrayComb.jar \
	   IntListComb.jar CharListComb.jar IntArrayInsertion.jar \
	   CharArrayInsertion.jar IntListInsertion.jar CharListInsertion.jar \
	   IntArrayShell.jar CharArrayShell.jar IntListShell.jar \
	   CharListShell.jar IntArraySelection.jar CharArraySelection.jar \
           IntListSelection.jar CharListSelection.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

ARRAYCLASSES = CompArray.class Comp.class Sortable.class SortAlg.class
INTARRAY = IntArray.class MyInt.class $(ARRAYCLASSES)
CHARARRAY = CharArray.class MyChar.class $(ARRAYCLASSES)
LISTCLASSES = CompList.class Comp.class Sortable.class SortAlg.class
INTLIST = IntList.class MyInt.class $(LISTCLASSES)
CHARLIST = CharList.class MyChar.class $(LISTCLASSES)

IntArrayBubble.jar: IntArrayBubble.class BubbleSort.class $(INTARRAY)
	jar -cvfe IntArrayBubble.jar IntArrayBubble $^

CharArrayBubble.jar: CharArrayBubble.class BubbleSort.class $(CHARARRAY)
	jar -cvfe CharArrayBubble.jar CharArrayBubble $^

IntListBubble.jar: IntListBubble.class BubbleSort.class $(INTLIST)
	jar -cvfe IntListBubble.jar IntListBubble $^

CharListBubble.jar: CharListBubble.class BubbleSort.class $(INTLIST)
	jar -cvfe CharListBubble.jar CharListBubble $^

IntArrayComb.jar: IntArrayComb.class CombSort.class $(INTARRAY)
	jar -cvfe IntArrayComb.jar IntArrayComb $^

CharArrayComb.jar: CharArrayComb.class CombSort.class $(CHARARRAY)
	jar -cvfe CharArrayComb.jar CharArrayComb $^

IntListComb.jar: IntListComb.class CombSort.class $(INTLIST)
	jar -cvfe IntListComb.jar IntListComb $^

CharListComb.jar: CharListComb.class CombSort.class $(INTLIST)
	jar -cvfe CharListComb.jar CharListComb $^

IntArrayInsertion.jar: IntArrayInsertion.class InsertionSort.class $(INTARRAY)
	jar -cvfe IntArrayInsertion.jar IntArrayInsertion $^

CharArrayInsertion.jar: CharArrayInsertion.class InsertionSort.class $(CHARARRAY)
	jar -cvfe CharArrayInsertion.jar CharArrayInsertion $^

IntListInsertion.jar: IntListInsertion.class InsertionSort.class $(INTLIST)
	jar -cvfe IntListInsertion.jar IntListInsertion $^

CharListInsertion.jar: CharListInsertion.class InsertionSort.class $(INTLIST)
	jar -cvfe CharListInsertion.jar CharListInsertion $^

IntArrayShell.jar: IntArrayShell.class ShellSort.class CombSort.class $(INTARRAY)
	jar -cvfe IntArrayShell.jar IntArrayShell $^

CharArrayShell.jar: CharArrayShell.class ShellSort.class CombSort.class $(CHARARRAY)
	jar -cvfe CharArrayShell.jar CharArrayShell $^

IntListShell.jar: IntListShell.class ShellSort.class CombSort.class $(INTLIST)
	jar -cvfe IntListShell.jar IntListShell $^

CharListShell.jar: CharListShell.class ShellSort.class CombSort.class $(INTLIST)
	jar -cvfe CharListShell.jar CharListShell $^

IntArraySelection.jar: IntArraySelection.class SelectionSort.class $(INTARRAY)
	jar -cvfe IntArraySelection.jar IntArraySelection $^

CharArraySelection.jar: CharArraySelection.class SelectionSort.class $(CHARARRAY)
	jar -cvfe CharArraySelection.jar CharArraySelection $^

IntListSelection.jar: IntListSelection.class SelectionSort.class $(INTLIST)
	jar -cvfe IntListSelection.jar IntListSelection $^

CharListSelection.jar: CharListSelection.class SelectionSort.class $(INTLIST)
	jar -cvfe CharListSelection.jar CharListSelection $^




