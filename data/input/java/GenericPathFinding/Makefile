## Define the two arguments that the 'template' expects
AS_JARS := BreadthFirstLine.jar BreadthFirstBiLine.jar BreadthFirstLadder.jar \
	   BreadthFirstPancake.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

GRAPH = Graph.class Edge.class Node.class
INST = Instance.class InstanceGeneration.class AbsoluteGoalInstance.class
BFS = BreadthFirstSearch.class SearchNode.class
PANCAKE = PancakeNode.class PancakeInstance.class

BreadthFirstLine.jar: BreadthFirstLine.class $(BFS) $(GRAPH) $(INST)
	jar -cvfe BreadthFirstLine.jar BreadthFirstLine $^

BreadthFirstBiLine.jar: BreadthFirstBiLine.class $(BFS) $(GRAPH) $(INST)
	jar -cvfe BreadthFirstBiLine.jar BreadthFirstBiLine $^

BreadthFirstLadder.jar: BreadthFirstLadder.class $(BFS) $(GRAPH) $(INST)
	jar -cvfe BreadthFirstLadder.jar BreadthFirstLadder $^

BreadthFirstPancake.jar: BreadthFirstPancake.class $(BFS) $(GRAPH) $(INST) $(PANCAKE)
	jar -cvfe BreadthFirstPancake.jar BreadthFirstPancake $^
