SUBDIRS = \
	AProVE_10 CyclicList RodCutting hashIterative loop_iter	AddSecret EvenSecret MatrixChainMultOrder SecretRand helloWorld \
	Julia_10_Iterative NoOps TestPrintln infLeak recursiveFib Aprove_09 FastExpBigInt Julia_10_Recursive NonTerm Trees largest \
	squaring ArrayListLeak FastExpInt LCM OddExceptions account login stringCompare ArraySorting FixedSuppliedSeed Life \
	OddSecret countOnes loginFixed sudoku BadHash GCD ListTest PrimalityTests fibonacci loginIterative Costa_Julia_09 \
	GenericPathFinding LogSecret PrimeLeak genericSort loop Costa_Julia_09-recursive InfoFlowTests PythagTriples hash \
	loopAsRecursion

all:
	@for i in $(SUBDIRS); do \
	echo "Building in $$i..."; \
	(cd $$i; make all; cd ..); done

inttrs:
	@for i in $(SUBDIRS); do \
	echo "Building in $$i..."; \
	(cd $$i; make inttrs; cd ..); done

.PHONY: clean

clean:
	@for i in $(SUBDIRS); do \
        echo "Clearing in $$i..."; \
        (cd $$i; make clean; cd ..); done
