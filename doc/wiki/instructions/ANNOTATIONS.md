JSON Interface Files for AProVE
=============================

This file describes the JSON interface files for AProVE. Note that the
exact format is in a very provisional state and will very likely
change and be extended in the near future.

----

An interface file for AProVE is a JSON file which is a single record (association list)
with the following fields (mandatory fields are marked with *):

1. created: A string (possibly) containing the date and time of creation
2. class*: A string containing the fully qualified name of the class which is described by the interface.
3. functions*: A list of *association lists* containing (potentially) the following fields
   i.   descriptor*: A string containing the method descriptor (see [1])
   ii.  args*: A list of strings representing the names of the arguments (used to describe the subsequent fields)
   iii. secretArgs: A list of integers representing the indexes of the function arguments that are always assumed to contain secret information to not be leaked.
   iv.  pure: Specifies whether or not a function is pure (contains side-effects visible from the outside).
   v.   complexity: An association lists with the following fields, which are all associated to strings representing polynomial functions in the input variables:
     a. lowerSize: A lower bound on the output size.
     b. upperSize: An upper bound on the output size.
     c. upperTime: An upper bound on the computation time (as a number of "elementary" reductions).
     d. lowerTime: A lower bound on the computation time.
     e. upperSpace: An upper bound on the computation space (as a number of cells).
     f. lowerSpace: A lower bound on the computation space.

The time, size, and space bounds (3.v.a-f) are polynomials over args (3.ii) and the variable "env" which is used to
model additional inputs (reading from the file system or network). For example, "env" is a valid upper bound on the
output size of java.nio.file.Files.readAllBytes.



[1] https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.3.3
