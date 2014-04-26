# ScaLP

This is a simple Scala-wrapper for Gurobi's Java libraries (see http://www.gurobi.com/documentation/5.6/reference-manual/java_reference_manual).
Nothing fancy here, just what was required to get a small project running.

It was run using Gurobi 5.6 and Scala 2.10.

To build it, just create a lib folder in this project if not already there, copy the gurobi.jar from your Gurobi installation to it and run `sbt package`.