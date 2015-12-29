# ScaLP

This is a simple Scala-wrapper for Gurobi's Java libraries (see http://www.gurobi.com/documentation/6.5/refman/java_api_overview.html).
Nothing fancy here, just what was required to get a small project running.

It was run using Gurobi 6.5 and Scala 2.11.x.

To build it, just create a lib folder in this project if not already there, copy the gurobi.jar from your Gurobi installation to it and run `sbt package`.