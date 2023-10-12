This simulation code adopts a sequential optimal design as a multi-objective optimal design for a Type-II progressive censoring with a dependent GLM-based random removal mechanism.
The simulation code of the sequential approach has two steps for combining three objective criteria, including Q1, Q2, and Q3.


In a simulation study, the ordering of the simulation code that should be run is as follows;
1.	Run two functions, “func-generat data-plogit.R” and “func-variance measure.R”
2.	Run step 1, “step1.R”.
by determining the primary parameters such as (m,n) for type II progressive censoring,
(lambda, beta) for the Weibull distribution and (alpha0, alpha1) For GLM-mechanism.
3.	Run step2, “step2.R” .
This step needs the data that is calculated from the previous step.


![image](https://github.com/hassantabar/Type-II-progressive-censoring-optimization/assets/145746409/588dc8f1-7d1f-4153-842f-b4d54299a9c7)

 


 
