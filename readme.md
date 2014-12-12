
**OVERVIEW:**

This is a simulated annealing algorithm for use in variable selection for linear models with many predictors. Given a large number of predictors, this algorithm is unlikely to find the best possible variable subset, but it is likely to find a subset that performs well. The algorithm finds the subset that minimizes the root mean squared error of the model. The function outputs a list of variables that ere selected. 


**FUNCTION ARGUMENTS:**

dataset = the data frame used in variable selection

outcome = the continuous outcome variable to be predicted

initial.temp = the starting temperature for the algorithm. Larger values will extend the search time (default = 1000)

delta.temp = the decrease in temperature with each step of the search. Smaller values will extend the search time (default = 0.1)

initial.subset.size = the number of randomly selected variables that are included in the initial model (default = 2)

perturbation.factor = the number of variables that are subtracted or added to the model with each step. A larger perturbation factor will result in bigger changes in the model with each step (default = 2)


**EXAMPLE FUNCTION CALL:**

Function calls look like this:

get.coeffs("simdata", y", 1000, 0.1, 2, 2)