

############################################################################
# SIMULATED ANNEALING ALGORITHM - FOR VARIABLE SELECTION IN LINEAR MODELS
############################################################################

get.coeffs <- function(dataset, outcome, initial.temp = 1000, delta.temp = 0.1, initial.subset.size = 2, perturbation.factor = 2)
{


### QUICK FUNCTION TO CALCULATE ROOT MEAN SQUARED ERROR
RMSE.function <- function(resids)
{return(sqrt(mean(resids^2)))}


### COLLECT INFORMATION ON VARIABLES
num.predictors <- ncol(subset(get(dataset),select = -c(get(outcome))))
outcome.name <- colnames(subset(get(dataset),select = c(get(outcome))))
predictor.names <- colnames(subset(get(dataset),select = -c(get(outcome))))


### DRAW RANDOM, INITIAL SUBSET OF VARIABLES
initial.subset.names <- sample(predictor.names, initial.subset.size, replace=F)


### CREATE THE FORMULA FOR THE INITIAL SUBSET
for (q in (1:initial.subset.size))
	{
	if (q == 1) {temp.formula <- ""}
	if (q == initial.subset.size)
	{
	temp.formula <- paste(temp.formula, initial.subset.names[q], sep = ""? ) 
	} else {
	temp.formula <- paste(temp.formula, initial.subset.names[q], " + ", sep = ""? ) 	
	}
	}


### CALCULATE INITIAL FORMULA RMSE, SAVE THIS AS BEST 'E'
initial.formula <- paste(outcome.name, " ~ ", temp.formula, sep= "")
E.best <- RMSE.function(lm(as.formula(initial.formula), get(dataset))$residuals)


### MAIN LOOP BEGINS HERE
for (i in (0:(initial.temp/delta.temp)))
	{
	if (i == 0) {best.subset.names <- initial.subset.names}
	if (i == 0) {current.temp <- initial.temp}
	if (i == 0) {current.subset.size <- initial.subset.size}
	if (i == 0) {current.subset.names <- initial.subset.names}
	
	
	### PERTURB CURRENT SUBSET
	### DETERMINE WHETHER VARIABLES ARE ADDED OR SUBTRACTED
	### IF 0, THEN DECREASE NUMBER OF VARIABLES, IF 1 THEN INCREASE 
	subtract.or.add <- sample(0:1, 1)
	
	### IF AN INCREASE IS SELECTED, AND THE SUBSET WILL BE LESS THAN MAXIMUM 
	### ALLOW AN INCREASE
	if (subtract.or.add == 1 & ((current.subset.size + perturbation.factor) <= num.predictors)) {
		
		any.repeats <- TRUE
		while (any.repeats == TRUE) {
				new.sample.names <- sample(predictor.names, perturbation.factor)
				if (length(intersect(new.sample.names, current.subset.names)) == 0 ) {any.repeats <- FALSE}
				}
			 
		current.subset.names <- append(current.subset.names, new.sample.names)
		current.subset.size <- length(current.subset.names)
		
	### IF AN INCREASE IS SELECTED, AND THE SUBSET WILL BE GREATER THAN MAXIMUM
	### FORCE A DECREASE
	} else if (subtract.or.add == 1 & ((current.subset.size + perturbation.factor) > num.predictors)) {
	
	remove.names <- sample(current.subset.names, perturbation.factor, replace=F)
	current.subset.names <- setdiff(current.subset.names, remove.names)
	current.subset.size <- length(current.subset.names)
		
		
	### IF AN DECREASE IS SELECTED, AND THE SUBSET WILL BE GREATER THAN ZERO
	### ALLOW A DECREASE	
	} else if (subtract.or.add == 0 & ((current.subset.size - perturbation.factor) >= 1)) {
		
	remove.names <- sample(current.subset.names, perturbation.factor, replace=F)
	current.subset.names <- setdiff(current.subset.names, remove.names)
	current.subset.size <- length(current.subset.names)

		
	### IF AN DECREASE IS SELECTED, AND THE SUBSET WILL BE LOWER TO ZERO
	### FORCE IT TO ADD VARIABLES
	} else if (subtract.or.add == 0 & ((current.subset.size - perturbation.factor) < 1)) {
		
		any.repeats <- TRUE
		while (any.repeats == TRUE) {
				new.sample.names <- sample(predictor.names, perturbation.factor)
				if (length(intersect(new.sample.names, current.subset.names)) == 0 ) {any.repeats <- FALSE}
				}
			 
		current.subset.names <- append(current.subset.names, new.sample.names)		
		current.subset.size <- length(current.subset.names)

	} 
		
	### CREATE THE FORMULA FOR THE CURRENT SUBSET
	for (q in (1:current.subset.size))
	{
	if (q == 1) {temp.formula <- ""}
	if (q == current.subset.size)
	{
	temp.formula <- paste(temp.formula, current.subset.names[q], sep = ""? ) 
	} else {
	temp.formula <- paste(temp.formula, current.subset.names[q], " + ", sep = ""? ) 	
	}
	}


	### CALCULATE CURRENT FORMULA RMSE
	current.formula <- paste(outcome.name, " ~ ", temp.formula, sep= "")
	E.current <- RMSE.function(lm(as.formula(current.formula), get(dataset))$residuals)
	#E.current <- AIC(lm(as.formula(current.formula), get(dataset)))
	
	
	### IF E.CURRENT < E.BEST THEN ACCEPT CURRENT SUBSET AS BEST
	### IF E.CURRENT IS NOT BETTER, THAN DO PROBABILITY CALCULATIONS...
	if (E.current < E.best) {best.subset.names <- current.subset.names} else {
		
	prob.value <- exp((E.best - E.current)/current.temp)
	rand.value <- runif(1)
		
		if (prob.value <= rand.value) {best.subset.names <- current.subset.names} else {}
		
	}

	current.temp <- (current.temp - delta.temp)
	}


### REPORT BEST MODEL WHEN LOOP ENDS
print(best.subset.names, quote=F)

}


