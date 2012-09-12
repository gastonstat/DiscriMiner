binarize <- 
function(variables)
{
  # binary super-indicator matrix (aka Complete Disjunctive Table)
  # variables: matrix or data.frame with explanatory variables
  
  # make sure variables is a data frame with factors
  fac_check = sapply(variables, class)
  if (!is.data.frame(variables) && any(fac_check != "factor"))
    stop("\n'variables' must be a data frame with factors")
  # no missing values allowed
  if (length(complete.cases(variables)) != nrow(variables))
    stop("\nSorry, no missing values allowed in 'variables'")    
  
  # build super-indicator matrix Z
  Z = my_tdc(variables)  
  Z
}