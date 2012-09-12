easyMCA <- 
function(variables)
{
  # Perform multiple correspondence analysis
  # X: data frame with categorical variables as factors
  
  # check input
  fac_check = sapply(variables, class)
  if (!is.data.frame(variables) && any(fac_check != "factor"))
    stop("\nA data frame with factors was expected")
  # check for missing values
  if (length(complete.cases(variables)) != nrow(variables))
    stop("\nSorry: no missing values allowed")
  
  # apply MCA
  res = my_mca(variables)
  res
}
