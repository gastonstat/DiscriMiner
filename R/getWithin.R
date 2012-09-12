getWithin <- 
function(variables, group)
{
  # within-class pooled covariance matrix
  # variables: matrix or data frame with explanatory variables
  # group: vector or factor with group memberships
  
  # check inputs
  verify_Xy = my_verify(variables, group, na.rm=FALSE)
  X = verify_Xy$X
  y = verify_Xy$y
  
  Within = my_withinCov(X, y)
  Within
}