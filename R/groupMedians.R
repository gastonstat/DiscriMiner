groupMedians <-
function(variables, group, na.rm=FALSE)
{
  # Calculate medians by group
  # variables: matrix or data frame with explanatory variables
  # group: vector or factor with group memberships
  # na.rm: logical indicating whether missing values should be removed
  
  # check inputs
  verify_Xy = my_verify(variables, group, na.rm=na.rm)
  X = verify_Xy$X
  y = verify_Xy$y
  
  # how many groups
  ng = nlevels(y)
  # matrix with group medians
  Meds = matrix(0, ncol(X), ng)
  for (j in 1:ncol(X))
  {
    Meds[j,] = tapply(X[,j], y, FUN=median, na.rm=na.rm)
  }
  # add names
  if (is.null(colnames(X))) {
    rownames(Meds) = paste("X", 1:ncol(X), sep="")
  } else {
    rownames(Meds) = colnames(X)
  }
  colnames(Meds) = levels(y)
  # results
  Meds
}
