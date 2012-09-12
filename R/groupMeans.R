groupMeans <-
function(variables, group, na.rm=FALSE)
{
  # Calculate means by group
  # variables: matrix or data frame with explanatory variables
  # group: vector or factor with group memberships
  # na.rm: logical indicating whether missing values should be removed
  
  # check inputs
  verify_Xy = my_verify(variables, group, na.rm=na.rm)
  X = verify_Xy$X
  y = verify_Xy$y

  # how many groups
  ng = nlevels(y)
  # matrix with group means
  Means = matrix(0, ncol(X), ng)
  for (j in 1:ncol(X))
  {
    Means[j,] = tapply(X[,j], y, FUN=mean, na.rm=na.rm)
  }
  # add names
  if (is.null(colnames(X))) {
    rownames(Means) = paste("X", 1:ncol(X), sep="")
  } else {
    rownames(Means) = colnames(X)
  }
  colnames(Means) = levels(y)
  # results
  Means
}
