groupQuants <-
function(variables, group, prob, na.rm=FALSE)
{
  # Calculate quantile deviations by group
  # variables: matrix or data frame with explanatory variables
  # group: vector or factor with group memberships
  # prob: probability value in [0,1]
  # na.rm: logical indicating whether missing values should be removed
  
  # check inputs
  verify_Xy = my_verify(variables, group, na.rm=na.rm)
  X = verify_Xy$X
  y = verify_Xy$y
  # quantile
  if (missing(prob)) {
    stop("\nOoops, argument 'prob' is missing")
  } else {
    if (length(prob) > 1)
      stop("\nOoops, 'prob' has length > 1")
    if (prob < 0 || prob > 1)
      stop("\nOoops, invalid 'prob' value")
  }
  
  # how many groups
  ng = nlevels(y)
  # matrix with group quantiles
  Quants = matrix(0, ncol(X), ng)
  for (j in 1:ncol(X))
  {
    Quants[j,] = tapply(X[,j], y, FUN=quantile, probs=prob, na.rm=na.rm)
  }
  # add names
  if (is.null(colnames(X))) {
    rownames(Quants) = paste("X", 1:ncol(X), sep="")
  } else {
    rownames(Quants) = colnames(X)
  }
  colnames(Quants) = levels(y)
  # results
  Quants
}
