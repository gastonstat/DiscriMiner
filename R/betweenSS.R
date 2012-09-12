betweenSS <-
function(variables, group)
{
  # Between-class sum of squares and cross products matrix
  # variables: matrix or data frame with explanatory variables
  # group: vector or factor with group memberships
  
  # check inputs
  verify_Xy = my_verify(variables, group, na.rm=FALSE)
  X = verify_Xy$X
  y = verify_Xy$y
  
  # group levels and number of levels
  glevs = levels(y)
  ng = nlevels(y)
  # global mean
  mean_all = colMeans(X)
  # matrix to store results
  Between = matrix(0, ncol(X), ncol(X))
  # calculate between Sum of squares
  for (k in 1:ng)
  {
    tmp <- y == glevs[k]
    nk = sum(tmp)
    mean_k = colMeans(X[tmp,])
    dif_k = mean_k - mean_all
    between_k = nk * dif_k %*% t(dif_k)
    Between = Between + between_k
  }
  # add names
  if (is.null(colnames(variables))) {
    var_names = paste("X", 1:ncol(X), sep="")
    dimnames(Between) = list(var_names, var_names)
  } else {
    dimnames(Between) = list(colnames(variables), colnames(variables))
  }
  # result
  Between
}
