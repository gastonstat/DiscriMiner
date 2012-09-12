betweenCov <-
function(variables, group, div_by_n=FALSE)
{
  # Between-class covariance matrix
  # variables: matrix or data frame with explanatory variables
  # group: vector or factor with group memberships
  # div_by_n: logical indicating division by num of observations
  
  # check inputs
  verify_Xy = my_verify(variables, group, na.rm=FALSE)
  X = verify_Xy$X
  y = verify_Xy$y
  
  # how many obs and variables
  n = nrow(X)
  p = ncol(X)
  # group levels and number of levels
  glevs = levels(y)
  ng = nlevels(y)
  # global mean
  mean_global = colMeans(X)
  # matrix to store results
  Between = matrix(0, p, p)
  # pooled between-class covariance matrix
  for (k in 1:ng)
  {
    # select obs of k-th group
    tmp <- y == glevs[k]
    # how many obs in group k
    nk = sum(tmp)
    # mean k-th group
    mean_k = colMeans(X[tmp,])
    # mean k-th group - global mean
    dif_k = mean_k - mean_global
    # k-th group between cov matrix    
    if (div_by_n) {
      between_k = (nk/n) * tcrossprod(dif_k)
    } else {
      between_k = (nk/(n-1)) * tcrossprod(dif_k)
    }
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
