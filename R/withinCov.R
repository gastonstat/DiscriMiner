withinCov <-
function(variables, group, div_by_n=FALSE)
{
  # within-class pooled covariance matrix
  # variables: matrix or data frame with explanatory variables
  # group: vector or factor with group memberships
  # div_by_n: logical indicating division by num of observations
  
  # check inputs
  verify_Xy = my_verify(variables, group, na.rm=FALSE)
  X = verify_Xy$X
  y = verify_Xy$y
  
  # how many observations
  n = nrow(X)
  # how many variables
  p = ncol(X)
  # group levels and number of levels
  glevs = levels(y)
  ng = nlevels(y)
  # within cov matrix
  Within = matrix(0, p, p)
  for (k in 1:ng)
  {
    tmp <- y == glevs[k]
    nk = sum(tmp)
    if (div_by_n) {
      Wk = ((nk-1)/n) * var(X[tmp,])
    } else {
      # R version / SPSS
      #Wk = ((nk-1)/(n-ng)) * var(X[tmp,])
      Wk = ((nk-1)/(n-1)) * var(X[tmp,])
    }
    Within = Within + Wk
  }
  # add names
  if (is.null(colnames(variables))) {
    var_names = paste("X", 1:ncol(X), sep="")
    dimnames(Within) = list(var_names, var_names)
  } else {
    dimnames(Within) = list(colnames(variables), colnames(variables))
  }
  # result
  Within
}
