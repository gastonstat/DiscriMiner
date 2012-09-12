classify <- 
function(DA_object, newdata)
{
  # classify new data using discrimination rule of a DA_object
  # DA_object: oject of class "discriminant analysis"
  # newdata: vector, matrix or data frame with variables
  
  ## Check arguments
  # make sure DA_object is valid
  DA_classes = c("geoda", "linda", "quada", "plsda", "disqual")
  class_da <- class(DA_object) %in% DA_classes
  if (!class_da)
    stop("\nInvalid 'DA_object'")
  # if vector then convert to matrix
  if (is.vector(newdata))
    newdata = t(as.matrix(newdata))
  # newdata matrix or data.frame
  if (is.null(dim(newdata))) 
    stop("\nSorry, 'newdata' is not a matrix")  
  # no missing values allowed
  if (length(complete.cases(newdata)) != nrow(newdata))
    stop("\nSorry, no missing values allowed in 'newdata'")  
  # check compatibility
  if (class(DA_object) != "disqual") 
  {
    if (ncol(newdata) != DA_object$specs$p)
      stop("\n'newdata' is not compatible with 'DA_object'")
  } else {
    # disqual method
    # newdata as binarized matrix (0s and 1s)
    if (!any(newdata %in% c(0,1)))
      stop("'newdata' must contain only 0's and 1's (binary format)")
    # number of variables
    if (dim(newdata)[1] == 1)
    {
      if (sum(newdata) != DA_object$specs$p)
        stop("\n'newdata' is not compatible with 'DA_object'")
    } else {
      if (any(rowSums(newdata) != DA_object$specs$p))
        stop("\n'newdata' is not compatible with 'DA_object'")
    }
  }
  
  # how many observations and variables
  n = nrow(newdata)
  p = DA_object$specs$p
  # how many groups
  ng = DA_object$specs$ng
  glevs = DA_object$specs$glevs
  
  ## geometric/linear/PLS discriminant analysis
  if (class(DA_object) %in% c("geoda","linda","plsda"))
  {
    # put newdata in right format
    if (is.data.frame(newdata)) newdata = as.matrix(newdata)
    ## constant terms and coefficients
    cons = DA_object$functions[1,]
    Betas = DA_object$functions[-1,]  
    # matrix of constant terms
    A = matrix(rep(cons,n), n, ng, byrow=TRUE)
    # apply discrim functions
    Disc = newdata %*% Betas + A
    dimnames(Disc) = list(1:n, glevs)
    # predicted class
    pred = apply(Disc, 1, function(u) which(u == max(u)))
    names(pred) = NULL
    # assign class values
    pred_class = factor(pred, levels=seq_along(glevs), labels=glevs)    
  }
  ## quadratic discriminant analysis
  if (class(DA_object) == "quada")
  {
    # put newdata in right format
    if (is.data.frame(newdata)) newdata = as.matrix(newdata)
    # discrimination matrix to store results
    Disc = matrix(0, n, ncol = ng)
    # ingredients
    GM = DA_object$GM
    WMqr = DA_object$WMqr
    ldet = DA_object$ldet
    prior = DA_object$prior
    # calculate distances (the lower, the better)
    for (k in 1:ng) 
    {
      # group means
      Xk = matrix(GM[k,], n, p, byrow = TRUE)
      dev = (newdata - Xk) %*% WMqr[[k]]
      Disc[,k] = 0.5 * rowSums(dev^2) + 0.5 * ldet[k] - log(prior[k])
      dimnames(Disc) = list(1:n, glevs)
    }
    # Disc in terms of probability
    Disc <- exp( -(Disc - apply(Disc, 1, min, na.rm=TRUE))) 
    # predicting classes
    pred = Disc / drop(Disc %*% rep(1, ng))
    # predicted class
    pred_class = factor(max.col(pred), levels=seq_along(glevs), labels=glevs)  
  }
  ## DISQUAL method
  if (class(DA_object) == "disqual")
  {
    ## coefficients (there is no constant term)
    Betas = DA_object$norm_coefs  
    # apply discrim functions
    Disc = newdata %*% Betas
    dimnames(Disc) = list(1:n, glevs)
    # predicted class
    pred = apply(Disc, 1, function(u) which(u == max(u)))
    names(pred) = NULL
    # assign class values
    pred_class = factor(pred, levels=seq_along(glevs), labels=glevs)
  }
  
  ## results
  res = list(scores=Disc, pred_class=pred_class)
}
