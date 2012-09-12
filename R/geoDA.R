geoDA <- 
function(variables, group, validation = NULL, learn = NULL, test = NULL)
{
  # Perform a geometric predictive discriminant analysis
  # variables: matrix or data.frame with explanatory variables
  # group: vector or factor with group membership
  # validation: NULL, "crossval", "learntest"
  # learn: vector of learn-set
  # test: vector of test-set
  
  # check inputs
  verify_Xy = my_verify(variables, group, na.rm=FALSE)
  X = verify_Xy$X
  y = verify_Xy$y
  # type of validation
  if (is.null(validation)) {
    validation = "none"
  } else {
    vali = validation %in% c("crossval", "learntest")
    if (!vali)
      stop("nIncorrect type of validation")
  }
  
  # how many observations
  n = nrow(X)
  # how many variables
  p = ncol(X)
  # how many groups
  ng = nlevels(y)
  glevs = levels(y)
  # how many obs in each group
  nobs_group = as.vector(table(y))
  # proportions
  props = nobs_group / n
  
  ## geoDA with no validation
  if (validation == "none") {
    get_geoda = my_geoDA(X, y, 1:n, 1:n)
    err = 1 - sum(diag(get_geoda$conf)) / n
  }
  
  ## geoDA with learn-test sets validation
  if (validation == "learntest")
  {
    if (any(learn) <= 0 || any(learn) > n)
      stop("\nsubscript out of bounds in 'learn' set")
    if (any(test) <= 0 || any(test) > n)
      stop("\nsubscript out of bounds in 'test' set")
    # apply linDA
    get_geoda = my_geoDA(X, y, learn, test)
    # misclassification error rate
    err = 1 - sum(diag(get_geoda$conf))/length(test)
  }
  
  ## geoDA with crossvalidation
  if (validation == "crossval")
  {
    # geoDA for all observations
    get_geoda = my_geoDA(X, y, 1:n, 1:n)
    # elements in each group 
    elems_group = vector("list", ng)
    for (k in 1:ng) {
      elems_group[[k]] = which(group == glevs[k])
    }
    # misclassification error rate
    mer = 0
    # 10 crossvalidation samples
    for (r in 1:10)
    {
      test = vector("list", ng)
      test_sizes = floor(n * props / 10)
      for (k in 1:ng) {
        test[[k]] = sample(elems_group[[k]], test_sizes[k])
      }
      test = unlist(test)
      learn = (1:n)[-test]
      # apply DA
      geoda_cv = my_geoDA(X, y, learn, test)
      # misclassification error rate
      mer = mer + sum(diag(geoda_cv$conf))/n
    }
    # total misclassification error rate
    err = 1 - mer
  }
  
  ## specifications
  specs = list(n=n, p=p, ng=ng, glevs=glevs, 
               nobs_group=nobs_group, validation=validation)
  ## results
  structure(list(functions = get_geoda$FDF, 
                 confusion = get_geoda$conf,
                 scores = get_geoda$Disc, 
                 classification = get_geoda$pred_class,
                 error_rate = err,               
                 specs = specs),
            class = "geoda")
}
