linDA <- 
function(variables, group, prior = NULL, validation = NULL, 
                  learn = NULL, test = NULL, prob = FALSE)
{
  # Perform a linear discriminant analysis
  # variables: matrix or data.frame with explanatory variables
  # group: vector or factor with group membership
  # prior: vector of prior probabilities (NULL = proportions)
  # validation: NULL, "crossval", "learntest"
  # learn: vector of learn-set
  # test: vector of test-set
  # prob: logical indicating results in probability terms
  
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
  # how many obs in each group
  nobs_group = as.vector(table(y))
  # prior probabilities
  if (!is.null(prior))
  {
    if (!is.numeric(prior) || !is.vector(prior))
      stop("\n'prior' probabilities incorrectly defined")
    if (length(prior) != ng) 
      stop("\n'prior' probabilities don't match number of groups")
    if (any(prior > 1) || any(prior < 0))
      stop("'prior' probabilities must range between [0,1]")
    if (round(sum(prior), 5) != 1)
      stop("'prior' probabilities don't add to 1")
  } else {
    # prior as proportions
    prior = nobs_group / n
    props = prior
  }
  # group levels
  glevs = levels(y)
  
  ## linDA with no validation
  if (validation == "none") {
    get_linda = my_linDA(X, y, 1:n, 1:n, prior, prob)
    err = 1 - sum(diag(get_linda$conf)) / n
  }
  
  ## linDA with learn-test sets validation
  if (validation == "learntest")
  {
    if (any(learn) <= 0 || any(learn) > n)
      stop("\nsubscript out of bounds in 'learn' set")
    if (any(test) <= 0 || any(test) > n)
      stop("\nsubscript out of bounds in 'test' set")
    # apply linDA
    get_linda = my_linDA(X, y, learn, test, prior, prob)
    # misclassification error rate
    err = 1 - sum(diag(get_linda$conf))/length(test)
  }
  
  ## linDA with crossvalidation
  if (validation == "crossval")
  {
    # linDA for all observations
    get_linda = my_linDA(X, y, 1:n, 1:n, prior, prob)
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
      # apply linDA
      linda_cv = my_linDA(X, y, learn, test, prior, prob)
      # misclassification error rate
      mer = mer + sum(diag(linda_cv$conf))/n
    }
    # total misclassification error rate
    err = 1 - mer
  }
  
  ## specifications
  specs = list(n=n, p=p, ng=ng, glevs=glevs, 
               nobs_group=nobs_group, validation=validation)
  ## results
  structure(list(functions = get_linda$FDF, 
                 confusion = get_linda$conf,
                 scores = get_linda$Disc, 
                 classification = get_linda$pred_class,
                 error_rate = err,
                 specs = specs),
            class = "linda")
}
