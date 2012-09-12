plsDA <- 
function(variables, group, autosel = TRUE, comps = 2,
         validation = NULL, learn = NULL, test = NULL)
{
  # Perform a PLS discriminant analysis
  # variables: matrix or data.frame with explanatory variables
  # group: vector or factor with group membership
  # autosel: logical indicating automatic selection of PLS comps
  # comps: number of PLS components (only when autosel=FALSE)
  # validation: NULL, "crossval", "learntest"
  # learn: vector of learn-set
  # test: vector of test-set
  
  # check inputs
  verify_Xy = my_verify(variables, group, na.rm=FALSE)
  X = verify_Xy$X
  y = verify_Xy$y
  # autosel
  if (!is.logical(autosel)) autosel = TRUE
  # number of components
  if (!autosel) {
    if (mode(comps)!="numeric" || length(comps)!=1 || comps<=1 || (comps%%1)!=0)
      stop("\nInvalid argument 'comps' (number of components)")
  }
  # type of validation
  if (is.null(validation)) {
    validation = "none"
  } else {
    vali = validation %in% c("crossval", "learntest")
    if (!vali)
      stop("nIncorrect type of validation")
  }
  
  # how many observations and variables
  n = nrow(X)
  p = ncol(X)
  # how many groups
  ng = nlevels(y)
  # how many obs in each group
  nobs_group = as.vector(table(y))
  # group levels
  glevs = levels(y)
  
  ## plsDA with no validation
  if (validation %in% c("none","crossval")) {
    get_plsda = my_plsDA(X, y, 1:n, 1:n, autosel, comps)
    err = 1 - sum(diag(get_plsda$conf)) / n
  }
  
  ## plsDA with learn-test sets validation
  if (validation == "learntest")
  {
    if (any(learn) <= 0 || any(learn) > n)
      stop("\nsubscript out of bounds in 'learn' set")
    if (any(test) <= 0 || any(test) > n)
      stop("\nsubscript out of bounds in 'test' set")
    # apply plsDA
    get_plsda = my_plsDA(X, y, learn, test, autosel, comps)
    # misclassification error rate
    err = 1 - sum(diag(get_plsda$conf))/length(test)
  }
  
  ## specifications
  specs = list(n=n, p=p, ng=ng, glevs=glevs, 
               nobs_group=nobs_group, validation=validation)
  ## results
  structure(list(functions = get_plsda$coeffs, 
                 confusion = get_plsda$conf,
                 scores = get_plsda$Disc, 
                 classification = get_plsda$pred_class,
                 error_rate = err,
                 components = get_plsda$components,
                 Q2 = get_plsda$Q2T,
                 R2 = get_plsda$R2,
                 VIP = get_plsda$VIP,
                 comp_vars = get_plsda$cor_tx,
                 comp_group = get_plsda$cor_ty,
                 specs = specs),
            class = "plsda")
}
