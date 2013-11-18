#' Discriminant Analysis on Qualitative Variables
#' 
#' Implementation of the DISQUAL methodology. Disqual performs a Fishers
#' Discriminant Analysis on components from a Multiple Correspondence Analysis
#' 
#' When \code{validation=NULL} there is no validation \cr When
#' \code{validation="crossval"} cross-validation is performed by randomly
#' separating the observations in ten groups. \cr When
#' \code{validation="learntest"} validationi is performed by providing a
#' learn-set and a test-set of observations. \cr
#' 
#' @param variables data frame with qualitative explanatory variables (coded as
#' factors)
#' @param group vector or factor with group memberships
#' @param validation type of validation, either \code{"crossval"} or
#' \code{"learntest"}. Default \code{NULL}
#' @param learn optional vector of indices for a learn-set. Only used when
#' \code{validation="learntest"}. Default \code{NULL}
#' @param test optional vector of indices for a test-set. Only used when
#' \code{validation="learntest"}. Default \code{NULL}
#' @param autosel logical indicating automatic selection of MCA components
#' @param prob probability level for automatic selection of MCA components.
#' Default \code{prob = 0.05}
#' @return An object of class \code{"disqual"}, basically a list with the
#' following elements:
#' @return \item{raw_coefs}{raw coefficients of discriminant functions}
#' @return \item{norm_coefs}{normalizaed coefficients of discriminant functions,
#' ranging from 0 - 1000}
#' @return \item{confusion}{confusion matrix}
#' @return \item{scores}{discriminant scores for each observation}
#' @return \item{classification}{assigned class}
#' @return \item{error_rate}{misclassification error rate}
#' @author Gaston Sanchez
#' @seealso \code{\link{easyMCA}}, \code{\link{classify}},
#' \code{\link{binarize}}
#' @references Lebart L., Piron M., Morineau A. (2006) \emph{Statistique
#' Exploratoire Multidimensionnelle}. Dunod, Paris.
#' 
#' Saporta G. (2006) \emph{Probabilites, analyse des donnees et statistique}.
#' Editions Technip, Paris.
#' 
#' Saporta G., Niang N. (2006) Correspondence Analysis and Classification. In
#' \emph{Multiple Correspondence Analysis and Related Methods}, Eds. Michael
#' Greenacre and Jorg Blasius, 371-392. Chapman and Hall/CRC
#' @export
#' @examples
#' 
#'   \dontrun{
#'   # load insurance dataset
#'   data(insurance)
#' 
#'   # disqual analysis with no validation
#'   my_disq1 = disqual(insurance[,-1], insurance[,1], validation=NULL)
#'   my_disq1
#'   
#'   # disqual analysis with cross-validation
#'   my_disq2 = disqual(insurance[,-1], insurance[,1], validation="crossval")
#'   my_disq2
#'   }
#' 
disqual <- 
function(variables, group, validation = NULL, 
         learn = NULL, test = NULL, autosel = TRUE, prob = 0.05)
{
  # Perform discriminant analysis on qualitative variables
  # variables: data frame with categorical explanatory variables
  # group: vector or factor with group membership
  # validation: NULL, "crossval", "learntest"
  # learn: vector of learn-set
  # test: vector of test-set
  # autosel: logical indicating automatic selection of MCA comps
  # prob: probability level for automatic selection
  
  ## check inputs
  verify_Xy = my_verify(variables, group, qualitative=TRUE, na.rm=FALSE)
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
  # probability value
  if (!is.logical(autosel)) 
    stop("\nargument 'autosel' incorrectly defined")
  if (autosel) {
    if (prob < 0 || prob >= 1)
      stop("\nargument 'prob' must be between range [0,1)")    
  }
  
  # how many observations and variables
  n = nrow(X)
  p = ncol(X)
  # how many groups
  ng = nlevels(y)
  glevs = levels(y)
  # how many obs in each group
  nobs_group = as.vector(table(y))
  # proportions
  props = nobs_group / n
  
  ## catDA with no validation
  if (validation == "none") {
    get_catda = my_catDA(X, y, 1:n, 1:n, autosel, prob)
    err = 1 - sum(diag(get_catda$conf)) / n
  }
  
  ## catDA with learn-test sets validation
  if (validation == "learntest")
  {
    if (any(learn) <= 0 || any(learn) > n)
      stop("\nsubscript out of bounds in 'learn' set")
    if (any(test) <= 0 || any(test) > n)
      stop("\nsubscript out of bounds in 'test' set")
    # apply DA
    get_catda = my_catDA(X, y, learn, test, autosel, prob)
    # misclassification error rate
    err = 1 - sum(diag(get_catda$conf))/length(test)
  }
  
  ## catDA with crossvalidation
  if (validation == "crossval")
  {
    # catDA for all observations
    get_catda = my_catDA(X, y, 1L:n, 1L:n, autosel, prob)
    # elements in each group 
    elems_group = vector("list", ng)
    for (k in 1L:ng) {
      elems_group[[k]] = which(group == glevs[k])
    }
    # misclassification error rate
    mer = 0
    # 10 crossvalidation samples
    for (r in 1L:10)
    {
      test = vector("list", ng)
      test_sizes = floor(n * props / 10)
      for (k in 1L:ng) {
        test[[k]] = sample(elems_group[[k]], test_sizes[k])
      }
      test = unlist(test)
      learn = (1L:n)[-test]
      # resample if learn set is not good for crossvalidation
      bad_learn_sample = test_bad_learn_set(X[learn,])
      while (bad_learn_sample)
      {
        test[[k]] = sample(elems_group[[k]], test_sizes[k])
        test = unlist(test)
        learn = (1L:n)[-test]
        bad_learn_sample = test_bad_learn_set(X[learn,])        
      }
      # apply DA
      catda_cv = my_catDA(X, y, learn, test, autosel, prob)
      # misclassification error rate
      mer = mer + sum(diag(catda_cv$conf))/n
    }
    # total misclassification error rate
    err = 1 - mer
  }
  
  ## specifications
  specs = list(n=n, p=p, ng=ng, glevs=glevs, 
               nobs_group=nobs_group, validation=validation)
  # results
  structure(list(raw_coefs = get_catda$Raw, 
                 norm_coefs = get_catda$Norm,
                 confusion = get_catda$conf, 
                 scores = get_catda$Disc,
                 classification = get_catda$pred_class,
                 error_rate = err,
                 specs = specs),
            class = "disqual")
}


#' @title Test Bad Learning Set in Disqual
#' @description Test Bad Learning Set in Disqual
#' @param learn_data learning dataset
#' @return whether the learn data has constant values
#' @keywords internal
#' @export
test_bad_learn_set <- function(learn_data)
{    
  # build super-indicator matrix Z
  Z = my_tdc(learn_data)   
  # number of obs per category
  nopc = colSums(Z)
  # are there any zeros?
  if (any(nopc == 0)) TRUE else FALSE
}
