desDA <-
function(variables, group, covar="within")
{
  # Perform a factorial discriminant analysis
  # variables: matrix or data.frame with explanatory variables
  # group: vector or factor with group membership
  # covar: character string indicating the covariance matrix
  
  # check inputs
  verify_Xy = my_verify(variables, group, na.rm=FALSE)
  check_cov <- covar %in% c("within", "total")
  if (!check_cov)
    warning("\nInvalid covar value; 'covar = within' is used")
  # get ingredients
  X = verify_Xy$X
  y = verify_Xy$y
  
  # how many obs and variables
  n = nrow(X)
  p = ncol(X)
  # group levels and number of levels
  glevs = levels(y)
  ng = nlevels(y)
  # how many obs in each group
  nk = as.vector(table(y))
  # number of factors
  nf = min(p, ng-1)
  # global mean
  gm = colMeans(X)
  
  # total covariance
  V = var(X)
  # between-class covariance matrix
  B = my_betweenCov(X, y)
  # estimated within-class covariance matrix
  W = my_withinCov(X, y)
  # within-class covariance matrix for disc-power
  Ww = matrix(0, p, p)
  for (k in 1:ng)
  {
    tmp <- y == glevs[k]
    nk = sum(tmp)
    Wk = ((nk-1)/(n-1)) * var(X[tmp,])
    Ww = Ww + Wk
  }
    
  ## Discriminant importance of explanatory variables
  # F-statistics
  F_statistic = ((n-ng)/(ng-1)) * (diag(B) / diag(Ww))
  p_values = 1 - pf(F_statistic, ng-1, n-ng)
  # Wilk's lambdas
  wilks_lamb = diag(Ww / V)
  # correlation ratio
  cor_ratio = diag(B) / diag(V)
  # table of disc power
  tab1 = cbind(cor_ratio, wilks_lamb, F_statistic, p_values)
  
  ## Discriminant axes
  # select covariance matrix
  if (covar == "within") Cov = W else Cov = ((n-1)/n) * var(X)
  # group means matrix
  GM = groupMeans(X, y)
  # decomposing between-class matrix:  B = CC'
  GM_gm = sweep(GM, 1, gm, FUN="-")
  C = sweep(GM_gm, 2, sqrt(nk/n), FUN="*")
  # eigen-decomposition
  EIG = eigen(t(C) %*% solve(Cov) %*% C)
  # eigenvalues
  lam = EIG$values[1:nf]
  # eigenvectors
  U = solve(V) %*% C %*% EIG$vectors[,1:nf]
  # normalizing eigenvectors
  aux = sqrt(diag(t(U) %*% Cov %*% U))
  Unorm = sweep(U, 2, aux, FUN="/")
  # table of eigenvalues
  tab2 = cbind(lam, 100*lam/sum(lam), 100*cumsum(lam)/sum(lam))
  colnames(tab2) = c("value", "proportion", "accumulated")
  rownames(tab2) = paste("DF", 1:nf, sep="")
  
  # Linear Discriminant Functions
  alphas = (-1) * gm %*% Unorm
  tab3 = rbind(alphas, Unorm)
  rownames(tab3) = c("constant", colnames(X))
  colnames(tab3) = paste("DF", 1:nf, sep="")
  
  # factor coordinates and correlations with expl variables
  Fs = X %*% Unorm + matrix(rep(alphas,each=n), n, nf)
  colnames(Fs) = paste("z", 1:nf, sep="")
  tab4 = cor(X, Fs)
  colnames(tab4) = paste("DF", 1:nf, sep="")
  
  # results
  structure(list(power = tab1, 
                 values = tab2, 
                 discrivar = tab3, 
                 discor = tab4, 
                 scores = Fs),
            class = "desda")
}
