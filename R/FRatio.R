FRatio <-
function(variable, group)
{
  # F ratio (anova)
  # variable: vector with explanatory variable
  # group: vector or factor with group memberships
  
  if (!is.numeric(variable)) 
    stop("\nSorry, argument 'x' must be a numeric vector")
  if (!is.factor(group)) group = as.factor(group)
  if (nlevels(group) == 1)
    stop("\nSorry, 'group' has only one category")
  # correlation ratio (eta)
  Ftest_temp = oneway.test(variable ~ group, var.equal=TRUE)
  fstat = Ftest_temp$statistic
  pval = Ftest_temp$p.value
  # result
  c(fstat, p_value=pval)
}
