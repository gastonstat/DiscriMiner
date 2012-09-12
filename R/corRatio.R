corRatio <-
function(variable, group)
{
  # Correlation ratio
  # variable: vector with explanatory variable
  # group: vector or factor with group memberships
  
  if (!is.numeric(variable)) 
    stop("\nSorry, 'variabe' must be a numeric vector")
  if (!is.factor(group)) group = as.factor(group)
  if (nlevels(group) == 1)
    stop("\nSorry, 'group' has only one category")
  # correlation ratio (eta)
  lm_temp = lm(variable ~ group)
  eta = summary(lm_temp)$r.squared
  # result
  eta
}
