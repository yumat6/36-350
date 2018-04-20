generate_data = function(n,p){
  covariates = matrix(rnorm(n*p),n,p)
  responses = rnorm(n)
  return(list(covariates,responses))
}

model_select = function(covariates,responses,cutoff){
  cov.lm = lm(responses ~ covariates)
  p.values = summary(cov.lm)$coefficients[,4]
  if (length(covariates[covariates<=cutoff])==0){
    return(c())
  }
  cov.lm.2 = lm(responses ~ covariates[,covariates<=cutoff])
  p.values.2 = summary(cov.lm.2)$coefficients[,4]
  return(summary(cov.lm.2))
}

