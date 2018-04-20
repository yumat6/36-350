generate_data = function(n,p){
  covariates = matrix(rnorm(n*p),n,p)
  responses = rnorm(n)
  return(list(covariates,responses))
}
