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
  return(p.values.2)
}

run_simulation = function(n_trial=1, n, p, cutoff=0.05){
  n=c(100,1000,10000)
  p=c(10,20,50)
  for (i in 1:length(n_trials)){
    for (j in 1:3){
      for (k in 1:3){
        ml = model_select(generate_data(n[j],p[k])$covariates,generate_data(n[j],p[k])$responses,cutoff)
        saveRDS(ml,file="p-values.rds")
      }
    }
  }
}

make_plot = function(datapath){
  hist(load("p-values.rds"))
}

