### 2a
generate_data = function(n, p) {
  responses = rep(rnorm(1),3^2)
  covariates = matrix(responses,n,n)
  return(list(covariates=covariates,responses=responses))
}

### 2b
model_select = function(covariates, responses, cutoff) {
  linearModel = lm(responses ~ covariates)
  linearModel = linearModel[which((summary(linearModel)$coefficients[c("responses","covariates"),4] <= cutoff))]
  if(length(linearModel) == 0) return(vector())
  newLinearModel = lm(linearModel ~ covariates)
  return(summary(newLinearModel)$coefficients[c("linearModel","covariates"),4])
}

# - **2c.** Write a function `run_simulation(n_trials, n, p, cutoff)`
# which uses the previous two functions to run `n_trials`
# simulations which uses data from `generate_data` in
# `model_select`, collects the returned p-values and displays a
# histogram of the p-values. Under the null hypothesis (that the
#                                                       regression coefficients are zero) these p-values should be
# uniformly distributed between 0 and 1; does this seem to be the
# case? Create and save figures for all combinations of `n = c(100,
#                                                              1000, 10000)`, `p = c(10, 20, 50)` and set `n_trials = 1000` and
# `cutoff = 0.05`. Don't include the figures in the commit, only the
# code. *HINT*: Write a `for` loop; alternatively the functions
# `expand.grid` and `m_ply` may prove useful.

### 2c
run_simulation = function(n_trials, n, p, cutoff) {
  for(i in 1:length(n)) {
    simulation1 = generate_data(n[1],p[1])
    simulation1 = model_select(simulation[[1]],simulation[[2]],cutoff)
    simulation2 = generate_data(n[2],p[2])
    simulation2 = model_select(simulation[[1]],simulation[[2]],cutoff)
    simulation3 = generate_data(n[3],p[3])
    simulation3 = model_select(simulation[[1]],simulation[[2]],cutoff2
  }
  return(list(simulation1,simulation2,simulation3))
}

plots = run_simulation(1000,n=c(100,1000,10000),p=c(10,20,50),cutoff=0.05)
for (i in 1:3){
  plot(plots[[i]])
}
