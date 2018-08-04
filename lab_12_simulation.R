### 2a
generate_data = function(n, p) {
  responses = rep(rnorm(1),3^2)
  covariates = matrix(responses,n,n)
}

### 2b
model_select = function(covariates, responses, cutoff) {
  linearModel = lm(responses ~ covariates)
  linearModel = linearModel[which((summary(linearModel)$coefficients[c("responses","covariates"),4] <= cutoff))]
}
