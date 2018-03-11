add.output.noise <- function(n,d,lambda,epsilon){
  dir = rnorm(d)
  dir = dir/ sqrt(sum(dir^2))
  noise = 2/(epsilon*lambda*n) * sum(rexp(d, 1)) * dir
  return(noise)
}

DPLogisticRegressionClassifier <- function(y, x, alpha, lambda, epsilon = 0, mechanism = c("output", "objective")){
  library(glmnet)
  param.out = NULL
  if(mechanism == "output"){
    model= glmnet(x, y, alpha = alpha, lambda = lambda)
    param.out = as.matrix(coef(model))
    print(param.out)
    n = nrow(x)
    d = ncol(x) + 1 # plus one since model matrix doesn't include intercept matrix
    if(epsilon>0){
      noise = add.output.noise(n,d,lambda,epsilon)
      param.out = param.out + noise
    }
  }else if (mechanism == "objective"){
  }
  return(param.out)
}


# DPSupportVectorMachineClassifier <- function(){
#   library(e1071)
# }



# EXAMPLES
# library(ISLR)
# names(Hitters)
# test_lr = na.omit(Hitters)
# y = test_lr$Salary
# x = model.matrix(Salary~.,test_lr)[,-1]
# lambda = 0.1
# epsilon = 0.5
# alpha=0
# DPLogisticRegressionClassifier(y, x, alpha, lambda, epsilon = 0, mechanism = "output")
# DPLogisticRegressionClassifier(y, x, alpha, lambda, epsilon = 1, mechanism = "output")
# DPLogisticRegressionClassifier(y, x, alpha, lambda, epsilon = 2, mechanism = "output")

