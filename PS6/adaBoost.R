# Author:       Uwe Hoenig and help of classmates
# Course:       Advanced Computing
# Last update:  07.03.16
# Type:         PS6

adaBoost <- function(formula,data,depth,noTrees){
  library(rpart)

  y <- data[,all.vars(formula)[1]] #access the target variable and y
  
  y <- ifelse(y==1,1,-1)

  matDamon <- vector(mode = "list",length = noTrees)
  w <- rep(1/nrow(data),nrow(data)) # start out with equal weights in the beginning
  alpha <- rep(NA, noTrees)  # quality of each tree
  predictions <- matrix(NA,nrow = nrow(data), ncol = noTrees) # store prediction labels for each tree here
  
  
  error <- rep(NA,noTrees) #  global/total error
  
  
  environment(formula) <- environment() #making sure the formula is accessible in local env.
  
  
  for(i in 1:noTrees){
    dt <- rpart(formula=formula, data = data, weights = w, method = "class",
                control = list(maxdepth = depth))
    matDamon[[i]] <- dt # here is where we store each model for the corresponding iteration
    predictions[,i] <- predict(dt,newdata = data,type="class") 
    predictions[,i] <- ifelse(predictions[,i]==2,1,-1) #retransform possible mislabels back to our scale
  
      err <- sum(w*(predictions[,i]!=y)) / sum(w) 
    
      alpha[i] <- log((1-err)/err) #loss function(value)
    
      w <- w * exp(alpha[i] * (predictions[,i]!=y)) #readjusting the weights
    pred <- sign(rowSums(t(alpha[1:i]*t(predictions[,1:i]))))
    error[i] <- 1 - sum(pred==y)/length(y)
  
  }
  
  predLabels <- sign(rowSums(t(alpha*t(predictions))))
  
  return(list(predLabels=predLabels,dtmat=matDamon,trainerror=error,alpha=alpha))
}


