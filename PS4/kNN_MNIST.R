# Author:       Uwe Hoenig
# Course:       15D012 - Advanced Computational Methods
# Last update:  02.02.16
# Type:         Problemset 4 - Exercise 1

### Clear workspace
#rm(list = ls())

### Load Packages 
if (!require("mvtnorm")) install.packages("mvtnorm"); library(mvtnorm)
if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)
if (!require("caret")) install.packages("caret"); library(caret)
if (!require("class")) install.packages("class"); library(class)
#let's load in the data (for me it's in the subfolder)
MNISTtrain=read.csv("MNIST-selected/MNIST_training.csv")
MNISTtest=read.csv("MNIST-selected/MNIST_test.csv")

labels=MNISTtrain$X4
features=MNISTtrain[,-1]

#Now run a test for the optimal k and p, I use my own kNN here
testing <- function(){
  performance=c()
  kp = c()
  for (p in 1:4){
    
      for (k in 1:8){#k>8 should give same results
        pred=kNN(features,labels,k,p)
        performance=c(performance,sum(labels==pred)/length(labels))
        kp = c(kp, paste(k, p))#create pairs or k and p
        #return(performance)
      }
  }
  return(kp[which.max(performance)])
}
testing()
###################BEST REUSLTS
#k=3,p=2
###################

#now let's apply the result (k=3,p=2) on the test data
predictedLabels=as.numeric(knn(features,MNISTtest,cl=labels,k=3,p=2))
output=as.data.frame(predictedLabels)
#and save the output as a numeric result
write.csv(output, file = "MNIST_predictions.csv", row.names = FALSE)

