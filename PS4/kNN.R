# Author:       Uwe Hoenig
# Course:       15D012 - Advanced Computational Methods
# Last update:  02.02.16
# Type:         Problemset 4 - Exercise 1

### Clear workspace
rm(list = ls())

### Load Packages 
if (!require("mvtnorm")) install.packages("mvtnorm"); library(mvtnorm)
if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)

#Tasks
#1 create a function called kNN [x]
#2 inputs at least: df/mat "features", vec "labels", k, p etc [x]
#3 verify inputs are of correct format use assertthat package [x]
#4 (vector of predicted labels), prob (vector of probabilities for predicted labels). [x]
#5 Function should be able to deal with multiple classes [x]
#6 test kNN on classmate's dataset and print result in csv and plot [x] 
#7 create decision boundary [x]
#8 MNST Task [x]


######################
#####Teesting#########
#testing with random data, performance obvi lowest :)
#labels=sample(c(0,1,2,3,4),20, replace=T)
#features= matrix(rnorm(40),nrow=20)
#k=3
#######################
#######################

#system.time(kNN())
kNN <- function(features, labels, k=3,p=2, testfeatures=matrix(100,10,10), predict=F){
  
  
  library(assertthat)
  library(ggplot2)
  
  # test the inputs
  assert_that(nrow(features) == length(labels))
  assert_that(is.data.frame(features) || is.matrix(features))
  assert_that(is.data.frame(testfeatures) || is.matrix(testfeatures))
  assert_that(is.vector(labels))
  not_empty(features); not_empty(labels)
  #############################################################################
  #############IF YOU ONLY WANT TO PREDICT ON EXISTING POINTS###############
  
  if(predict == F){
  #calculate the distance from each point to every other. Display as df
  dist_df=as.data.frame(as.matrix(dist(features,upper=T,p=p)))
  
  #k nearest neighbors:
  #I select the positions of the k-nearest neighbors of each point
  #from the dist_df dataframe. With this I can get the labels of the
  #neighbors from the label vector.
  
  #nearest is basically a list of vectors which contain the labels of all
  #the k neighbors.
  nearest=lapply(lapply(lapply(dist_df,order),function(x) x[2:(k+1)]),
                 function(x) labels[x])
  
  #get the majority lable of the k nearest neighbors
  modes=lapply(lapply(lapply(nearest,function(x) table(as.vector(x))),
                  function(x) names(x)[x == max(x)]), as.numeric)
  
  # This following part is my technique of making sure that 
  # I am extracting the right label (especially in case of a tie between
  # two or more labels.
  # Rule: if there are two (or more) predominating labels in my neighborhood
  # then I will select the label of the points that are closer to my point.
  # In addition I calculate the relative frecuency (probability) of that label
  # occuring among the whole k neighbors
  labeloutput=c()
  probability=c()
  for(i in (1:length(labels))) {
    
    labeloutput[i]=intersect(nearest[[i]],modes[[i]])[1]
    probability[i]=length(which(labeloutput[i]==nearest[[i]]))/k
  }
  
  #Create a result matrix with the desired columns
  result=data.frame(predLabels=labeloutput,prob=round(probability,2))
  
  #optional performance measure of my kNN
  performance=sum(labels==labeloutput)/length(labels)
  
  return(result)
  }
  
  #############################################################################
  #############################################################################
  #############################################################################
  #IF WE WANT TO TEST STH NOW
  if (predict==T){
    features=rbind(features,testfeatures) #concatenate training and test data
    dist_df=as.matrix(dist(features), upper=T,p=p) # create a sym. dist matrix
    dist_df=t(dist_df[(nrow(data)+1):(nrow(data)+nrow(testfeatures)),1:nrow(data)])
    #subset only the lower left submatrix, the one which starts from the rows where 
    #the grid or testpoints start, and have the same amount of columns as are in ur 
    #orig training data (I take transpose here because lapply works column wise)
    dist_df=as.data.frame(dist_df)#lapply works on df
    dist_df=lapply(dist_df,order)#i select the right order in ascending way
    
   
    #1)here I take my k nearest points and get their labels from the label vector 
    #  of my training set
    nearest=lapply(lapply(dist_df,function(x) x[1:k]),
                   function(x) labels[x])
    #2)now I select most common label, and take care of ties
    modes=lapply(lapply(lapply(nearest,function(x) table(as.vector(x))),
                        function(x) names(x)[x == max(x)]), as.numeric)
    
    #3)taking care of ties and making sure the right probability is computed
    labeloutput=c()
    probability=c()
    for(i in (1:length(labels))) {
      
      labeloutput[i]=intersect(nearest[[i]],modes[[i]])[1]
      probability[i]=length(which(labeloutput[i]==nearest[[i]]))/k
    }
    
    #Create a result matrix with the desired columns
    result=data.frame(predLabels=labeloutput,prob=round(probability,2))
  }
  return(result)#that's it - bit long and tons of minor debugging , but it's fast and gets
  #the job done
  #############################################################################
}

