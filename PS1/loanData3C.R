# Author:       Uwe Hoenig
# Course:       15D012 - Advanced Computational Methods
# Last update:  15.01.16
# Type:         Problemset 1 - Exercise 2

### Clear workspace
rm(list = ls())

### Load Packages 
if (!require("mvtnorm")) install.packages("mvtnorm"); library(mvtnorm)
if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)

### Initialize auxilliary functions

# Create covariance matrix
sigmaXY <- function(rho, sdX, sdY) {
  covTerm  <- rho * sdX * sdY
  VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2),
                     2, 2, byrow = TRUE)
  return(VCmatrix)
}

# Generate bivariate normal data 
genBVN <- function(n = 1, seed = NA, muXY=c(0,1), sigmaXY=diag(2)) {
  if(!is.na(seed)) set.seed(seed)
  rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
  return(rdraws)
}
#####################################
#####################################
#Task 2

loanData <- function(noApproved, noDenied, noUndecided,
                     muApproved, muDenied, muUndecided,
                     sdApproved, sdDenied, sdUndecided,
                     rhoApproved, rhoDenied, rhoUndecided, 
                     seed=1111) 
  {
  sigmaApproved <- sigmaXY(rho=rhoApproved, sdX=sdApproved[1], sdY=sdApproved[2])
  sigmaDenied <- sigmaXY(rho=rhoDenied, sdX=sdDenied[1], sdY=sdDenied[2])
  sigmaUndecided <- sigmaXY(rho=rhoUndecided, sdX=sdUndecided[1], sdY=sdUndecided[2])
  
  approved <- genBVN(noApproved, muApproved, sigmaApproved, seed = seed)
  denied <- genBVN(noDenied, muDenied, sigmaDenied, seed = seed+1)
  undecided <- genBVN(noUndecided, muUndecided, sigmaUndecided, seed = seed+2)

  loanDf <- as.data.frame(rbind(approved,denied, undecided))
  status <- c(rep("Approved", noApproved), rep("Denied", noDenied), rep("Undecided", noUndecided))
  target1 <- c(rep(1, noApproved), rep(0, noDenied),rep(0, noUndecided)) 
  target2 <- c(rep(0, noApproved), rep(1, noDenied),rep(0, noUndecided))
  target3 <- c(rep(0, noApproved), rep(0, noDenied),rep(1, noUndecided))
  
  loanDf <- data.frame(loanDf, status, target1, target2, target3)
  colnames(loanDf) <- c("PIratio", "solvency", "status", "target1", "target2", "target3")
  return(loanDf) 
}

#Create Dataset

noApproved <- 50
noDenied   <- 50
noUndecided  <- 50  

loanDf <- loanData(noApproved, noDenied, noUndecided,
                   c(10, 150), c(13, 100), c(11, 250),
                   c(2,  10), c( 2,  5), c( 3,  15),
                   -0.5, 0.3, 0.5)

#create three regressions to get the slopes and intercepts
datafit1 <- lm(target1 ~ solvency + PIratio , data=loanDf)
#summary(datafit1)
weights1 <- coef(datafit1)[c("solvency", "PIratio")]
bias1 <- coef(datafit1)[1]

datafit2 <- lm(target2 ~ solvency + PIratio , data=loanDf)
#summary(datafit2)
weights2 <- coef(datafit2)[c("solvency", "PIratio")]
bias2 <- coef(datafit2)[1]

datafit3 <- lm(target3 ~ solvency + PIratio , data=loanDf)
#summary(datafit3)
weights3 <- coef(datafit3)[c("solvency", "PIratio")]
bias3 <- coef(datafit3)[1]

#now create my line functions which i will later use in my ggplot with geom_abline

intercept1 <- (-bias1 + 0.5)/weights1["PIratio"] 
slope1 <- -(weights1["solvency"]/weights1["PIratio"])

intercept2 <- (-bias2 + 0.5)/weights2["PIratio"] 
slope2 <- -(weights2["solvency"]/weights2["PIratio"])

intercept3 <- (-bias3 + 0.5)/weights3["PIratio"] 
slope3 <- -(weights3["solvency"]/weights3["PIratio"])

##let's create another way of getting our regression coefficients

X <- as.matrix(cbind(ind=rep(1, nrow(loanDf)), loanDf[,c("PIratio", "solvency")]))
Y <- cbind(target1 = c(rep(1, noApproved), rep(0, noDenied),rep(0, noUndecided)),
           target2 = c(rep(0, noApproved), rep(1, noDenied),rep(0, noUndecided)),
           target3 = c(rep(0, noApproved), rep(0, noDenied),rep(1, noUndecided)))

#weights
W <- solve(t(X)%*%X) %*% t(X) %*% Y
predictions <- X %*% W
largest=apply(predictions, 1, max)
#let's label accordingly
label = rep(NA,nrow(loanDf))

label1 = (predictions==apply(predictions, 1, max))[,1]
label = ifelse(label1,"Approved",label)

label2 = (predictions==apply(predictions, 1, max))[,2]
label = ifelse(label2,"Denied",label)

label3 = (predictions==apply(predictions, 1, max))[,3]
label = ifelse(label3,"Undecided",label)

predictions <- cbind(predictions, label=label)

#let's analyze the certainty of the predictions
confMatrixFreq <- table(loanDf$status, label)
confMatrixFreq

confMatrixProp <- prop.table(confMatrixFreq, 1)
confMatrixProp


#now put all things together

######Output

# Prepare final dataframe
loanDf <- cbind(loanDf[,c('PIratio','solvency','status')],predicted=label, probability=largest)

# Export dataset
write.table(loanDf, file = "predictions.csv",row.names=FALSE, na="",col.names=TRUE, sep=";") 

# Plot result
pdf("discFunction3C.pdf")
ggplot(data = loanDf, 
       aes(x = solvency, y = PIratio,colour=status,fill=status)) + 
  geom_point() +
  xlab("solvency") +
  ylab("PI ratio") +
  theme_bw()+
  geom_abline(intercept = intercept1, slope = slope1)+
  geom_abline(intercept = intercept2, slope = slope2)+
  geom_abline(intercept = intercept3, slope = slope3)
dev.off()
###################################