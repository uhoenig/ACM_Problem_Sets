# Author:       Uwe Hoenig
# Course:       15D012 - Advanced Computing
# Last update:  29.01.16
# Type:         Problemset 2 - Shiny 

library(mvtnorm)
library(ggplot2)
shinyServer(function(input, output) {
  # generate Covariance matrix 
  ####
  sigmaXY <- function(rho, sdX, sdY) {
    covTerm <- rho * sdX * sdY
    VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2), 
                       2, 2, byrow = TRUE)
    return(VCmatrix)
  }
  #generate Random variables
  genBVN <- function(n = 1, seed = NA, muXY=c(0,1), sigmaXY=diag(2)) {
    if(!is.na(seed)) set.seed(seed)
    rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
    return(rdraws)
  }
  #create a dataframe with loandata
  loanData <- function(noApproved=50, noDenied=50, muApproved, muDenied, sdApproved,
                       sdDenied, rhoApproved=-0.1, rhoDenied=0.6, seed=1111) {
    
    sigmaApproved <- sigmaXY(rho=rhoApproved, sdX=sdApproved[1], sdY=sdApproved[2])
    sigmaDenied <- sigmaXY(rho=rhoDenied, sdX=sdDenied[1], sdY=sdDenied[2])
    approved <- genBVN(noApproved, muApproved, sigmaApproved, seed = seed)
    denied <- genBVN(noDenied, muDenied, sigmaDenied, seed = seed+1)
    loanDf <- as.data.frame(rbind(approved,denied))
    deny <- c(rep("Approved", noApproved), rep("Denied", noDenied))
    target = c(rep(0, noApproved), rep(1, noDenied))
    loanDf <- data.frame(loanDf, deny, target)
    colnames(loanDf) <- c("PIratio", "solvency", "deny", "target")
    return(loanDf)
    
  }
  
  
  #get the input from the interface
  selectedData <- reactive({
    
    muApproved <- c()
    muApproved[1] <- input$muPIa
    muApproved[2] <- input$muSola
    
    muDenied <- c()
    muDenied[1] <- input$muPId
    muDenied[2] <- input$muSold
    
    sdApproved <- c()
    sdApproved[1] <- input$sdPIa
    sdApproved[2] <- input$sdSola
    
    sdDenied <- c()
    sdDenied[1] <- input$sdPId
    sdDenied[2] <- input$sdSold
    
    df <- loanData(muApproved=muApproved,muDenied=muDenied,
                    sdApproved=sdApproved,sdDenied=sdDenied)
    
    return(df)        
  })
  
  #now generate the plot
  generatePlot <- function(){
    loanDf <- data.frame(selectedData())
    
    datafit <- lm(target ~ solvency + PIratio + 1, data=loanDf)
    weights <- coef(datafit)[c("solvency", "PIratio")]
    bias <- coef(datafit)[1]
    intercept <- (-bias + 0.5)/weights["PIratio"]
    slope <- -(weights["solvency"]/weights["PIratio"])
    
    plot <- ggplot(data = loanDf, aes(x = solvency, y = PIratio,
                                      colour=deny, fill=deny)) +
      geom_point() +
      xlab("solvency") +
      ylab("Weight") +
      theme_bw() +
      geom_abline(intercept = intercept, slope = slope)
    
    return(plot)
  }
  
  output$plot <- renderPlot({
    generatePlot()}
  )
  
  
  #now let us create the confusion matrix
  generateMatrix <- function(){
    loanDf <- data.frame(selectedData())
    
    loanDf <- cbind(loanDf,
                    target1 = c(rep(0, 50), rep(1, 50)),
                    target2 = c(rep(1, 50), rep(0, 50))
    )
    
    X <- as.matrix(cbind(ind=rep(1, nrow(loanDf)),
                         loanDf[,c("PIratio", "solvency")]))
    Y <- cbind(target1 = c(rep(0, 50), rep(1, 50)),
               target2 = c(rep(1, 50), rep(0, 50))
    )
    weightsOptim <- solve(t(X)%*%X) %*% t(X) %*% Y
    
    predictions <- X %*% weightsOptim
    
    denied <- (predictions==apply(predictions, 1, max))[,1]
    
    predictedLabels <- ifelse(denied, "Denied", "Approved")
    
    confMatrixFreq <- table(loanDf$deny, predictedLabels)
    
    return(confMatrixFreq)
  }
  
  output$Confusion <- renderPrint({
    generateMatrix()
  })
  
}) 
