# Author:       Uwe Hoenig
# Course:       15D012 - Advanced Computing
# Last update:  29.01.16
# Type:         Problemset 2 - Shiny 

shinyUI( pageWithSidebar(
  headerPanel('Interactive Loan Data'),
  sidebarPanel( numericInput("muPIa","PI mean approved",3),
                numericInput("muSola","Sol mean approved",100),
                numericInput("sdPIa","PI sd approved",1),
                numericInput("sdSola","Sol sd approved",15),
                numericInput("muPId","PI mean denied",12),
                numericInput("muSold","Sol mean denied",120),
                numericInput("sdPId","PI sd denied",3),
                numericInput("sdSold","Sol sd denied",20)
  ),
  
  mainPanel(
    plotOutput("plot"),
    verbatimTextOutput("Confusion")
  )
)
)  
