#rm(list=ls())
if(!require(shiny)) install.packages("shiny")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(DT)) install.packages("DT")
library(ggplot2)
library(DT)
library(shiny)
#setwd("/Users/LUSI_ITAM/Documents/farid/estadisticaComputacional_Clase/tareas/tarea4_setupReaingTable")
setwd("/home/farid/Documents/estadistica computacional 2016b/tarea4_despligaDatos")
data <- read.csv(file="cheese.csv", header=T)

Taste <- data$taste
data <- data[, !names(data) %in% c("id","taste")]

set.seed(28112016)
n = 100
A <- 1
B <- 5
C <- 3
x <- seq(-10, 10, length.out = n)
y <- A*x + B + rnorm(n, mean=0, sd=C)
###########################################################
#declaring some functions##################################
loglikelihood <- function(x, y, theta){
  a   <- theta[1]
  b   <- theta[2]
  std <- theta[3]
  
  yy <- a*x+b
  singleLikelihood <- dnorm(y, mean=yy, sd=std, log=T)
  sumll <- sum(singleLikelihood)
  return(sumll)
}

logprior <- function(theta){
  a   <- theta[1]
  b   <- theta[2]
  std <- theta[3]
  
  aa <- dunif(a, min=0, max=50, log=T)
  bb <- dnorm(b, sd=5, log=T)     #jump size
  std0 <- dunif(std, min=0, max=50, log=T)
  return(aa+bb+std0)
}

logposteriori <- function(x, y, theta){
  return(loglikelihood(x, y, theta) + logprior(theta))
}

proposal <- function(theta){
  a   <- theta[1]
  b   <- theta[2]
  std <- theta[3]
  #sd is jump size
  return(rnorm(3, mean=c(a,b,std), sd=c(0.1, 0.5, 0.3)))
}

runMCMC <- function(x, y, startValue, iterations){
  chain <- array(dim = c(iterations+1,3))
  chain[1,] <- startValue
  for (i in 1:iterations){
    prop <- proposal(chain[i,])
    probab <- exp(logposteriori(x,y,prop)- logposteriori(x,y,chain[i,]))
    if(runif(1) < probab){
      chain[i+1, ] = prop
    } else {
      chain[i+1, ] = chain[i, ]
      #i <- i-1
    }
  }
  return(chain)
}
############################################################################
############################################################################
ui <- fluidPage(
  titlePanel("Basic DataTable"),
  
  sidebarLayout(
    sidebarPanel(
     checkboxGroupInput("cVariables", h3("Variables"),
                              choices = names(data)),
     numericInput("nCadenas", "cadenas a simular", value=1, min=1, max=10, step=1),
     sliderInput("sLongitud", "longitud de cadenas", min=1000, max=10000, value=1000),
     
     h4("Parámetros aPriori"),
     sliderInput("a", "a", min=1, max=10, value=5),
     sliderInput("sSigma", "sigma", min=1, max=10, value=5),
     sliderInput("e", "error", min=1, max=10, value=5)
  ),
  
  mainPanel("Tarea 4",
  fluidRow(
    splitLayout(cellWidths=c("50%", "50%"),DT::dataTableOutput("table"), plotOutput("Graph1")),
    splitLayout(cellWidths=c("50%", "50%"),DT::dataTableOutput("table2"))
    ),
  plotOutput("gPriori")
   )
  )
)

server <- function(input, output) {
  n <- dim(data)[1]
  
  dataInput <- reactive({
    if(is.null(input$cVariables))
      return()
    aux <- cbind(Taste, data[, input$cVariables])
    aux
  })
  
  chain <- reactive({
    if(is.null(input$cVariables))
      return()
    else{
      theta0 <- c(1,1,1)
      
      temp <- dataInput()
      chain <- runMCMC(x=data[, input$cVariables[1]], y=Taste, startValue=theta0, iterations=input$sLongitud)
      #chain <- aux1
      return(chain)
    }
  })
  
  nmes <- renderText({
    input$cVariables
    })
  
   output$table <- DT::renderDataTable(DT::datatable({
     if(is.null(input$cVariables))
       return()
    else {
      return(chain())
      }
     
  }))
   
   output$table2 <- DT::renderDataTable(DT::datatable({
     if(is.null(input$cVariables))
       return()
     else 
       return(dataInput())
     
   }))
   
   
   output$Graph1 <- renderPlot({
     if(is.null(input$cVariables))
       return()
     else{
      return(plot(dataInput()))
     }
   })
   
 output$gPriori <- renderPlot({
   #aux <- runMCMC(x, y, theta0, 10000)
   hist(chain()[,1], title=paste("MCMC"))
 }) 
 
}

shinyApp(ui = ui, server = server)
