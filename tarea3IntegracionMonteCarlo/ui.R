rm(list=ls())
if(!require(shiny)) install.packages("shiny")
set.seed(24082016)
library(shiny)

shinyUI(fluidPage(  
  h1("Tarea2 Integracion Monte Carlo"),
  h6("Angel Farid Fajardo Oroz"),
  h6("MCC ITAM"),
  h1(""),
  
  sidebarLayout(
    sidebarPanel(
      textInput(inputId="inpFunc", label="Funcion a evaluar",
                value="function (x) {4/(1+x^2)}"),
      
      sliderInput(inputId = "lmts", label="Limites de la inegral",
                  max=10, min=0, value=c(0,1)),
      
      sliderInput(inputId = "alfa", label="Intervalo de confianza",
                  max=0.1, min=0.01, value=0.05, step=0.01),
      
      sliderInput("n", 
                  "Number of random points:", 
                  value = 100,
                  min = 2, 
                  max = 1000)
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Simulacion MonteCarlo", plotOutput("plot")), 
                  tabPanel("Intervalos", plotOutput("intervals")),
                  tabPanel("Trapecio vs MonteCarlo", dataTableOutput("comparation"))
      )
    )
  )
))

#shiny::runGitHub("compstat2016", "farid7", subdir = "tarea3_integracionMonteCarlo")