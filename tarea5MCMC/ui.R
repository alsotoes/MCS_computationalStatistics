rm(list=ls())
if(!require(shiny)) install.packages("shiny")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(DT)) install.packages("DT")
library(ggplot2)
library(DT)
library(shiny)
#setwd("/Users/LUSI_ITAM/Documents/farid/estadisticaComputacional_Clase/tareas/tarea4_setupReaingTable")
#setwd("/home/farid/Documents/estadisticaComputacional/compstat2016/tarea5_MCMC")
data <- read.csv(file="cheese.csv", header=T)

Taste <- data$taste
data <- data[, !names(data) %in% c("id","taste")]

Rcpp::sourceCpp("funciones.cpp")

set.seed(28112016)

shinyUI(
 fluidPage(
  titlePanel("Tarea 5: MCMC"),
  h3("Angel Farid Fajardo Oroz"),
  h4("MCC"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("cVariables", h3("Variables"),
                         choices = names(data)),
      numericInput("nCadenas", "cadenas a simular", value=1, min=1, max=10, step=1),
      sliderInput("sLongitud", "longitud de cadenas", min=10000, max=1000000, value=1000),
      sliderInput("sBurnin", "Burnin", min=10, max=10000, value=5000),
      actionButton("button", "Calcula MCMC"),  #Calcula MCMC
      
      h4("Parámetros aPriori"),
      sliderInput("s_a", "a -> Unif ", min=1, max=10, value=c(5,8)),
      sliderInput("s_b", "b <- Norm", min=1, max=10, value=5),
      sliderInput("s_sigma", "sigma -> Unif", min=1, max=10, value=c(5, 6))
    ),
    
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("datos", 
                           fluidRow(
                             column(8, plotOutput("plot_data")),
                             column(12, DT::dataTableOutput("table"))
                           )
                  ),
                  tabPanel("distribuciones aPriori",
                           fluidRow(
                             column(4, plotOutput("plot_hist_A")),
                             column(4, plotOutput("plot_hist_B")),
                             column(4, plotOutput("plot_hist_Sd")),
                             column(4, plotOutput("plot_hist_Total"))
                           )
                  ),
                  tabPanel("Parámetros de la regresión",
                           fluidRow(
                             column(4, plotOutput("hist_posteriori_A")),
                             column(4, plotOutput("hist_posteriori_B")),
                             column(4, plotOutput("hist_posteriori_Sd")),
                             column(4, plotOutput("plot_posteriori_A")),
                             column(4, plotOutput("plot_posteriori_B")),
                             column(4, plotOutput("plot_posteriori_Sd"))
                           )
                  ),
                  tabPanel("Multiples cadenas",
                           fluidRow(
                             column(4, verbatimTextOutput("summary")),
                             column(4, plotOutput("regresionCalc")),
                             column(12, DT::dataTableOutput("cadenasMCMC"))
                           )),
                  tabPanel("Convergencia de MCMC's", 
                           plotOutput("pConvergencia_A"),
                           plotOutput("pConvergencia_B"),
                           plotOutput("pConvergencia_Sd"))
      )
    )
  )
)
)

#shiny::runGitHub("compstat2016", "farid7", subdir = "tarea5_MCMC")
