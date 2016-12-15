rm(list=ls())
if(!require(shiny)) install.packages("shiny")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(DT)) install.packages("DT")

library(ggplot2)
library(DT)
library(shiny)

data <- read.csv(file="cheese.csv", header=T)

shinyUI(fluidPage(
  titlePanel("Tarea4_DataTable"),
  h4("Angel Farid Fajardo Oroz"),
  h5("MCC"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("cVariables", h3("Variables"),
                         choices = names(data)),
      h4("Parámetros aPriori"),
      sliderInput("t4_s_a", "a -> Unif ", min=1, max=10, value=c(5,8)),
      sliderInput("t4_s_b", "b <- Norm", min=1, max=10, value=5),
      sliderInput("t4_s_sigma", "sigma -> Unif", min=1, max=10, value=c(5, 6))
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs", 
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
                  )
      )
      
    )
  )))

#shiny::runGitHub("compstat2016", "farid7", subdir = "tarea4_despligaDatos")