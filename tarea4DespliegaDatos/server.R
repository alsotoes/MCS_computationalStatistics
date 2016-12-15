rm(list=ls())
if(!require(shiny)) install.packages("shiny")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(DT)) install.packages("DT")

library(ggplot2)
library(DT)
library(shiny)
data <- read.csv(file="./cheese.csv", header=T)

shinyServer(function(input, output) {
  n <- dim(data)[1]
  
  dataInput <- reactive({
    if(is.null(input$cVariables))
      return()
    aux <- data[, input$cVariables]
    aux
  })
  
  priori_a <- reactive({
    runif(n, min=input$t4_s_a[1], max = input$t4_s_a[2])
  })
  priori_b <- reactive({
    rnorm(n, mean=0, sd = input$t4_s_b)
  })
  priori_sd <- reactive({
    runif(n, min=input$t4_s_sigma[1], max = input$t4_s_sigma[2])
  })
  
  nmes <- renderText({
    input$cVariables
  })
  
  output$table <- DT::renderDataTable(DT::datatable({
    if(is.null(input$cVariables))
      return()
    else 
      return(dataInput())
    
  }))
  
  output$plot_data <- renderPlot({
    if(is.null(input$cVariables))
      return()
    else{
      return(plot(dataInput(), main="Grafica de dispersion para quesos"))
    }
  })
  
  output$plot_hist_A <- renderPlot({
    hist(priori_a())
  })
  
  output$plot_hist_B <- renderPlot({
    hist(priori_b())
  })
  
  output$plot_hist_Sd <- renderPlot({
    hist(priori_sd())
  })
  
  output$plot_hist_Total <- renderPlot({
    hist(priori_a() * priori_b() * priori_sd(), main="distribucin aPriori")
  })
})
