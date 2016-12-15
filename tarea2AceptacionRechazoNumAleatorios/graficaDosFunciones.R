rm(list=ls())
if(!require(shiny)) install.packages("shiny")
set.seed(24082016)
library(shiny)

ui <- fluidPage(
  h1("Tarea2 Aceptación Rechazo"),
  h6("Angel Farid Fajardo Oroz"),
  h6("MCC ITAM"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("tarea", 
                   label="Escoge Tarea",
                   choices=c("Aceptación Rechazo" = "aceptRech",
                     "Funcion Inversa" = "funInv"),
                   selected = "aceptRech")
  ),
  
  mainPanel(
    conditionalPanel(
      condition="input.tarea=='aceptRech'",
      h2("Aceptacion-Rechazo"),
      textInput(
        inputId = "expresion1",
        label = "Función f",
        value = "function(x) 2*x"
      ),
      textInput(
        inputId = "expresion2",
        label = "Función g",
        value = "function(x) x^2"
      ),
      numericInput("minimoGraf", "xmin", 0),
      numericInput("maximoGraf", "xmax", 10),
      plotOutput("Grafica")
    )
  )
 )
)

server <- function(input, output) {
 fun1 <- reactive({
   eval(parse(text = input$expresion1))
 })
 fun2 <- reactive({
   eval(parse(text = input$expresion2))
 })
 
 output$Grafica <- renderPlot({
   x <- seq(input$minimoGraf, input$maximoGraf, length.out=100)
   y1 <- sapply(x, fun1())
   y2 <- sapply(x, fun2())
   plot(x, y1, type="l", col="blue", main="Grafica f(x) y g(x)")
   lines(x, y2, col="red")
 })
}

shinyApp(ui = ui, server = server)

#shiny::runGitHub("compstat2016", "farid7", subdir = "Tarea1_GeneradorNumerosAleatorios")