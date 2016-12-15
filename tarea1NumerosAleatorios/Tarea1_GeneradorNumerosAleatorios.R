if(!require(shiny)) install.packages("shiny")
set.seed(24082016)
library(shiny)

ui <- fluidPage(
    h1("Tarea1 Generador de números aleatorios"),
    h6("Angel Farid Fajardo Oroz"),
    h6("MCC ITAM"),

    fluidRow(column(4, offset = 1, sliderInput("num", h5("Cantidad de números a simular"),
                                               min = 20, max = 1000,
                                               value = 50)),
             column(4, offset= 1, sliderInput("bins", h5("Cantidad de barras en histograma"),
                                              min = 5, max = 100, value = 10)),
             column(3, offset= 4, sliderInput("lda", h5("Parametro Lamda para fun(exp)"),
                                              min = 1, max = 50, value = 1))
    ),

    h3("Pruebas de bondad de ajuste"),
    #h5("Kolmogorov-Smirnov y Chi-Square Test"),
    fluidRow(column(5, verbatimTextOutput("ksTest")),
             column(5, verbatimTextOutput("chiTest"))
    ),


    sidebarLayout(
        sidebarPanel(
            fluidRow(radioButtons("radioBtn", "Tipo de distribución:",
                                  c("Uniforme (GCL)"         = "UNIF",
                                    "Exponencial (Fnc-Inv)" = "EXP",
                                    "Normal (Box-Müller)"               = "NORM",
                                    "Geometrica"             = "GEOM")),
            column(5, offset=1, downloadButton('downloadData', 'Descargar Datos')))
        ),

        mainPanel(
            tabsetPanel(
                tabPanel("histograma", plotOutput("hist")),
                tabPanel("qqPlot",     plotOutput("qqPlot"))
            ),
            verbatimTextOutput("stats")
        )
    )
)

server <- function(input, output) {
    data <- reactive({
        switch(input$radioBtn,
               #generador de congruencia lineal
               UNIF = {
                   sapply(seq(1,input$num), function(x, a=22695477, c=1, m=2**32){
                       return(((a*x + c) %% m) / m)
                   })
               },
               #distribución uniforme (BoxMüller)
               NORM = {
                   sapply(seq(1,input$num), function(x){
                       u1 <- runif(1)
                       u2 <- runif(1)
                       return(sqrt(-2*log(u1))*cos(2*pi*u2))
                   })
                   },
               #función inversa
               EXP  = {
                   sapply(seq(1, input$num), function(x, lambda=input$lda){
                       u <- runif(length(x))
                       return(-log(1-u)/lambda)
                   })
               },
               GEOM = {
                   sapply(seq(1, input$num), function(x, prob=0.5){
                       u <- runif(length(x))
                       return(log(u)/log(1-prob))
                       #http://www.win.tue.nl/~marko/2WB05/lecture8.pdf
                   })
               })
    })

    kolmogorovTest <- reactive({
        switch(input$radioBtn,
               UNIF = ks.test(data(), runif(input$num)),
               NORM = ks.test(data(), "pnorm"),
               EXP  = ks.test(data(), "pexp"),
               GEOM = ks.test(data(), rgeom(input$num, prob=0.5)))
    })

    chiTest <- reactive({
        switch(input$radioBtn,
               UNIF = {breaks <- c(seq(0,10, by=1))
               o <- table(cut(data(), breaks = breaks))
               p <- diff(punif(breaks))
               chisq.test(o, p=p, rescale.p = T)},
               NORM = {breaks <- c(seq(0,10, by=1))
               o <- table(cut(data(), breaks = breaks))
               p <- diff(pnorm(breaks))
               chisq.test(o, p=p, rescale.p = T)},
               EXP  = {breaks <- c(seq(0,10, by=1))
               o <- table(cut(data(), breaks = breaks))
               p <- diff(pexp(breaks))
               chisq.test(o, p=p, rescale.p = T)},
               GEOM = {breaks <- c(seq(0,10, by=1))
               o <- table(cut(data(), breaks = breaks))
               p <- diff(pgeom(breaks, prob=0.5))
               chisq.test(o, p=p, rescale.p = T)}
        )
    })

    output$ksTest <- renderPrint({
        kolmogorovTest()
    })

    output$chiTest <- renderPrint({
        chiTest()
    })

    output$stats <- renderPrint({
        summary(data())
    })

    output$hist <- renderPlot({
        h <- hist(data(), breaks = input$bins, plot=F)
        d <- density(data())
        hist(data(), breaks = input$bins,
             main= isolate(input$title))
        lines(x=d$x, y=d$y*length(data())*diff(h$breaks)[1], ldw=2)
    })

    output$qqPlot <- renderPlot({
        par(mfrow=c(1,2))
        switch(input$radioBtn,
               UNIF = {q1 = qunif(seq(0,1,0.01));
               q2 = quantile(data(), seq(0,1,0.01));
               plot(q1, q2, main="Q-Q Plot", ylab = "CLG", xlab="qunif")},
               NORM = {q1 = qnorm(seq(0,1,0.01));
               q2 = quantile(data(), seq(0,1,0.01));
               plot(q1, q2, main="Q-Q Plot", ylab="rnorm", xlab="qnorm")},
               EXP  = {q1 = qexp(seq(0,1,0.01));
               q2 = quantile(data(), seq(0,1,0.01));
               plot(q1, q2, main="Q-Q Plot", ylab = "expInv", xlab="qexp")},
               GEOM = {q1 = qgeom(seq(0,1,0.01), prob=0.5);
               q2 = quantile(data(), seq(0,1,0.01));
               plot(q1, q2, main="Q-Q Plot", ylab = "geomInv", xlab="qgeom")}
        )
        plot(data()[1:length(data())-1], data()[2:length(data())], main = "Secuencia en números")
    })
    
    output$downloadData <- downloadHandler(
      filename = function() { paste(input$radioBtn, '.csv', sep='') },
      content = function(file) {
        write.csv(data(), file)
      }
    )
}

shinyApp(ui = ui, server = server)

#shiny::runGitHub("compstat2016", "farid7", subdir = "Tarea1_GeneradorNumerosAleatorios")
