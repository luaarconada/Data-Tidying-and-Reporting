##App example: it creates an empty app.

library(shiny)
ui <- fluidPage()
server <- function(input, output){}
shinyApp(ui = ui, server = server)



##App example

server <- shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    n <- input$obs
    samp <- rnorm(n)
    #hist(samp, xlab = "samples", ylab = "Frequency", main = "Histogram")
    plot(density(samp))
    rug(samp)
  })
  
})

ui <- shinyUI(fluidPage(
  titlePanel("Hello World!"),
  sidebarLayout(
    sidebarPanel(  #the sidebar slider of the sample size
      sliderInput(inputId = "obs", label = "Sample size:", min = 1,
                  max = 1000, value = 500, step = 10)
    ),
    mainPanel(
      plotOutput(outputId = "distPlot")
    )
  )
))

# this last line is needed to run the app. In positcloud if we just click 'run app' while on ui or server it opens.
shinyApp(ui=ui,server=server)



##Eduardo's example of app:


#
# Shiny web application for illustrating the maximum likelihood estimates in
# the (simple) logistic regression
#

library(shiny)

# Data
set.seed(34567)
x <- rnorm(50, sd = 1.5)
y1 <- -0.5 + 3 * x
y2 <- 0.5 - 2 * x
y3 <- -2 + 5 * x
y1 <- rbinom(50, size = 1, prob = 1 / (1 + exp(-y1)))
y2 <- rbinom(50, size = 1, prob = 1 / (1 + exp(-y2)))
y3 <- rbinom(50, size = 1, prob = 1 / (1 + exp(-y3)))

# UI for application
ui <- fluidPage(align = "center",
                
                # Vertical layout with:
                # - the select input for the data pattern
                # - the slider inputs for intercept and slope
                verticalLayout(
                  
                  inputPanel(
                    
                    selectInput(inputId = "dataType", label = "Sample:",
                                choices = c("1", "2", "3")),
                    sliderInput(inputId = "beta0", label = "Intercept:",
                                min = -10, max = 10, value = 1, step = 0.1),
                    sliderInput(inputId = "beta1", label = "Slope:",
                                min = -10, max = 10, value = 1, step = 0.1)
                    
                  ),
                  
                  plotOutput("logisticPlot")
                  
                )
                
)

# Server logic
server <- function(input, output) {
  
  output$logisticPlot <- renderPlot({
    
    # Response
    y <- switch(input$dataType,
                "1" = y1,
                "2" = y2,
                "3" = y3)
    
    # Model
    xx <- seq(-5, 5, l = 200)
    prob <- input$beta0 + input$beta1 * x
    prob <- 1 / (1 + exp(-prob))
    real <- input$beta0 + input$beta1 * xx
    real <- 1 / (1 + exp(-real))
    
    # Plot
    par(mar = c(4, 4, 3, 1) + 0.1, oma = rep(0, 4))
    plot(x, y, xlim = c(-5, 5), ylim = c(-0.1, 1), pch = 16)
    title(main = paste("Log-likelihood:",
                       sprintf("%.3f", sum(y * log(prob) +
                                             (1 - y) * log(1 - prob)))),
          cex.main = 1.5)
    lines(xx, real, col = 2, lwd = 3)
    legend("bottomright", legend = c("Fitted regression"), col = 2, lwd = 3,
           lty = 1, cex = 1.5)
    
  }, width = 650, height = 650)
  
}

# Run the application
shinyApp(ui = ui, server = server)




##Another example

ui <- fluidPage(
  textInput("name", "What's your name?"),
  textOutput("greeting")
)

server <- function(input, output, session) {
  output$greeting <- renderText(string()) #we are using string as a function
  string=reactive(paste0("Hello ", input$name, "!"))
  #the order of this two linea doesn't matter
}
# Run the application
shinyApp(ui = ui, server = server)

