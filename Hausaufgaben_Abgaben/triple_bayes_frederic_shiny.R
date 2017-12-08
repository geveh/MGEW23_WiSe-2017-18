## Hausarbeit zum 12.12.2017
## Frederic Brieger

# Diese Shiny-App plottet das Bayes-Theorem f�r eine diskrete Wahrscheinlichkeitsverteilung (in diesem Fall Normalverteilung)

library(shiny)

# Define UI 
ui <- fluidPage(
  titlePanel("Bayes für diskrete Wahrscheinlichkeitsverteilungen"),
  textOutput("Text"),
  sliderInput(inputId = "ii", value = 9, label = "Anzahl Priors", min = 0, max = 100, step = 1),
  numericInput(inputId = "xi", value = 1, label = "Daten: Richtig", min = 0, max = 500, step = 1),
  numericInput(inputId = "yi", value = 0, label = "Daten: Falsch", min = 0, max = 500, step = 1),
  plotOutput("tripleplot")
)

# Define server logic
server <- function(input, output) {
  output$tripleplot <- renderPlot({
    i <- input$ii
    x <- input$xi
    y <- input$yi
    t <- seq(from = 0, to = 1, length.out = i) # ts zwischen 0 und 1
    pt_norm <- dnorm(t, mean = 0.5, sd = 1) # normalverteilter Vektor der Länge t
    pt <- pt_norm/sum(pt_norm) # Vektor auf 1 normiert
  
    # Likelihood
    like <- choose(x+y, x)*(t^x)*((1-t)^y) # Ist Bernoulli-verteilt
  
    # Evidence
    evi <- sum(like*pt)
  
    # Posterior
    post <- (like * pt) / evi
    
    # Plots
    par(mfrow=c(3,1)) # Die nächsten drei Plots untereinander
    plot(t, pt, main = "Prior", type = "h", lwd = 3) # Prior
    abline(v = 0.5, col = "red", lwd = 2)
    plot(t,like, main = "Likelihood", type = "h", lwd = 3) # Likelihood
    abline(v = 0.5, col = "red", lwd = 2)
    plot(t, post, main = "Posterior", type = "h", lwd = 3) # Posterior
    abline(v = 0.5, col = "red", lwd = 2)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

