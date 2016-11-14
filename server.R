
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)


shinyServer(function(input, output) {
  
  source("global.R", local=TRUE)
  
  output$client.buys.rnd <- renderUI({
    choices <- as.character(g.ccy[g.ccy$name == input$ccy, c("base", "quote")])
    radioButtons(inputId="client.buys", label="Client Buys", choices=choices, selected=NULL, inline=TRUE, width=NULL)
  })
  
  output$notional.ccy.rnd <- renderUI({
    choices <- as.character(g.ccy[g.ccy$name == input$ccy, c("base", "quote")])
    radioButtons(inputId="notional.ccy", label="Notional Ccy", choices=choices, selected=NULL, inline=TRUE, width=NULL)
  })
  
  # output$client.buys.rnd <- renderUI({
  #   choices <- as.character(g.ccy[g.ccy$name == input$ccy, c("base", "quote")])
  #   selectInput(inputId="client.buys", label="Client Buys", choices=choices)
  # })
  
  # output$notional.ccy.rnd <- renderUI({
  #   choices <- as.character(g.ccy[g.ccy$name == input$ccy, c("base", "quote")])
  #   selectInput(inputId="notional.ccy", label="Notional Ccy", choices=choices)
  # })
  
  output$notional.all.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 2)
      return(numericInput(inputId="notional.all", label=NULL, value=NULL, min=100, max=10^7, step=0.01))
  })
  
  output$ratio.all.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 2)
      return(numericInput(inputId="ratio.all", label=NULL, value=NULL, min=100, max=10^7, step=0.01))
  })
  
  output$strike.all.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 2)
    {
      z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
      return(numericInput(inputId="strike.all", label=NULL, value=NULL, min=z$low, max=z$high, step=z$s.step))
    }
  })
  
  output$reduction.all.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 2)
    {
      z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
      return(numericInput(inputId="reduction.all", label=NULL, value=NULL, min=z$low, max=z$high, step=z$s.step))
    }
  })
  
  output$barrier.all.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 2)
    {
      z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
      return(numericInput(inputId="barrier.all", label=NULL, value=NULL, min=z$low, max=z$high, step=z$s.step))
    }
  })
  
  output$expiry1.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 1)
      return(dateInput(inputId="expiry1", label=NULL, value=NULL, format="dd-M-yyyy", weekstart=1, startview="month"))
  })
  
  output$expiry2.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 2)
      return(dateInput(inputId="expiry2", label=NULL, value=NULL, format="dd-M-yyyy", weekstart=1, startview="month"))
  })
  
  output$expiry3.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 3)
      return(dateInput(inputId="expiry3", label=NULL, value=NULL, format="dd-M-yyyy", weekstart=1, startview="month"))
  })
  
  output$expiry4.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 4)
      return(dateInput(inputId="expiry4", label=NULL, value=NULL, format="dd-M-yyyy", weekstart=1, startview="month"))
  })
  
  output$expiry5.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 5)
      return(dateInput(inputId="expiry5", label=NULL, value=NULL, format="dd-M-yyyy", weekstart=1, startview="month"))
  })
  
  output$expiry6.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 6)
      return(dateInput(inputId="expiry6", label=NULL, value=NULL, format="dd-M-yyyy", weekstart=1, startview="month"))
  })
  
  output$notional1.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 1)
      return(numericInput(inputId="notional1", label=NULL, value=NULL, min=100, max=10^7, step=0.01))
  })
  
  output$notional2.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 2)
      return(numericInput(inputId="notional2", label=NULL, value=NULL, min=100, max=10^7, step=0.01))
  })
  
  output$notional3.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 3)
      return(numericInput(inputId="notional3", label=NULL, value=NULL, min=100, max=10^7, step=0.01))
  })
  
  output$notional4.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 4)
      return(numericInput(inputId="notional4", label=NULL, value=NULL, min=100, max=10^7, step=0.01))
  })
  
  output$notional5.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 5)
      return(numericInput(inputId="notional5", label=NULL, value=NULL, min=100, max=10^7, step=0.01))
  })
  
  output$notional6.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 6)
      return(numericInput(inputId="notional6", label=NULL, value=NULL, min=100, max=10^7, step=0.01))
  })
  
  output$ratio1.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 1)
      return(numericInput(inputId="ratio1", label=NULL, value=NULL, min=100, max=10^7, step=0.01))
  })
  
  output$ratio2.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 2)
      return(numericInput(inputId="ratio2", label=NULL, value=NULL, min=100, max=10^7, step=0.01))
  })
  
  output$ratio3.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 3)
      return(numericInput(inputId="ratio3", label=NULL, value=NULL, min=100, max=10^7, step=0.01))
  })
  
  output$ratio4.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 4)
      return(numericInput(inputId="ratio4", label=NULL, value=NULL, min=100, max=10^7, step=0.01))
  })
  
  output$ratio5.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 5)
      return(numericInput(inputId="ratio5", label=NULL, value=NULL, min=100, max=10^7, step=0.01))
  })
  
  output$ratio6.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 6)
      return(numericInput(inputId="ratio6", label=NULL, value=NULL, min=100, max=10^7, step=0.01))
  })
  
  output$strike1.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 1)
    {
      z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
      return(numericInput(inputId="strike1", label=NULL, value=NULL, min=z$low, max=z$high, step=z$s.step))
    }
  })
  
  output$strike2.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 2)
    {
      z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
      return(numericInput(inputId="strike2", label=NULL, value=NULL, min=z$low, max=z$high, step=z$s.step))
    }
  })
  
  output$strike3.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 3)
    {
      z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
      return(numericInput(inputId="strike3", label=NULL, value=NULL, min=z$low, max=z$high, step=z$s.step))
    }
  })
  
  output$strike4.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 4)
    {
      z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
      return(numericInput(inputId="strike4", label=NULL, value=NULL, min=z$low, max=z$high, step=z$s.step))
    }
  })
  
  output$strike5.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 5)
    {
      z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
      return(numericInput(inputId="strike5", label=NULL, value=NULL, min=z$low, max=z$high, step=z$s.step))
    }
  })
  
  output$strike6.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 6)
    {
      z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
      return(numericInput(inputId="strike6", label=NULL, value=NULL, min=z$low, max=z$high, step=z$s.step))
    }
  })
  
  output$reduction1.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 1)
    {
      z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
      return(numericInput(inputId="reduction1", label=NULL, value=NULL, min=z$low, max=z$high, step=z$s.step))
    }
  })
  
  output$reduction2.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 2)
    {
      z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
      return(numericInput(inputId="reduction2", label=NULL, value=NULL, min=z$low, max=z$high, step=z$s.step))
    }
  })
  
  output$reduction3.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 3)
    {
      z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
      return(numericInput(inputId="reduction3", label=NULL, value=NULL, min=z$low, max=z$high, step=z$s.step))
    }
  })
  
  output$reduction4.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 4)
    {
      z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
      return(numericInput(inputId="reduction4", label=NULL, value=NULL, min=z$low, max=z$high, step=z$s.step))
    }
  })
  
  output$reduction5.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 5)
    {
      z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
      return(numericInput(inputId="reduction5", label=NULL, value=NULL, min=z$low, max=z$high, step=z$s.step))
    }
  })
  
  output$reduction6.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 6)
    {
      z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
      return(numericInput(inputId="reduction6", label=NULL, value=NULL, min=z$low, max=z$high, step=z$s.step))
    }
  })
  
  output$barrier1.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 1)
    {
      z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
      return(numericInput(inputId="barrier1", label=NULL, value=NULL, min=z$low, max=z$high, step=z$s.step))
    }
  })
  
  output$barrier2.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 2)
    {
      z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
      return(numericInput(inputId="barrier2", label=NULL, value=NULL, min=z$low, max=z$high, step=z$s.step))
    }
  })
  
  output$barrier3.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 3)
    {
      z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
      return(numericInput(inputId="barrier3", label=NULL, value=NULL, min=z$low, max=z$high, step=z$s.step))
    }
  })
  
  output$barrier4.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 4)
    {
      z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
      return(numericInput(inputId="barrier4", label=NULL, value=NULL, min=z$low, max=z$high, step=z$s.step))
    }
  })
  
  output$barrier5.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 5)
    {
      z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
      return(numericInput(inputId="barrier5", label=NULL, value=NULL, min=z$low, max=z$high, step=z$s.step))
    }
  })
  
  output$barrier6.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 6)
    {
      z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
      return(numericInput(inputId="barrier6", label=NULL, value=NULL, min=z$low, max=z$high, step=z$s.step))
    }
  })
  
  
  
  
  output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')

  })
  
  
  
  output$strike.inv <- renderText(round(1/input$strike, g.ccy$s.digits[g.ccy$name == input$ccy]))
  output$init.strike.inv <- renderText(round(1/input$init.strike, g.ccy$b.digits[g.ccy$name == input$ccy]))
  
  

})
