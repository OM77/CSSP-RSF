
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  titlePanel("CS SP: Ratio Spread Forward Extra"),
  
  br(),
  
  fluidRow(
    column(selectInput(inputId="ccy", label="Ccy Pair", choices=g.ccy$name, selectize=TRUE), width=2),
    column(uiOutput("client.buys.rnd"), width=2),
    column(uiOutput("notional.ccy.rnd"), width=2)
  ),
  
  radioButtons(inputId="strip.occurrences", label="Strip (Occurrences)", choices=1:6, inline=TRUE),
  
  fluidRow(
    column(uiOutput("expiry.all.rnd"), width=2),
    column(uiOutput("notional.all.rnd"), width=2),
    column(uiOutput("ratio.all.rnd"), width=2),
    column(uiOutput("strike.all.rnd"), width=2),
    column(uiOutput("reduction.all.rnd"), width=2),
    column(uiOutput("barrier.all.rnd"), width=2)
  ),
  
  fluidRow(
    column(strong("Expiry Date"),
           uiOutput("expiry1.rnd"),
           uiOutput("expiry2.rnd"),
           uiOutput("expiry3.rnd"),
           uiOutput("expiry4.rnd"),
           uiOutput("expiry5.rnd"),
           uiOutput("expiry6.rnd"), width=2),
    column(strong("Notional Amount"),
           uiOutput("notional1.rnd"),
           uiOutput("notional2.rnd"),
           uiOutput("notional3.rnd"),
           uiOutput("notional4.rnd"),
           uiOutput("notional5.rnd"),
           uiOutput("notional6.rnd"), width=2),
    column(strong("Ratio Amount"),
           uiOutput("ratio1.rnd"),
           uiOutput("ratio2.rnd"),
           uiOutput("ratio3.rnd"),
           uiOutput("ratio4.rnd"),
           uiOutput("ratio5.rnd"),
           uiOutput("ratio6.rnd"), width=2),
    column(strong("Strike Rate"),
           uiOutput("strike1.rnd"),
           uiOutput("strike2.rnd"),
           uiOutput("strike3.rnd"),
           uiOutput("strike4.rnd"),
           uiOutput("strike5.rnd"),
           uiOutput("strike6.rnd"), width=2),
    column(strong("Reduction Rate"),
           uiOutput("reduction1.rnd"),
           uiOutput("reduction2.rnd"),
           uiOutput("reduction3.rnd"),
           uiOutput("reduction4.rnd"),
           uiOutput("reduction5.rnd"),
           uiOutput("reduction6.rnd"), width=2),
    column(strong("Barrier Rate"),
           uiOutput("barrier1.rnd"),
           uiOutput("barrier2.rnd"),
           uiOutput("barrier3.rnd"),
           uiOutput("barrier4.rnd"),
           uiOutput("barrier5.rnd"),
           uiOutput("barrier6.rnd"), width=2)
  )
  
  

  
))
