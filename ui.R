
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)


source("global.R", local=TRUE)


shinyUI(fluidPage(

  titlePanel(paste("CS SP:", g.sp.name["long"])),
  
  br(),
  
  fileInput(inputId="upload.scheme", label="Upload Scheme", accept=".csv", width=NULL),
  
  fluidRow(
    column(selectInput(inputId="ccy", label="Ccy Pair", choices=g.ccy$name, selected=param$ccy, selectize=TRUE), width=2),
    column(radioButtons(inputId="client.buys", label="Client Buys", choices=as.character(g.ccy[1, c("base", "quote")]), selected=param$client.buys, inline=TRUE, width=NULL), width=2),
    column(radioButtons(inputId="notional.ccy", label="Notional Ccy", choices=as.character(g.ccy[1, c("base", "quote")]), selected=param$notional.ccy, inline=TRUE, width=NULL), width=2)
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
           dateInput(inputId="expiry1", label=NULL, value=param$expiry[1], format="dd-M-yyyy", weekstart=1, startview="month"),
           uiOutput("expiry2.rnd"),
           uiOutput("expiry3.rnd"),
           uiOutput("expiry4.rnd"),
           uiOutput("expiry5.rnd"),
           uiOutput("expiry6.rnd"), width=2),
    column(strong("Notional Amount"),
           numericInput(inputId="notional1", label=NULL, value=param$notional[1], min=0, max=10^7, step=0.01),
           uiOutput("notional2.rnd"),
           uiOutput("notional3.rnd"),
           uiOutput("notional4.rnd"),
           uiOutput("notional5.rnd"),
           uiOutput("notional6.rnd"), width=2),
    column(strong("Ratio Amount"),
           numericInput(inputId="ratio1", label=NULL, value=param$ratio[1], min=0, max=10^7, step=0.01),
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
           uiOutput("reduction6.rnd"), width=2)
  ),
  
  fluidRow(
    column(actionLink("update.spot.ref","Spot Reference Rate", icon=icon(name="refresh", lib="font-awesome")),
           uiOutput("spot.ref.rnd"), width=2)
    
  ),
  
  br(),
  br(),
  
  actionButton("sp.calc", label="Calculate", icon=icon(name="gears", lib="font-awesome"), width=150),
  
  br(),
  br(),
  br(),
  
  p(strong("Download Scheme")),
  
  fluidRow(
    column(textInput(inputId="download.scheme.file.name", label=NULL, value=""), width=2),
    column(downloadButton(outputId="download.scheme", label="Download"), width=2)
  ),
  
  
  br(),
  br(),

  verbatimTextOutput("log.msg")

  

  
))
