#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# OpenBoneMin_Dashboard

library(shiny)
library("OpenBoneMin")
library("ggplot2")
library("mapdeck")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("OpenBoneMin Dashboard"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     
      sidebarPanel(
         #selectInput("ptype", "Output", c("DENCP", "DENMOL", "BMDlsDENchange"), selected = "DENCP"),
         selectInput("ptype", "Output", c("DENCP", "DENMOL", "BMDlsDENchange", "All"), selected = "DENCP"),
         sliderInput("dose","Doseage", min = 1, max = 100, value = 60, post = " mg"),
         sliderInput("ii","Dosing interval", min = 1, max = 24, value = 6, post = " months"),
         sliderInput("dur","Number of doses", min = 1, max = 10, value = 3),
         sliderInput("delta","Simulation time", min = 1, max = 24, value = 4, post = " hours")
         ),
      
      # Show a plot 
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  # request all
  request <- "DENCP,DENMOL,BMDlsDENchange"
  
  # create the model
  mod <- BoneMin()
  cmtn <- mrgsolve::cmtn(mod,"DENSC")
  
  # reactive inputs  
  dose <- reactive({input$dose})
  ii <- reactive({input$ii*28*24})
  dur <- reactive({input$dur})
  delta <-reactive({input$delta})
  rtype <- reactive({input$ptype}) 
  
  tscale  <- 1/(24*28)
  
  data <- reactive({
    expand.ev(amt = amt_denos(dose()), ii=ii(), addl=dur()-1, cmt=cmtn)
  })
  
  # get the dataframe
  
  df <- reactive({
    mrgsim_df(mod, data=data(), delta=delta(), end=(dur()+1)*ii(), tscale=tscale, Req=request)
  })
  
    output$distPlot <- renderPlot({
      
      if( rtype() == "All") {
        plot( df()[,"time"], df()[,"DENCP"],  ylab= "", xlab = "", col ="blue")
        par(new = TRUE)
        plot( df()[,"time"], df()[,"DENMOL"],  ylab= "", xlab = "", col ="red")
        par(new = TRUE)
        plot( df()[,"time"], df()[,"BMDlsDENchange"], ylab = rtype(), xlab = "Time", col ="dark green")
      }
      else{
        plot( df()[,"time"], df()[,rtype()], ylab = rtype(), xlab = "Time" )
      }
      
    })

}

# Run the application 
shinyApp(ui = ui, server = server)

