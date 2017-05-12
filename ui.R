library(RCurl)
library(httr)
library(XML)
library(shiny)
library(ggplot2)
library(dplyr)
library(emoGG)
library(shinythemes)

drivers <- list(
  "Fernando ALONSO",
  "Valtteri BOTTAS" ,
  "Marcus ERICSSON" ,
  "Antonio GIOVINAZZI" ,
  "Romain GROSJEAN" ,
  "Lewis HAMILTON" ,
  "Nico HULKENBERG",
  "Daniil KVYAT" ,
  "Kevin MAGNUSSEN" ,
  "Felipe MASSA" ,
  "Esteban OCON" ,
  "Jolyon PALMER" ,
  "Sergio PEREZ" ,
  "Kimi RAIKKONEN" ,
  "Daniel RICCIARDO" ,
  "Carlos SAINZ" ,
  "Lance STROLL" ,
  "Stoffel VANDOORNE" ,
  "Max VERSTAPPEN" ,
  "Sebastian VETTEL" ,
  "Pascal WEHRLEIN"
)

ui <- fluidPage(theme = shinytheme("superhero"),
  tags$head(tags$style(".rightAlign{float:right;color:#999999;}")),
  headerPanel('Formula 1 Results'),
  mainPanel(
    p(HTML("Created by Jake Warner </br> More info: <a href='www.jacobwarner.net'>jacobwarner.net</a></br>
           &nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp
           <a href='www.2-bitbio.com'>2-bitbio.com</a>"), class = 'rightAlign'),
    selectInput("driver", "Select a driver", drivers, selected = NULL, multiple = FALSE,
                selectize = TRUE, width = NULL, size = NULL),
    width = 10),
  mainPanel(plotOutput("plot1"),
            h3("Driver Results Table"),
            tableOutput("table1"),
            width = 10)
)

#shinyApp(ui = ui, server = server)
