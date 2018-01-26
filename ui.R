library(RCurl)
library(httr)
library(XML)
library(shiny)
library(ggplot2)
library(emoGG)
library(jsonlite)
library(shinythemes)
library(shinyjs)


#this list will be input into the selectInput()
drivers <- fromJSON("http://ergast.com/api/f1/drivers/.json?limit=2500")
parsedrivers= data.frame(name=character())
alldriversresults <- lapply(1:as.numeric(drivers[[1]][6][1]), function(i){
  firstname <- drivers[[1]][[7]][[2]][[3]][i]
  lastname <- toupper(drivers[[1]][[7]][[2]][[4]][i])
  parsedrivers=rbind(parsedrivers,
                     paste0(lastname,", ",firstname)
  )
  names(parsedrivers) = NULL
  parsedrivers
})
AllDrivers <- unlist(lapply(alldriversresults, '[[', 1))

Currentdrivers <- fromJSON("http://ergast.com/api/f1/2017/drivers/.json?limit=2500")
parseCurrdrivers= data.frame(name=character())
Currdriversresults <- lapply(1:as.numeric(Currentdrivers[[1]][6][1]), function(i){
  id <- Currentdrivers[[1]][[7]][[3]][[1]][i]
  firstname <- Currentdrivers[[1]][[7]][[3]][[5]][i]
  lastname <- toupper(Currentdrivers[[1]][[7]][[3]][[6]][i])
  parseCurrdrivers=rbind(parseCurrdrivers,data.frame(
                         paste0(lastname,", ",firstname),
                         id)
  )
  names(parseCurrdrivers) = NULL
  parseCurrdrivers
})
CurrDrivers <- unlist(lapply(Currdriversresults, '[[', 1))
#CurrDriverIDs <- unlist(lapply(Currdriversresults, '[[', 2))


ui <- navbarPage(
  id = "navbar",
  selected = 'home',
  header = tagList(
    useShinyjs(),
    extendShinyjs("www/app-shinyjs.js", functions = c("updateHistory"))
  ),
  tags$head(tags$style(".rightAlign{float:right;color:#999999;}")),
  #tags$head(tags$script(src="maps.js")),
  
  theme = shinytheme("superhero"),
  title="Formula 1 Stats",
    tabPanel("Driver Data",value = "home",

    #headerPanel('Formula 1 Results'),
    mainPanel(
      p(HTML("Created by <a href='http://www.jacobwarner.net'>Jake Warner</a>
             <br>Data from the <a href='http://ergast.com/mrd/'>Ergast API</a>"), class = 'rightAlign'),
     #selectInput("driver", "Select a current driver", CurrDrivers, selected = NULL, multiple = FALSE,
      #            selectize = TRUE, width = NULL, size = NULL),
      checkboxInput("showonlycurrent","Limit to 2017 Drivers"),
      uiOutput("driverSelection"),
      sliderInput("date", label = h3("Filter by date"), 
                  min = as.Date("1950-01-01","%Y-%m-%d"),
                  max = as.Date(Sys.time()),
                  value=c(as.Date("1950-01-01","%Y-%m-%d"),as.Date(Sys.time())),
                  timeFormat="%Y", step=365),
      #selectInput("driver", "or Select any driver", AllDrivers, selected = NULL, multiple = FALSE,
      #            selectize = TRUE, width = NULL, size = NULL),
     width = 10),
   mainPanel(plotOutput("plot1"),
             h3("Driver Results Table"),
              tableOutput("table1"),
              width = 10)
    ),
    tabPanel("Race Data",value = "races",
      selectInput("lap_season", "Select a season", c("2017"), selected = NULL, multiple = FALSE,
                 selectize = TRUE, width = NULL, size = NULL),
      selectInput("lap_race", "Select a race", c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20), selected = NULL, multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL),
      mainPanel(h2(textOutput('track_info')),
                h4('Lap Position'),
                plotOutput("plot2"),
                h4("Pace"),
                plotOutput("plot3"),
               width = 10)
    )
)
