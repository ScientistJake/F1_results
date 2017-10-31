library(RCurl)
library(httr)
library(XML)
library(shiny)
library(ggplot2)
library(emoGG)
library(jsonlite)
library(reshape)
library(RSQLite)
library(dplyr)
library(tidyr)
source('heatmap.2.R')
library(emoGG)
library(directlabels)

#color mappings for constrcutor
cols <- c(  
  "Adams"= "burlywood1",
  "AFM" = "gray40",
  "AGS" = "midnightblue",
  "Alfa Romeo" = "darkblue",
  "Alta"= "lightblue1",
  "Amon"="cornflowerblue",
  "Andrea Moda"= "gray30",
  "Apollon"= "gold2",
  "Arrows"= "darkorange",
  "Arzani-Volpini"="red3",
  "Aston Butterworth"= "grey70",
  "Aston Martin"= "yellow2",
  "ATS"= "yellow",
  "BAR" = "white",
  "Behra-Porsche" = "red",
  "Bellasi"= "orangered3",
  "Benetton"= "green",
  "BMW"= "royalblue4",
  "BMW Sauber"= "darkgray",
  "Boro"= "ivory",
  "Brabham"= "oldlace",
  "Brabham-Alfa Romeo"= "oldlace",
  "Brabham-BRM"= "oldlace",
  "Brabham-Climax"= "oldlace",
  "Brabham-Ford"= "oldlace",
  "Brabham-Repco"= "oldlace",
  "Brawn"= "chartreuse",
  "BRM"= "gray80",
  "BRM-Ford"= "gray80",
  "Bromme"= "darkolivegreen",
  "BRP"= "darkseagreen1",
  "Bugatti" = 'slateblue',
  "Caterham"= "forestgreen",
  "Christensen"= "darkviolet",
  "Cisitalia"= "gray80",
  "Coloni"= "palegoldenrod",
  "Connaught" = "forestgreen",
  "Connew"= "firebrick",
  "Cooper"= "royalblue4",
  "Cooper-Alfa Romeo"= "royalblue4",
  "Cooper-ATS"= "royalblue4",
  "Cooper-Borgward"= "royalblue4",
  "Cooper-BRM"= "royalblue4",
  "Cooper-Castellotti"= "royalblue4",
  "Cooper-Climax"= "royalblue4",
  "Cooper-Ferrari"= "royalblue4",
  "Cooper-Ford"= "royalblue4",
  "Cooper-Maserati"= "royalblue4",
  "Cooper-OSCA"= "royalblue4",
  "Dallara"= "brown1",
  "De Tomaso"= "orangered",
  "De Tomaso-Alfa Romeo"= "orangered",
  "De Tomaso-Ferrari"= "orangered",
  "De Tomaso-Osca"= "orangered",
  "Deidt"= "gray60",
  "Del Roy"= "gray70",
  "Derrington"= "coral2",
  "Dunn"= "gray80",
  "Eagle"= "gray50",
  "Eagle-Climax"= "gray50",
  "Eagle-Weslake"= "gray50",
  "Elder"= "gray80",
  "Embassy Hill" = "ivory",
  "Emeryson"= "gray50",
  "EMW"= "gray90",
  "ENB"= "yellow1",
  "Ensign"= "orangered3",
  "Epperly" = "gray90",
  "ERA"= "gold",
  "Euro Brun"= "orange",
  "Ewing"= "gray70",
  "Ferguson"= "purple3",
  "Ferrari" = "red",
  "Fittipaldi"= "yellow2",
  "Fondmetal"= "darkblue",
  "Footwork" = "gainsboro",
  "Force India"= "pink",
  "Forti"= "yellow",
  "Frazer Nash"= "gray80",
  "Fry" = "white",
  "Gilby"= "gray70",
  "Gordini"= "lightskyblue1",
  "Haas F1 Team"= "brown",
  "Hall"= "darkred",
  "Hesketh"= "white",
  "Honda"= "firebrick",
  "HRT"= "grey10",
  "HWM"= "gray88",
  "Iso Marlboro"= "orangered4",
  "Jaguar"= "green3",
  "JBW"= "darkgreen",
  "Jordan"= "yellow",
  "Kauhsen"= "gray60",
  "Klenk"= "gray80",
  "Kojima"= "azure3",
  "Kurtis Kraft"= "khaki1",
  "Kuzma"= "ivory",
  "Lambo"= "dodgerblue3",
  "Lancia"= "red",
  "Langley"= "deeppink",
  "Larrousse"= "forestgreen",
  "LDS" = "darkblue",
  "LDS-Alfa Romeo" = "darkblue",
  "LDS-Climax" = "darkblue",
  "LEC"= "skyblue4",
  "Lesovsky"= "gray20",
  "Leyton House"= "cyan",
  "Life"= "red",
  "Ligier"= "dodgerblue1",
  "Lola"= "dodgerblue4",
  "Lotus"= "black",
  "Lotus F1"= "black",
  "Lotus-Borgward"= "forestgreen",
  "Lotus-BRM"= "forestgreen",
  "Lotus-Climax"= "forestgreen",
  "Lotus-Ford"= "forestgreen",
  "Lotus-Maserati"= "forestgreen",
  "Lotus-Pratt &amp; Whitney"= "forestgreen",
  "Lyncar"= "springgreen4",
  "Maki"="white",
  "Manor Marussia"= "blue4",
  "March"= "orangered",
  "March-Alfa Romeo"= "orangered",
  "March-Ford"= "orangered",
  "Marchese"= "gray80",
  "Martini" = "white",
  "Marussia"= "orangered1",
  "Maserati"= "tomato2",
  "Matra"= "lightslateblue",
  "Matra-Ford"= "lightslateblue",
  "MBM" = "red3",
  "McGuire" = "darkorange3",
  "McLaren"= "orange",
  "McLaren-Alfa Romeo"= "orange",
  "McLaren-BRM"= "orange",
  "McLaren-Ford"= "orange",
  "McLaren-Serenissima"= "orange",
  "Mercedes"= "turquoise",
  "Merzario"= "lightgoldenrod2",
  "Meskowski"= "white",
  "MF1"= "firebrick1",
  "Milano" = "red",
  "Minardi"= "darkgoldenrod1",
  "Moore"= "royalblue1",
  "Nichels"= "gray50",
  "Olson" = "gay50",
  "Onyx"= "royalblue",
  "OSCA"= "red4",
  "Osella"= "paleturquoise",
  "Pacific" = "powderblue",
  "Pankratz"= "gray60",
  "Parnelli"="ivory2",
  "Pawl"= "gray90",
  "Penske"= "mediumblue",
  "Phillips"= "navyblue",
  "Politoys"= "royalblue1",
  "Porsche"= "gray90",
  "Prost" = "dodgerblue2",
  "Protos"= "firebrick3",
  "Rae"= "firebrick3",
  "RAM"= "forestgreen",
  "RE"= "yellow3",
  "Rebaque"= "wheat2",
  "Red Bull"= "darkblue",
  "Renault"= "yellow",
  "Rial"= "royalblue1",
  "Sauber"= "cornflowerblue",
  "Scarab"= "steelblue",
  "Schroeder"= "darkslategray4",
  "Scirocco"= "blue",
  "Shadow"= "black",
  "Shadow-Ford"= "black",
  "Shadow-Matra"= "black",
  "Shannon"= "grey70",
  "Sherman"= "papayawhip",
  "Simca"= "royalblue1",
  "Simtek"= "darkorchid3",
  "Snowberger"= "ivory",
  "Spirit"= "dodgerblue4",
  "Spyker"= "darkorange3",
  "Spyker MF1"= "darkorange3",
  "Stebro"= "gray80",
  "Stevens"= "white",
  "Stewart" = "white",
  "Super Aguri"= "white",
  "Surtees"= "royalblue3",
  "Sutton"= "gainsboro",
  "Talbot-Lago" = "cyan3",
  "Team Lotus"= "black",
  "Tec-Mec"= "darkred",
  "Tecno"= "firebrick1",
  "Theodore"= "white",
  "Token" = "forestgreen",
  "Toleman"= "white",
  "Toro Rosso"= "blue",
  "Toyota"= "firebrick3",
  "Trevis"="gray80",
  "Trojan"= "red3",
  "Turner"= "steelblue3",
  "Tyrrell"= "skyblue2",
  "Vanwall"= "gray80",
  "Veritas"= "gray40",
  "Virgin"= "orangered",
  "Watson"= "gray30",
  "Wetteroth"= "tomato2",
  "Williams"= "grey70",
  "Wolf"= "wheat1",
  "Zakspeed"= "brown4"
)

driver_lines <- c(
  "ALO" = "dashed",
  "BOT" = "dashed",
  "BUT" = "solid",
  "DIR" = "solid",
  "ERI" = "dashed",
  "GIO" = "solid",
  "GRO" = "dashed",
  "HAM"= "solid",
  "HUL"= "dashed",
  "PAL"= "solid",
  "MAG"= "solid",
  "KVY" = "dashed",
  "VER" = "dashed",
  "MAS" = "solid",
  "OCO" = "dashed",
  "PER" = "solid",
  "RAI" = "solid",
  "RIC" = "solid",
  "SAI" = "solid",
  "HAR" = "solid",
  "STR" = "dashed",
  "VAN" = "solid",
  "VET" = "dashed",
  "WEH" = "solid",
  "GAS" = "dashed"
)

server <- function(input, output, session) {

  #query the db to get the basic tables. we use these in various functions below
  con = dbConnect(RSQLite::SQLite(), dbname="ergast.sqlite")
  drivers <- dbGetQuery(con, "SELECT * FROM drivers")
  races <- dbGetQuery(con, "SELECT * FROM races")
  status <- dbGetQuery(con, "SELECT * FROM status")
  constructors <- dbGetQuery(con, "SELECT constructorId, constructorRef, name, url FROM constructors") #%>%
  colnames(constructors) <- c("constructorId","constructorRef","constructorName","constructorUrl")
  circuits <- dbGetQuery(con, "SELECT circuitId, name, location, country, url FROM circuits")
  colnames(circuits) <- c("circuitId","circuitName","circuitLocation","circuitCountry","circuitUrl")
  
  #make driver names
  drivers$names = paste0(toupper(drivers$surname),", ",drivers$forename)
  
  #this gets the current drivers by calling the ergast api.
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
  AllDrivers <-  drivers[order(drivers$names),]
  
  output$driverSelection <- renderUI({
    req(con)
    if (as.numeric(input$showonlycurrent) >0 ){
      selectInput("chauffeur", "Select a current driver", CurrDrivers, selected = NULL, multiple = FALSE,
                              selectize = TRUE, width = NULL, size = NULL)
   } else {
     selectInput("chauffeur", "or Select any driver", AllDrivers$names, selected = NULL, multiple = FALSE,
                              selectize = TRUE, width = NULL, size = NULL)
   }
  })

#this chunk queries the db using the driver as a query term.
#it first gets the driver id from 'drivers' using the name to subset
results <- reactive({
  req(input$chauffeur)
  driverID <- drivers[which(drivers$names == input$chauffeur),1]
  results <- dbGetQuery(con, paste0("SELECT * FROM results where ((driverId = ",paste(as.numeric(driverID),sep=""),"))"))
  results_race<- merge(results, races, by.x="raceId",by.y="raceId")
  results_race <- results_race %>%
    left_join(status) %>%
    left_join(constructors) %>%
    left_join(circuits)
  results_race$YR_RACE <- paste0(results_race$year,"_",results_race$circuitLocation)
  results_race <- results_race[(order(as.Date(results_race$date))),]
  results_race$YR_RACE <- factor(results_race$YR_RACE, levels = results_race$YR_RACE)
  results_race
})

#this filters the data using the slider inputs for the date
resultsFilter <-  reactive({
  req(results())
  resultsf <- results()
  resultsf$Date <- as.Date(results()$date,"%Y-%m-%d")
  resultsf <- resultsf[which(as.numeric(format(as.Date(resultsf$Date),'%Y')) >= as.numeric(format(input$date[1],'%Y'))),]
  resultsf[which(as.numeric(format(as.Date(resultsf$Date),'%Y')) <= as.numeric(format(input$date[2],'%Y'))),]
})

#pull out any wins
wins <- reactive({
  req(resultsFilter())
  first <- resultsFilter()[which(resultsFilter()$position == "1"),]
  first$position <- as.numeric(first$position) - 1
  first
})

#plot it all
#check out geom_emoji!!! It's what makes the trophies:
#https://github.com/dill/emoGG

#note there's two plot options, one sets a 22 driver scale for the 2017 which looks nicer, all others use a 30 driver scale
output$plot1 <- renderPlot({
  req(resultsFilter())
  if (as.numeric(input$showonlycurrent) >0 ){
  p <-  ggplot(data=resultsFilter(), aes(x=YR_RACE, y=as.numeric(as.character(position)), colour=resultsFilter()$constructorName)) +
      geom_point() +
      geom_emoji(data=wins(), aes(x=wins()$YR_RACE, y=as.numeric(as.character(wins()$position)), colour=wins()$constructorName), emoji="1f3c6") +
      scale_y_reverse(breaks=c(1,2,3,10,20), labels = c("1","2","3","10","20"), lim=c(22,-1)) +
      theme(axis.text.y = element_text(color = c("white", "white", "white", "white", "red")),
            axis.ticks.y = element_line(color = c("white", "white", "white", "white", "red"),
                                        size = c(1,1,1,1,1,1)),
            axis.title.x = element_text(colour = "white"),
            axis.title.y = element_text(colour = "white"),
            panel.background = element_rect(fill = "transparent") # bg of the panel
            , plot.background = element_rect(fill = "transparent")
            , panel.grid.major = element_blank() # get rid of major grid
            , panel.grid.minor = element_blank() # get rid of minor grid
            , legend.background = element_rect(fill = "transparent") # get rid of legend bg
            , legend.box.background = element_rect(fill = "transparent")
            ,legend.title = element_text(colour="white", size=10, 
                                         face="bold")
            ,legend.text = element_text(colour="white", size=10, 
                                        face="bold")) +
      scale_colour_manual(values = cols) +
      labs(colour='Constructor') +
      ylab("Race Finish") +
      xlab("Grand Prix") +
      theme(axis.text.x = element_text(size = rel(0.8),angle = 90, hjust = 1,colour = "white"))
  } else {
    p <-  ggplot(data=resultsFilter(), aes(x=YR_RACE, y=as.numeric(as.character(position)), colour=resultsFilter()$constructorName)) +
      geom_point() +
      geom_emoji(data=wins(), aes(x=wins()$YR_RACE, y=as.numeric(as.character(wins()$position)), colour=wins()$constructorName), emoji="1f3c6") +
      scale_y_reverse(breaks=c(1,2,3,10,20), labels = c("1","2","3","10","20"), lim=c(30,-1)) +
      theme(axis.text.y = element_text(color = c("white", "white", "white", "white", "white"))
            , axis.ticks.y = element_line(color = c("white", "white", "white", "white", "white"),
                                        size = c(1,1,1,1,1,1))
            , axis.title.x = element_text(colour = "white")
            , axis.title.y = element_text(colour = "white")
            , panel.background = element_rect(fill = "transparent") # bg of the panel
            , plot.background = element_rect(fill = "transparent")
            , panel.grid.major = element_blank() # get rid of major grid
            , panel.grid.minor = element_blank() # get rid of minor grid
            , legend.background = element_rect(fill = "transparent") # get rid of legend bg
            , legend.box.background = element_rect(fill = "transparent")
            , legend.title = element_text(colour="white", size=10, 
                                         face="bold")
            , legend.text = element_text(colour="white", size=10, 
                                        face="bold")
            , legend.key = element_rect(fill = "transparent", colour = "transparent")
            ) +
      scale_colour_manual(values = cols, na.value='grey80') +
      labs(colour='Constructor') +
      ylab("Race Finish") +
      xlab("Grand Prix") +
      theme(axis.text.x = element_text(size = rel(0.8),angle = 90, hjust = 1,colour = "white"))
  }
  print(p)
}, bg="transparent")

#this outputs the ordered, filtered dataframe
output$table1 <- renderTable({
  req(resultsFilter())
  z <- resultsFilter()
  z <- z %>%
    select(year,round,name,date,constructorName,grid,position,status)
  z[rev(order(as.Date(z$date))),]
})

##
## Race Data
##

#this pulls a raced Id using the input_season and input$round specified by the user
raceId <- reactive({
  req(input$lap_season, input$lap_race)
  races[races$year == input$lap_season & races$round == input$lap_race, ]
})

laptimes_drivers <- reactive({
  req(raceId())
  
  #this chunk grabs the laptimes using the raceId from the input season and race
  lapTimes <- dbGetQuery(con, paste0("SELECT * FROM lapTimes where ((raceId = ",paste(as.numeric(raceId()[1,1]),sep=""),"))"))
  laptimes_drivers <- merge(lapTimes,drivers, by.x='driverId',by.y='driverId')
  
  #this chunk extracts and binds the constructor to drivers. 
  #It's used for the color mapping in plot q
  racedriver_constructor <- levels(as.factor(laptimes_drivers$driverId)) %>%
    lapply(., function(x){
      racedriver_constructor <- dbGetQuery(con, paste0("SELECT driverId, constructorId FROM results where ((driverId = ",paste0(as.numeric(x)),")) AND ((raceId = ",paste0(as.numeric(raceId()[1,1])),"))"))
    }) %>%
    do.call(rbind,.) %>%
    left_join(.,constructors)
  
  #this combines and returns the laptimes and the constructor
  laptimes_drivers <- left_join(laptimes_drivers,racedriver_constructor)
  laptimes_drivers
})

#This chunk is similar to above except it pulls the pit stops.
#we keep it separate because it's plotted as a dot plot overlaying the position plot in plot q
laptimes_pits <- reactive({
  pits <- dbGetQuery(con, paste0("SELECT driverId, stop, lap FROM pitStops where ((raceId = ",paste(as.numeric(raceId()[1,1]),sep=""),"))"))
  laptimes_drivers() %>%
    full_join(pits) %>%
    filter(stop>=1)
})

#Automatically generate the race label for the plots
output$track_info <- renderText({
  req(raceId())
  raceId()[1,5]
})

#this is the lap-plot. It's a lineplot with position by driver overlayed with a dotplot of the pits.
#colors are mapped to the constructor name the values of which are specified as 'cols'
#the linetype is entered manually, I'd like to make something more automated.
output$plot2 <- renderPlot({
  req(laptimes_drivers(),laptimes_pits())
  q <- ggplot(laptimes_drivers(), aes(x=as.numeric(as.character(laptimes_drivers()$lap)),y=as.numeric(as.character(laptimes_drivers()$position)),colour=constructorName,group=code,linetype=code, label=code))
  q + geom_line() +
    geom_point(data = laptimes_pits(), aes(x=as.numeric(as.character(laptimes_pits()$lap)), y=as.numeric(as.character(laptimes_pits()$position)), fill=constructorName),shape=21,size=4) +
    geom_point(data = laptimes_pits(), aes(x=as.numeric(as.character(laptimes_pits()$lap)), y=as.numeric(as.character(laptimes_pits()$position))),shape=utf8ToInt("P"),size=3.5, color='white') +
    scale_y_reverse() +
    scale_linetype_manual(name="Driver", values=driver_lines, na.value='solid') +
    scale_colour_manual(name="Driver", values = cols, na.value='grey80') +
    scale_fill_manual(name="Driver", values = cols, na.value='grey80') +
    ylab("Position") +
    xlab("Lap") +
    geom_dl(aes(label = code), method = list(dl.combine("last.points"))) +
    theme(axis.text.x = element_text(color = "white")
          , axis.text.y = element_text(color = "white")
          , axis.ticks.x = element_line(color = "white", size = 1)
          , axis.ticks.y = element_line(color = "white", size = 1)
          , axis.title.x = element_text(colour = "white")
          , axis.title.y = element_text(colour = "white")
          , panel.background = element_rect(fill = "transparent") # bg of the panel
          , plot.background = element_rect(fill = "transparent")
          , panel.grid.major = element_blank() # get rid of major grid
          , panel.grid.minor = element_blank() # get rid of minor grid
          , legend.position='none') +
    guides(colour = guide_legend(override.aes = list(shape=32)), fill=FALSE)
}, bg="transparent")

#this is the heatmap to look at lap times.
#it uses a modified version of the heatmap.2 function, the modification makes the text white since there's no option for that in heatmap.2
output$plot3 <- renderPlot({
  req(laptimes_drivers())
  
  laptimes_matrix<-laptimes_drivers() %>%
    mutate(seconds=milliseconds/1000) %>%
    select(lap,seconds,code) %>%
    spread(lap,seconds)
  row.names(laptimes_matrix) <- laptimes_matrix[,1]
  laptimes_matrix <- laptimes_matrix[-c(1)]
  
  laptimes_matrix <- as.matrix(laptimes_matrix)
  
  quantile.range <- quantile(laptimes_matrix, probs = seq(0, 1, 0.01),na.rm=T)
  palette.breaks <- seq(quantile.range["0%"], quantile.range["80%"], 0.1)
  color.palette  <- colorRampPalette(c("#fcf9c2","#bf3975","#3f176d","#000006"))(length(palette.breaks) - 1)
  source('heatmap.2.R')
  heatmap.2(laptimes_matrix,
            margins = c(3,5),
            col = color.palette,
            breaks = palette.breaks,
            dendrogram = "none",
            symm=F,
            symkey =F,
            Rowv = T,
            Colv = F,
            srtCol = 0,
            #sepwidth = c(0.1,0.1),
            scale= c('none'),
            trace = c('none'),
            xlab = c("Lap"),
            ylab = c("Driver"),
            key.ylab = NA,
            key.title = NA,
            key.xlab = "Pace (seconds)",
            key.par = list(col.axis = 'white',col.lab='white'),
            colRow =  c(rep("white",length(laptimes_matrix[,1]))),
            colCol = c(rep("white",length(laptimes_matrix[1,]))),
            col.main= 'white',
            lmat=rbind(4:3,2:1),
            lhei=c(1,4),
            lwid=c(0.65,3.5)
  )
}, bg="transparent")

#this chunk closes the connection to the sqlite db
session$onSessionEnded(function() {
  dbDisconnect(con)
})
}
