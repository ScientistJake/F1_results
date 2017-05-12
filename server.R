library(RCurl)
library(httr)
library(XML)
library(shiny)
library(ggplot2)
library(dplyr)
library(emoGG)

server <- function(input, output) {
  cols <- c("Ferrari" = "red", 
            "Force India" = "pink",
            "Mercedes" = "turquoise",
            "Red Bull" = "darkblue",
            "Williams" = "grey70",
            "Toro Rosso" = "blue",
            "Haas" = "brown",
            "Renault" = "yellow",
            "Sauber" = "cornflowerblue",
            "McLaren" = "orange",
            "Minardi" = "darkgoldenrod1",
            "HRT" = "grey10",
            "Caterham" = "forestgreen",
            "Lotus" = "black",
            "Manor" = "blue4",
            "BMW Sauber" = "darkgray"
  )
  
  chauffeur <- reactive({
    a <- tolower(input$driver)
    a <- gsub(" ", "-", a)
    a
  })
 
   results <- reactive({
    url <- paste0("http://www.statsf1.com/en/",chauffeur(),"/grand-prix.aspx")
    h <- handle(url)
    res <- GET(handle = h)
    resXML <- htmlParse(content(res, as = "text"))
    
    Year<- getNodeSet(resXML, '//*//tr/td[2]') %>% sapply(., xmlValue)
    Year <- Year[Year != ""]
    Year <- Year[-c(1)]
    GrandPrix<- getNodeSet(resXML, '//*//tr/td[3]') %>% sapply(., xmlValue)
    GrandPrix <- GrandPrix[GrandPrix != ""]
    GrandPrix <- GrandPrix[-c(1)]
    Team<- getNodeSet(resXML, '//*//tr/td[4]') %>% sapply(., xmlValue)
    Team <- Team[-c(1)]
    Team <- Team[Team != ""]
    Num<- getNodeSet(resXML, '//*//tr/td[5]') %>% sapply(., xmlValue)
    Num <- Num[Num != ""]
    Num <- Num[-c(1)]
    Constructor<- getNodeSet(resXML, '//*//tr/td[6]') %>% sapply(., xmlValue)
    Constructor <- Constructor[Constructor != ""]
    Constructor <- Constructor[-c(1)]
    Car <- getNodeSet(resXML, '//*//tr/td[7]') %>% sapply(., xmlValue)
    Car <- Car[-c(1)]
    Car <- Car[Car != ""]
    Engine<- getNodeSet(resXML, '//*//tr/td[8]') %>% sapply(., xmlValue)
    Engine <- Engine[-c(1)]
    Engine <- Engine[Engine != ""]
    Type<- getNodeSet(resXML, '//*//tr/td[9]') %>% sapply(., xmlValue)
    Type <- Type[Type != ""]
    Type <- Type[-c(1)]
    Tyre<- getNodeSet(resXML, '//*//tr/td[10]') %>% sapply(., xmlValue)
    Tyre <- Tyre[Tyre != ""]
    Tyre <- Tyre[-c(1)]
    Grid<- getNodeSet(resXML, '//*//tr/td[11]') %>% sapply(., xmlValue)
    Grid <- Grid[Grid != ""]
    Race<- getNodeSet(resXML, '//*//tr/td[12]') %>% sapply(., xmlValue)
    Race <- Race[Race != ""]
    Race[Race == "ab"] <- "22"
    Note<- getNodeSet(resXML, '//*//tr/td[13]') %>% sapply(., xmlValue)
    Note <- Note[Note != ""]
    #Note[Note = ""] <- "NA"
    
    df <- data.frame(Year,GrandPrix,Team,Num,Constructor,Car,Engine,Type,Tyre,Grid,Race,Note, stringsAsFactors = F)
    df$YR_RACE <- paste0(Year,"_",GrandPrix)
    df
  })
   
  wins <- reactive({
    x <- results()[which(results()$Race == "1"),]
    x$Race <- as.numeric(x$Race) - 1
    x
  })
  
  p <- reactive({
    ggplot(data=results(), aes(x=YR_RACE, y=as.numeric(as.character(Race)), colour=results()$Constructor)) +
      geom_point() +
      geom_emoji(data=wins(), aes(x=wins()$YR_RACE, y=as.numeric(as.character(wins()$Race)), colour=wins()$Constructor), emoji="1f3c6") +
      scale_y_reverse(breaks=c(1,2,3,10,22), labels = c("1","2","3","10","DNF"), lim=c(22,-1)) +
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
           , legend.box.background = element_rect(fill = "transparent")) +
      scale_colour_manual(values = cols) +
      labs(colour='Constructor') +
      ylab("Race Finish") +
      xlab("Grand Prix") +
      theme(axis.text.x = element_text(size = rel(0.8),angle = 90, hjust = 1,colour = "white"))
  })
  
  output$plot1 <- renderPlot({
    print(p())
  }, bg="transparent")
    
  output$table1 <- renderTable({
    results()[-c(13)]
  })
}
