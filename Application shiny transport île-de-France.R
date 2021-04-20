#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(gdalUtils)
library(gdalUtilities)
library(gdalcubes)
library(rsconnect)
library(shiny)
library(shinydashboard)
library(sf)
library(stringr)
library(cartography)
library(cartogram)
library(readxl)
library(countrycode)
library(readr)
# Import des librairies nécessaires
library("sp")
library("rgdal")
library("sf")
library("RColorBrewer")
library("classInt")
library("tidyverse")
library("ggplot2")
library("ggmap")
library("maptools")
library("rgeos")
library(cartography)
library(dplyr)
library(leaflet)
library(dplyr)
library(rgdal)
library(rgdax)
library(rgeos)
library(raster)
#####################################################################################################################
st_drivers()

emplacement_des_gares <- read_delim("emplacement-des-gares.csv",";", escape_double = FALSE, trim_ws = TRUE)
gares_routieres_idf <- read_delim("gares-routieres-idf.csv",";", escape_double = FALSE, trim_ws = TRUE)
ligne <- st_read(dsn ="lignes-gtfs.shp",stringsAsFactors = F)
ligne$route_type[723]="Tram"

listestations <- read_delim("listestations.csv",
                            ";", escape_double = FALSE, trim_ws = TRUE)

nomligne=listestations %>%separate(col = "Geo Point",into = paste0("evenement", 1:2), sep = ",",extra = "merge") %>%
    subset(select = c(evenement2:evenement1,nom_gare,mode_,ligne))

colnames(nomligne)=c("longitude","latitude","nom_gare","mode","ligne")

nomligne$longitude=as.numeric(nomligne$longitude)
nomligne$latitude=as.numeric(nomligne$latitude)
#####################################################################################################################

bus <- read_delim("bus.csv", 
                  ";", escape_double = FALSE, trim_ws = TRUE)

bus=bus %>%separate(col = "Geo Point",into = paste0("evenement", 1:2), sep = ",",extra = "merge") %>%
    subset(select = c(evenement2:evenement1, nom ))

colnames(bus)=c("longitude","latitude","nom")

bus$longitude=as.numeric(bus$longitude)
bus$latitude=as.numeric(bus$latitude)

######################################################################
gare_localisation=emplacement_des_gares %>%separate(col = "Geo Point",into = paste0("evenement", 1:2), sep = ",",extra = "merge") %>%
    subset(select = c(evenement2:evenement1,train,rer,metro,tramway,navette,nom_long))

gare_localisation$bus=0


######################################################################
colnames(gare_localisation)=c("longitude","latitude","train","rer","metro","tramway","navette","nom_gare","bus")

######################################################################
gares_routieres_idf$train=0
gares_routieres_idf$rer=0
gares_routieres_idf$metro=0
gares_routieres_idf$tramway=0
gares_routieres_idf$navette=0
gares_routieres_idf$bus=1
######################################################################
gare_routiere=gares_routieres_idf %>%separate(col = "Geo Point",into = paste0("evenement", 1:2), sep = ",",extra = "merge") %>%
    subset(select = c(evenement2:evenement1,train,rer,metro,tramway,navette,GARE_NOM,bus))

######################################################################
colnames(gare_routiere)=c("longitude","latitude","train","rer","metro","tramway","navette","nom_gare","bus")

######################################################################
DT<-rbind(gare_routiere,gare_localisation)

######################################################################
DT$type=0
######################################################################
for ( i in 1:nrow(DT)) {
    
    if (DT$bus[i]==1){
        DT$type[i]=1
    } else if (DT$train[i]==1){
        DT$type[i]=2
    } else if (DT$rer[i]==1){
        DT$type[i]=3
    }else if (DT$metro[i]==1){
        DT$type[i]=4
    }else if (DT$tramway[i]==1){
        DT$type[i]=5
    }else {
        DT$type[i]=2
    }
    
}

######################################################################

getColor <- function(DT) {
    sapply(DT$type, function(type) {
        if (type == 1) {
            "blue"
        }else if(type==2) {
            "orange"
        } else if(type == 3) {
            "yellow"
        }else if(type == 4) {
            "green"
        }else if(type == 5) {
            "red"
        }else {
            "black"
        } })
}


icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = getColor(DT)
)
######################################################################
DT$longitude<-as.numeric(DT$longitude)
DT$latitude<-as.numeric(DT$latitude)

carte=leaflet(DT) %>% addTiles()  %>% addProviderTiles("Esri.WorldGrayCanvas") 

######################################################################
Tout=leaflet(DT) %>% addTiles()  %>%  setView(2.3522219,48.856614,zoom = 8) %>%
    addAwesomeMarkers(lng = ~longitude, lat = ~latitude, icon=icons, label=~as.character(nom_gare),clusterOptions = markerClusterOptions()) %>%
    addPolygons(data = ligne[ligne$route_type=="Subway",] ,fill=FALSE, color = "green",opacity = 1)%>%
    addPolygons(data = ligne[ligne$route_type=="Tram",] ,fill=FALSE, color = "red",opacity = 0.8)%>%
    addPolygons(data = ligne[ligne$route_type=="Bus",] ,fill=FALSE, color = "blue",opacity = 0.3)%>%
    addPolygons(data = ligne[ligne$route_type=="Rail",] ,fill=FALSE, color = "orange",opacity = 0.6) %>%
    addPolygons(data = ligne[ligne$route_type=="Rail"&ligne$route_long_=="A",] ,fill=FALSE, color = "red",opacity = 0.3)%>%
    addPolygons(data = ligne[ligne$route_type=="Rail"&ligne$route_long_=="B",] ,fill=FALSE, color = "red",opacity = 0.3)%>%
    addPolygons(data = ligne[ligne$route_type=="Rail"&ligne$route_long_=="C",] ,fill=FALSE, color = "red",opacity = 0.3)%>%
    addPolygons(data = ligne[ligne$route_type=="Rail"&ligne$route_long_=="D",] ,fill=FALSE, color = "red",opacity = 0.3)%>%
    addPolygons(data = ligne[ligne$route_type=="Rail"&ligne$route_long_=="E",] ,fill=FALSE, color = "red",opacity = 0.3)
######################################################################
iconstrain <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = "orange"
)

iconsstatonrer <- awesomeIcons(icon = 'ios-close',iconColor = 'black',library = 'ion',markerColor = "yellow")


Train = leaflet(DT[DT$train==1,]) %>% addTiles()   %>% addAwesomeMarkers(lng = ~longitude, lat = ~latitude, icon=iconstrain, label=~as.character(nom_gare),clusterOptions = markerClusterOptions()) %>% setView(2.3522219,48.856614,zoom = 8) %>%
    addPolygons(data = ligne[ligne$route_type=="Rail",] ,fill=FALSE, color = "orange",opacity = 1)

#Train=carte%>% addPolygons(data = ligne[ligne$route_type=="Rail",] ,fill=FALSE, color = "orange",opacity = 1)
######################################################################

iconsbus <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = "blue"
)



Bus=leaflet(bus) %>% addTiles()  %>% 
    addAwesomeMarkers(lng = ~longitude, lat = ~latitude, icon=iconsbus, label=~as.character(nom),clusterOptions = markerClusterOptions()) %>% setView(2.3522219,48.856614,zoom = 8) %>%
    addPolygons(data = ligne[ligne$route_type=="Bus",] ,fill=FALSE, color = "blue",opacity = 0.2)

#Bus=carte%>%addPolygons(data = ligne[ligne$route_type=="Bus",] ,fill=FALSE, color = "blue",opacity =1)
######################################################################

iconstram <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = "red"
)

iconsr <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'red',
    library = 'ion',
    markerColor = "black"
)


iconsmetro <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = "green"
)

Tram = leaflet(DT[DT$tramway==1,]) %>% addTiles()    %>% setView(2.3522219,48.856614,zoom = 10) %>% addAwesomeMarkers(lng = ~longitude, lat = ~latitude, icon=iconsr, label=~as.character(nom_gare),clusterOptions = markerClusterOptions()) %>%
    addPolygons(data = ligne[ligne$route_type=="Tram",] ,fill=FALSE, color = "black",opacity = 1)

#Tram=carte%>% addPolygons(data = ligne[ligne$route_type=="Tram",] ,fill=FALSE, color = "red",opacity = 1)
######################################################################


Metro = leaflet(DT[DT$metro==1,]) %>% addTiles()  %>% addAwesomeMarkers(lng = ~longitude, lat = ~latitude, icon=iconsmetro, label=~as.character(nom_gare),clusterOptions = markerClusterOptions()) %>% setView(2.3522219,48.856614,zoom = 10) %>%
    addPolygons(data = ligne[ligne$route_type=="Subway",] ,fill=FALSE, color = "green",opacity = 1)

#Metro=carte%>% ddPolygons(data = ligne[ligne$route_type=="Subway",] ,fill=FALSE, color = "green",opacity = 1)



RER=leaflet() %>% addTiles()%>% setView(2.3522219,48.856614,zoom = 9) %>%
    addAwesomeMarkers(data=nomligne[nomligne$ligne== "RER A"|nomligne$ligne== "RER B"|nomligne$ligne== "RER C"|nomligne$ligne== "RER D"|nomligne$ligne== "RER E",],lng = ~longitude, lat = ~latitude, icon=iconstram, label=~as.character(nom_gare),clusterOptions = markerClusterOptions()) %>%
    addPolygons(data = ligne[ligne$route_type=="Rail"&ligne$route_long_=="A",] ,fill=FALSE, color = "red",opacity = 1)%>%
    addPolygons(data = ligne[ligne$route_type=="Rail"&ligne$route_long_=="B",] ,fill=FALSE, color = "red",opacity = 1)%>%
    addPolygons(data = ligne[ligne$route_type=="Rail"&ligne$route_long_=="C",] ,fill=FALSE, color = "red",opacity = 1)%>%
    addPolygons(data = ligne[ligne$route_type=="Rail"&ligne$route_long_=="D",] ,fill=FALSE, color = "red",opacity = 1)%>%
    addPolygons(data = ligne[ligne$route_type=="Rail"&ligne$route_long_=="E",] ,fill=FALSE, color = "red",opacity = 1)





#####################################################################################################################
#####################################################################################################################

library(shiny)
library(leaflet)

ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("mymap", width = "100%", height = "100%"),
    absolutePanel(top = 10, right = 10,
                  selectInput("select", label = h3("Type de transport"),
                              choices = list("Reseau entier"="Tout","Train" ="Train" ,"RER"="RER", "Bus" =  "Bus", "Tram" = "Tram", "Metro" = "Metro"),
                              selected = 1),
                  checkboxInput("legend", "Legend", TRUE),
                  uiOutput("cityControls")
    )
)



server <- function(input, output, session) {
    
    
    output$cityControls <- renderUI({
        vide=c("Choisir le type de transport")
        listeM=c(ligne[ligne$route_type=="Subway",]$route_long_,"--")
        listeB=c(ligne[ligne$route_type=="Bus",]$route_long_,"--")
        listeRER=c("A","B","C","D","E","--")
        listeTr=c("TER" ,"P" ,  "J", "N" ,  "R" ,  "K","N" ,  "R" ,  "K","--")
        listeT=c(ligne[ligne$route_type=="Tram",]$route_short,"--")
        if(input$select=="Tout"){
            selectInput("line", "Ligne ", vide )
        }else if (input$select=="Train"){
            selectInput("line", "Ligne ", sort(listeTr))
        }else if (input$select=="RER"){
            selectInput("line", "Ligne ", sort(listeRER))
        }else if (input$select=="Bus"){
            selectInput("line", "Ligne ", sort(listeB))
        }else if (input$select=="Tram"){
            selectInput("line", "Ligne", sort(listeT))
        }else if (input$select=="Metro"){
            selectInput("line", "Ligne ", sort(listeM))
        }
        
    })
    
    output$mymap <- renderLeaflet({
        nomligne$ligne=as.character(nomligne$ligne)
        ligne$route_short=as.character(ligne$route_short)
        ligne$route_long_=as.character(ligne$route_long_)
        
        if(input$select=="Tout"){Tout
        }else if (input$select=="Train"){
            if (input$line=="--"){Train}
            else if (input$line=="H"|input$line=="J"|input$line=="K"|input$line=="L"|input$line=="N"|input$line=="P"|input$line=="R"|input$line=="U"){leaflet() %>% addTiles()%>%
                    addAwesomeMarkers(data=nomligne[nomligne$ligne== paste("LIGNE", input$line ),],lng = ~longitude, lat = ~latitude, icon=iconstrain, label=~as.character(nom_gare)) %>%
                    addPolygons(data = ligne[ligne$route_type=="Rail"&ligne$route_long==input$line,] ,fill=FALSE, color = "orange",opacity = 1)}
            else if (input$line=="TER"){leaflet() %>% addTiles()%>%
                    addAwesomeMarkers(data=nomligne[nomligne$ligne==  "GRD LIGNES",],lng = ~longitude, lat = ~latitude, icon=iconstrain, label=~as.character(nom_gare)) %>%
                    addPolygons(data = ligne[ligne$route_type=="Rail"&ligne$route_long=="TER",] ,fill=FALSE, color = "orange",opacity = 1)}
        }else if (input$select=="RER"){
            if (input$line=="--"){RER}
            else if(input$line=="A"|input$line=="B"|input$line=="C"|input$line=="D"|input$line=="E"){
                leaflet() %>% addTiles()%>%
                    addAwesomeMarkers(data=nomligne[nomligne$ligne== paste("RER", input$line ),],lng = ~longitude, lat = ~latitude, icon=iconstram, label=~as.character(nom_gare)) %>%
                    addPolygons(data = ligne[ligne$route_type=="Rail"&ligne$route_long_==input$line,] ,fill=FALSE, color = "red",opacity = 1)}
            
        }else if (input$select=="Bus"){
            if (input$line=="--"){Bus}
            else if (input$line!="--"){leaflet(bus) %>% addTiles()  %>% 
                    addAwesomeMarkers(lng = ~longitude, lat = ~latitude, icon=iconsbus, label=~as.character(nom),clusterOptions = markerClusterOptions()) %>%
                    addPolygons(data = ligne[ligne$route_type=="Bus"&ligne$route_long_==input$line,] ,fill=FALSE, color = "blue",opacity = 0.8)}}
        else if (input$select=="Tram"){
            if (input$line=="--"){Tram}
            else if (input$line!="--"){leaflet() %>% addTiles()%>%
                    addAwesomeMarkers(data=nomligne[nomligne$ligne==input$line,],lng = ~longitude, lat = ~latitude, icon=iconsr, label=~as.character(nom_gare)) %>%
                    addPolygons(data = ligne[ligne$route_type=="Tram"&ligne$route_short==input$line,] ,fill=FALSE, color = "black",opacity =1)}
        }else if (input$select=="Metro"){
            if (input$line=="--"){Metro}
            else if (input$line=="7B"){leaflet() %>% addTiles()%>%
                    addAwesomeMarkers(data=nomligne[nomligne$ligne=="7b",],lng = ~longitude, lat = ~latitude, icon=iconsmetro, label=~as.character(nom_gare)) %>%
                    addPolygons(data = ligne[ligne$route_type=="Subway"&ligne$route_long_=="7B",] ,fill=FALSE, color = "green",opacity = 0.5)}
            else if (input$line=="3B"){leaflet() %>% addTiles()%>%
                    addAwesomeMarkers(data=nomligne[nomligne$ligne=="M3bis",],lng = ~longitude, lat = ~latitude, icon=iconsmetro, label=~as.character(nom_gare)) %>%
                    addPolygons(data = ligne[ligne$route_type=="Subway"&ligne$route_long_=="3B",] ,fill=FALSE, color = "green",opacity = 0.5)}
            else if (input$line!="--"){ leaflet() %>% addTiles()%>%
                    addAwesomeMarkers(data=nomligne[nomligne$ligne==input$line,],lng = ~longitude, lat = ~latitude, icon=iconsmetro, label=~as.character(nom_gare)) %>%
                    addPolygons(data = ligne[ligne$route_type=="Subway"&ligne$route_long_==input$line,] ,fill=FALSE, color = "green",opacity = 0.5)}
        }
        
    })
    
    
    
    
    observe({
        proxy <- leafletProxy("mymap", data = DT)
        
        # Remove any existing legend, and only if the legend is
        # enabled, create a new one.
        proxy %>% clearControls()
        if (input$legend) {
            proxy %>%
                addLegend("bottomright", colors =c( "blue", "orange", "red", "green","black"),
                          labels= c("bus","Train","RER","Metro", "Tramway"),
                          title= "Type",
                          opacity = 0.85)%>% addControl("Carte du reseau transilien en Ile-de-France", position = "topleft")%>% addControl("Sabaye Fried-Junior", position = "bottomleft")%>% addControl("Kashala Ilunga Caleb", position = "bottomleft")
        }
    })
    
    
    
    
    
    
}




shinyApp(ui, server)

