# global file for Final Project
# 
# created 11/06/2017
# 
# used for the data processing, workspace load and global package load.
# 
## Packages

packages.used <- 
  c("geosphere", # For spatial methods  
    "threejs",   # threejs is used for 3-D interactive Earth Visualization
    "rworldmap", # For creating earth map
    "leaflet",   # Leaflet for R provides functions to control and integrate Leaflet, a JavaScript library for interactive maps, within R.
    "rgeos",      # Provides functions for handling operations on topologies.
    "raster",     # For raster image
    "DT",         # For creating interactive tables
    "ggplot2",
    "sp"   ,       # For Spatial processing of data
    "ggmap",       # To reverse geocode Long/Lat
    "knitr",        # TO enable 3-D visualization embedding in the HTML page
    "rglwidget",
    "rgl",
    "plyr",
    "reshape2",
    "maptools",
    "shiny",
    "googleVis",
    "dplyr",
    "plotly",
    "RColorBrewer",
    "treemap",
    "gplots",
    "readxl",
    "shinythemes",
    "countrycode",
    "wordcloud",
    "tm",
    "hunspell",
    "NLP",
    "pdftools",
    "tokenizers",
    # "RWeka", text mining package, ran to some error on Macbook, so I just swtich to another package and will a close look later, 20171204
    "SnowballC"
    
  )

# check packages that need to be installed.
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))
# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}

#load the packages
library("gplots")
library("plyr")
library("dplyr")
library("reshape2")
library("geosphere")
library("threejs")
library("rworldmap")
library("leaflet")
library("rgeos")
library("raster")
library("DT")
library("ggplot2")
library("sp")
library("ggmap")
library("knitr")
library("rglwidget")
library("rgl")
library("maptools")
library("shiny")
library("googleVis")
library("plotly")
library("grid")
library("gtable")
library("treemap")
library("RColorBrewer")
library("readxl")
library("shinythemes")
library("countrycode")
library("tm")
library("pdftools") # used for read pdf
library("hunspell") # used for collection of the words
# library("RWeka")
library("NLP")
library("wordcloud")
library("SnowballC")
library("tokenizers")
library("readxl")

## Data load and proceed
input_data =  read.csv("mydata.with.region.csv",header = T,as.is = T)
input_data = input_data[!is.na(input_data$longitude),]
input_data = input_data[input_data$value != 0,]
input_data[!is.na(input_data$Commodity_Name) & input_data$Commodity_Name == "COCOA",10] = "Cocoa"
#Load the data for Google motion data
country<-read.csv("country.cleaned.csv")
# force all values in country dataset to be numeric
for (i in 3:15){
  country[,i]<-as.numeric(country[,i])
}

data(wrld_simpl) # Basic country shapess

bgcolor = "#000000"
arc_colors = c("#ffdbdb","#c4e0ff","#e8fff0","#ffe9bf","pink","orange")
map_pal = data.frame(AnnualAggregate = c("red"),Chocolate = c("blue"),Coffee = c("green"),COCOA = c("#ffe9bf"),Spices = c("pink"),Tea = c("orange"))
names(map_pal)[1] = "Annual Aggregate"


cluster_data_import = read.csv("clustering.import.csv")
code = read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')[,c(1,3)]
# this data is just used for the label part in the market share panel, so sounds little useless.
sample_data <- read.csv("sample.total.csv")


link <- "world_trade_report17_e.pdf"
# used for the pdf view
word <- read.csv("common.words.0317.csv")

## save the workspace to .RData
## 
## load the Data from the .RData, which can fasten the process.
## 