  # the official link for learning that api: https://rstudio.github.io/leaflet/markers.html
  ## Group 6
  output$CrimeMap <- renderLeaflet({

    ## Control Icon size and looks
    new_data <- read.csv("crime_to_plot.csv")
    tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
    basemap <- leaflet(width = "100%", height = "400px") %>%
      addTiles(tilesURL)
    colors = c("red", "orange", "yellow")
    basemap %>%
      addMinicharts(
        new_data$LONG, new_data$LAT,
        type = "pie",
        chartdata = new_data[, c("FELONY", "MISDEMEANOR", "VIOLATION")], 
        colorPalette = colors, 
        width = 60 * sqrt(new_data$TOTAL) / sqrt(max(new_data$TOTAL)), transitionTime = 0
      )
 
  })

  # this is some of the operation of leaflet design, basicly,
  # how to add the links to some specific points
  # how to add the lines and legends for the leaflet map
  # there are more good elements of leaflet for the 

  ## group 7
  ## Leaflet map
  ### Part 3: Find Your Location
  
  ## Leaflet map
  output$usmap <- renderLeaflet({
    # Extract the data based on the user's choice
    # Subset the data to the occupation that the user has chosen
    tmp<-subset(data,OCC_TITLE == as.character(input$occupation))
    
    # Match the default state name in mapStates to the state name in our data (tmp)
    state.shortname <- as.data.frame(substr(mapStates$names, 1, 8))
    colnames(state.shortname) <- "state.shortname"
    tmp$state.shortname <- substr(tolower(tmp$STATE), 1, 8)
    tmp.ordered <- merge(state.shortname, tmp, by="state.shortname", all.x = T)
    tmp.ordered <- cbind(mapStates$names, tmp.ordered)
    
    # Only draw the map when occupation is selected (tmp is not NULL)
    if(nrow(tmp)>0){
      
      # Map the different levels of salary to different colors
      pal <- colorNumeric(palette="YlGnBu", domain=tmp.ordered$A_MEAN)
      
      # Prepare the layer labels to show state name and salary 
      # when the mouse is over certain states
      labels <- sprintf(
        "<strong>%s</strong><br/>$ %g Annual Salary",
        tmp.ordered$STATE, tmp.ordered$A_MEAN) %>% 
        lapply(htmltools::HTML)
      
      # Draw the leaflet map
      leaflet(data = mapStates) %>%
        addProviderTiles("Stamen.TonerLite") %>%  # default map, base layer
        
        # Draw color polygons over states according to salary in selected occupation
        addPolygons(fillColor = ~pal(tmp.ordered$A_MEAN),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    layerId = ~tmp.ordered[,1],
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    label = labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal",
                                   padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        addLegend("bottomleft", pal = pal, values = ~tmp.ordered$A_MEAN,
                  title = "Salary Level",
                  labFormat = labelFormat(prefix = "$"),
                  opacity = 1)
      
    }
    
  })
  
  # Output Panel
  # todo, here is an observeEvent which can enable the interactive user case
  observeEvent(input$usmap_shape_click, {
    # Observe the map shape click
    click <- input$usmap_shape_click
    
    tmp<-subset(data,OCC_TITLE == "Management Occupations")
    state.shortname <- as.data.frame(substr(mapStates$names, 1, 8))
    colnames(state.shortname) <- "state.shortname"
    tmp$state.shortname <- substr(tolower(tmp$STATE), 1, 8)
    tmp.ordered <- merge(state.shortname, tmp, by="state.shortname", all.x = T)
    tmp.ordered <- cbind(mapStates$names, tmp.ordered)
    state.name <- tmp.ordered[tmp.ordered[,1] == click$id, "STATE"]

    # display the state name in the output panel
    output$state_name <- renderText(state.name)
    
    # display the line chart of GDP trend in the output panel
    output$click_gdp_trend<- renderPlotly({
      df <- as.data.frame(t(gdp.aer.rpp)[1:4,])
      colnames(df) <- gdp.aer.rpp[,1]
      df <- df[-1,]
      plot.df <- data.frame(year=2014:2016,gdp=df[,state.name])
      plot_ly(x=plot.df$year,y=plot.df$gdp, type='scatter', mode = 'lines') %>%
        layout(xaxis=list(title="Years",tickfont=list(size=9)),
               yaxis=list(title="GDP",tickfont=list(size=9)))
    })
    
    # display the pie chart of recreation level in the output panel
    output$click_amusement_pie<- renderPlotly({
      df <- as.data.frame(t(gdp.aer.rpp)[c(1,7),])
      colnames(df) <- gdp.aer.rpp[,1]
      df <- df[-1,]
      plot.df <- data.frame(year=2016,aer=df[,state.name])
      plot.df$aer <- as.character(plot.df$aer)
      plot.df$aer <- as.numeric(substr(plot.df$aer,1,nchar(plot.df$aer)-1))
      plot.df$uaer <- 100-plot.df$aer
      plot.df[2,] <- c("YEAR","AER","UAER")
      plot.df <- t(plot.df)
      plot.df <- plot.df[-1,]
      plot.df[,1] <- as.numeric(as.character(plot.df[,1]))/100
      plot.df <- as.data.frame(plot.df)
      plot_ly(data=plot.df,values = ~plot.df[,1],labels = ~plot.df[,2], type='pie') %>%
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
  })

  ## group 2
  ## it just soso

  ## Group 5
  ## they do a nice interactive charts and the iron is also replaced.
  ##For display of Customer Review
  ### also, they have a good ccs, and I can learn from that
  ## also, for the last part,they also make a fantastic good selection plot, also interactive and interesting.
  ## download the app and try it myself. there are lots of things can be learned from 

  output$carrierPlot2 <- renderFormattable({
    data_rate <- data_satisfy[data_satisfy$CARRIER == input$carrier2, ]
    df <- data.frame(
      'Rating_Type' = c("Food and Drink", "Entertainment", "Seat Comfort",
                        "Stuff Service", "Value For Money"),
      'Rating_Scores' = as.numeric(data_rate[,3:7])
    )
    names(df) <- c("Rating Type", "Rating Scores")
    
    image_tile <- formatter("img",
                            src = x ~ carr_select(x),
                            NA)
    
    formattable(df, list('Rating Scores' = image_tile))
  })

  ##For display  of Recommendation
  sub_data<-reactive({list(data=combined_data[combined_data$ORIGIN_CITY==as.character(str_to_title(input$from)) & 
                                                combined_data$DEST_CITY==as.character(str_to_title(input$to)) & 
                                                combined_data$QUARTER==as.numeric(input$quarter),
                                              -c(1,2,3,4)])})
  output$par <- renderParcoords({
    parcoords(sub_data()$data, rownames = F
              , brushMode = "1d-axes"
              , reorderable = T
              , queue = F
              , color = list(
                colorBy = "Carrier"
                , colorScale = htmlwidgets::JS("d3.scale.category10()"))
    )
  }) 

  ## Group 4
  ## they give out a link which is really good, and I want to know what kinds of tech are they using in the template app
  ## https://ny.eater.com/maps/best-new-nyc-restaurants-heatmap
  ## by the way, they also have a good css file and a good theme, learn how to do that.
  ## also, in the third panel, they combine a datatable and select(interative) function, which needs to be checked
  ## download that app and examine it more detailed
  ## it seems that there are api for the datatable because they just use the rank2_rows_select
  output$maprec2 <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -73.9712, lat = 40.7500, zoom = 13) %>%
      addMarkers(lng = as.numeric(Rank2$longitude),
                 lat = as.numeric(Rank2$latitude),
                 popup = paste("<b>Rank:</b>", Rank2$Rank, "<br>",
                               "<b>Name:</b>", Rank2$Name, "<br>",
                               "<b>Address:</b>", Rank2$Address, "<br>",
                               "<b>Description:</b>", Rank2$Description)) %>%
      addPopups(lng = as.numeric(Rank2$longitude[input$Rank2_rows_selected]),
                lat = as.numeric(Rank2$latitude[input$Rank2_rows_selected]),
                popup = paste("<b>Rank:</b>", Rank2$Rank[input$Rank2_rows_selected], "<br>",
                              "<b>Name:</b>", Rank2$Name[input$Rank2_rows_selected], "<br>",
                              "<b>Address:</b>", Rank2$Address[input$Rank2_rows_selected], "<br>",
                              "<b>Description:</b>", Rank2$Description[input$Rank2_rows_selected]))
  })


  ## Group 9
  # I m not so interested in that thesis, but it seems that it have interactive photo gallery, and I will check it latter
  # https://github.com/TZstatsADS/Spring2018-Project2-Group9/blob/master/app/server.R
  # also, some good visualization part, and some 3d bar plot may be useful latter
  # https://yiyi-zhang-cu.shinyapps.io/finding_coral/
  # also, the interactive plotly work with their own themes is also nice to check\
  # download the app and see more

  ## Group 3
  # show the trace function is really good, and I want know how to do that
  # also, the good thing is that the dot panel, the database is how organized and where to download
  # https://github.com/TZstatsADS/Spring2018-Project2-Group3
  # todo, for the template part and the trace markup
  # download and examine for more


  # http://shiny.rstudio.com/articles/debugging.html
  # check the downloaded tutorial and recheck the details.

  ### Group 1
  # a good interface look, check the css
  # a good 911 fire reponse panel, a good animation and how do they archieve that
  # but their presentation and using of the plots is not so good
  # statistics incidents in bouroughs is a nice panel


  ## group 10
  # how to scrape the data from linkedin ?? I need to check that 
  # RLinkedin https://github.com/TZstatsADS/Spring2018-Project2-Group10
  # the panels works good, and check their website design.
  # 

  ## group 8
  # https://spring-2018-project2-group8.shinyapps.io/group8/
  # https://github.com/TZstatsADS/Spring2018-Project2-Group8
  # that is really nice because it can use the clicks to zoom in more detailed information and much better than the other leaflet apps
  output$map <- renderLeaflet({
    content <- paste(sep = "<br/>",
                     paste("<font size=1.8>","<font color=green>","<b>",v3()$Hospital.Name,"</b>"),
                     paste("<font size=1>","<font color=black>",v3()$Address),
                     paste(v3()$City, v3()$State, v3()$ZIP.Code, sep = " "),  
                     paste("(",substr(v3()[ ,"Phone.Number"],1,3),") ",substr(v3()[ ,"Phone.Number"],4,6),"-",substr(v3()[ ,"Phone.Number"],7,10),sep = ""), 
                     paste("<b>","Hospital Type: ","</b>",as.character(v3()$Hospital.Type)),  
                     paste("<b>","Provides Emergency Services: ","</b>",as.character(v3()[ ,"Emergency.Services"])),

                     paste("<b>","Overall Rating: ","</b>", as.character(v3()[ ,"Hospital.overall.rating"])),
                     paste("<b>","Personalized Ranking: ","</b>",v3()$Rank))
    
    
    mapStates = map("state", fill = TRUE, plot = FALSE)
    leaflet(data = mapStates) %>% addTiles() %>%
      addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE) %>%
      addMarkers(v2()$lon, v2()$lat, popup = content, icon = hospIcons[v2()$TF], clusterOptions = markerClusterOptions())
  })
