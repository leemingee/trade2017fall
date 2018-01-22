# server file for Final Project
# 
# created 11/06/2017 by Ming Li
# 
# used for the implememtation of the backend of the data selection and plotting.
# 
## Server function

server<- function(input, output){
  
  ## Introduction page
  output$blankspace = renderUI({
    HTML("<br/><br/><br/><br/><br/><br/><br/><br/>")
  })
  output$Introduction = renderUI({
    HTML("<br/><br/><br/>Our project looks into the trade of coffee, tea, chocolate, cocoa and spices<br/>
         between USA and the rest of the world<br/><br/><br/><br/>")
  })
  output$teammates <- renderUI({
    HTML("Team Nevergiveup: Ming Li, Jinhan Chen")
  })
  output$thanks <- renderUI({
    HTML("<br/><br/>Thanks for Prof. Shilane to be the guider for us in this project<br/><br/>Thanks for the all packages we used for creating this webpage<br/><br/>Specially thanks for the computers located in the second floor of Sci & Eng Lib, inspite of their GRANDPA version of R packages, 64GB memory and the excellent 27-inch monitor much fasten the development process")
  })
  
  ## todo, 20171108
  ## the UI has created clusters of output objects, and generate and modify the objects including plots, charts and two maps on earth.
  # output $ so many many things to be completed
  
  ## ===============================  
  ## 3D globe page
  ## get the code from Internet
  output$Globe <- renderGlobe({
    ##### subset dataframe
    temp = input_data
    temp = subset(temp,Commodity_Name == as.character(input$commodity_3D))
    temp = subset(temp,Year == as.integer(input$year_3D))
    temp = subset(temp,type == as.character(input$type))
    temp = arrange(temp,desc(value))[1:input$number_countries,]
#     index = match(input$commodity_3D,
#                   c("Milk", "Wheat", "Rice", "Barley", "Maize", "Other.Cereals", 
#                     "Juices", "Coffee", "Cocoa", "Tea", "Silk", "Cotton", "Annual Aggregate"))
    index = match(input$commodity_2D,c('Annual Aggregate','Chocolate', 'Coffee','Cocoa','Spices','Tea'))
    maxValue = log(max(temp$value))
    ##### end subset

    ##### map colors creation
    earth <- tempfile(fileext=".jpg")
    jpeg(earth, width=2048, height=1024, quality=100, bg=bgcolor, antialias="default")
    par(mar = c(0,0,0,0),    pin = c(4,2),    pty = "m",    xaxs = "i",
        xaxt = "n",          xpd = FALSE,    yaxs = "i",    yaxt = "n")

    map_palette = map_pal[,index]
    clrs = rep('#050505', length(wrld_simpl$NAME))
    names(clrs) = wrld_simpl$NAME
    clrs[temp$Country] = alpha(map_palette[1], log(temp$value)/maxValue*0.1)

    plot(wrld_simpl,  col=clrs,   bg=bgcolor,  border="#757575", cex = 0.1,  ann=FALSE,
         axes=FALSE,  xpd=FALSE,  xlim=c(-180,180), ylim=c(-90,90),  setParUsrBB=TRUE)

    graphics.off()
    ##### end map creation

    ##### globe plotting
    globejs(earth, bg="white", emissive="#77B8FF",
            fov = 38,
            arcs=temp[,c(4,3,9,8)],
            arcsHeight=0.35,
            arcsLwd=2,
            arcsColor = arc_colors[index],
            arcsOpacity=0.5,
            atmosphere=TRUE, height=600, width = 600
    )
  })
  
  ## end 3D globe page
  ## ===============================  
  
  ## 2D global plot page
  ## get the code from the Internet and Google map API
  output$mymap <- renderLeaflet({
    ## Control Icon size and looks
    levelIcon <- iconList(
      level1 = makeIcon("trade-icon_1.png", iconAnchorX = 19, iconAnchorY = 19),
      level2 = makeIcon("trade-icon_2.png", iconAnchorX = 19, iconAnchorY = 19),
      level3 = makeIcon("trade-icon_3.png", iconAnchorX = 19, iconAnchorY = 19),
      level4 = makeIcon("trade-icon_4.png", iconAnchorX = 19, iconAnchorY = 19),
      level5 = makeIcon("trade-icon_5.png", iconAnchorX = 19, iconAnchorY = 19),
      level6 = makeIcon("trade-icon_6.png", iconAnchorX = 19, iconAnchorY = 19),
      level7 = makeIcon("trade-icon_7.png", iconAnchorX = 19, iconAnchorY = 19),
      level8 = makeIcon("trade-icon_8.png", iconAnchorX = 19, iconAnchorY = 19)
    )
    Icon = makeIcon(iconAnchorX = 19, iconAnchorY = 19,
                    iconWidth = 38, iconHeight = 38)
    ## subset the data
    US = data.frame(Country = "US",longitude = -95.71289,latitude = 37.09024)
    ##### subset dataframe
    tmp = input_data
    tmp = subset(tmp,Commodity_Name == as.character(input$commodity_2D))
    tmp = subset(tmp,Year == as.integer(input$year_2D))
    tmp = subset(tmp,type == as.character(input$type_2D))
    tmp = arrange(tmp,desc(value))[1:50,]
    min = min(tmp$value, na.rm = TRUE)
    tmp = tmp[1:input$num_countries,]
    rank = 1:nrow(tmp)
    max = max(tmp$value, na.rm = TRUE)
    
    Log = paste("level",floor(log((tmp$value) - min + 1, base =1.0001)/log(max - min + 1, base =1.0001) * 7 + 1),sep = "")
    tmp$rank = paste(tmp$Country,"<br/>",
                     "ranks No.",rank,"<br/>",
                     "Annual Trade Value: $",tmp$value,"<br/>",sep = "",
                     "<a href='https://en.wikipedia.org/wiki/",tmp$Country,"'>Wikipedia Page</a>","<br/>",
                     "<a href='https://www.wsj.com/search/term.html?KEYWORDS=",tmp$Country,"'>Wall Street Journal Page</a>"
    )
    index = match(input$commodity_2D,c('Annual Aggregate','Chocolate', 'Coffee','Cocoa','Spices','Tea'))
    Colors = c("#231d65","#276d98","#2586a4","#3c6049","#216957","#4abf8c","#9eae1e","#eff09e")
    Labels = paste("Level:",1:8)
    ##### end subset      
    leaflet(tmp)%>%addProviderTiles("Esri.WorldStreetMap")%>%
      addMarkers(popup=~rank,icon = ~levelIcon[Log])%>%
      addMarkers(data = US, 
                 popup=~Country,icon = ~Icon)%>%  
      setView(lng=-30,lat=28,zoom=2)%>%#put US in the centre
      addLegend("topright", colors = Colors, labels = Labels,
                title = "Trade Level<br/>From Small to Large",
                labFormat = labelFormat(prefix = "$"),
                opacity = 1)
  })
  
  ## end 2D global page
  ## ===============================
  
  ## statistical summary page
  ## just copy the pattern from the statistics website: https://wits.worldbank.org/CountryProfile/en/Country/CHN/Year/LTST/TradeFlow/Export/Partner/by-country/Product/Total, including the share view, the top chart and so on, can be more.
  ## 
  output$treemap<-renderPlot({
    #selcet a year and a one of the five categories
    sub_country<-country[country$Year==input$year_tree,]
    sub_country[sub_country$Country == "WorldTotal", ][, 3:length(sub_country)] <- 0
    sub_country[nrow(sub_country)+1,3:length(sub_country)] <- colSums(sub_country[,3:length(sub_country)])
    for(i in 3:length(sub_country)){
      sub_country[,i]<-sub_country[,i]/sub_country[nrow(sub_country),i]
    }
    sub_country<-sub_country[1:nrow(sub_country)-1, ]
    sub_country$label<-paste(sub_country$Country,", ",round(100*sub_country[,as.character(input$com_tree)]),"%",sep="")
    treemap(sub_country, index='label', vSize=input$com_tree, 
            vColor="Country", type="categorical", 
            palette="RdYlBu",aspRatio=30/30,
            drop.unused.levels = FALSE, 
            position.legend="none")
  })

  output$ggplot <- renderPlot({
    
    ##### subset dataframe
    temp = input_data
    temp = subset(temp,Commodity_Name == as.character(input$com_tree))
    temp[temp$Country == "WorldTotal", ][, "value"] <- 0
    temp = subset(temp,Year == as.integer(input$year_tree))
    temp = subset(temp,type == as.character(input$type))
    temp = arrange(temp,desc(value))[1:input$number_countries_tree,]
    index = match(input$commodity_3D,c('Annual Aggregate','Chocolate', 'Coffee','Cocoa','Spices','Tea'))
    maxValue = log(max(temp$value))
    map_palette = map_pal[,index]
    clrs = rep('#050505', length(wrld_simpl$NAME))
    names(clrs) = wrld_simpl$NAME
    clrs[temp$Country] = alpha(map_palette[1], log(temp$value)/maxValue*0.1)
    ##### end subset
    
    g = ggplot(data = temp, aes(x = Country, y = value))+
      geom_bar(stat='identity',position = "dodge")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black")) +
      theme(legend.position="none") + 
      theme(legend.background = element_rect(),
            panel.grid.major.y = element_blank(), 
            panel.grid.minor.y = element_blank(), 
            panel.grid.major.x = element_blank(), 
            panel.grid.minor.x = element_blank())+ 
      geom_bar(stat = "identity", aes(fill=temp$value))+
      scale_fill_gradient(low = "#c12a2a", high = "#c12a2a")+
      scale_x_discrete(limits = temp$Country) 
    g
    #print(g,vp = viewport(angle=-90))
    
  })
  ## end ggplot
 
  ## ===============================
  ## clustering part
  ## Cluster visuals
  output$cluster <- renderPlotly({
    k = input$number_clusters
    newcountry <- country[country$Year==input$year_cluster,]
    newcountry <- na.omit(newcountry)
    # choose the five columns with different commodity values
    newcountry1 <- newcountry[,3:length(newcountry)]
    #### modify for data
    #store the result of kmeans cluster in "cls_result"
    cls_result<- kmeans(newcountry1, k)
    clusters <- cls_result$cluster
    df = as.data.frame(clusters)
    df$COUNTRY = newcountry[,1]
    df = merge(x = df, y = code, all.y = TRUE)
    df[is.na(df$clusters),2] = 0
    ## end cluster visual
    
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    
    plot_geo(df) %>%
      add_trace(
        z = ~clusters, color = ~clusters, colors = brewer.pal(k, "RdYlGn"), type = "scatter", 
        text = ~COUNTRY, locations = ~CODE, marker = list(line = 'l')
      ) %>%
      colorbar(title = 'Cluster number', tickprefix = '') %>%
      layout(
        title = paste(k,"clusters for all countries concerning","Import",sep=" "),
        geo = g
      ) 
    
  })
  
  output$text_1<- renderText({
    "Click on a Country to view cluster result" 
  })
  output$text_2<- renderText({
    "Trade Magnitude and number of countries in each cluster as follows:"
  })
  
  output$mytable<-renderDataTable({
    k=input$number_clusters
    newcountry <- country[country$Year==input$year_cluster,]
    newcountry<-na.omit(newcountry)
    #choose the five columns with different commodity values
    newcountry1<-newcountry[,3:length(newcountry)]
    cls_result<-kmeans(newcountry1,k)
    
    newcountry1$cluster = cls_result$cluster
    by_clust = group_by(newcountry1,cluster)
    by_clust = as.data.frame(summarise(by_clust, 
                                       mean(Coffee),
                                       mean(Tea), 
                                       mean(Cotton), 
                                       mean(Silk), 
                                       mean(Cocoa),
                                       mean(Maize),
                                       mean(Wheat),
                                       mean(Rice),
                                       mean(Barley),
                                       mean(Other.Cereals),
                                       mean(Alcohol),
                                       mean(Milk),
                                       mean(Juices)
                                       ))
    by_clust[,2:length(by_clust)] = t(apply(by_clust[, 2:length(by_clust)],1,log))
    names(by_clust) = c("by_cluster",
                        "Magitude(Of Coffee)",
                        "Magitude(Of Tea)",
                        "Magitude(Of Cotton)",
                        "Magitude(Of Silk)",
                        "Magitude(Of Cocoa)",
                        "Magitude(Of Maize)",
                        "Magitude(Of Wheat)",
                        "Magitude(Of Rice)",
                        "Magitude(Of Barley)",
                        "Magitude(Of Other.Cereals)",
                        "Magitude(Of Alcohol)",
                        "Magitude(Of Milk)",
                        "Magitude(Of Juices)"
                        ) 
    table2<-cbind(data.frame(Cluster = unique(cls_result$cluster)),data.frame(Size = cls_result$size),by_clust[,2:length(by_clust)])
    table2<-round(table2,1)
    #create row names for "table2"
    name_table2<-c()
    for(i in 1:nrow(table2)){
      name_table2<-c(name_table2,paste("cluster means",i,sep = " "))
    }
    rownames(table2)=name_table2
    table2<-table2[order(table2[,1]),]
    table2
  })
  ## end clustering Panel
  ## ==============================
  ## work, 20171201
  
  ## data presentation panel
  output$data.origin <- renderDataTable({
    selected.data <- sample_data
    datatable(selected.data)
  })

  output$selection <- renderDataTable({
    if(input$data.selection == "data for clustering"){
      selected <- cluster_data_import
    }
    if(input$data.selection == "data for 3D globe"){
      selected <- country
    }
    if(input$data.selection == "data for 2D globe"){
      selected <- input_data
    }
    datatable(selected)                                          
  })

#   output$imp_pdf <- renderImage({
#     "1.pdf"
#   }) 

#     output$pdfviewer <- renderText({
#       return(paste('<iframe style="height:600px; width:100%" src="', input$pdfurl, '"></iframe>', sep = ""))
#     })

  output$wordcloud <- renderPlot({
    word.tmp <- subset(word, year == input$year)
    wordcloud(words = word.tmp$word, freq = word.tmp$count, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
  })


}

