# UI file for Final Project
# 
# created 11/06/2017
# 
# used for the UI design.
# 
## UI Function


ui<- navbarPage(
  
  ##link to css.file
  theme = "2.css", 
  
  # theme = shinytheme("simplex"),
  
  ##Project Title
  "World Trade with USA",
  
  tabPanel("Home",
           htmlOutput("blankspace"),
           titlePanel("World Trade with USA"),
           h4(htmlOutput("Introduction")),
           h5(htmlOutput("teammates")),
           h5(htmlOutput("thanks"))
           ),
  
  tabPanel("Data",
           tabsetPanel(
               tabPanel("Origin Data",
                        dataTableOutput("data.origin")
               ),
               
               tabPanel("Processed Data", 
                        selectInput(inputId = "data.selection",
                                    label  = "Select the data.frame",
                                    choices = c("data for clustering",
                                                "data for 3D globe",
                                                "data for 2D globe"),
                                    selected ="data for 2D globe"),
                        dataTableOutput("selection")
               ),
                 
               
                tabPanel("Text Data",
                         tags$iframe(style="height:800px; width:100%; 
                                     scrolling=yes",
                                     src=link
                                     )
                )
               
#                tabPanel("Text Data",
#                         imageOutput("imp_pdf",width="500px",height="500px")
#                         ),

# 
#               tabPanel(
#                 
#                 tags$div(
#                   class = "container",
#                   
#                   row(
#                     col(3, textInput("pdfurl", "PDF URL"))
#                   ),
#                   row(
#                     col(6, htmlOutput('pdfviewer')),
#                     col(6, tags$iframe(style="height:600px; width:100%", src="http://localhost/ressources/pdf/R-Intro.pdf"))
#                   )
#                 )
#               )
               
               
           )
  ),
  
  
#   tabsetPanel(
#     # using iframe along with tags() within tab to display pdf with scroll, height and width could be adjusted
#     tabPanel("Reference", 
#              tags$iframe(style="height:400px; width:100%; scrolling=yes", 
#                          src="https://cran.r-project.org/doc/manuals/r-release/R-intro.pdf"))
#     #         tabPanel("Summary"),
#     #         tabPanel("Plot")
#   ),
  
  
  tabPanel("Why these goods",
           titlePanel("Some text Mining result"),
           selectInput(inputId = "year",
                       label  = "Select the year for text mining",
                       choices = c(2003:2017),
                       selected =2017),
           plotOutput("wordcloud")
           
           ),
  
  
  ## 3D Globe tab
  tabPanel("3D Globe",
           titlePanel("Products traded between USA and the world"),
           absolutePanel(id = "controls", class = "panel panel-default",
                         draggable = TRUE, 
                         top = 180, left = 60, right = "auto", bottom = "auto",
                         width = 350, height = "auto",
                         
                         h2("3D Explorer"),
                         
                         radioButtons(inputId = "type",
                                      label  = "Choose import/export",
                                      choices = c('Export','Import'),
                                      selected ='Import'),
                         sliderInput(inputId = "year_3D",
                                     label = "Select a year",
                                     value = 1996, min =1996, max =2016),
                         sliderInput(inputId = "number_countries",
                                     label = "Top Countries in Trade",
                                     value = 10,min = 1,max = 50),
                         selectInput(inputId = "commodity_3D",
                                     label  = "Select the commodity",
                                     choices = c('Annual Aggregate', 'Silk', 'Cotton',
                                                 'Wheat', 'Rice', 'Barley', 'Maize', 
                                                 'Other.Cereals',
                                                 'Coffee','Cocoa',
                                                 'Tea', 'Milk', 'Juice'),
                                     selected ='Coffee')
                         ),
           
           
           
           globeOutput("Globe",width="100%",height="650px")
           ## reference: https://rpubs.com/aagarwal29/r3dglobe
           ),
  ## end 3D Globe tab
  ## fitted new data, this panel works, 20171201
  
  ## 2D Map tab
  tabPanel("2D Map",
           titlePanel("Products imported to USA from world"),
           
           leafletOutput("mymap",width = "100%", height = 600),
           
           absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                         draggable = TRUE, 
                         top = 180, left = 60, right = "auto", bottom = "auto",
                         width = 350, height = "auto",
                         
                         h2("2D Explorer"),
                         
                         radioButtons(inputId = "type_2D",
                                      label  = "Choose import/export",
                                      choices = c('Export','Import'),
                                      selected ='Import'),
                         sliderInput(inputId = "year_2D",
                                     label = "Select a year",
                                     value = 2017, min =1996, max =2017),
                         sliderInput(inputId = "num_countries",
                                     label = "Top Countries in Trade",
                                     value = 20,min = 1,max = 50),
                         selectInput(inputId = "commodity_2D",
                                     label  = "Select the commodity",
                                     choices = c('Annual Aggregate', 'Silk', 'Cotton',
                                                 'Wheat', 'Rice', 'Barley', 'Maize', 
                                                 'Other.Cereals',
                                                 'Coffee','Cocoa',
                                                'Tea', 'Milk', 'Juice'),
                                     selected ='Cocoa')
                         
           )
  ),
  
  ## end 2D Map tab
  ## fitted new data, this panel works, 20171201
  
  ## Summary Statistics tab
  ##Regional Findings tabset
  #tabPanel("Regional Findings",
           
             
#              ##Continent & Region
#              tabPanel("Regional statistics",
#                       titlePanel("Continent & Region"),
#                       sidebarLayout(
#                         sidebarPanel(
#                           selectInput(inputId = "commodity",
#                                       label  = "choose the commodity",
#                                       choices = c("Coffee,Tea"),
#                                       selected ='Coffee'),
#                           sliderInput(inputId = "year",
#                                       label = "Select a year",
#                                       value = 2017, min =1996, max =2017),
#                           width = 3
#                           
#                         ),
#                         
#                         mainPanel(
#                           
#                           ## todo to be completed with some charts and some graphs
#                         )
#                         
#                       )
#              ),
             
             ### Tree Map
             tabPanel("Market Share",
                      titlePanel("Market Share of Countries"),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "com_tree",
                                      label  = "Select the commodity",
                                      choices = c('Silk', 'Cotton',
                                                  'Wheat', 'Rice', 'Barley', 'Maize', 
                                                  'Other.Cereals',
                                                  'Coffee','Cocoa',
                                                  'Tea', 'Milk', 'Juice'),
                                      # here should delete the "Annual Aggregate" option
                                      selected ='Coffee'),
                          sliderInput(
                            inputId = "year_tree",
                            label = "Select a year",
                            value = 2017, min =1996, max =2017),
                          sliderInput(inputId = "number_countries_tree",
                                      label = "Top Countries in Trade",
                                      value = 10,min = 1,max = 20),
                          
                          width = 3
                        ),
                        
                        mainPanel(
                          plotOutput("treemap",width = "100%", height = 600),
                          absolutePanel(id = "controls", 
                                        class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, 
                                        top = 600, left = 20, 
                                        right = "auto", bottom = "auto",
                                        width = 350, height = "auto",
                                        plotOutput("ggplot",width="100%",height="250px"))
                        )
                      )
             ),
           
           ### end Tree Map
           
  #),

  
  ## begin the clustering Panel, based on the kmeans() function
  tabPanel("Clustering Analysis",
           titlePanel("Clustering Analysis"),
           sidebarLayout(
             sidebarPanel(
               
               sliderInput(inputId = "number_clusters",
                           label = "Number of Clusters",
                           value = 5,min = 2,max = 10),
               sliderInput(
                 inputId = "year_cluster",
                 label = "Select a year",
                 value = 2017, min =1996, max =2017),
               width = 3
             ),
             
             mainPanel(
               plotlyOutput("cluster", width = "100%", height = "400px"),
               textOutput("text_1"),
               textOutput("text_2"),
               dataTableOutput("mytable")
             )
           )
           
  )
  ## end Clustering tab
             
  )
