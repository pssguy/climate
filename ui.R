
dashboardPage(skin="red",
             # dashboardHeader(title = img(src="logo.jpg", height = 50, align = "left")),
              dashboardHeader(title = "Climate"),
  
    dashboardSidebar(
      includeCSS("custom.css"),
       uiOutput("sb"),
 
  sidebarMenu(id="sbMenu",
             
              menuItem("Earthquakes",tabName= "earthquakes", selected=TRUE),
              menuItem("US State Capital Temperatures",tabName= "statetemps"),
             menuItem("Weather Stations", tabName = "stations",icon = icon("map-marker")),
   
 
#   menuItem("Data", tabName = "data",icon = icon("database")),
  menuItem("Info", tabName = "info",icon = icon("info")),
#   
#   menuItem("Code",icon = icon("code-fork"),
#            href = "https://github.com/pssguy/fortune500"),
#   
  menuItem("Other Dashboards",
           menuSubItem("MLB",href = "https://mytinyshinys.shinyapps.io/mlbCharts"),
           menuSubItem("Fortune 500",href = "https://mytinyshinys.shinyapps.io/fortune500"),
           menuSubItem("WikiGuardian",href = "https://mytinyshinys.shinyapps.io/wikiGuardian"),
           menuSubItem("World Soccer",href = "https://mytinyshinys.shinyapps.io/worldSoccer")
           
  ),
  
  menuItem("", icon = icon("twitter-square"),
           href = "https://twitter.com/pssGuy"),
  
  menuItem("", icon = icon("envelope"),
           href = "mailto:agcur@rogers.com")
    )
    ),
    
  dashboardBody( 
    tabItems(
      tabItem("earthquakes",
                     box(
                         status = "success", solidHeader = TRUE,
                         title = "Recent Earthquakes - Click on Circle for more data",
                         leafletOutput("quakeMap")
                         
                     )
      ),
                    
      tabItem("stations",
  fluidRow(
    
    column(width=6,
           box(width=12,
               status = "success", solidHeader = TRUE,
               title = "Click on circle for Station Name and Monthly Temperature Chart",
               leafletOutput("locations")
               
           ),
    box(width=12,
      status = "success", solidHeader = TRUE,
      title = "Annual Averages, Highs and Lows",
      textOutput("monthTitleA"),
      DT::dataTableOutput("hotColdTable")
      
    ),
    box(width=12,
        status = "success", solidHeader = TRUE,
        title = "Days below OC and above 20C - Year to Date",
        plotOutput("hotColdChart")
        
    
    )
    ),
    column(width=6,
           box(width=12,
                       
                       status = "warning", solidHeader = TRUE,
                       title = "Select Years Required. There may be missing data",
                       collapsible = TRUE, collapsed = FALSE,
                       uiOutput("a")
#sliderInput("years","Select Years",min=2000,max=2015,value=c(2012,2015),sep="",ticks=FALSE)
    ),
                       
           
    box(width=12,

      status = "success", solidHeader = TRUE,
      title = "Mean Monthly Temperatures - Click on Point  for Daily Data",
      collapsible = TRUE, collapsed = FALSE,
     textOutput("monthTitle"),
     ggvisOutput("monthly")
      
    ),
    box(width=12,
        status = "success", solidHeader = TRUE,
        title = "Daily Temperatures - Click on Point  for Hourly Data",
        collapsible = TRUE, collapsed = FALSE,
        textOutput("dayTitle"),
        ggvisOutput("daily")
        
    ),
    box(width=12,
        status = "success", solidHeader = TRUE,
        title = "Hourly Data",
        collapsible = TRUE, collapsed = FALSE,
        textOutput("hourTitle"),
        ggvisOutput("hourly")
        
    )
    )
  )
      ),

# tabItem("data",
#           fluidRow(
#             column(width=8,offset=2,
#           
#           box(width=12,
#             status = "info", solidHeader = FALSE,
#             includeMarkdown("data.md")
#           ),
#           box(width=12,
#             DT::dataTableOutput("data")
#           )
#             ))
#         ),
# 
tabItem("statetemps",
        box(width=6,
            status = "success", solidHeader = TRUE,
            title = "Latest Temps (takes few seconds) - Click Circle for value",
            collapsible = FALSE, collapsed = FALSE,
            textOutput("temperatureCheck"),
            leafletOutput("stateTempsMap")
            
        ),
        box(width=6,
          status = "success", solidHeader = TRUE,
          title = "Refreshes automatically every 10 minutes",
          collapsible = FALSE, collapsed = FALSE,
         
          DT::dataTableOutput("stateTempsTable")
          
        )
        
        ),
 tabItem("info",includeMarkdown("info.md"))

) 
       
        
)
)




