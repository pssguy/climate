library(shinyURL)
dashboardPage(
  skin = "red",
  # dashboardHeader(title = img(src="logo.jpg", height = 50, align = "left")),
  dashboardHeader(title = "Climate"),
  
  dashboardSidebar(
    includeCSS("custom.css"),
    uiOutput("sb"),
    
    sidebarMenu(
      id = "sbMenu",
      
      menuItem(
        "Earthquakes",#badgeLabel="new",badgeColor="green", cannot have with subitems
        menuSubItem("Map",tabName = "earthquakes",  icon = icon("map-marker")),
        menuSubItem("Info",tabName = "earthquakes_info",icon = icon("info"))
      ),
      menuItem(
        "US State Capital Temperatures",
        menuSubItem("Map",tabName = "statetemps", icon = icon("map-marker")),
        menuSubItem("Info",tabName = "statetemps_info",icon = icon("info")),
        menuSubItem("Data",tabName = "statetemps_data",icon = icon("database"))
        #menuSubItem("Code",icon = icon("code-fork"),
                   # href = "https://github.com/pssguy/fortune500")
      ),
      menuItem(
        "Weather Stations",
        menuSubItem("Maps",tabName = "stations", selected = TRUE),
        menuSubItem("Data",tabName = "stations_data",icon = icon("database")),
        menuSubItem("Info",tabName = "stations_info",icon = icon("info"))
      ),
      tags$hr(),
      shinyURL.ui(label="Copy URL for Sharing",copyURL=FALSE), # cannot set id for CSS purposes
    
    
    
      tags$hr(),
      menuItem(text="",href="https://mytinyshinys.shinyapps.io/dashboard",badgeLabel = "All Dashboards and Trelliscopes (14)"),
      tags$hr(),
      
      tags$body(
        a(class="addpad",href="https://twitter.com/pssGuy", target="_blank",img(src="images/twitterImage25pc.jpg")),
        a(class="addpad2",href="mailto:agcur@rogers.com", img(src="images/email25pc.jpg")),
        a(class="addpad2",href="https://github.com/pssguy",target="_blank",img(src="images/GitHub-Mark30px.png")),
        a(href="https://rpubs.com/pssguy",target="_blank",img(src="images/RPubs25px.png"))
      )

  
)#sideBoardmenu
), #dashboardSidebar

dashboardBody(
  tabItems(
    tabItem(
      "earthquakes",
      # fluidRow(
      #  column(width=12,
      box(
        status = "success", solidHeader = TRUE,
        title = "Recent Earthquakes - Click on Circle for more data",
        leafletOutput("quakeMap")
        
      ),
      box(
        height = 460,
        status = "success", solidHeader = TRUE,
        title = "Tectonic Plates",
        h4(
          "As can be seen from the image below, the pattern of earthquakes closely
          matches tectonic plate boundaries"
        ),
        br(),
        
        hr(),
        imageOutput("myImage")
        
        )
      #    )
      #   )
    ),
    
    tabItem("stations",
            fluidRow(
              column(
                width = 6,
                box(
                  width = 12,
                  status = "success", solidHeader = TRUE,
                  collapsible = TRUE, collapsed = FALSE,
                  title = "Click on circle for Details and select data to download for Monthly Temperature Chart",
                  leafletOutput("locations")
                  
                ),
                box(
                  width = 12,
                  status = "success", solidHeader = TRUE,
                  collapsible = TRUE, collapsed = TRUE,
                  title = "Annual Averages, Highs and Lows",
                  textOutput("monthTitleA"),
                  DT::dataTableOutput("hotColdTable")
                  
                ),
                box(
                  width = 12,
                  status = "success", solidHeader = TRUE,
                  collapsible = TRUE, collapsed = FALSE,
                  title = "Days below OC or above 20C - Year to Date" ,
                  plotOutput("hotColdChart")
                  
                  
                )
              ),
              column(
                width = 6,
                box(
                  width = 12,
                  
                  status = "warning", solidHeader = TRUE,
                  title = "Select Years Required. There may be missing data",
                  collapsible = TRUE, collapsed = FALSE,
                  uiOutput("a")
                  #sliderInput("years","Select Years",min=2000,max=2015,value=c(2012,2015),sep="",ticks=FALSE)
                ),
                
                
                box(
                  width = 12,
                  
                  status = "success", solidHeader = TRUE,
                  title = "Monthly Means. Hover for Details, Click for Daily data",
                  collapsible = TRUE, collapsed = FALSE,
                  textOutput("monthTitle"),
                  ggvisOutput("monthly")
                  
                ),
                box(
                  width = 12,
                  status = "success", solidHeader = TRUE,
                  title = "Daily Temperatures - Click on Point  for Hourly Data",
                  collapsible = TRUE, collapsed = FALSE,
                  textOutput("dayTitle"),
                  ggvisOutput("daily")
                  
                ),
                box(
                  width = 12,
                  status = "success", solidHeader = TRUE,
                  title = "Hourly Data",
                  collapsible = TRUE, collapsed = FALSE,
                  textOutput("hourTitle"),
                  ggvisOutput("hourly")
                  
                )
              )
            )),
    
    
    tabItem(
      "statetemps",
      fluidRow(
        column(
          width = 3,
          sliderInput(
            "minMax","Select Pleasant Range",min = 0,max = 110,value = c(60,90),step =
              10
          )
        ),
        column(width = 3,
               infoBoxOutput("hotBox", width = 12)),
        column(width = 3,
               infoBoxOutput("mildBox", width = 12)),
        column(width = 3,
               infoBoxOutput("coldBox", width = 12))
      ),
      
      fluidRow(column(
        width = 6,
        
        box(
          width = 12,
          status = "success", solidHeader = TRUE,
          title = "Latest Temps (takes few seconds) - Click Circle for value",
          collapsible = FALSE, collapsed = FALSE,
          textOutput("temperatureCheck"),
          
          leafletOutput("stateTempsMap")
          
        )
      ),
      column(
        width = 6,
        box(
          width = 12,
          status = "success", solidHeader = TRUE,
          title = "Refreshes automatically every 10 minutes",
          collapsible = FALSE, collapsed = FALSE,
          
          DT::dataTableOutput("stateTempsTable")
          
        )
      ))
      
    ),
    ## data pages
    tabItem("statetemps_data",
            fluidRow(column(
              width = 8,offset = 2,
              
              box(
                width = 12,
                
                status = "success", solidHeader = TRUE,
                title = "State Capitals Dataset",
                collapsible = FALSE, collapsed = FALSE,
                DT::dataTableOutput("stateCapitals")
                
              )
            ))),
    
    tabItem("stations_data",
            fluidRow(column(
              width = 12,
              
              box(
                width = 12,
                
                status = "success", solidHeader = TRUE,
                title = "Weather Stations Raw Dataset",
                collapsible = FALSE, collapsed = FALSE,
                DT::dataTableOutput("weatherStations")
                
              )
            ))),
    
    ### info pages
    tabItem("earthquakes_info",includeMarkdown("earthquakes_info.md")),
    tabItem("statetemps_info",includeMarkdown("statetemps_info.md")),
    tabItem("stations_info",includeMarkdown("stations_info.md"))
    
  ) #tabItems
) #dashboardbody
) # dashboard page
