# Set up reactive values initially as null
values <- reactiveValues()

# function to get Season data from graph and apply to table

getHour = function(data,location,session) {
  if (is.null(data))
    return(NULL)
  
  values$theDay <- data$day
  
  
  
  observe({
    if (input$tempScale == "Celsius") {
      theLocData()$met_data %>%
        filter(year == values$theYear &
                 month == values$theMonth &
                 day == values$theDay) %>%
        ggvis(~ hour, ~ temp) %>%
        layer_lines() %>%
        layer_points(size = 3) %>%
        
        add_axis("y", title = "temp C") %>%
        add_axis("x", title = "Hour of Day") %>%
        
        set_options(width = 480) %>%
        bind_shiny("hourly")
    } else {
      theLocData()$met_data %>%
        filter(year == values$theYear &
                 month == values$theMonth &
                 day == values$theDay) %>%
        mutate(temp = temp * 9 / 5 - 32) %>%
        ggvis(~ hour, ~ temp) %>%
        layer_lines() %>%
        layer_points(size = 3) %>%
        
        add_axis("y", title = "temp F") %>%
        add_axis("x", title = "Hour of Day") %>%
        
        set_options(width = 480) %>%
        bind_shiny("hourly")
    }
  })
}


getDay = function(data,location,session) {
  if (is.null(data))
    return(NULL)
  
  values$theMonth <- data$month
  values$theYear <- data$year
  
  
  
  observe({
    if (input$tempScale == "Celsius") {
      theLocData()$met_data %>%
        filter(year == values$theYear &
                 month == values$theMonth) %>%
        group_by(day) %>%
        summarize(
          min = min(temp, na.rm = T),max = max(temp, na.rm = T), mean = round(mean(temp, na.rm =
                                                                                     T),1)
        ) -> dailyAv
      theTitle <- "temp C"
    } else {
      theLocData()$met_data %>%
        filter(year == values$theYear &
                 month == values$theMonth) %>%
        mutate(temp = temp * 9 / 5 + 32) %>%
        group_by(day) %>%
        summarize(
          min = min(temp, na.rm = T),max = max(temp, na.rm = T), mean = round(mean(temp, na.rm =
                                                                                     T),1)
        ) -> dailyAv
      theTitle <- "temp F"
    }
    
    
    dailyAv.gather <- dailyAv %>%
      gather(cats,temp,-day)
    
    
    
    # this shows
    # glimpse(dailyAv.gather)
    
    dailyAv.gather %>%
      group_by(cats) %>%
      ggvis(~ day, ~ temp) %>%
      layer_lines(stroke =  ~ cats) %>%
      layer_points() %>%
      add_legend(scales = "stroke",title = "") %>%
      add_axis("y", title = theTitle) %>%
      add_axis("x", title = "Day of Month", format = 'd') %>%
      handle_click(getHour) %>%
      set_options(width = 480) %>%
      bind_shiny("daily")
    
    
  })
  
}


# this should be start but is not even being entered
output$locations <- renderLeaflet({
  print("enter locations")
  print(input$country)
  if (is.null(input$country))
    return()
  
  df <-  allStations %>%
    
    filter(country_name == input$country &
             begin <= 2013 & end == 2015)
  
  df <- data.frame(df)
  df %>%    leaflet() %>%
    addTiles() %>%
    addCircles(
      radius = 5,popup =  ~ popup,layerId =  ~ stationId
    )
  
})

## need to split this to get years requesteds

output$a <- renderUI({
  print("enter ui")
  print(input$locations_shape_click$id)
  if (is.null(input$locations_shape_click$id))
    return()
  
  # use the clicked state as filter for data
  
  #stateID <-input$choropleth_shape_click$id
  
  station <- input$locations_shape_click$id
  print(station)
  yr1 <-
    allStations[allStations$stationId == input$locations_shape_click$id,]$begin
  yr2 <-
    allStations[allStations$stationId == input$locations_shape_click$id,]$end
  print(yr1)
  print(yr2)
  print("years printed")
  #yr1 <- 2000
  # yr2 <- 2015
  inputPanel(id="downloads",
    sliderInput(
      "years","Select Years",min = yr1,max = yr2,value = c(yr2 - 2,yr2),sep =
        "",ticks = FALSE
    ),
    
    actionButton("getYears","Download Data")
  )
  
})

theLocData <- eventReactive(input$getYears,{
  print("enter reactive")
  if (is.null(input$locations_shape_click$id))
    return()
  print("station clicked")
  input$getYears
  print("inputgetYears")
  print(input$getYears)
  if (is.null(input$years))
    return()
  print("inputyears1")
  print(input$years[1])
  print(input$years[2])
  
  year1 <- input$years[1]
  year2 <- input$years[2]
  
  station <- input$locations_shape_click$id
  print(station)
  met_data <- get_isd_station_data(
    station_id = station,
    startyear = input$years[1],
    endyear = input$years[2]  # tried isolate here did not seem to  work
  )
  
  # print("data received")
  print(glimpse(met_data))
  info = list(met_data = met_data,year1 = year1,year2 = year2)
  #   print("met_data")
  return(info)
  
})

output$monthTitle <- renderText({
  if (is.null(theLocData()$met_data))
    return()
  st <- theLocData()$met_data[1]$usaf
  name <- allStations[allStations$usaf == st,]$name
  paste0(name,", ",input$country)
})

output$monthTitleA <- renderText({
  if (is.null(theLocData()$met_data))
    return()
  st <- theLocData()$met_data[1]$usaf
  name <- allStations[allStations$usaf == st,]$name
  paste0(name,", ",input$country)
})

output$dayTitle <- renderText({
  if (is.null(theLocData()$met_data))
    return()
  st <- theLocData()$met_data[1]$usaf
  name <- allStations[allStations$usaf == st,]$name
  paste0(values$theMonth," ",values$theYear," ",name,", ",input$country)
})


output$hourTitle <- renderText({
  if (is.null(theLocData()$met_data))
    return()
  st <- theLocData()$met_data[1]$usaf
  name <- allStations[allStations$usaf == st,]$name
  paste0(values$theDay," ",values$theMonth," ",values$theYear," ",name,", ",input$country)
})


observe({
  print("enter monthly")
  if (is.null(theLocData()$met_data))
    return()
  print(input$tempScale)
  print(nrow(theLocData()$met_data))
  
  if (input$tempScale == "Celsius") {
    theLocData()$met_data %>%
      group_by(year) %>%
      mutate(dayOfYear = row_number(day)) %>%
      group_by(year,month) %>%
      summarize(
        min = min(temp, na.rm = T),max = max(temp, na.rm = T), mean = round(mean(temp, na.rm =
                                                                                   T),1)
      ) -> monthlyAv#,readings=n())
    
    theTitle <- "temp C"
  } else {
    theLocData()$met_data %>%
      mutate(temp = (temp * 9 / 5 + 32)) %>%
      group_by(year) %>%
      mutate(dayOfYear = row_number(day)) %>%
      group_by(year,month) %>%
      summarize(
        min = min(temp, na.rm = T),max = max(temp, na.rm = T), mean = round(mean(temp, na.rm =
                                                                                   T),1)
      ) -> monthlyAv#,readings=n()) -> monthlyAv
    
    theTitle <- "temp F"
  }
  print(nrow(monthlyAv))
  
  monthlyAv <- cbind(monthlyAv, id = seq_len(nrow(monthlyAv)))
  
  all_values_1 <- function(x) {
    if (is.null(x))
      return(NULL)
    row <-
      monthlyAv[monthlyAv$id == x$id, c("year","mean","min","max")]
    paste0(names(row),": ",format(row), collapse = "<br />")
  }
  
  monthlyAv %>%
    group_by(year) %>%
    ggvis(~ month, ~ mean, key := ~ id) %>%
    layer_points() %>%
    layer_lines(stroke =  ~ as.character(year)) %>%
    add_tooltip(all_values_1, "hover") %>%
    add_axis("y", title = theTitle) %>%
    add_axis("x", title = "Month") %>%
    add_legend(scales = "stroke",title = "") %>%
    handle_click(getDay) %>%
    set_options(width = 480) %>%
    bind_shiny("monthly")
  
  
})


output$hotColdTable <- DT::renderDataTable({
  if (is.null(theLocData()$met_data))
    return()
  
  ## table of min/max
  if (input$tempScale == "Celsius") {
    hottest <- theLocData()$met_data %>%
      group_by(year) %>%
      mutate(maxTemp = max(temp,na.rm = T)) %>%
      filter(temp == maxTemp) %>%
      select(
        year,monthH = month,dayH = day,hourH = hour,tempH = temp
      ) %>%
      slice(1)
  } else {
    hottest <- theLocData()$met_data %>%
      group_by(year) %>%
      
      mutate(temp = (temp * 9 / 5 + 32),maxTemp = max(temp,na.rm = T)) %>%
      filter(temp == maxTemp) %>%
      select(
        year,monthH = month,dayH = day,hourH = hour,tempH = temp
      ) %>%
      slice(1)
  }
  
  if (input$tempScale == "Celsius") {
    coldest <- theLocData()$met_data %>%
      group_by(year) %>%
      mutate(minTemp = min(temp,na.rm = T)) %>%
      filter(temp == minTemp) %>%
      select(
        year,monthC = month,dayC = day,hourC = hour,tempC = temp
      ) %>%
      slice(1)
  } else {
    coldest <- theLocData()$met_data %>%
      group_by(year) %>%
      mutate(temp = (temp * 9 / 5 + 32),minTemp = min(temp,na.rm = T)) %>%
      filter(temp == minTemp) %>%
      select(
        year,monthC = month,dayC = day,hourC = hour,tempC = temp
      ) %>%
      slice(1)
    
    
    
    
  }
  ## may not be same for every hour
  mean <- theLocData()$met_data %>%
    group_by(year) %>%
    summarize(avTemp = round(mean(temp,na.rm = T),1))
  
  meanToDate <- theLocData()$met_data %>%
    filter(month < 7) %>%
    group_by(year) %>%
    summarize(avTempYTD = round(mean(temp,na.rm = T),1))
  
  
  hottest %>%
    inner_join(coldest) %>%
    inner_join(mean) %>%
    inner_join(meanToDate) %>%
    mutate(dateH = paste0(dayH,"-",monthH),dateC = paste0(dayC,"-",monthC)) %>%
    arrange(desc(year)) %>%
    select(year,dateH,tempH,dateC,tempC,av = avTemp,ytd = avTempYTD) %>%
    DT::datatable(
      container = temp_format,rownames = FALSE,options = list(
        paging = FALSE, searching = FALSE,info = FALSE,
        columnDefs = list(list(
          className = 'dt-center', targets = c(1,3)
        ))
      )
    )
})


output$hotColdChart <- renderPlot({
  if (is.null(theLocData()$met_data))
    return()
  print("begin calcs")
  
  belowZero <- theLocData()$met_data %>%
    filter(month < 7) %>%
    group_by(year,month,day) %>%
    summarize(min = min(temp, na.rm = T)) %>%
    filter(min < 0) %>%
    group_by(year) %>%
    tally() %>%
    rename(cold = n)
  
  
  
  print(nrow(belowZero))
  print(glimpse(belowZero))
  
  aboveTwenty <- theLocData()$met_data %>%
    filter(month < 7) %>%
    group_by(year,month,day) %>%
    summarize(max = max(temp, na.rm = T)) %>%
    filter(max > 19.9) %>%
    group_by(year) %>%
    tally() %>%
    rename(hot = n)
  
  print("below now above")
  print(nrow(aboveTwenty))
  print(glimpse(aboveTwenty))
  
  print(theLocData()$year1)
  print(theLocData()$year2)
  reps  <- theLocData()$year2 - theLocData()$year1 + 1
  
  
  if (nrow(aboveTwenty) == 0) {
    aboveTwenty <-
      data.frame(
        year = c(theLocData()$year1:theLocData()$year2),hot = rep(0,reps)
      )
  }
  
  print(glimpse(aboveTwenty))
  combo <- belowZero %>%
    inner_join(aboveTwenty) %>%
    gather(cat,count,-year)
  
  #print(glimpse(combo))
  combo$year <- as.integer(combo$year)
  
  write_csv(theLocData()$met_data, "test.csv")
 ## need to revist this 
 theLocData()$met_data %>% 
   filter(year==max(year)) %>% 
   nrow() -> days
  
  #yTitle <- paste0(days," days of Year")
  
  p <- ggplot(combo,aes(x = year,y = count,fill = factor(cat))) +
    geom_bar(
      stat = "identity",position = "dodge",width = .75,alpha = 0.8
    ) +
    scale_fill_manual(
      name = "",
      values = c("blue","red"),
      labels = c("Cold Days", "Hot Days")
    ) +
    scale_y_continuous(breaks = pretty_breaks()) +
    #scale_x_discrete() + issues
    scale_x_continuous(breaks = c(2013:2015)) +
    xlab("January - June") +
    ylab(yTitle) +
    theme_bw()
  p
})