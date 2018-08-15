library(leaflet)
library(geosphere)
library(magrittr)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)


# Leaflet bindings are a bit slow; for now we'll just sample to compensate
#set.seed(100)
#site_locations <- allzips[sample.int(nrow(allzips), 10000),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
site_locations <- site_locations[order(site_locations$country_Desc),]

function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        #urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        urlTemplate = "https://api.mapbox.com/styles/v1/cbrofft/civlehgui00242jrtvs9q4lds/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiY2Jyb2ZmdCIsImEiOiJjaXZsZGRjaGowNGQ0MnlxdGU1bWlyajI5In0.xZ3zaqkyY_ghg0hhBKRqgA",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
    #setView(lng = -93.85, lat = 37.45, zoom = 2)
    setView(lng = 0, lat = 0, zoom = 2)%>%
    setMaxBounds(lng1=-180,lat1=-90,lng2=180,lat2=90)
  
  })
  
  
  # A reactive expression that returns the set of zips that are
  # in bounds right now
 #  LocInBounds <- reactive({
 #   if (is.null(input$map_bounds))
 #     return(site_locations[FALSE,])
 #   bounds <- input$map_bounds
 #   latRng <- range(bounds$north, bounds$south)
 #   lngRng <- range(bounds$east, bounds$west)
 # 
 #   subset(site_locations,
 #     Latitude >= latRng[1] & Latitude <= latRng[2] &
 #      Longitude >= lngRng[1] & Longitude <= lngRng[2])
 # })
 # 
 #  # Precalculate the breaks we'll need for the two histograms
  # CountShipments <- hist(plot = FALSE, uniqshipments$Count.of.alarms, breaks = 20)$breaks
  # 
  # output$shipments2 <- renderPlot({
  #   # If no zipcodes are in view, don't plot
  #   if (nrow(LocInBounds()) == 0)
  #     return(NULL)
  # 
  #   hist(LocInBounds()$shipments2,
  #     breaks = centileBreaks,
  #     main = "SuperZIP score (visible zips)",
  #     xlab = "Percentile",
  #     xlim = range(allzips$centile),
  #     col = '#00DD00',
  #     border = 'white')
  # })
 # 
 #  output$scatterCollegeIncome <- renderPlot({
 #    # If no zipcodes are in view, don't plot
 #    if (nrow(zipsInBounds()) == 0)
 #      return(NULL)
 # 
 #    print(xyplot(income ~ college, data = zipsInBounds(), xlim = range(allzips$college), ylim = range(allzips$income)))
 #  })

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$color
    sizeBy <- input$size
    filter2 <- input$alarmpct

    # if (colorBy == "superzip") {
    #   # Color and palette are treated specially in the "superzip" case, because
    #   # the values are categorical instead of continuous.
    #   colorData <- ifelse(uniqueroutes$centile >= (100 - input$threshold), "yes", "no")
    #   pal <- colorFactor("Spectral", 10)
    # } else {
    #   colorData <- uniqueroutes[[colorBy]]
    #   pal <- colorBin("Spectral", gg3[[i]][1,7], 11, pretty = FALSE)
    #   pal <- colorFactor("Spectral", gg3[[i]][1,7])
    # }
    

      # ship_counts <- nrow(uniqshipments[as.character(uniqshipments$shipmentitemproductdesc) == input$prod & as.character(uniqshipments$shipmentorigindesc) == input$origin & as.character(uniqshipments$shipmentdestinationdesc) == input$destination,])
      # alarm_counts <- nrow(uniqshipments[as.character(uniqshipments$shipmentitemproductdesc) == input$prod & as.character(uniqshipments$shipmentorigindesc) == input$origin & as.character(uniqshipments$shipmentdestinationdesc) == input$destination & uniqshipments$Alarm.Triggered == TRUE,])
      # percentage_alarm <- (alarm_counts/ship_counts)*100
      # percentage_alarm <- round(percentage_alarm, digits = 2)
      
  
    
    colfunc <- colorNumeric(c("dark green", "red"), 0:100)
    colfunc2 <-colorNumeric(c("blue","grey27", "red1"), min(temp2_prod$tempmeanvalue, na.rm = TRUE):max(temp2_prod$tempmeanvalue, na.rm = TRUE))
    colfunc3 <-colorNumeric(c("blue","grey27", "red1"), min(temp2_prod$tempmaxvalue, na.rm = TRUE):max(temp2_prod$tempmaxvalue, na.rm = TRUE))
    colfunc4 <-colorNumeric(c("blue","grey27", "red1"), min(temp2_prod$tempminvalue, na.rm = TRUE):max(temp2_prod$tempminvalue, na.rm = TRUE))
    
    df_count <- reactive({
      data <- uniqshipments %>% select(shipmentid,
                                       shipmentshippeddate,
                                       shipmentcarrierdesc, 
                                       shipmentcontainerdesc,
                                       shipmentorigindesc, 
                                       shipmentdestinationdesc,
                                       shipmentitemproductdesc,
                                       route,Alarm.Triggered,
                                       Count.of.alarms)
      if (input$origin != "All Origins") {
        data <- data[data$shipmentorigindesc == input$origin,]
      }
      if (input$destination != "All Destinations") {
        data <- data[data$shipmentdestinationdesc == input$destination,]
      }
      # if (input$carrier != "All") {
      #   data <- data[data$ShipmentCarrierDesc == input$shipmentCarrierDesc,]
      # }
      # if (input$shipmentRootCauseofTempExcursionDesc != "All") {
      #   data <- data[data$ShipmentRootCauseofTempExcursionDesc == input$shipmentRootCauseofTempExcursionDesc,]
      #}
      return(data)
    })
    
    

    # if (sizeBy == "superzip") {
    #   # Radius is treated specially in the "superzip" case.
    #   radius <- ifelse(site_locations$centile >= (100 - input$threshold), 30000, 3000)
    # } else {
    #   radius <- site_locations[[sizeBy]] / max(site_locations[[sizeBy]]) * 30000
    # }
    
    # if (sizeBy == "superzip") {
    #   # Radius is treated specially in the "superzip" case.
    #   radius <- ifelse(uniqueroutes$centile >= (100 - input$threshold), 30000, 3000)
    # } else {
      radius <- 3 / 1 * 3000
    # }
    # 
    

    
    leafletProxy("map", data = cleanroutes) %>%
      clearShapes() %>%
      addCircles(~routes3.Longitude.x, ~routes3.Latitude.x, radius=radius, layerId=~routes3.shipmentorigindesc,
        stroke=FALSE, fillOpacity=0.5, fillColor="black") %>%
      addLegend("bottomleft", pal=colfunc, values=0:100, bins=10, labFormat = labelFormat(suffix="%"), title="Alarm Rates",
                layerId="colorLegend")

     leafletProxy("map", data = cleanroutes) %>%
      #clearShapes() %>%
      addCircles(~routes3.Longitude.y, ~routes3.Latitude.y, radius=radius, layerId=~routes3.shipmentdestinationdesc,
                 stroke=FALSE, fillOpacity=0.5, fillColor= "red") %>%
       addLegend("bottomleft", pal=colfunc, values=0:100, bins=10, labFormat = labelFormat(suffix="%"), title="Alarm Rates",
                 layerId="colorLegend")
if(input$exclusions == FALSE){
if(input$viewchange == "All" || (input$viewchange == "Product" & input$prod == "All Products"))  { 
  
     for(i in 1:nrow(cleanroutes4)) {
       

      
       if(input$alarmpct > as.numeric(gg3[[i]][1,7]) || input$shipments > as.numeric(gg3[[i]][1,4]) || (input$origin != "All Origins" && input$origin != as.character(gg3[[i]][1,2])) || (input$destination != "All Destinations" && input$destination != as.character(gg3[[i]][1,3])))
       {}
       else {
       if(ncol(gg3[[i]]) < 9){
        temp <- as.data.frame(gg3[[i]][,8][1])
        tmp2 <- as.matrix(temp)
        temp2 <- as.data.frame(gg3[[i]][,8][2])
        tmp3 <- as.matrix(temp2)
        tmp4 <- rbind(tmp2,tmp3)
        leafletProxy("map", data = alarm_rate_Pct1) %>%
          #   #clearShapes() %>%
          addPolylines(data = tmp2, color =colfunc(as.numeric(gg3[[i]][1,7])), weight = 1.5, layerId=gg3[[i]][1][1], popup = as.character(tagList(
            tags$strong(HTML(sprintf("%s",
                                     gg3[[i]][1,1]
            ))), tags$br(),
            sprintf("Origin: %s", gg3[[i]][1,2]), tags$br(),
            sprintf("Destination: %s", gg3[[i]][1,3]), tags$br(),
            sprintf("Shipments: %s", gg3[[i]][1,4]), tags$br(),
            sprintf("Alarms: %s", gg3[[i]][1,5]), tags$br(),
            sprintf("Alarm Rate: %.1f %%", as.numeric(gg3[[i]][1,7]))
            
          )))%>%
          addPolylines(data = tmp3, color =colfunc(as.numeric(gg3[[i]][1,7])), weight = 1.5, layerId=gg3[[i]][1][2], popup = as.character(tagList(
            tags$strong(HTML(sprintf("%s",
                                     gg3[[i]][1,1]
            ))), tags$br(),
            sprintf("Origin: %s", gg3[[i]][1,2]), tags$br(),
            sprintf("Destination: %s", gg3[[i]][1,3]), tags$br(),
            sprintf("Shipments: %s", gg3[[i]][1,4]), tags$br(),
            sprintf("Alarms: %s", gg3[[i]][1,5]), tags$br(),
            sprintf("Alarm Rate: %.1f %%", as.numeric(gg3[[i]][1,7]))
            
          )))%>%
        

          addLegend("bottomleft", pal=colfunc, values=0:100, bins=10, labFormat = labelFormat(suffix="%"), title="Alarm Rates",
                    layerId="colorLegend")
      }
       else {
       
        leafletProxy("map", data = alarm_rate_Pct1) %>%
         #   #clearShapes() %>%
         addPolylines(data = unlist(cbind(as.numeric(gg3[[i]][,8]), as.numeric(gg3[[i]][,9]))), color =colfunc(as.numeric(gg3[[i]][1,7])), weight = 1.5, layerId=gg3[[i]][1], popup = as.character(tagList(
           tags$strong(HTML(sprintf("%s",
                                    gg3[[i]][1,1]
           ))), tags$br(),
           sprintf("Route: %s", gg3[[i]][1,1]), tags$br(),
           sprintf("Origin: %s", gg3[[i]][1,2]), tags$br(),
           sprintf("Destination: %s", gg3[[i]][1,3]), tags$br(),
           sprintf("Shipments: %s", gg3[[i]][1,4]), tags$br(),
           sprintf("Alarms: %s", gg3[[i]][1,5]), tags$br(),
           sprintf("Alarm Rate: %.1f %%", as.numeric(gg3[[i]][1,7]))
           
         )))%>% 
         addLegend("bottomleft", pal=colfunc, values=0:100, bins=10, labFormat = labelFormat(suffix="%"), title="Alarm Rates",
                   layerId="colorLegend")
         }
     }} 
     
}
    

else if(input$viewchange == "Product" & input$prod != "All Products")  { 
  for(i in 1:nrow(cleanproductroutes2)) {
    if(input$alarmpct > as.numeric(prodlist[[i]][1,8]) || input$shipments > as.numeric(prodlist[[i]][1,5]) || (input$origin != "All Origins" && input$origin != as.character(prodlist[[i]][1,3])) || (input$destination != "All Destinations" && input$destination != as.character(prodlist[[i]][1,4])))
    {}
    else {
    if(input$prod != as.character(prodlist[[i]][1,2]))
    {}
    else {
      if(ncol(prodlist[[i]]) < 10){
        temp <- as.data.frame(prodlist[[i]][,9][1])
        tmp2 <- as.matrix(temp)
        temp2 <- as.data.frame(prodlist[[i]][,9][2])
        tmp3 <- as.matrix(temp2)
        tmp4 <- rbind(tmp2,tmp3)
        leafletProxy("map", data = alarm_rate_Pct1_prod) %>%
          #   #clearShapes() %>%
          addPolylines(data = tmp2, color =colfunc(as.numeric(prodlist[[i]][1,8])), weight = 1.5, layerId=c(prodlist[[i]][1,1][1],prodlist[[i]][1,2][1]), popup = as.character(tagList(
            tags$strong(HTML(sprintf("%s",
                                     prodlist[[i]][1,1]
            ))), tags$br(),
            sprintf("Route: %s", prodlist[[i]][1,1]), tags$br(),
            sprintf("Product: %s", prodlist[[i]][1,2]), tags$br(),
            sprintf("Origin: %s", prodlist[[i]][1,3]), tags$br(),
            sprintf("Destination: %s", prodlist[[i]][1,4]), tags$br(),
            sprintf("Shipments: %s", prodlist[[i]][1,5]), tags$br(),
            sprintf("Alarms: %s", prodlist[[i]][1,6]), tags$br(),
            sprintf("Alarm Rate: %.1f %%", as.numeric(prodlist[[i]][1,8]))
            
          )))%>%
          addPolylines(data = tmp3, color =colfunc(as.numeric(prodlist[[i]][1,8])), weight = 1.5, layerId=c(prodlist[[i]][1,1][2],prodlist[[i]][1,2][2]), popup = as.character(tagList(
            tags$strong(HTML(sprintf("%s",
                                     prodlist[[i]][1,1]
            ))), tags$br(),
            sprintf("Route: %s", prodlist[[i]][1,1]), tags$br(),
            sprintf("Product: %s", prodlist[[i]][1,2]), tags$br(),
            sprintf("Origin: %s", prodlist[[i]][1,3]), tags$br(),
            sprintf("Destination: %s", prodlist[[i]][1,4]), tags$br(),
            sprintf("Shipments: %s", prodlist[[i]][1,5]), tags$br(),
            sprintf("Alarms: %s", prodlist[[i]][1,6]), tags$br(),
            sprintf("Alarm Rate: %.1f %%", as.numeric(prodlist[[i]][1,8]))
            
          )))%>%
          addLegend("bottomleft", pal=colfunc, values=0:100, bins=10, labFormat = labelFormat(suffix="%"), title="Alarm Rates",
                    layerId="colorLegend")
      }
      else {
        
        leafletProxy("map", data = alarm_rate_Pct1_prod) %>%
          #   #clearShapes() %>%
          addPolylines(data = unlist(cbind(as.numeric(prodlist[[i]][,9]), as.numeric(prodlist[[i]][,10]))), color =colfunc(as.numeric(prodlist[[i]][1,8])), weight = 1.5, layerId=c(prodlist[[i]][1,1],prodlist[[i]][1,2]), popup = as.character(tagList(
            tags$strong(HTML(sprintf("%s",
                                     prodlist[[i]][1,1]
            ))), tags$br(),
            sprintf("Route: %s", prodlist[[i]][1,1]), tags$br(),
            sprintf("Product: %s", prodlist[[i]][1,2]), tags$br(),
            sprintf("Origin: %s", prodlist[[i]][1,3]), tags$br(),
            sprintf("Destination: %s", prodlist[[i]][1,4]), tags$br(),
            sprintf("Shipments: %s", prodlist[[i]][1,5]), tags$br(),
            sprintf("Alarms: %s", prodlist[[i]][1,6]), tags$br(),
            sprintf("Alarm Rate: %.1f %%", as.numeric(prodlist[[i]][1,8]))
            
          )))%>% 
          addLegend("bottomleft", pal=colfunc, values=0:100, bins=10, labFormat = labelFormat(suffix="%"), title="Alarm Rates",
                    layerId="colorLegend")}
    }} }
  
}

else if(input$viewchange == "Avg. Temperature")
{
  for(i in 1:nrow(temp2_prod)) {
    if(input$prod != as.character(prodtemplist[[i]][1,2]) || input$shipments > as.numeric(prodtemplist[[i]][1,5]) || input$alarmpct > as.numeric(prodtemplist[[i]][1,8]) || (input$origin != "All Origins" && input$origin != as.character(prodtemplist[[i]][1,3])) || (input$destination != "All Destinations" && input$destination != as.character(prodtemplist[[i]][1,4])))
    {}
    else {
      if(ncol(prodtemplist[[i]]) < 13){
        temp <- as.data.frame(prodtemplist[[i]][,12][1])
        tmp2 <- as.matrix(temp)
        temp2 <- as.data.frame(prodtemplist[[i]][,12][2])
        tmp3 <- as.matrix(temp2)
        tmp4 <- rbind(tmp2,tmp3)
        leafletProxy("map", data = temp2_prod) %>%
          #   #clearShapes() %>%
          addPolylines(data = tmp2, color =colfunc2(as.numeric(prodtemplist[[i]][1,9])), weight = 1.5, layerId=c(prodtemplist[[i]][1,1][1],prodtemplist[[i]][1,2][1]), popup = as.character(tagList(
            tags$strong(HTML(sprintf("%s",
                                     prodtemplist[[i]][1,1]
            ))), tags$br(),
            sprintf("Route: %s", prodtemplist[[i]][1,1]), tags$br(),
            sprintf("Product: %s", prodtemplist[[i]][1,2]), tags$br(),
            sprintf("Origin: %s", prodtemplist[[i]][1,3]), tags$br(),
            sprintf("Destination: %s", prodtemplist[[i]][1,4]), tags$br(),
            sprintf("Shipments: %s", prodtemplist[[i]][1,5]), tags$br(),
            sprintf("Alarms: %s", prodtemplist[[i]][1,6]), tags$br(),
            sprintf("Alarm Rate: %.1f %%", as.numeric(prodtemplist[[i]][1,8])),tags$br(),
            sprintf("Average Mean Temperature: %.1f", as.numeric(prodtemplist[[i]][1,9])),tags$br(),
            sprintf("Average Max Temperature: %.1f", as.numeric(prodtemplist[[i]][1,10])),tags$br(),
            sprintf("Average Min Temperature: %.1f", as.numeric(prodtemplist[[i]][1,11]))
          )))%>%
          addPolylines(data = tmp3, color =colfunc2(as.numeric(prodtemplist[[i]][1,9])), weight = 1.5, layerId=c(prodtemplist[[i]][1,1][2],prodtemplist[[i]][1,2][2]), popup =  as.character(tagList(
            tags$strong(HTML(sprintf("%s",
                                     prodtemplist[[i]][1,1]
            ))), tags$br(),
            sprintf("Route: %s", prodtemplist[[i]][1,1]), tags$br(),
            sprintf("Product: %s", prodtemplist[[i]][1,2]), tags$br(),
            sprintf("Origin: %s", prodtemplist[[i]][1,3]), tags$br(),
            sprintf("Destination: %s", prodtemplist[[i]][1,4]), tags$br(),
            sprintf("Shipments: %s", prodtemplist[[i]][1,5]), tags$br(),
            sprintf("Alarms: %s", prodtemplist[[i]][1,6]), tags$br(),
            sprintf("Alarm Rate: %.1f %%", as.numeric(prodtemplist[[i]][1,8])),tags$br(),
            sprintf("Average Mean Temperature: %.1f", as.numeric(prodtemplist[[i]][1,9])),tags$br(),
            sprintf("Average Max Temperature: %.1f", as.numeric(prodtemplist[[i]][1,10])),tags$br(),
            sprintf("Average Min Temperature: %.1f", as.numeric(prodtemplist[[i]][1,11]))
            
          )))%>%
          addLegend("bottomleft", pal=colfunc2, values=min(temp2_prod$tempmeanvalue):max(temp2_prod$tempmeanvalue), bins=10, labFormat = labelFormat(suffix=" °C"), title="Alarm Rates",
                    layerId="colorLegend")
      }
      else {
        
        leafletProxy("map", data = temp2_prod) %>%
          #   #clearShapes() %>%
          addPolylines(data = unlist(cbind(as.numeric(prodtemplist[[i]][,12]), as.numeric(prodtemplist[[i]][,13]))), color =colfunc2(as.numeric(prodtemplist[[i]][1,9])), weight = 1.5, layerId=c(prodtemplist[[i]][1,1],prodtemplist[[i]][1,2]), popup =  as.character(tagList(
            tags$strong(HTML(sprintf("%s",
                                     prodtemplist[[i]][1,1]
            ))), tags$br(),
            sprintf("Route: %s", prodtemplist[[i]][1,1]), tags$br(),
            sprintf("Product: %s", prodtemplist[[i]][1,2]), tags$br(),
            sprintf("Origin: %s", prodtemplist[[i]][1,3]), tags$br(),
            sprintf("Destination: %s", prodtemplist[[i]][1,4]), tags$br(),
            sprintf("Shipments: %s", prodtemplist[[i]][1,5]), tags$br(),
            sprintf("Alarms: %s", prodtemplist[[i]][1,6]), tags$br(),
            sprintf("Alarm Rate: %.1f %%", as.numeric(prodtemplist[[i]][1,8])),tags$br(),
            sprintf("Average Mean Temperature: %.1f", as.numeric(prodtemplist[[i]][1,9])),tags$br(),
            sprintf("Average Max Temperature: %.1f", as.numeric(prodtemplist[[i]][1,10])),tags$br(),
            sprintf("Average Min Temperature: %.1f", as.numeric(prodtemplist[[i]][1,11]))
          )))%>% 
          addLegend("bottomleft", pal=colfunc2, values=min(temp2_prod$tempmeanvalue):max(temp2_prod$tempmeanvalue), bins=10, labFormat = labelFormat(suffix=" °C"), title="Alarm Rates",
                    layerId="colorLegend")}
    }} 
  
  
}
     
else if(input$viewchange == "Min Temperature")
     {
  for(i in 1:nrow(temp2_prod)) {
    if(input$prod != as.character(prodtemplist[[i]][1,2]) || input$shipments > as.numeric(prodtemplist[[i]][1,5]) || input$alarmpct > as.numeric(prodtemplist[[i]][1,8]) || (input$origin != "All Origins" && input$origin != as.character(prodtemplist[[i]][1,3])) || (input$destination != "All Destinations" && input$destination != as.character(prodtemplist[[i]][1,4])) || (input$temperature > as.numeric(prodtemplist[[i]][1,11])))
    {}
    else {
      if(ncol(prodtemplist[[i]]) < 13){
        temp <- as.data.frame(prodtemplist[[i]][,12][1])
        tmp2 <- as.matrix(temp)
        temp2 <- as.data.frame(prodtemplist[[i]][,12][2])
        tmp3 <- as.matrix(temp2)
        tmp4 <- rbind(tmp2,tmp3)
        leafletProxy("map", data = temp2_prod) %>%
          #   #clearShapes() %>%
          addPolylines(data = tmp2, color =colfunc4(as.numeric(prodtemplist[[i]][1,11])), weight = 1.5, layerId=c(prodtemplist[[i]][1,1][1],prodtemplist[[i]][1,2][1]), popup = as.character(tagList(
            tags$strong(HTML(sprintf("%s",
                                     prodtemplist[[i]][1,1]
            ))), tags$br(),
            sprintf("Route: %s", prodtemplist[[i]][1,1]), tags$br(),
            sprintf("Product: %s", prodtemplist[[i]][1,2]), tags$br(),
            sprintf("Origin: %s", prodtemplist[[i]][1,3]), tags$br(),
            sprintf("Destination: %s", prodtemplist[[i]][1,4]), tags$br(),
            sprintf("Shipments: %s", prodtemplist[[i]][1,5]), tags$br(),
            sprintf("Alarms: %s", prodtemplist[[i]][1,6]), tags$br(),
            sprintf("Alarm Rate: %.1f %%", as.numeric(prodtemplist[[i]][1,8])),tags$br(),
            sprintf("Average Mean Temperature: %.1f", as.numeric(prodtemplist[[i]][1,9])),tags$br(),
            sprintf("Average Max Temperature: %.1f", as.numeric(prodtemplist[[i]][1,10])),tags$br(),
            sprintf("Average Min Temperature: %.1f", as.numeric(prodtemplist[[i]][1,11]))
          )))%>%
          addPolylines(data = tmp3, color =colfunc4(as.numeric(prodtemplist[[i]][1,11])), weight = 1.5, layerId=c(prodtemplist[[i]][1,1][2],prodtemplist[[i]][1,2][2]), popup =  as.character(tagList(
            tags$strong(HTML(sprintf("%s",
                                     prodtemplist[[i]][1,1]
            ))), tags$br(),
            sprintf("Route: %s", prodtemplist[[i]][1,1]), tags$br(),
            sprintf("Product: %s", prodtemplist[[i]][1,2]), tags$br(),
            sprintf("Origin: %s", prodtemplist[[i]][1,3]), tags$br(),
            sprintf("Destination: %s", prodtemplist[[i]][1,4]), tags$br(),
            sprintf("Shipments: %s", prodtemplist[[i]][1,5]), tags$br(),
            sprintf("Alarms: %s", prodtemplist[[i]][1,6]), tags$br(),
            sprintf("Alarm Rate: %.1f %%", as.numeric(prodtemplist[[i]][1,8])),tags$br(),
            sprintf("Average Mean Temperature: %.1f", as.numeric(prodtemplist[[i]][1,9])),tags$br(),
            sprintf("Average Max Temperature: %.1f", as.numeric(prodtemplist[[i]][1,10])),tags$br(),
            sprintf("Average Min Temperature: %.1f", as.numeric(prodtemplist[[i]][1,11]))
            
          )))%>%
          addLegend("bottomleft", pal=colfunc4, values=min(temp2_prod$tempminvalue):max(temp2_prod$tempminvalue), bins=10, labFormat = labelFormat(suffix=" °C"), title="Alarm Rates",
                    layerId="colorLegend")
      }
      else {
        
        leafletProxy("map", data = temp2_prod) %>%
          #   #clearShapes() %>%
          addPolylines(data = unlist(cbind(as.numeric(prodtemplist[[i]][,12]), as.numeric(prodtemplist[[i]][,13]))), color =colfunc4(as.numeric(prodtemplist[[i]][1,11])), weight = 1.5, layerId=c(prodtemplist[[i]][1,1],prodtemplist[[i]][1,2]), popup =  as.character(tagList(
            tags$strong(HTML(sprintf("%s",
                                     prodtemplist[[i]][1,1]
            ))), tags$br(),
            sprintf("Route: %s", prodtemplist[[i]][1,1]), tags$br(),
            sprintf("Product: %s", prodtemplist[[i]][1,2]), tags$br(),
            sprintf("Origin: %s", prodtemplist[[i]][1,3]), tags$br(),
            sprintf("Destination: %s", prodtemplist[[i]][1,4]), tags$br(),
            sprintf("Shipments: %s", prodtemplist[[i]][1,5]), tags$br(),
            sprintf("Alarms: %s", prodtemplist[[i]][1,6]), tags$br(),
            sprintf("Alarm Rate: %.1f %%", as.numeric(prodtemplist[[i]][1,8])),tags$br(),
            sprintf("Average Mean Temperature: %.1f", as.numeric(prodtemplist[[i]][1,9])),tags$br(),
            sprintf("Average Max Temperature: %.1f", as.numeric(prodtemplist[[i]][1,10])),tags$br(),
            sprintf("Average Min Temperature: %.1f", as.numeric(prodtemplist[[i]][1,11]))
          )))%>% 
          addLegend("bottomleft", pal=colfunc4, values=min(temp2_prod$tempminvalue):max(temp2_prod$tempminvalue), bins=10, labFormat = labelFormat(suffix=" °C"), title="Alarm Rates",
                    layerId="colorLegend")}
    }} 
  
  
       
     }     
     
else if(input$viewchange == "Max Temperature")
     {
  for(i in 1:nrow(temp2_prod)) {
    if(input$prod != as.character(prodtemplist[[i]][1,2]) || input$shipments > as.numeric(prodtemplist[[i]][1,5]) || input$alarmpct > as.numeric(prodtemplist[[i]][1,8]) || (input$origin != "All Origins" && input$origin != as.character(prodtemplist[[i]][1,3])) || (input$destination != "All Destinations" && input$destination != as.character(prodtemplist[[i]][1,4])))
    {}
    else {
      if(ncol(prodtemplist[[i]]) < 13){
        temp <- as.data.frame(prodtemplist[[i]][,12][1])
        tmp2 <- as.matrix(temp)
        temp2 <- as.data.frame(prodtemplist[[i]][,12][2])
        tmp3 <- as.matrix(temp2)
        tmp4 <- rbind(tmp2,tmp3)
        leafletProxy("map", data = temp2_prod) %>%
          #   #clearShapes() %>%
          addPolylines(data = tmp2, color =colfunc3(as.numeric(prodtemplist[[i]][1,10])), weight = 1.5, layerId=c(prodtemplist[[i]][1,1][1],prodtemplist[[i]][1,2][1]), popup = as.character(tagList(
            tags$strong(HTML(sprintf("%s",
                                     prodtemplist[[i]][1,1]
            ))), tags$br(),
            sprintf("Route: %s", prodtemplist[[i]][1,1]), tags$br(),
            sprintf("Product: %s", prodtemplist[[i]][1,2]), tags$br(),
            sprintf("Origin: %s", prodtemplist[[i]][1,3]), tags$br(),
            sprintf("Destination: %s", prodtemplist[[i]][1,4]), tags$br(),
            sprintf("Shipments: %s", prodtemplist[[i]][1,5]), tags$br(),
            sprintf("Alarms: %s", prodtemplist[[i]][1,6]), tags$br(),
            sprintf("Alarm Rate: %.1f %%", as.numeric(prodtemplist[[i]][1,8])),tags$br(),
            sprintf("Average Mean Temperature: %.1f", as.numeric(prodtemplist[[i]][1,9])),tags$br(),
            sprintf("Average Max Temperature: %.1f", as.numeric(prodtemplist[[i]][1,10])),tags$br(),
            sprintf("Average Min Temperature: %.1f", as.numeric(prodtemplist[[i]][1,11]))
          )))%>%
          addPolylines(data = tmp3, color =colfunc3(as.numeric(prodtemplist[[i]][1,10])), weight = 1.5, layerId=c(prodtemplist[[i]][1,1][2],prodtemplist[[i]][1,2][2]), popup =  as.character(tagList(
            tags$strong(HTML(sprintf("%s",
                                     prodtemplist[[i]][1,1]
            ))), tags$br(),
            sprintf("Route: %s", prodtemplist[[i]][1,1]), tags$br(),
            sprintf("Product: %s", prodtemplist[[i]][1,2]), tags$br(),
            sprintf("Origin: %s", prodtemplist[[i]][1,3]), tags$br(),
            sprintf("Destination: %s", prodtemplist[[i]][1,4]), tags$br(),
            sprintf("Shipments: %s", prodtemplist[[i]][1,5]), tags$br(),
            sprintf("Alarms: %s", prodtemplist[[i]][1,6]), tags$br(),
            sprintf("Alarm Rate: %.1f %%", as.numeric(prodtemplist[[i]][1,8])),tags$br(),
            sprintf("Average Mean Temperature: %.1f", as.numeric(prodtemplist[[i]][1,9])),tags$br(),
            sprintf("Average Max Temperature: %.1f", as.numeric(prodtemplist[[i]][1,10])),tags$br(),
            sprintf("Average Min Temperature: %.1f", as.numeric(prodtemplist[[i]][1,11]))
            
          )))%>%
          addLegend("bottomleft", pal=colfunc3, values=min(temp2_prod$tempmaxvalue):max(temp2_prod$tempmaxvalue), bins=10, labFormat = labelFormat(suffix=" °C"), title="Alarm Rates",
                    layerId="colorLegend")
      }
      else {
        
        leafletProxy("map", data = temp2_prod) %>%
          #   #clearShapes() %>%
          addPolylines(data = unlist(cbind(as.numeric(prodtemplist[[i]][,12]), as.numeric(prodtemplist[[i]][,13]))), color =colfunc3(as.numeric(prodtemplist[[i]][1,10])), weight = 1.5, layerId=c(prodtemplist[[i]][1,1],prodtemplist[[i]][1,2]), popup =  as.character(tagList(
            tags$strong(HTML(sprintf("%s",
                                     prodtemplist[[i]][1,1]
            ))), tags$br(),
            sprintf("Route: %s", prodtemplist[[i]][1,1]), tags$br(),
            sprintf("Product: %s", prodtemplist[[i]][1,2]), tags$br(),
            sprintf("Origin: %s", prodtemplist[[i]][1,3]), tags$br(),
            sprintf("Destination: %s", prodtemplist[[i]][1,4]), tags$br(),
            sprintf("Shipments: %s", prodtemplist[[i]][1,5]), tags$br(),
            sprintf("Alarms: %s", prodtemplist[[i]][1,6]), tags$br(),
            sprintf("Alarm Rate: %.1f %%", as.numeric(prodtemplist[[i]][1,8])),tags$br(),
            sprintf("Average Mean Temperature: %.1f", as.numeric(prodtemplist[[i]][1,9])),tags$br(),
            sprintf("Average Max Temperature: %.1f", as.numeric(prodtemplist[[i]][1,10])),tags$br(),
            sprintf("Average Min Temperature: %.1f", as.numeric(prodtemplist[[i]][1,11]))
          )))%>% 
          addLegend("bottomleft", pal=colfunc3, values=min(temp2_prod$tempmaxvalue):max(temp2_prod$tempmaxvalue), bins=10, labFormat = labelFormat(suffix=" °C"), title="Alarm Rates",
                    layerId="colorLegend")}
    }} 
  
  
       
     }     

else if(input$viewchange == "Carrier") 
  { 
       for(i in 1:nrow(cleancarrierroutes2)) {
         if(input$alarmpct > as.numeric(carlist[[i]][1,8]) || input$shipments > as.numeric(carlist[[i]][1,5]) || (input$origin != "All Origins" && input$origin != as.character(carlist[[i]][1,3])) || (input$destination != "All Destinations" && input$destination != as.character(carlist[[i]][1,4])))
         {}
         else {
           if(input$carrier != as.character(carlist[[i]][1,2]) && input$carrier != "All Carriers")
           {}
           else {
             if(ncol(carlist[[i]]) < 10){
               temp <- as.data.frame(carlist[[i]][,9][1])
               tmp2 <- as.matrix(temp)
               temp2 <- as.data.frame(carlist[[i]][,9][2])
               tmp3 <- as.matrix(temp2)
               tmp4 <- rbind(tmp2,tmp3)
               leafletProxy("map", data = alarm_rate_Pct1_car) %>%
                 #   #clearShapes() %>%
                 addPolylines(data = tmp2, color =colfunc(as.numeric(carlist[[i]][1,8])), weight = 1.5, layerId=c(carlist[[i]][1,1][1],carlist[[i]][1,2][1]), popup = as.character(tagList(
                   tags$strong(HTML(sprintf("%s",
                                            carlist[[i]][1,1]
                   ))), tags$br(),
                   sprintf("Route: %s", carlist[[i]][1,1]), tags$br(),
                   sprintf("Carrier: %s", carlist[[i]][1,2]), tags$br(),
                   sprintf("Origin: %s", carlist[[i]][1,3]), tags$br(),
                   sprintf("Destination: %s", carlist[[i]][1,4]), tags$br(),
                   sprintf("Shipments: %s", carlist[[i]][1,5]), tags$br(),
                   sprintf("Alarms: %s", carlist[[i]][1,6]), tags$br(),
                   sprintf("Alarm Rate: %.1f %%", as.numeric(carlist[[i]][1,8]))
                   
                 )))%>%
                 addPolylines(data = tmp3, color =colfunc(as.numeric(carlist[[i]][1,8])), weight = 1.5, layerId=c(carlist[[i]][1,1][2],carlist[[i]][1,2][2]), popup = as.character(tagList(
                   tags$strong(HTML(sprintf("%s",
                                            carlist[[i]][1,1]
                   ))), tags$br(),
                   sprintf("Route: %s", carlist[[i]][1,1]), tags$br(),
                   sprintf("Carrier: %s", carlist[[i]][1,2]), tags$br(),
                   sprintf("Origin: %s", carlist[[i]][1,3]), tags$br(),
                   sprintf("Destination: %s", carlist[[i]][1,4]), tags$br(),
                   sprintf("Shipments: %s", carlist[[i]][1,5]), tags$br(),
                   sprintf("Alarms: %s", carlist[[i]][1,6]), tags$br(),
                   sprintf("Alarm Rate: %.1f %%", as.numeric(carlist[[i]][1,8]))
                   
                 )))%>%
                 addLegend("bottomleft", pal=colfunc, values=0:100, bins=10, labFormat = labelFormat(suffix="%"), title="Alarm Rates",
                           layerId="colorLegend")
             }
             else {
               
               leafletProxy("map", data = alarm_rate_Pct1_car) %>%
                 #   #clearShapes() %>%
                 addPolylines(data = unlist(cbind(as.numeric(carlist[[i]][,9]), as.numeric(carlist[[i]][,10]))), color =colfunc(as.numeric(carlist[[i]][1,8])), weight = 1.5, layerId=c(carlist[[i]][1,1],carlist[[i]][1,2]), popup = as.character(tagList(
                   tags$strong(HTML(sprintf("%s",
                                            carlist[[i]][1,1]
                   ))), tags$br(),
                   sprintf("Route: %s", carlist[[i]][1,1]), tags$br(),
                   sprintf("Carrier: %s", carlist[[i]][1,2]), tags$br(),
                   sprintf("Origin: %s", carlist[[i]][1,3]), tags$br(),
                   sprintf("Destination: %s", carlist[[i]][1,4]), tags$br(),
                   sprintf("Shipments: %s", carlist[[i]][1,5]), tags$br(),
                   sprintf("Alarms: %s", carlist[[i]][1,6]), tags$br(),
                   sprintf("Alarm Rate: %.1f %%", as.numeric(carlist[[i]][1,8]))
                   
                 )))%>% 
                 addLegend("bottomleft", pal=colfunc, values=0:100, bins=10, labFormat = labelFormat(suffix="%"), title="Alarm Rates",
                           layerId="colorLegend")}
           }} }
       
} 
     
else if(input$viewchange == "Container") 
     { 
       for(i in 1:nrow(cleancontainerroutes2)) {
         if(input$alarmpct > as.numeric(conlist[[i]][1,8]) || input$shipments > as.numeric(conlist[[i]][1,5]) || (input$origin != "All Origins" && input$origin != as.character(conlist[[i]][1,3])) || (input$destination != "All Destinations" && input$destination != as.character(conlist[[i]][1,4])))
         {}
         else {
           if(input$color != as.character(conlist[[i]][1,2]))
           {}
           else {
             if(ncol(conlist[[i]]) < 10){
               temp <- as.data.frame(conlist[[i]][,9][1])
               tmp2 <- as.matrix(temp)
               temp2 <- as.data.frame(conlist[[i]][,9][2])
               tmp3 <- as.matrix(temp2)
               tmp4 <- rbind(tmp2,tmp3)
               leafletProxy("map", data = alarm_rate_Pct1_con) %>%
                 #   #clearShapes() %>%
                 addPolylines(data = tmp2, color =colfunc(as.numeric(conlist[[i]][1,8])), weight = 1.5, layerId=c(conlist[[i]][1,1][1],conlist[[i]][1,2][1]), popup = as.character(tagList(
                   tags$strong(HTML(sprintf("%s",
                                            conlist[[i]][1,1]
                   ))), tags$br(),
                   sprintf("Route: %s", conlist[[i]][1,1]), tags$br(),
                   sprintf("Container: %s", conlist[[i]][1,2]), tags$br(),
                   sprintf("Origin: %s", conlist[[i]][1,3]), tags$br(),
                   sprintf("Destination: %s", conlist[[i]][1,4]), tags$br(),
                   sprintf("Shipments: %s", conlist[[i]][1,5]), tags$br(),
                   sprintf("Alarms: %s", conlist[[i]][1,6]), tags$br(),
                   sprintf("Alarm Rate: %.1f %%", as.numeric(conlist[[i]][1,8]))
                   
                 )))%>%
                 addPolylines(data = tmp3, color =colfunc(as.numeric(conlist[[i]][1,8])), weight = 1.5, layerId=c(conlist[[i]][1,1][2],conlist[[i]][1,2][2]), popup = as.character(tagList(
                   tags$strong(HTML(sprintf("%s",
                                            conlist[[i]][1,1]
                   ))), tags$br(),
                   sprintf("Route: %s", conlist[[i]][1,1]), tags$br(),
                   sprintf("Container: %s", conlist[[i]][1,2]), tags$br(),
                   sprintf("Origin: %s", conlist[[i]][1,3]), tags$br(),
                   sprintf("Destination: %s", conlist[[i]][1,4]), tags$br(),
                   sprintf("Shipments: %s", conlist[[i]][1,5]), tags$br(),
                   sprintf("Alarms: %s", conlist[[i]][1,6]), tags$br(),
                   sprintf("Alarm Rate: %.1f %%", as.numeric(conlist[[i]][1,8]))
                   
                 )))%>%
                 addLegend("bottomleft", pal=colfunc, values=0:100, bins=10, labFormat = labelFormat(suffix="%"), title="Alarm Rates",
                           layerId="colorLegend")
             }
             else {
               
               leafletProxy("map", data = alarm_rate_Pct1_con) %>%
                 #   #clearShapes() %>%
                 addPolylines(data = unlist(cbind(as.numeric(conlist[[i]][,9]), as.numeric(conlist[[i]][,10]))), color =colfunc(as.numeric(conlist[[i]][1,8])), weight = 1.5, layerId=c(conlist[[i]][1,1],conlist[[i]][1,2]), popup = as.character(tagList(
                   tags$strong(HTML(sprintf("%s",
                                            conlist[[i]][1,1]
                   ))), tags$br(),
                   sprintf("Route: %s", conlist[[i]][1,1]), tags$br(),
                   sprintf("Container: %s", conlist[[i]][1,2]), tags$br(),
                   sprintf("Origin: %s", conlist[[i]][1,3]), tags$br(),
                   sprintf("Destination: %s", conlist[[i]][1,4]), tags$br(),
                   sprintf("Shipments: %s", conlist[[i]][1,5]), tags$br(),
                   sprintf("Alarms: %s", conlist[[i]][1,6]), tags$br(),
                   sprintf("Alarm Rate: %.1f %%", as.numeric(conlist[[i]][1,8]))
                   
                 )))%>% 
                 addLegend("bottomleft", pal=colfunc, values=0:100, bins=10, labFormat = labelFormat(suffix="%"), title="Alarm Rates",
                           layerId="colorLegend")}
           }} }
       
}  
}
else if(input$exclusions == TRUE)
     {
  if(input$viewchange == "All" || (input$viewchange == "Product" & input$prod == "All Products"))  { 
    
    for(i in 1:nrow(cleanroutes4)) {
      
      
      
      if(input$alarmpct > as.numeric(gg31[[i]][1,7]) || input$shipments > as.numeric(gg31[[i]][1,4]) || (input$origin != "All Origins" && input$origin != as.character(gg31[[i]][1,2])) || (input$destination != "All Destinations" && input$destination != as.character(gg31[[i]][1,3])))
      {}
      else {
        if(ncol(gg31[[i]]) < 9){
          temp <- as.data.frame(gg31[[i]][,8][1])
          tmp2 <- as.matrix(temp)
          temp2 <- as.data.frame(gg31[[i]][,8][2])
          tmp3 <- as.matrix(temp2)
          tmp4 <- rbind(tmp2,tmp3)
          leafletProxy("map", data = alarm_rate_Pct1ex) %>%
            #   #clearShapes() %>%
            addPolylines(data = tmp2, color =colfunc(as.numeric(gg31[[i]][1,7])), weight = 1.5, layerId=gg31[[i]][1][1], popup = as.character(tagList(
              tags$strong(HTML(sprintf("%s",
                                       gg31[[i]][1,1]
              ))), tags$br(),
              sprintf("Origin: %s", gg31[[i]][1,2]), tags$br(),
              sprintf("Destination: %s", gg31[[i]][1,3]), tags$br(),
              sprintf("Shipments: %s", gg31[[i]][1,4]), tags$br(),
              sprintf("Alarms: %s", gg31[[i]][1,5]), tags$br(),
              sprintf("Alarm Rate: %.1f %%", as.numeric(gg31[[i]][1,7]))
              
            )))%>%
            addPolylines(data = tmp3, color =colfunc(as.numeric(gg31[[i]][1,7])), weight = 1.5, layerId=gg31[[i]][1][2], popup = as.character(tagList(
              tags$strong(HTML(sprintf("%s",
                                       gg31[[i]][1,1]
              ))), tags$br(),
              sprintf("Origin: %s", gg31[[i]][1,2]), tags$br(),
              sprintf("Destination: %s", gg31[[i]][1,3]), tags$br(),
              sprintf("Shipments: %s", gg31[[i]][1,4]), tags$br(),
              sprintf("Alarms: %s", gg31[[i]][1,5]), tags$br(),
              sprintf("Alarm Rate: %.1f %%", as.numeric(gg31[[i]][1,7]))
              
            )))%>%
            
            
            addLegend("bottomleft", pal=colfunc, values=0:100, bins=10, labFormat = labelFormat(suffix="%"), title="Alarm Rates",
                      layerId="colorLegend")
        }
        else {
          
          leafletProxy("map", data = alarm_rate_Pct1ex) %>%
            #   #clearShapes() %>%
            addPolylines(data = unlist(cbind(as.numeric(gg31[[i]][,8]), as.numeric(gg31[[i]][,9]))), color =colfunc(as.numeric(gg31[[i]][1,7])), weight = 1.5, layerId=gg31[[i]][1], popup = as.character(tagList(
              tags$strong(HTML(sprintf("%s",
                                       gg31[[i]][1,1]
              ))), tags$br(),
              sprintf("Route: %s", gg31[[i]][1,1]), tags$br(),
              sprintf("Origin: %s", gg31[[i]][1,2]), tags$br(),
              sprintf("Destination: %s", gg31[[i]][1,3]), tags$br(),
              sprintf("Shipments: %s", gg31[[i]][1,4]), tags$br(),
              sprintf("Alarms: %s", gg31[[i]][1,5]), tags$br(),
              sprintf("Alarm Rate: %.1f %%", as.numeric(gg31[[i]][1,7]))
              
            )))%>% 
            addLegend("bottomleft", pal=colfunc, values=0:100, bins=10, labFormat = labelFormat(suffix="%"), title="Alarm Rates",
                      layerId="colorLegend")
        }
      }} 
    
  }
  
  
  else if(input$viewchange == "Product" && input$prod != "All Products")  { 
    for(i in 1:nrow(cleanproductroutes2)) {
      if(input$alarmpct > as.numeric(prodlistex[[i]][1,8]) || input$shipments > as.numeric(prodlistex[[i]][1,5]) || (input$origin != "All Origins" && input$origin != as.character(prodlistex[[i]][1,3])) || (input$destination != "All Destinations" && input$destination != as.character(prodlistex[[i]][1,4])))
      {}
      else {
        if(input$prod != as.character(prodlistex[[i]][1,2]))
        {}
        else {
          if(ncol(prodlistex[[i]]) < 10){
            temp <- as.data.frame(prodlistex[[i]][,9][1])
            tmp2 <- as.matrix(temp)
            temp2 <- as.data.frame(prodlistex[[i]][,9][2])
            tmp3 <- as.matrix(temp2)
            tmp4 <- rbind(tmp2,tmp3)
            leafletProxy("map", data = alarm_rate_Pct1_prod) %>%
              #   #clearShapes() %>%
              addPolylines(data = tmp2, color =colfunc(as.numeric(prodlistex[[i]][1,8])), weight = 1.5, layerId=c(prodlistex[[i]][1,1][1],prodlistex[[i]][1,2][1]), popup = as.character(tagList(
                tags$strong(HTML(sprintf("%s",
                                         prodlistex[[i]][1,1]
                ))), tags$br(),
                sprintf("Route: %s", prodlistex[[i]][1,1]), tags$br(),
                sprintf("Product: %s", prodlistex[[i]][1,2]), tags$br(),
                sprintf("Origin: %s", prodlistex[[i]][1,3]), tags$br(),
                sprintf("Destination: %s", prodlistex[[i]][1,4]), tags$br(),
                sprintf("Shipments: %s", prodlistex[[i]][1,5]), tags$br(),
                sprintf("Alarms: %s", prodlistex[[i]][1,6]), tags$br(),
                sprintf("Alarm Rate: %.1f %%", as.numeric(prodlistex[[i]][1,8]))
                
              )))%>%
              addPolylines(data = tmp3, color =colfunc(as.numeric(prodlistex[[i]][1,8])), weight = 1.5, layerId=c(prodlistex[[i]][1,1][2],prodlistex[[i]][1,2][2]), popup = as.character(tagList(
                tags$strong(HTML(sprintf("%s",
                                         prodlistex[[i]][1,1]
                ))), tags$br(),
                sprintf("Route: %s", prodlistex[[i]][1,1]), tags$br(),
                sprintf("Product: %s", prodlistex[[i]][1,2]), tags$br(),
                sprintf("Origin: %s", prodlistex[[i]][1,3]), tags$br(),
                sprintf("Destination: %s", prodlistex[[i]][1,4]), tags$br(),
                sprintf("Shipments: %s", prodlistex[[i]][1,5]), tags$br(),
                sprintf("Alarms: %s", prodlistex[[i]][1,6]), tags$br(),
                sprintf("Alarm Rate: %.1f %%", as.numeric(prodlistex[[i]][1,8]))
                
              )))%>%
              addLegend("bottomleft", pal=colfunc, values=0:100, bins=10, labFormat = labelFormat(suffix="%"), title="Alarm Rates",
                        layerId="colorLegend")
          }
          else {
            
            leafletProxy("map", data = alarm_rate_Pct1_prod) %>%
              #   #clearShapes() %>%
              addPolylines(data = unlist(cbind(as.numeric(prodlistex[[i]][,9]), as.numeric(prodlistex[[i]][,10]))), color =colfunc(as.numeric(prodlistex[[i]][1,8])), weight = 1.5, layerId=c(prodlistex[[i]][1,1],prodlistex[[i]][1,2]), popup = as.character(tagList(
                tags$strong(HTML(sprintf("%s",
                                         prodlistex[[i]][1,1]
                ))), tags$br(),
                sprintf("Route: %s", prodlistex[[i]][1,1]), tags$br(),
                sprintf("Product: %s", prodlistex[[i]][1,2]), tags$br(),
                sprintf("Origin: %s", prodlistex[[i]][1,3]), tags$br(),
                sprintf("Destination: %s", prodlistex[[i]][1,4]), tags$br(),
                sprintf("Shipments: %s", prodlistex[[i]][1,5]), tags$br(),
                sprintf("Alarms: %s", prodlistex[[i]][1,6]), tags$br(),
                sprintf("Alarm Rate: %.1f %%", as.numeric(prodlistex[[i]][1,8]))
                
              )))%>% 
              addLegend("bottomleft", pal=colfunc, values=0:100, bins=10, labFormat = labelFormat(suffix="%"), title="Alarm Rates",
                        layerId="colorLegend")}
        }} }
    
  }
  
  else if(input$viewchange == "Avg. Temperature")
  {
    for(i in 1:nrow(temp2_prod)) {
      if(input$prod != as.character(prodtemplistex[[i]][1,2]) || input$shipments > as.numeric(prodtemplistex[[i]][1,5]) || input$alarmpct > as.numeric(prodtemplistex[[i]][1,8]) || (input$origin != "All Origins" && input$origin != as.character(prodtemplistex[[i]][1,3])) || (input$destination != "All Destinations" && input$destination != as.character(prodtemplistex[[i]][1,4])))
      {}
      else {
        if(ncol(prodtemplistex[[i]]) < 13){
          temp <- as.data.frame(prodtemplistex[[i]][,12][1])
          tmp2 <- as.matrix(temp)
          temp2 <- as.data.frame(prodtemplistex[[i]][,12][2])
          tmp3 <- as.matrix(temp2)
          tmp4 <- rbind(tmp2,tmp3)
          leafletProxy("map", data = temp2_prod) %>%
            #   #clearShapes() %>%
            addPolylines(data = tmp2, color =colfunc2(as.numeric(prodtemplistex[[i]][1,9])), weight = 1.5, layerId=c(prodtemplistex[[i]][1,1][1],prodtemplistex[[i]][1,2][1]), popup = as.character(tagList(
              tags$strong(HTML(sprintf("%s",
                                       prodtemplistex[[i]][1,1]
              ))), tags$br(),
              sprintf("Route: %s", prodtemplistex[[i]][1,1]), tags$br(),
              sprintf("Product: %s", prodtemplistex[[i]][1,2]), tags$br(),
              sprintf("Origin: %s", prodtemplistex[[i]][1,3]), tags$br(),
              sprintf("Destination: %s", prodtemplistex[[i]][1,4]), tags$br(),
              sprintf("Shipments: %s", prodtemplistex[[i]][1,5]), tags$br(),
              sprintf("Alarms: %s", prodtemplistex[[i]][1,6]), tags$br(),
              sprintf("Alarm Rate: %.1f %%", as.numeric(prodtemplistex[[i]][1,8])),tags$br(),
              sprintf("Average Mean Temperature: %.1f", as.numeric(prodtemplistex[[i]][1,9])),tags$br(),
              sprintf("Average Max Temperature: %.1f", as.numeric(prodtemplistex[[i]][1,10])),tags$br(),
              sprintf("Average Min Temperature: %.1f", as.numeric(prodtemplistex[[i]][1,11]))
            )))%>%
            addPolylines(data = tmp3, color =colfunc2(as.numeric(prodtemplistex[[i]][1,9])), weight = 1.5, layerId=c(prodtemplistex[[i]][1,1][2],prodtemplistex[[i]][1,2][2]), popup =  as.character(tagList(
              tags$strong(HTML(sprintf("%s",
                                       prodtemplistex[[i]][1,1]
              ))), tags$br(),
              sprintf("Route: %s", prodtemplistex[[i]][1,1]), tags$br(),
              sprintf("Product: %s", prodtemplistex[[i]][1,2]), tags$br(),
              sprintf("Origin: %s", prodtemplistex[[i]][1,3]), tags$br(),
              sprintf("Destination: %s", prodtemplistex[[i]][1,4]), tags$br(),
              sprintf("Shipments: %s", prodtemplistex[[i]][1,5]), tags$br(),
              sprintf("Alarms: %s", prodtemplistex[[i]][1,6]), tags$br(),
              sprintf("Alarm Rate: %.1f %%", as.numeric(prodtemplistex[[i]][1,8])),tags$br(),
              sprintf("Average Mean Temperature: %.1f", as.numeric(prodtemplistex[[i]][1,9])),tags$br(),
              sprintf("Average Max Temperature: %.1f", as.numeric(prodtemplistex[[i]][1,10])),tags$br(),
              sprintf("Average Min Temperature: %.1f", as.numeric(prodtemplistex[[i]][1,11]))
              
            )))%>%
            addLegend("bottomleft", pal=colfunc2, values=min(temp2_prod$tempmeanvalue):max(temp2_prod$tempmeanvalue), bins=10, labFormat = labelFormat(suffix=" °C"), title="Alarm Rates",
                      layerId="colorLegend")
        }
        else {
          
          leafletProxy("map", data = temp2_prod) %>%
            #   #clearShapes() %>%
            addPolylines(data = unlist(cbind(as.numeric(prodtemplistex[[i]][,12]), as.numeric(prodtemplistex[[i]][,13]))), color =colfunc2(as.numeric(prodtemplistex[[i]][1,9])), weight = 1.5, layerId=c(prodtemplistex[[i]][1,1],prodtemplistex[[i]][1,2]), popup =  as.character(tagList(
              tags$strong(HTML(sprintf("%s",
                                       prodtemplistex[[i]][1,1]
              ))), tags$br(),
              sprintf("Route: %s", prodtemplistex[[i]][1,1]), tags$br(),
              sprintf("Product: %s", prodtemplistex[[i]][1,2]), tags$br(),
              sprintf("Origin: %s", prodtemplistex[[i]][1,3]), tags$br(),
              sprintf("Destination: %s", prodtemplistex[[i]][1,4]), tags$br(),
              sprintf("Shipments: %s", prodtemplistex[[i]][1,5]), tags$br(),
              sprintf("Alarms: %s", prodtemplistex[[i]][1,6]), tags$br(),
              sprintf("Alarm Rate: %.1f %%", as.numeric(prodtemplistex[[i]][1,8])),tags$br(),
              sprintf("Average Mean Temperature: %.1f", as.numeric(prodtemplistex[[i]][1,9])),tags$br(),
              sprintf("Average Max Temperature: %.1f", as.numeric(prodtemplistex[[i]][1,10])),tags$br(),
              sprintf("Average Min Temperature: %.1f", as.numeric(prodtemplistex[[i]][1,11]))
            )))%>% 
            addLegend("bottomleft", pal=colfunc2, values=min(temp2_prod$tempmeanvalue):max(temp2_prod$tempmeanvalue), bins=10, labFormat = labelFormat(suffix=" °C"), title="Alarm Rates",
                      layerId="colorLegend")}
      }} 
    
    
  }
  
  else if(input$viewchange == "Min Temperature")
  {
    for(i in 1:nrow(temp2_prod)) {
      if(input$prod != as.character(prodtemplistex[[i]][1,2]) || input$shipments > as.numeric(prodtemplistex[[i]][1,5]) || input$alarmpct > as.numeric(prodtemplistex[[i]][1,8]) || (input$origin != "All Origins" && input$origin != as.character(prodtemplistex[[i]][1,3])) || (input$destination != "All Destinations" && input$destination != as.character(prodtemplistex[[i]][1,4])) || (input$temperature > as.numeric(prodtemplistex[[i]][1,11])))
      {}
      else {
        if(ncol(prodtemplistex[[i]]) < 13){
          temp <- as.data.frame(prodtemplistex[[i]][,12][1])
          tmp2 <- as.matrix(temp)
          temp2 <- as.data.frame(prodtemplistex[[i]][,12][2])
          tmp3 <- as.matrix(temp2)
          tmp4 <- rbind(tmp2,tmp3)
          leafletProxy("map", data = temp2_prod) %>%
            #   #clearShapes() %>%
            addPolylines(data = tmp2, color =colfunc4(as.numeric(prodtemplistex[[i]][1,11])), weight = 1.5, layerId=c(prodtemplistex[[i]][1,1][1],prodtemplistex[[i]][1,2][1]), popup = as.character(tagList(
              tags$strong(HTML(sprintf("%s",
                                       prodtemplistex[[i]][1,1]
              ))), tags$br(),
              sprintf("Route: %s", prodtemplistex[[i]][1,1]), tags$br(),
              sprintf("Product: %s", prodtemplistex[[i]][1,2]), tags$br(),
              sprintf("Origin: %s", prodtemplistex[[i]][1,3]), tags$br(),
              sprintf("Destination: %s", prodtemplistex[[i]][1,4]), tags$br(),
              sprintf("Shipments: %s", prodtemplistex[[i]][1,5]), tags$br(),
              sprintf("Alarms: %s", prodtemplistex[[i]][1,6]), tags$br(),
              sprintf("Alarm Rate: %.1f %%", as.numeric(prodtemplistex[[i]][1,8])),tags$br(),
              sprintf("Average Mean Temperature: %.1f", as.numeric(prodtemplistex[[i]][1,9])),tags$br(),
              sprintf("Average Max Temperature: %.1f", as.numeric(prodtemplistex[[i]][1,10])),tags$br(),
              sprintf("Average Min Temperature: %.1f", as.numeric(prodtemplistex[[i]][1,11]))
            )))%>%
            addPolylines(data = tmp3, color =colfunc4(as.numeric(prodtemplistex[[i]][1,11])), weight = 1.5, layerId=c(prodtemplistex[[i]][1,1][2],prodtemplistex[[i]][1,2][2]), popup =  as.character(tagList(
              tags$strong(HTML(sprintf("%s",
                                       prodtemplistex[[i]][1,1]
              ))), tags$br(),
              sprintf("Route: %s", prodtemplistex[[i]][1,1]), tags$br(),
              sprintf("Product: %s", prodtemplistex[[i]][1,2]), tags$br(),
              sprintf("Origin: %s", prodtemplistex[[i]][1,3]), tags$br(),
              sprintf("Destination: %s", prodtemplistex[[i]][1,4]), tags$br(),
              sprintf("Shipments: %s", prodtemplistex[[i]][1,5]), tags$br(),
              sprintf("Alarms: %s", prodtemplistex[[i]][1,6]), tags$br(),
              sprintf("Alarm Rate: %.1f %%", as.numeric(prodtemplistex[[i]][1,8])),tags$br(),
              sprintf("Average Mean Temperature: %.1f", as.numeric(prodtemplistex[[i]][1,9])),tags$br(),
              sprintf("Average Max Temperature: %.1f", as.numeric(prodtemplistex[[i]][1,10])),tags$br(),
              sprintf("Average Min Temperature: %.1f", as.numeric(prodtemplistex[[i]][1,11]))
              
            )))%>%
            addLegend("bottomleft", pal=colfunc4, values=min(temp2_prod$tempminvalue):max(temp2_prod$tempminvalue), bins=10, labFormat = labelFormat(suffix=" °C"), title="Alarm Rates",
                      layerId="colorLegend")
        }
        else {
          
          leafletProxy("map", data = temp2_prod) %>%
            #   #clearShapes() %>%
            addPolylines(data = unlist(cbind(as.numeric(prodtemplistex[[i]][,12]), as.numeric(prodtemplistex[[i]][,13]))), color =colfunc4(as.numeric(prodtemplistex[[i]][1,11])), weight = 1.5, layerId=c(prodtemplistex[[i]][1,1],prodtemplistex[[i]][1,2]), popup =  as.character(tagList(
              tags$strong(HTML(sprintf("%s",
                                       prodtemplistex[[i]][1,1]
              ))), tags$br(),
              sprintf("Route: %s", prodtemplistex[[i]][1,1]), tags$br(),
              sprintf("Product: %s", prodtemplistex[[i]][1,2]), tags$br(),
              sprintf("Origin: %s", prodtemplistex[[i]][1,3]), tags$br(),
              sprintf("Destination: %s", prodtemplistex[[i]][1,4]), tags$br(),
              sprintf("Shipments: %s", prodtemplistex[[i]][1,5]), tags$br(),
              sprintf("Alarms: %s", prodtemplistex[[i]][1,6]), tags$br(),
              sprintf("Alarm Rate: %.1f %%", as.numeric(prodtemplistex[[i]][1,8])),tags$br(),
              sprintf("Average Mean Temperature: %.1f", as.numeric(prodtemplistex[[i]][1,9])),tags$br(),
              sprintf("Average Max Temperature: %.1f", as.numeric(prodtemplistex[[i]][1,10])),tags$br(),
              sprintf("Average Min Temperature: %.1f", as.numeric(prodtemplistex[[i]][1,11]))
            )))%>% 
            addLegend("bottomleft", pal=colfunc4, values=min(temp2_prod$tempminvalue):max(temp2_prod$tempminvalue), bins=10, labFormat = labelFormat(suffix=" °C"), title="Alarm Rates",
                      layerId="colorLegend")}
      }} 
    
    
    
  }     
  
  else if(input$viewchange == "Max Temperature")
  {
    for(i in 1:nrow(temp2_prod)) {
      if(input$prod != as.character(prodtemplistex[[i]][1,2]) || input$shipments > as.numeric(prodtemplistex[[i]][1,5]) || input$alarmpct > as.numeric(prodtemplistex[[i]][1,8]) || (input$origin != "All Origins" && input$origin != as.character(prodtemplistex[[i]][1,3])) || (input$destination != "All Destinations" && input$destination != as.character(prodtemplistex[[i]][1,4])))
      {}
      else {
        if(ncol(prodtemplistex[[i]]) < 13){
          temp <- as.data.frame(prodtemplistex[[i]][,12][1])
          tmp2 <- as.matrix(temp)
          temp2 <- as.data.frame(prodtemplistex[[i]][,12][2])
          tmp3 <- as.matrix(temp2)
          tmp4 <- rbind(tmp2,tmp3)
          leafletProxy("map", data = temp2_prod) %>%
            #   #clearShapes() %>%
            addPolylines(data = tmp2, color =colfunc3(as.numeric(prodtemplistex[[i]][1,10])), weight = 1.5, layerId=c(prodtemplistex[[i]][1,1][1],prodtemplistex[[i]][1,2][1]), popup = as.character(tagList(
              tags$strong(HTML(sprintf("%s",
                                       prodtemplistex[[i]][1,1]
              ))), tags$br(),
              sprintf("Route: %s", prodtemplistex[[i]][1,1]), tags$br(),
              sprintf("Product: %s", prodtemplistex[[i]][1,2]), tags$br(),
              sprintf("Origin: %s", prodtemplistex[[i]][1,3]), tags$br(),
              sprintf("Destination: %s", prodtemplistex[[i]][1,4]), tags$br(),
              sprintf("Shipments: %s", prodtemplistex[[i]][1,5]), tags$br(),
              sprintf("Alarms: %s", prodtemplistex[[i]][1,6]), tags$br(),
              sprintf("Alarm Rate: %.1f %%", as.numeric(prodtemplistex[[i]][1,8])),tags$br(),
              sprintf("Average Mean Temperature: %.1f", as.numeric(prodtemplistex[[i]][1,9])),tags$br(),
              sprintf("Average Max Temperature: %.1f", as.numeric(prodtemplistex[[i]][1,10])),tags$br(),
              sprintf("Average Min Temperature: %.1f", as.numeric(prodtemplistex[[i]][1,11]))
            )))%>%
            addPolylines(data = tmp3, color =colfunc3(as.numeric(prodtemplistex[[i]][1,10])), weight = 1.5, layerId=c(prodtemplistex[[i]][1,1][2],prodtemplistex[[i]][1,2][2]), popup =  as.character(tagList(
              tags$strong(HTML(sprintf("%s",
                                       prodtemplistex[[i]][1,1]
              ))), tags$br(),
              sprintf("Route: %s", prodtemplistex[[i]][1,1]), tags$br(),
              sprintf("Product: %s", prodtemplistex[[i]][1,2]), tags$br(),
              sprintf("Origin: %s", prodtemplistex[[i]][1,3]), tags$br(),
              sprintf("Destination: %s", prodtemplistex[[i]][1,4]), tags$br(),
              sprintf("Shipments: %s", prodtemplistex[[i]][1,5]), tags$br(),
              sprintf("Alarms: %s", prodtemplistex[[i]][1,6]), tags$br(),
              sprintf("Alarm Rate: %.1f %%", as.numeric(prodtemplistex[[i]][1,8])),tags$br(),
              sprintf("Average Mean Temperature: %.1f", as.numeric(prodtemplistex[[i]][1,9])),tags$br(),
              sprintf("Average Max Temperature: %.1f", as.numeric(prodtemplistex[[i]][1,10])),tags$br(),
              sprintf("Average Min Temperature: %.1f", as.numeric(prodtemplistex[[i]][1,11]))
              
            )))%>%
            addLegend("bottomleft", pal=colfunc3, values=min(temp2_prod$tempmaxvalue):max(temp2_prod$tempmaxvalue), bins=10, labFormat = labelFormat(suffix=" °C"), title="Alarm Rates",
                      layerId="colorLegend")
        }
        else {
          
          leafletProxy("map", data = temp2_prod) %>%
            #   #clearShapes() %>%
            addPolylines(data = unlist(cbind(as.numeric(prodtemplistex[[i]][,12]), as.numeric(prodtemplistex[[i]][,13]))), color =colfunc3(as.numeric(prodtemplistex[[i]][1,10])), weight = 1.5, layerId=c(prodtemplistex[[i]][1,1],prodtemplistex[[i]][1,2]), popup =  as.character(tagList(
              tags$strong(HTML(sprintf("%s",
                                       prodtemplistex[[i]][1,1]
              ))), tags$br(),
              sprintf("Route: %s", prodtemplistex[[i]][1,1]), tags$br(),
              sprintf("Product: %s", prodtemplistex[[i]][1,2]), tags$br(),
              sprintf("Origin: %s", prodtemplistex[[i]][1,3]), tags$br(),
              sprintf("Destination: %s", prodtemplistex[[i]][1,4]), tags$br(),
              sprintf("Shipments: %s", prodtemplistex[[i]][1,5]), tags$br(),
              sprintf("Alarms: %s", prodtemplistex[[i]][1,6]), tags$br(),
              sprintf("Alarm Rate: %.1f %%", as.numeric(prodtemplistex[[i]][1,8])),tags$br(),
              sprintf("Average Mean Temperature: %.1f", as.numeric(prodtemplistex[[i]][1,9])),tags$br(),
              sprintf("Average Max Temperature: %.1f", as.numeric(prodtemplistex[[i]][1,10])),tags$br(),
              sprintf("Average Min Temperature: %.1f", as.numeric(prodtemplistex[[i]][1,11]))
            )))%>% 
            addLegend("bottomleft", pal=colfunc3, values=min(temp2_prod$tempmaxvalue):max(temp2_prod$tempmaxvalue), bins=10, labFormat = labelFormat(suffix=" °C"), title="Alarm Rates",
                      layerId="colorLegend")}
      }} 
    
    
    
  }     
  
  else if(input$viewchange == "Carrier") 
  { 
    for(i in 1:nrow(cleancarrierroutes2)) {
      if(input$alarmpct > as.numeric(carlistex[[i]][1,8]) || input$shipments > as.numeric(carlistex[[i]][1,5]) || (input$origin != "All Origins" && input$origin != as.character(carlistex[[i]][1,3])) || (input$destination != "All Destinations" && input$destination != as.character(carlistex[[i]][1,4])))
      {}
      else {
        if(input$carrier != as.character(carlistex[[i]][1,2]) && input$carrier != "All Carriers")
        {}
        else {
          if(ncol(carlistex[[i]]) < 10){
            temp <- as.data.frame(carlistex[[i]][,9][1])
            tmp2 <- as.matrix(temp)
            temp2 <- as.data.frame(carlistex[[i]][,9][2])
            tmp3 <- as.matrix(temp2)
            tmp4 <- rbind(tmp2,tmp3)
            leafletProxy("map", data = alarm_rate_Pct1_car) %>%
              #   #clearShapes() %>%
              addPolylines(data = tmp2, color =colfunc(as.numeric(carlistex[[i]][1,8])), weight = 1.5, layerId=c(carlistex[[i]][1,1][1],carlistex[[i]][1,2][1]), popup = as.character(tagList(
                tags$strong(HTML(sprintf("%s",
                                         carlistex[[i]][1,1]
                ))), tags$br(),
                sprintf("Route: %s", carlistex[[i]][1,1]), tags$br(),
                sprintf("Carrier: %s", carlistex[[i]][1,2]), tags$br(),
                sprintf("Origin: %s", carlistex[[i]][1,3]), tags$br(),
                sprintf("Destination: %s", carlistex[[i]][1,4]), tags$br(),
                sprintf("Shipments: %s", carlistex[[i]][1,5]), tags$br(),
                sprintf("Alarms: %s", carlistex[[i]][1,6]), tags$br(),
                sprintf("Alarm Rate: %.1f %%", as.numeric(carlistex[[i]][1,8]))
                
              )))%>%
              addPolylines(data = tmp3, color =colfunc(as.numeric(carlistex[[i]][1,8])), weight = 1.5, layerId=c(carlistex[[i]][1,1][2],carlistex[[i]][1,2][2]), popup = as.character(tagList(
                tags$strong(HTML(sprintf("%s",
                                         carlistex[[i]][1,1]
                ))), tags$br(),
                sprintf("Route: %s", carlistex[[i]][1,1]), tags$br(),
                sprintf("Carrier: %s", carlistex[[i]][1,2]), tags$br(),
                sprintf("Origin: %s", carlistex[[i]][1,3]), tags$br(),
                sprintf("Destination: %s", carlistex[[i]][1,4]), tags$br(),
                sprintf("Shipments: %s", carlistex[[i]][1,5]), tags$br(),
                sprintf("Alarms: %s", carlistex[[i]][1,6]), tags$br(),
                sprintf("Alarm Rate: %.1f %%", as.numeric(carlistex[[i]][1,8]))
                
              )))%>%
              addLegend("bottomleft", pal=colfunc, values=0:100, bins=10, labFormat = labelFormat(suffix="%"), title="Alarm Rates",
                        layerId="colorLegend")
          }
          else {
            
            leafletProxy("map", data = alarm_rate_Pct1_car) %>%
              #   #clearShapes() %>%
              addPolylines(data = unlist(cbind(as.numeric(carlistex[[i]][,9]), as.numeric(carlistex[[i]][,10]))), color =colfunc(as.numeric(carlistex[[i]][1,8])), weight = 1.5, layerId=c(carlistex[[i]][1,1],carlistex[[i]][1,2]), popup = as.character(tagList(
                tags$strong(HTML(sprintf("%s",
                                         carlistex[[i]][1,1]
                ))), tags$br(),
                sprintf("Route: %s", carlistex[[i]][1,1]), tags$br(),
                sprintf("Carrier: %s", carlistex[[i]][1,2]), tags$br(),
                sprintf("Origin: %s", carlistex[[i]][1,3]), tags$br(),
                sprintf("Destination: %s", carlistex[[i]][1,4]), tags$br(),
                sprintf("Shipments: %s", carlistex[[i]][1,5]), tags$br(),
                sprintf("Alarms: %s", carlistex[[i]][1,6]), tags$br(),
                sprintf("Alarm Rate: %.1f %%", as.numeric(carlistex[[i]][1,8]))
                
              )))%>% 
              addLegend("bottomleft", pal=colfunc, values=0:100, bins=10, labFormat = labelFormat(suffix="%"), title="Alarm Rates",
                        layerId="colorLegend")}
        }} }
    
  } 
  
  else if(input$viewchange == "Container") 
  { 
    for(i in 1:nrow(cleancontainerroutes2)) {
      if(input$alarmpct > as.numeric(conlistex[[i]][1,8]) || input$shipments > as.numeric(conlistex[[i]][1,5]) || (input$origin != "All Origins" && input$origin != as.character(conlistex[[i]][1,3])) || (input$destination != "All Destinations" && input$destination != as.character(conlistex[[i]][1,4])))
      {}
      else {
        if(input$color != as.character(conlistex[[i]][1,2]) && input$color != "All Containers")
        {}
        else {
          if(ncol(conlistex[[i]]) < 10){
            temp <- as.data.frame(conlistex[[i]][,9][1])
            tmp2 <- as.matrix(temp)
            temp2 <- as.data.frame(conlistex[[i]][,9][2])
            tmp3 <- as.matrix(temp2)
            tmp4 <- rbind(tmp2,tmp3)
            leafletProxy("map", data = alarm_rate_Pct1_con) %>%
              #   #clearShapes() %>%
              addPolylines(data = tmp2, color =colfunc(as.numeric(conlistex[[i]][1,8])), weight = 1.5, layerId=c(conlistex[[i]][1,1][1],conlistex[[i]][1,2][1]), popup = as.character(tagList(
                tags$strong(HTML(sprintf("%s",
                                         conlistex[[i]][1,1]
                ))), tags$br(),
                sprintf("Route: %s", conlistex[[i]][1,1]), tags$br(),
                sprintf("Container: %s", conlistex[[i]][1,2]), tags$br(),
                sprintf("Origin: %s", conlistex[[i]][1,3]), tags$br(),
                sprintf("Destination: %s", conlistex[[i]][1,4]), tags$br(),
                sprintf("Shipments: %s", conlistex[[i]][1,5]), tags$br(),
                sprintf("Alarms: %s", conlistex[[i]][1,6]), tags$br(),
                sprintf("Alarm Rate: %.1f %%", as.numeric(conlistex[[i]][1,8]))
                
              )))%>%
              addPolylines(data = tmp3, color =colfunc(as.numeric(conlistex[[i]][1,8])), weight = 1.5, layerId=c(conlistex[[i]][1,1][2],conlistex[[i]][1,2][2]), popup = as.character(tagList(
                tags$strong(HTML(sprintf("%s",
                                         conlistex[[i]][1,1]
                ))), tags$br(),
                sprintf("Route: %s", conlistex[[i]][1,1]), tags$br(),
                sprintf("Container: %s", conlistex[[i]][1,2]), tags$br(),
                sprintf("Origin: %s", conlistex[[i]][1,3]), tags$br(),
                sprintf("Destination: %s", conlistex[[i]][1,4]), tags$br(),
                sprintf("Shipments: %s", conlistex[[i]][1,5]), tags$br(),
                sprintf("Alarms: %s", conlistex[[i]][1,6]), tags$br(),
                sprintf("Alarm Rate: %.1f %%", as.numeric(conlistex[[i]][1,8]))
                
              )))%>%
              addLegend("bottomleft", pal=colfunc, values=0:100, bins=10, labFormat = labelFormat(suffix="%"), title="Alarm Rates",
                        layerId="colorLegend")
          }
          else {
            
            leafletProxy("map", data = alarm_rate_Pct1_con) %>%
              #   #clearShapes() %>%
              addPolylines(data = unlist(cbind(as.numeric(conlistex[[i]][,9]), as.numeric(conlistex[[i]][,10]))), color =colfunc(as.numeric(conlistex[[i]][1,8])), weight = 1.5, layerId=c(conlistex[[i]][1,1],conlistex[[i]][1,2]), popup = as.character(tagList(
                tags$strong(HTML(sprintf("%s",
                                         conlistex[[i]][1,1]
                ))), tags$br(),
                sprintf("Route: %s", conlistex[[i]][1,1]), tags$br(),
                sprintf("Container: %s", conlistex[[i]][1,2]), tags$br(),
                sprintf("Origin: %s", conlistex[[i]][1,3]), tags$br(),
                sprintf("Destination: %s", conlistex[[i]][1,4]), tags$br(),
                sprintf("Shipments: %s", conlistex[[i]][1,5]), tags$br(),
                sprintf("Alarms: %s", conlistex[[i]][1,6]), tags$br(),
                sprintf("Alarm Rate: %.1f %%", as.numeric(conlistex[[i]][1,8]))
                
              )))%>% 
              addLegend("bottomleft", pal=colfunc, values=0:100, bins=10, labFormat = labelFormat(suffix="%"), title="Alarm Rates",
                        layerId="colorLegend")}
        }} }
    
  }  
       
     }
})
  

  # # Show a popup at the given location
  # showZipcodePopup <- function(zipcode, lat, lng) {
  #   selectedLoc <- site[allzips$zipcode == zipcode,]
  #   content <- as.character(tagList(
  #     tags$h4("Score:", as.integer(selectedZip$centile)),
  #     tags$strong(HTML(sprintf("%s, %s %s",
  #       selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
  #     ))), tags$br(),
  #     sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
  #     sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
  #     sprintf("Adult population: %s", selectedZip$adultpop)
  #   ))
  #   leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  # }
  # 
  # # When map is clicked, show a popup with city info
  # observe({
  #   leafletProxy("map") %>% clearPopups()
  #   event <- input$map_shape_click
  #   if (is.null(event))
  #     return()
  # 
  #   isolate({
  #     showZipcodePopup(event$id, event$lat, event$lng)
  #   })
  # })


  showLocationPopup <- function(country_Desc, lat, lng) {
    selectedLoc <- site_locations[site_locations$country_Desc == country_Desc,]
    content <- as.character(tagList(
      tags$strong(HTML(sprintf("%s, %s",
                               selectedLoc$country_Desc, selectedLoc$Country
      ))), tags$br(),
      sprintf("Location: %s", selectedLoc$country_Desc), tags$br(),
      sprintf("Country: %s", selectedLoc$Country)

    ))
    leafletProxy("map") 
    #%>% addPopups(lng, lat, content, layerId = country_Desc)
  }
  
  # showLocationPopup <- function(id, lat, lng) {
  #   selectedLoc <- realtest3[realtest3$id == id,]
  #   content <- as.character(tagList(
  #     tags$strong(HTML(sprintf("%s, %s",
  #                              selectedLoc$id, selectedLoc$id
  #     ))), tags$br(),
  #     sprintf("Location: %s", selectedLoc$id), tags$br(),
  #     sprintf("Country: %s", selectedLoc$id)
  #     
  #   ))
  #   leafletProxy("map") %>% addPopups(lng, lat, content, layerId = id)
  # }


  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()

    isolate({
      showLocationPopup(event$id, event$lat, event$lng)
    })
  })
  #ROUTE POP UP WORK
  # showRoutePopup <- function(route, lat, lng) {
  #   selectedRoute <- outlist2[[1]][1,1:2]== route
  #   content <- as.character(tagList(
  #     tags$strong(HTML(sprintf("%s, %s",
  #                              selectedRoute[1], selectedRoute[2]
  #     ))), tags$br(),
  #     sprintf("Origin: %s", selectedRoute[1]), tags$br(),
  #     sprintf("Destination: %s", selectedRoute[2])
  # 
  #   ))
  #   leafletProxy("map") %>% addPopups(lng, lat, content, layerId = outlist2)
  # }
  # 
  # observe({
  #   leafletProxy("map") %>% clearPopups()
  #   event2 <- input$map_shape_click
  #   if (is.null(event2))
  #     return()
  # 
  #   isolate({
  #     showRoutePopup(event2$id, event2$lat, event2$lng)
  #   })
  # })
  
  
  #Info Box Calculations

  output$shipmentBox <- renderValueBox({
    if(input$exclusions == FALSE){
    data <- uniqshipments %>% select(shipmentid,
                                     shipmentshippeddate,
                                     shipmentcarrierdesc, 
                                     shipmentcontainerdesc,
                                     shipmentorigindesc, 
                                     shipmentdestinationdesc,
                                     shipmentitemproductdesc,
                                     route,Alarm.Triggered,
                                     Count.of.alarms)
    }
    else if(input$exclusions == TRUE){
      data <- uniqshipments5 %>% select(shipmentid,
                                       shipmentshippeddate,
                                       shipmentcarrierdesc, 
                                       shipmentcontainerdesc,
                                       shipmentorigindesc, 
                                       shipmentdestinationdesc,
                                       shipmentitemproductdesc,
                                       route,Alarm.Triggered,
                                       Count.of.alarms)
      
    }
    if (input$origin != "All Origins") {
      data <- data[data$shipmentorigindesc == input$origin,]
    }
    if (input$destination != "All Destinations") {
      data <- data[data$shipmentdestinationdesc == input$destination,]
    }
    if (input$viewchange != "All" && input$viewchange != "Carrier" && input$viewchange != "Container")
    {
      if(input$prod != "All Products")
      {
        data <- data[data$shipmentitemproductdesc == input$prod,]
        
      }
      
    }
    if (input$carrier != "All Carriers") {
      data <- data[data$shipmentcarrierdesc == input$carrier,]
    }
    
    if (input$color != "All Containers") {
      data <- data[data$shipmentcontainerdesc == input$color,]
    }
    # if (input$shipmentRootCauseofTempExcursionDesc != "All") {
    #   data <- data[data$ShipmentRootCauseofTempExcursionDesc == input$shipmentRootCauseofTempExcursionDesc,]
    #}
    
    
    valueBox(
     
      nrow(data), "Total Shipments", icon = icon("plane"),
      color = "purple"
    )
  })
  
  output$alarmBox <- renderValueBox({
    
    if(input$exclusions == FALSE){
      data2 <- uniqshipments %>% select(shipmentid,
                                       shipmentshippeddate,
                                       shipmentcarrierdesc, 
                                       shipmentcontainerdesc,
                                       shipmentorigindesc, 
                                       shipmentdestinationdesc,
                                       shipmentitemproductdesc,
                                       route,Alarm.Triggered,
                                       Count.of.alarms)
    }
    else if(input$exclusions == TRUE){
      data2 <- uniqshipments5 %>% select(shipmentid,
                                        shipmentshippeddate,
                                        shipmentcarrierdesc, 
                                        shipmentcontainerdesc,
                                        shipmentorigindesc, 
                                        shipmentdestinationdesc,
                                        shipmentitemproductdesc,
                                        route,Alarm.Triggered,
                                        Count.of.alarms)
      
    }
    if (input$origin != "All Origins") {
      data2 <- data2[data2$shipmentorigindesc == input$origin,]
    }
    if (input$destination != "All Destinations") {
      data2 <- data2[data2$shipmentdestinationdesc == input$destination,]
    }
    if (input$viewchange != "All" && input$viewchange != "Carrier" && input$viewchange != "Container")
    {
      if(input$prod != "All Products")
      {
        data2 <- data2[data2$shipmentitemproductdesc == input$prod,]
        
      }
      
    }
    if (input$carrier != "All Carriers") {
      data2 <- data2[data2$shipmentcarrierdesc == input$carrier,]
    }
    
    if (input$color != "All Containers") {
      data2 <- data2[data2$shipmentcontainerdesc == input$color,]
    }
    data2 <- data2[data2$Alarm.Triggered == TRUE,]
    
    
    valueBox(
    
      
      nrow(data2), "Shipments with Alarms", icon = icon("bell"),
      color = "yellow"
    )
  })
  
  output$pctBox <- renderInfoBox({
    if(input$exclusions == FALSE){
      data <- uniqshipments %>% select(shipmentid,
                                       shipmentshippeddate,
                                       shipmentcarrierdesc, 
                                       shipmentcontainerdesc,
                                       shipmentorigindesc, 
                                       shipmentdestinationdesc,
                                       shipmentitemproductdesc,
                                       route,Alarm.Triggered,
                                       Count.of.alarms)
    }
    else if(input$exclusions == TRUE){
      data <- uniqshipments5 %>% select(shipmentid,
                                        shipmentshippeddate,
                                        shipmentcarrierdesc, 
                                        shipmentcontainerdesc,
                                        shipmentorigindesc, 
                                        shipmentdestinationdesc,
                                        shipmentitemproductdesc,
                                        route,Alarm.Triggered,
                                        Count.of.alarms)
      
    }
    if (input$origin != "All Origins") {
      data <- data[data$shipmentorigindesc == input$origin,]
    }
    if (input$destination != "All Destinations") {
      data <- data[data$shipmentdestinationdesc == input$destination,]
    }
    if (input$viewchange != "All"  && input$viewchange != "Carrier" && input$viewchange != "Container")
    {
      if(input$prod != "All Products")
      {
        data <- data[data$shipmentitemproductdesc == input$prod,]
        
      }
      
    }
    if (input$carrier != "All Carriers") {
      data <- data[data$shipmentcarrierdesc == input$carrier,]
    }
    if (input$color != "All Containers") {
      data <- data[data$shipmentcontainerdesc == input$color,]
    }
    data2 <- data[data$Alarm.Triggered == TRUE,]
    pct <- round((nrow(data2)/nrow(data))*100,2)
    valueBox(
      pct, "Alarm Rate", icon = icon("percent"),
      color = "orange"
    )
  })
  

  # ship_counts <- nrow(uniqshipments[as.character(uniqshipments$shipmentitemproductdesc) == input$prod & as.character(uniqshipments$shipmentorigindesc) == input$origin & as.character(uniqshipments$shipmentdestinationdesc) == input$destination,])
  # alarm_counts <- nrow(uniqshipments[as.character(uniqshipments$shipmentitemproductdesc) == input$prod & as.character(uniqshipments$shipmentorigindesc) == input$origin & as.character(uniqshipments$shipmentdestinationdesc) == input$destination & uniqshipments$Alarm.Triggered == TRUE,])
  # percentage_alarm <- (alarm_counts/ship_counts)*100
  # percentage_alarm <- round(percentage_alarm, digits = 2)

  ## Data Explorer ###########################################

  # observe({
  #   cities <- if (is.null(input$states)) character(0) else {
  #     filter(cleantable, State %in% input$states) %>%
  #       `$`('City') %>%
  #       unique() %>%
  #       sort()
  #   }
  #   stillSelected <- isolate(input$cities[input$cities %in% cities])
  #   updateSelectInput(session, "cities", choices = cities,
  #     selected = stillSelected)
  # })
  
  observe({
    destinations <- if (is.null(input$origin)) character(0) else {
      filter(cleantable, Origin %in% input$origin) %>%
        `$`('Destination') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$destinations[input$destinations %in% destinations])
    updateSelectInput(session, "destinations", choices = destinations,
                      selected = stillSelected)
  })
  
  # observe({
  #   products <- if (is.null(input$products)) character(0) else {
  #     filter(cleantable, Product %in% input$products) %>%
  #       unique() %>%
  #       sort()
  #   }
  #   # stillSelected <- isolate(input$cities[input$cities %in% cities])
  #   # updateSelectInput(session, "cities", choices = cities,
  #   #   selected = stillSelected)
  # })

  observe({
    zipcodes <- if (is.null(input$states)) character(0) else {
      cleantable %>%
        filter(State %in% input$states,
          is.null(input$cities) | City %in% input$cities) %>%
        `$`('Zipcode') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectInput(session, "zipcodes", choices = zipcodes,
      selected = stillSelected)
  })

  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
  
  
  dt_filter <- reactive({
    

    
    if (input$origininput != "All Origins") {
      cleantable <- cleantable[cleantable$Origin == input$origininput,]
    }
    if (input$destinationinput != "All Destinations") {
      cleantable <- cleantable[cleantable$Destination == input$destinationinput,]
    }
    if (input$productinput != "All Products") {
      cleantable <- cleantable[cleantable$Product == input$productinput,]
    }
    if (input$carriersinput != "All Carriers") {
      cleantable <- cleantable[cleantable$Carrier == input$carriersinput,]
    }
    if (input$containersinput != "All Containers") {
      cleantable <- cleantable[cleantable$ContainerType== input$containersinput,]
    }
  
    if (input$alarminput != "No") {
      cleantable <- cleantable[cleantable$Alarm == TRUE,]
    }
    cleantable <- cleantable[cleantable$TempMeanValue >= input$temperatureinput,]
    cleantable <- cleantable[cleantable$ShipDate %in% input$dateRange]
    return(cleantable)
  })

  output$ziptable <- DT::renderDataTable({
    dt_filter()
           #Score >= input$minScore,
           #Score <= input$maxScore,
           # cleantable$input$dateRange,
           # is.null(input$productinput) | Product %in% input$productinput,
           # is.null(input$carriersinput) | cleantable$Carrier %in% input$carriersinput,
           # is.null(input$containersinput) | cleantable$ContainerType %in% input$containersinput,
           # is.null(input$alarm) | cleantable$Alarm %in% input$alarminput
    
    # #mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, cleantable)
    # # 
    DT::datatable(cleantable, rownames = FALSE,options = list(scrollX=TRUE,searchHighlight = TRUE), filter = 'top')
  })
}
