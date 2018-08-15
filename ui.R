library(shinydashboard)
library(leaflet)
vars2 <- c(
  "All",  "Product", "Container", "Carrier","Temperature Range", "Avg. Temperature", "Min Temperature","Max Temperature"
)

products <- unique(uniqshipments$shipmentitemproductdesc)
products <- products[order(products)]
origins <- unique(uniqshipments$shipmentorigindesc)
origins <- origins[order(origins)]
destinations <- unique(uniqshipments$shipmentdestinationdesc)
destinations <- destinations[order(destinations)]
productlist <- data.frame(products="Product")
carriers <- unique(uniqshipments$shipmentcarrierdesc)
carriers <- carriers[order(carriers)]
containers <- unique(uniqshipments$shipmentcontainerdesc)
containers <- containers[order(containers)]
alpharoutes <- unique(uniqshipments$route)
alpharoutes <- alpharoutes[order(alpharoutes)]
alphaorigins <- unique(uniqshipments$shipmentorigindesc)
alphaorigins <- alphaorigins[order(alphaorigins)]
alphadest <- unique(uniqshipments$shipmentdestinationdesc)
alphadest <- alphadest[order(alphadest)]

header <- dashboardHeader(title = "Shipping Department"
                          )

sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Maps", icon= icon("globe"), tabName = "map")
))

body <- dashboardBody( 
tabItems(
  tabItem(tabName = "map",
          
  fluidRow(
  box(collapsible = TRUE, width = 12, 
      fluidRow(
      column(3,selectInput("viewchange", "View Map By:", vars2, selected = "All", multiple=FALSE,
                  width = NULL)),
      column(3, checkboxInput("exclusions", "Exclude False Alarms"))
      ),
      fluidRow(
      conditionalPanel("input.viewchange == 'Max Temperature'",
                       column(3,sliderInput("temperature", "Temperature", min(as.integer(temp2_prod$tempmaxvalue)),max(as.integer(temp2_prod$tempmaxvalue)), min(as.integer(temp2_prod$tempmaxvalue)), step=1, ticks=TRUE))),
      conditionalPanel("input.viewchange == 'Min Temperature'",
                       column(3,sliderInput("temperature", "Temperature", min(as.integer(temp2_prod$tempminvalue)),max(as.integer(temp2_prod$tempminvalue)), min(as.integer(temp2_prod$tempminvalue)), step=1, ticks=TRUE))),
      conditionalPanel("input.viewchange == 'Avg. Temperature'",
                       column(3,sliderInput("temperature", "Temperature", min(as.integer(temp2_prod$tempmeanvalue)),max(as.integer(temp2_prod$tempmeanvalue)), min(as.integer(temp2_prod$tempmeanvalue)), step=1, ticks=TRUE)))),
      fluidRow(
      column(3,sliderInput("shipments", "Total Shipments", 1, max(cleanroutesalarms$freq), 1, step=1, ticks=TRUE)),
      column(3,sliderInput("alarmpct", "Alarm Percentage", 0, 100, 0, step=1, ticks=TRUE)),

      conditionalPanel("input.viewchange != 'All' && input.viewchange != 'Carrier' && input.viewchange != 'Container'",
                       column(3,selectInput("prod", "Product", c("All Products",as.character(products)), selected = "All Products")))),
      fluidRow(
      column(3,selectInput("origin", "Origin", c("All Origins",as.character(origins)), selected = "All Origins")),
      column(3,selectInput("destination", "Destination", c("All Destinations", as.character(destinations)), selected = "All Destinations")),
      conditionalPanel("input.viewchange == 'Carrier'",
                       column(3,selectInput("carrier", "Carrier", c("All Carriers", as.character(carriers)), selected = "All Carriers"))),
      conditionalPanel("input.viewchange == 'Container'",
                       column(3,selectInput("color", "Container", c("All Containers", as.character(containers)), selected = "All Containers"))))
      
  )),
  fluidRow(
  box(width = 12,        
  div(class="outer",
      
      tags$head(
        # Include our custom CSS
        includeCSS("styles.css")
        #includeScript("gomap.js")
      ),
      
      leafletOutput("map", width="100%", height="100%"))))),
  tabItem(tabName = "report",
          fluidRow(
            box(collapsible = TRUE, width = 12,
                fluidRow(
                  
                  column(2,
                         
                         selectInput("origininput", "Origin", c("All Origins"="", as.character(alphaorigins)), multiple=FALSE)
                  ),
                  column(2,
                         
                         selectInput("destinationinput", "Destination", c("All Destinations"="", as.character(alphadest)), multiple=FALSE)
                  ),
                  column(2,
                         selectInput("productinput", "Products", c("All Products"="", as.character(products)), multiple=FALSE)
                  ) 
                  
                ),
                fluidRow(
                  column(2,
                         conditionalPanel("carriersinput",
                                          selectInput("carriersinput", "Carriers", c("All Carriers"="", as.character(carriers)), multiple=FALSE)
                         )
                  ),
                  column(2,
                         conditionalPanel("containersinput",
                                          selectInput("containersinput", "Containers", c("All Containers"="", as.character(containers)), multiple=FALSE)
                         )
                  ),
                  column(2,
                         selectInput("alarminput", "Alarm Triggered", c("Yes", "No"), selected = "No", multiple = FALSE)
                  ),
                  column(2,
                         sliderInput("temperatureinput", "Average Temperature", min=-50, max=100, value=0)
                  )
                  
                ),
                fluidRow(
                  column(3,
                         dateRangeInput('dateRange',
                                        label = 'Date range',
                                        start = Sys.Date() - 366, end = Sys.Date()
                         )
                  )
                ),
                actionButton("button", "Download as CSV"),
                hr()
                
                
                )),
          fluidRow(
            box(width = 12,
                DT::dataTableOutput("ziptable")
              
              
              
            )
            
          )
  
  
  )
                       ))

dashboardPage(header, sidebar, body, title = "Shipping Department")