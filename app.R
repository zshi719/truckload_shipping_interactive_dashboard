# load packages
library(shinydashboard)
library(dashboardthemes)
library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)
library(leaflet)
library(sf)
library(tidycensus)
library(rnaturalearth)
library(geojsonio)
library(conflicted)

conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflict_prefer("box", "shinydashboard")

# setwd("~/Desktop/Final")
tl <- read_excel("data/TL2019.xlsx")

# convert data from wide to long format at state level, i.e. combine drop & pick
tl_state_all <- tl %>%
  select(`Pick State`, `Ship Month`, `Shipment Count`) %>%
  rename(State = `Pick State`) %>%
  rbind(
    tl %>%
      select(`Drop State`, `Ship Month`, `Shipment Count`) %>%
      rename(State = `Drop State`)
  )

# convert data from wide to long format at city level, i.e. combine drop & pick
tl_city_all <- tl %>%
  select(`PickCity`, `Ship Month`, `Shipment Count`) %>%
  rename(City = `PickCity`) %>%
  rbind(tl %>%
          select(`Drop City`, `Ship Month`, `Shipment Count`) %>%
          rename(City = `Drop City`))

# generate states level data for each plot
# 1
states1 <-
  geojsonio::geojson_read("data/us-states.geojson", what = "sp")
states1$state_abb <- state.abb[match(states1@data$name, state.name)]
states1@data <- tl_state_all %>%
  group_by(State) %>%
  summarize(shipment = sum(`Shipment Count`)) %>%
  right_join(states1@data,
             by = c("State" = "state_abb"),
             multiple = "all")

# 2
states2 <-
  geojsonio::geojson_read("data/us-states.geojson", what = "sp")
states2$state_abb <- state.abb[match(states2@data$name, state.name)]
states2@data <- tl_state_all %>%
  group_by(State, `Ship Month`) %>%
  summarize(shipment = sum(`Shipment Count`)) %>%
  right_join(states2@data,
             by = c("State" = "state_abb"),
             multiple = "all") %>%
  distinct()


# generate Chicago data
states3 <-
  geojsonio::geojson_read("data/us-states.geojson", what = "sp")
states3$state_abb <- state.abb[match(states3@data$name, state.name)]
states3@data <- tl %>%
  filter(`Pick State` == "IL") %>%
  group_by(`Drop State`) %>%
  summarize(shipment = sum(`Shipment Count`)) %>%
  right_join(states3@data,
             by = c("Drop State" = "state_abb"),
             multiple = "all")


# generate city level data for each plot
cities <-
  read_sf("data/ne_10m_populated_places/ne_10m_populated_places.shp",
          layer = "ne_10m_populated_places") %>%
  filter(SOV0NAME == "United States", ADM0_A3 == "USA") %>%
  mutate(CITY = toupper(NAME)) %>%
  select(CITY, LONGITUDE, LATITUDE, POP_MAX)

# cities data, months combined
cities1 <- cities %>% inner_join(tl_city_all %>%
                                   group_by(City) %>%
                                   summarize(shipment = sum(`Shipment Count`)),
                                 by = c("CITY" = "City"))

# cities data, months separated
cities2 <- cities %>% inner_join(
  tl_city_all %>%
    group_by(City, `Ship Month`) %>%
    summarize(shipment = sum(`Shipment Count`)),
  by = c("CITY" = "City"),
  multiple = "all"
)


# Define UI for application
ui <-
  dashboardPage(
    dashboardHeader(title = "Truckload Freight Shipping Dashboard", titleWidth = 350),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Overview", tabName = "tab0", icon = icon("truck")),
        menuItem(
          "Coverage",
          tabName = "tab1",
          icon = icon("location-pin")
        ),
        menuItem("Month", tabName = "tab2", icon = icon("chart-line")),
        menuItem("Chicago", tabName = "tab3", icon = icon("city")),
        menuItem("Info", tabName = "tab4", icon = icon("circle-info"))
      )
    ),
    dashboardBody(
      shinyDashboardThemes(theme = "onenote"),
      tabItems(
        tabItem(tabName = "tab0", box(
          h3("Welcome to Truckload Shipping Dashboard for 2019 Winter!"),
          h3(),
          h4(
            "This easy-to-use dashboard provides direct insights into the overall performance
            of truckload shipping operations during winter 2019 and helps you make informed decisions
            to increase supply chain efficiency and optimize supply chain management."
          ),
          width = 10,
          status = "primary"
        )),
        tabItem(tabName = "tab1", fluidRow(box(
          h3("State-and-city-wide transportation and logistics services"),
          width = 10
        )), fluidRow(
          box(
            title = div(
              h4('Truckload Shipping Service Range at State Level', style = "margin: 0;"),
              h4(''),
              h5(
                'Direct Transportation to 49 States, including Alaska and Hawaii',
                align = "center"
              )
            ),
            status = "primary",
            width = 10,
            leafletOutput("map_state1")
          )
        ), fluidRow(
          box(
            title = div(
              h4("Truckload Shipping Service Range at City Level", style = "margin: 0;"),
              h4(''),
              h5('Seamless Solutions across Major Cities')
            ),
            status = "primary",
            width = 10,
            leafletOutput("map_city1"),
            footer = "Note: Purple dots represent the cities covered,
            where the radius is proportional to the shipment amount from and to the city."
          )
        )),
        tabItem(
          tabName = "tab2",
          fluidRow(box(
            h3(
              "Continuous operation and consistent shipping service during winter"
            ),
            width = 10
          )),
          fluidRow(column(
            2, selectInput("month", "Month", choices = list(
              "November" = 11, "December" = 12
            ))
          ), column(
            10,
            box(
              leafletOutput("map_state2"),
              status = "primary",
              width = 10,
              title = "Truckload Shipping at State Level by Month"
            )
          )),
          fluidRow(column(
            2, selectInput("month_city", "Month", choices = list(
              "November" = 11, "December" = 12
            ))
          ), column(
            10,
            box(
              leafletOutput("map_city2"),
              status = "primary",
              width = 10,
              title = "Truckload Shipping at City Level by Month",
              footer = "Note: Purple dots represent the cities covered,
              where the radius is proportional to the shipment amount from and to the city."
            )
          ))
        ),
        tabItem(tabName = "tab3", fluidRow(
          box(h3("From Chicago, and beyond"), width = 10),
          box(
            title = "Destination and Shipment Count of Freight Flow out of Chicago",
            status = "primary",
            width = 10,
            leafletOutput("chicago"),
            footer = "Note: The star represents Chicago, the central hub of the freight company."
          )
          
        )),
        tabItem(tabName = "tab4", fluidRow(box(
          h3("Additional Information", style = "margin: 0;"), width = 10
        )), fluidRow(
          box(
            title = "Core concepts and insights",
            status = "primary",
            width = 12,
            tags$div(
              "This easy-to-use dashboard allows users to interact with the logistics data of the truckload freight company. It shows the coverage at the city and state levels, shipping volume in November and December across cities and states, and the total flow volume out of the hub of the freight company, Chicago. The dashboard provides direct and actionable insights into the overall performance of truckload shipping operations and allows professionals to make more informed decisions and optimize supply chain management. In addition, it reveals the strengths and weaknesses of the shipping service provided by the company and points out the directions for improvement.",
              tags$br(),
              tags$br(),
              "A. Insights into areas of improvement - WHAT'S GOING ON IN WYOMING",
              tags$br(),
              "For example, the visualization tells us Wyoming is the only state where no shipping or dropping occurs. It seems to be the only \"outcast\" when the company covers faraway states like Alaska and Hawaii. If this anomaly is not due to any errors during the data collection process, the decision-makers might ask: What's going on in Wyoming? What is the reason for the lack of shipping activity in both months? Is this because of the company's lack of freight centers in the region, or could Wyoming's geographical features make freight shipping challenging or unreliable? Or is the company outcompeted by some competitors in the region? Do they need to increase the number of shipping centers or provide alternative solutions to their transportation?",
              tags$br(),
              tags$br(),
              "B. Insights into the strength of the supply chain - RESILIENCE, RELIABILITY, AND POPULARITY",
              tags$br(),
              "- Resilience and Reliability: From the visualization of the truckload shipping service at the state and city levels by month, we can see ongoing shipping activities in November and December, the coldest time of the year. Consistency in quality and service range is essential to the freight company's success in the long run.",
              tags$br(),
              "- Popularity in distant states: there are frequent shipping activities going on in Alaska and Hawaii."
            )
          ),
          box(
            title = "Rationales for Widget and Color Tone Selection",
            status = "primary",
            width = 12,
            tags$div(
              "- On the tab 'Month', the \"select box\" widget allows users to select the month from the list, visualize the information for a given month, and compare and contrast shipping patterns in November and December, respectively.",
              tags$br(),
              tags$br(),
              "- If we have access to data that spans more months or years, we would make a widget that allows users to select 'date input' or 'date range' to visualize more steady patterns.",
              tags$br(),
              tags$br(),
              "- On our maps, the \"Stamen Toner\" theme of the leaflet is used. This theme comes with the abbreviation of states and allows users to obtain information about the states at a glance.",
              tags$br(),
              tags$br(),
              "- Color palettes including  \"viridis\",  \"plasma\",  and \"RdYlBu\" are chosen for their representation and smooth transition for qquantitativevariables. In addition, they work well with the Stomen Toner theme."
            )
          )
        ))
      )
    )
  )

# Define server logic required
server <- function(input, output) {
  # Tab 1: range of services, state & city
  # define color palette for states
  pal <- colorNumeric(palette = "plasma", domain = states1$shipment)
  output$map_state1 <- renderLeaflet({
    leaflet(states1) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles(providers$Stamen.Toner) %>%
      addPolygons(
        fillColor = ~ pal(shipment),
        stroke = TRUE,
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~ shipment,
        opacity = 0.7,
        title = "Shipment",
        position = "bottomright"
      )
  })
  
  # define color palette for cities
  pal_city1 <-
    colorNumeric(palette = "viridis", domain = cities$POP_MAX)
  output$map_city1 <- renderLeaflet({
    cities1 %>%
      leaflet() %>%
      setView(lng = -93.26801,
              lat = 44.29049,
              zoom = 4) %>%
      addTiles() %>%
      addCircles(
        lng = ~ LONGITUDE,
        lat = ~ LATITUDE,
        weight = 1,
        color = ~ pal_city1(POP_MAX),
        radius = ~ shipment * 30,
        popup = ~ CITY
      )
    
  })
  
  # Tab 2: shipment count in different shipping months
  
  map_filter_month <- reactive({
    # filter data according to user's choice of month
    states2@data %>% filter(`Ship Month` == input$month)
  })
  
  output$map_state2 <-
    renderLeaflet({
      states2@data <- map_filter_month()
      states2 %>%
        leaflet() %>%
        setView(-96, 37.8, 4) %>%
        addProviderTiles(providers$Stamen.Toner) %>%
        addPolygons(
          fillColor = ~ pal(shipment),
          stroke = TRUE,
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE
          )
        ) %>%
        addLegend(
          pal = pal,
          values = ~ shipment,
          opacity = 0.7,
          title = "Shipment",
          position = "bottomright"
        )
    })
  
  map_filter_month2 <-
    reactive({
      cities2 %>% filter(`Ship Month` == input$month_city)
    })
  
  pal_city2 <-
    colorNumeric(palette = "viridis", domain = cities2$POP_MAX)
  
  output$map_city2 <- renderLeaflet({
    cities2 <- map_filter_month2()
    cities2 %>%
      leaflet() %>%
      setView(lng = -93.26801,
              lat = 44.29049,
              zoom = 4) %>%
      addTiles() %>%
      addCircles(
        lng = ~ LONGITUDE,
        lat = ~ LATITUDE,
        weight = 1,
        color = ~ pal_city2(POP_MAX),
        radius = ~ shipment * 30,
        popup = ~ CITY
      )
  })
  
  
  # Tab 3
  pal3 <-
    colorNumeric(palette = "RdYlBu", domain = states3$shipment)
  output$chicago <- renderLeaflet({
    leaflet(states3) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles(providers$Stamen.TonerLite) %>%
      addPolygons(
        fillColor = ~ pal3(shipment),
        stroke = TRUE,
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "grey",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = pal3,
        values = ~ shipment,
        opacity = 0.7,
        title = "Shipment",
        position = "bottomright"
      ) %>%
      addMarkers(
        data = cities %>% filter(CITY == "CHICAGO"),
        lng = ~ LONGITUDE,
        lat = ~ LATITUDE,
        icon = list(iconUrl = 'https://icons.iconarchive.com/icons/goodstuff-no-nonsense/free-space/256/star-icon.png', iconSize = c(40, 40)),
        popup = "Chicago"
      )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)

