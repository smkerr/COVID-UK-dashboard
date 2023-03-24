### STEP #2: SHINY DASHBOARD

# load packages
library(tidyverse)
library(magrittr)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(png)
library(highcharter)
library(sf)

# load data
covid_ltla <- read_rds("covid_ltla.RDS")

# dashboard title
title <- tags$a(href = 'https://github.com/intro-to-data-science-21/data-project-covid_dashboard_uk',
                icon("shield-virus"), 'Covid Dashboard UK')

# list of age groups
m_age <- c(as.character(sort(unique(covid_ltla$age))))
# list of dates
m_date <- c(as.character(sort(unique(covid_ltla$date), decreasing = TRUE)))

# UI settings
ui <- dashboardPage(skin = 'black',
                    dashboardHeader(title = title), # add title 
                    dashboardSidebar(width = 275,
                                     
                                     # dynamically-generated user panel
                                     uiOutput("userpanel"),
                                     
                                     # side bar menu
                                     sidebarMenu(style = "position: scroll; overflow: hidden;",id = "sidebarmenu",
                                                 menuItem("COVID Dashboard UK", tabName = "cso", icon = icon("desktop")),
                                                 conditionalPanel(
                                                     "input.sidebarmenu === 'cso'",
                                                     
                                                     # add filters
                                                     useShinyjs(),
                                                     div(id = "form",
                                                         tags$hr(),
                                                         selectInput("i2_age", "Age Group", choices = m_age,bookmarkButton(id = "bookmark1")),
                                                         selectInput("i2_date", "Date", choices = m_date,bookmarkButton(id = "bookmark2")),
                                                         br(),
                                                         br(),
                                                         br(),
                                                         br(),
                                                         column(6, offset = 6,height = 100, style = 'padding100px;',
                                                                actionButton("reset_button", "Reset",icon = icon("redo")))
                                                         )
                                                 )
                                     )
                    ),
                    dashboardBody(
                        tags$style(type = "text/css", "#map1 {height: calc(100vh - 80px) !important;}"),
                        leafletOutput("map1")
                    )
)

# server settings
server <- function(input, output) {
    addClass(selector = "body", class = "sidebar-collapse")
    
    # exclude buttons from themselves being bookmarked
    setBookmarkExclude(c("bookmark1", "bookmark2"))
    
    # trigger bookmarking with either button
    observeEvent(input$bookmark1, {
        session$doBookmark()
    })
    observeEvent(input$bookmark2, {
        session$doBookmark()
    })
    
    js_click_line <- JS("function(event) {Shiny.onInputChange('line_clicked', [event.point.category]);}")
    
    observeEvent(input$reset_button, { # reset button
        reset("form")
    })
    
    id <- NULL
    observeEvent(input$reset_button, {
        id <<- showNotification(
            paste("Filters are Reset"),
            duration = 5, 
            type = "message"
        )
    })
    
    # data
    filt_iemap1 <- reactive({
        w <- covid_ltla %>% filter(age == input$i2_age & date == input$i2_date)
        return(w)
    })
    
    # maps
    mappalette <- reactive({
        bins <- c(0, 10, 50, 100, 200, 400, 800, Inf)
        colorBin("Reds", bins = bins)
    })
    
    # popup
    mappopup <- reactive({ 
        paste(sep = "<br/>",
              "<b>Lower Tier Local Authority: </b>",filt_iemap1()$areaName,
              "<b>Cases per 100,000: </b>",filt_iemap1()$rollingRate)
    })

    # labels
    labels <- reactive({ 
        sprintf(
            "<strong>%s</strong><br/>%g people / 100,000",
            filt_iemap1()$areaName, filt_iemap1()$rollingRate) %>% map(htmltools::HTML)  
    })
    
    output$map1 <- renderLeaflet({
        
        leaflet(filt_iemap1()) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView(lng = -1.65, lat = 52.95, zoom = 6)
        
    }) 
    
    # render Leaflet
    observe({
        pal1 <- mappalette()
        
        leafletProxy("map1", data = filt_iemap1()) %>%
            addPolygons(label = labels(),
                        stroke = TRUE,
                        weight = 0.75,
                        smoothFactor = 0.2,
                        fillOpacity = .75,
                        popup = mappopup(),
                        color = ~pal1(filt_iemap1()$rollingRate),
                        highlightOptions = highlightOptions(weight = 4,
                                                            color = "gray",
                                                            opacity = 1,
                                                            fillOpacity = 1,
                                                            bringToFront = TRUE)) %>%
            addMiniMap(position = "bottomleft", width = 150, height = 150,
                       collapsedWidth = 19, collapsedHeight = 19, zoomLevelOffset = -5,
                       zoomLevelFixed = FALSE, centerFixed = FALSE, zoomAnimation = TRUE,
                       toggleDisplay = TRUE, autoToggleDisplay = TRUE, minimized = TRUE,
                       aimingRectOptions = list(color = "#ff7800", weight = 1, clickable = TRUE),
                       shadowRectOptions = list(color = "#000000", weight = 1, clickable = TRUE,
                                                opacity = 0, fillOpacity = 0), 
                       strings = list(hideText = "Hide MiniMap", showText = "Show MiniMap"),
                       tiles = (providers$OpenStreetMap), mapOptions = list()) %>%
            addLegend("bottomright", pal = mappalette(), values = ~filt_iemap1()$rollingRate,
                      title = "Cases per 100k People",
                      opacity = 1)
    })
}

shinyApp(ui, server)
