#' ---
#' title: California Biodiversity Trends Engine
#' ---
#'
#' # Server setup
#' ## Load libraries
library(shiny)
library(shinydashboard)
library(shinybusy)
library(shinyBS)
library(leaflet)
library(leaflet.extras)
library(purrr)
library(shinyjs)
library(sf)
library(shinycssloaders)
library(dygraphs)
library(plotly)
library(readr)
library(DT)
library(shinyWidgets)
library(bslib)
#'
#' ## Load data
#' ### Areas of interest polygons
aoi_polygons <- readRDS("data/areas_of_interest_polygons.rds")
#' ## Add custom Javascript
scr <- tags$script(HTML(
  "
Shiny.addCustomMessageHandler(
  'removeleaflet',
  function(x){
    console.log('deleting',x)
    // get leaflet map
    var map = HTMLWidgets.find('#' + x.elid).getMap();
    // remove
    map.removeLayer(map._layers[x.layerid])
  })
"
))
tooltip_js <- "
$(function () {
  $('[data-toggle=tooltip]').tooltip()
})
"
#'
#'
#'
#' # User Interface
navbarPage(title = HTML("<span style='float: left; display: inline-block; padding-left: 15px;'><img src = 'cas_logo.png', height = '45'></span><span style='display: inline-block; padding: 12px 5px 5px 15px;'><p style = 'font-size: 22px; font-family: Archivo !important; color: #8EC852 !important;'><strong>California Biodiversity Trends Engine</strong></p></span>"), 
           windowTitle = "California Biodiversity Trends Engine", 
           id="nav", theme = "style.css", collapsible = TRUE,
           
           tags$head(
             HTML("<link href='https://fonts.googleapis.com/css2?family=Roboto&display=swap' rel='stylesheet'>"),
             HTML("<link href='https://fonts.googleapis.com/css2?family=Archivo&display=swap' rel='stylesheet'>"),
             HTML("<meta name='viewport' content='width=device-width, initial-scale=1'>")
           ),
           
           div(class="outer",
               
               useShinyjs(),     ## Call to use shinyJS
               
               scr,
               
                        layout_sidebar(
                          sidebar = sidebar(
                            id = "place_sidebar",
                            width = "25em",
                            padding = "1em",
                            list(
                            div(strong(h3("Select conservation place of interest", style = "margin-top: 0px; padding-top: 0px; color: #347AB7;")), style = "margin-top: 0px; text-align: center;"),
                            div(strong(h4("Choose from dropdown menu or from the map")), style = "text-align: center;"),
                            selectizeInput(inputId = "select_map_aoi",
                                           label = "",
                                           choices = c("", unique(aoi_polygons$aoi_name)), 
                                           multiple = FALSE,
                                           width = "100%",
                            ), 
                            div(strong(h4("OR")), 
                              strong(h4("Upload polygon from local files")), 
                              style = "text-align: center;"),
                            fileInput(inputId = "load_aoi", 
                                      label = "",
                                      accept = c(".shp",
                                                 ".kml",
                                                 ".rds"
                                      ),
                                      multiple = FALSE, 
                                      width = "100%"
                            ),
                            div(strong(h4("OR")),
                              strong(h4("Paste polygon URL below"))
                              , style = "text-align: center;"),
                            textInput(inputId = "get_aoi_from_url", 
                                      label = "", 
                                      value = NULL, 
                                      placeholder = "e.g. https://services.arcgis.com/F7DSX1DSNSiWmOqh/arcgis/rest/services/jldp_boundary/FeatureServer/2", 
                                      width = "100%"),
                            br(),
                            br(),
                            actionButton(inputId = "aoi_go", label = "Explore place", icon = icon("close"), block = TRUE, class = "btn-primary btn-lg", width = "100%", style = "font-size: 14px !important;")
                            )
                          ),
                          tabsetPanel(id = "pages", type = "tabs", 
                                      
                                      tabPanel("DATA", 
                                               
                                               useShinyjs(),     ## Call to use shinyJS
                                               
                                               fluidRow(style = "padding: 0 5px 0 15px;",
                                                                        leafletOutput("main_map", height = "50vh"),
                                                                        add_busy_spinner(spin = "circle", color = "#1F417D", margins = c("40vh", "50vw"), height = "75px", width = "75px")
                                               ),
                                               shinyjs::hidden(
                                               absolutePanel(id = "time_plot_panel",
                                                             class = "panel panel-default",
                                                             top = "12vh", right = "3vw", left = "auto", bottom = "auto",
                                                             width = "30vw",
                                                             height = "43vh",
                                                             style = "padding: 10px 10px 30px 10px; border: none; box-shadow: none !important; border-bottom: none; border-color: transparent; background-color: rgba(255, 255, 255, 0.7); z-index: 1000 !important; overflow-y: hidden !important; overflow-x: hidden;",
                                                             dygraphOutput("time_plot", height = "40vh")
                                               )
                                               ),
                                               
                                               shinyjs::hidden(
                                               div(id = "data_output",
                                               fluidRow(style = "padding: 0.5em 0 0 2em;",
                                                                        column(width = 6, style = "padding: 0 5px 10px 0;",
                                                                               htmlOutput("metric_total"), type = 7, proxy.height = "0px",
                                                                               tabsetPanel(id = "metric_switch", type = "pills",
                                                                                           tabPanel("Records", height = "100%"),
                                                                                           tabPanel("Species", height = "100%"),
                                                                                           tabPanel("Observers", height = "100%"),
                                                                                           tabPanel("Visits", height = "100%")
                                                                               )
                                                                        ),
                                                                        column(width = 6, 
                                                                               div(style = "position: relative; float: right; padding-top: 28px;",
                                                                                   materialSwitch(inputId = "redo_search", label = "Limit output \n to map bounds", width = "100%", right = TRUE, value = FALSE)
                                                                               ),
                                                                               div(style = "width: 11vw; position: relative; float: right; padding-top: 15px;",
                                                                                   checkboxInput("deselect_all", label = "Deselect all", value = FALSE)
                                                                               ),
                                                                               div(style = "width: 5vw; position: relative; float: right; padding-top: 15px;",
                                                                                      checkboxInput("select_all", label = "Select all", value = FALSE)
                                                                               )
                                                        )
                                               ),
                                               fluidRow(style = "padding: 0; overflow-x: scroll; scrollbar-color: #C7C7C7 rgba(255, 255, 255, 1) !important; min-height: 80vh;",
                                                        layout_sidebar(
                                                          sidebar = sidebar(
                                                            position = "right",
                                                            width = "17vw",
                                                            height = "80vh",
                                                            padding = "1em",
                                                            list(
                                                            br(),
                                                            selectizeInput(inputId = "select_species", label = "Select species", choices = NULL, multiple = TRUE),
                                                            h5("Select taxon"),
                                                            plotlyOutput("taxa_donut", width = "110%", height = "250px")
                                                            )
                                                          ), 
                                                          div(style = "min-height: 80vh;",
                                                            DT::dataTableOutput("records_table", height = "80vh")
                                                          )
                                                        )
                                               )
                                               
                                                 )
                                               )
                                      ),
                                      
                                      tabPanel("TRENDS", height = "100%",
                                               
                                               add_busy_spinner(spin = "circle", color = "#1F417D", margins = c("40vh", "50vw"), height = "75px", width = "75px"),
                                               
                                               fluidRow(style = "padding: 0 10px 0 30px;",
                                                        column(width = 6, style = "width: 50vw; padding-right: 30px;",
                                                               fluidRow(style = "overflow-x: scroll; scrollbar-color: #C7C7C7 rgba(255, 255, 255, 1) !important; ",
                                                                        fluidRow(
                                                                        column(width = 7,
                                                                        tabsetPanel(id = "species_trends_tabs", type = "pills",
                                                                                    tabPanel("At a Glance", height = "100%"),
                                                                                    tabPanel("Decreasing Species", height = "100%"),
                                                                                    tabPanel("Increasing Species", height = "100%"),
                                                                                    tabPanel("All Species", height = "100%")
                                                                        )
                                                                        ),
                                                                        column(width = 5, style = "padding-top: 0; margin-top: 0;",
                                                                               span(selectizeInput(inputId = "select_species_trend", label = "", choices = NULL, multiple = FALSE, options = list(placeholder = "Search for species")), style = "margin-top: -5px; float: right;")
                                                                               )
                                                                        ),
                                                                        DT::dataTableOutput("trends_table", width = "100%")
                                                               )
                                                        ),
                                                        column(width = 6, style = "width: 45vw; padding-top: 3em;",
                                                               fluidRow(style = "padding-right: 4px;",         
                                                                        plotlyOutput("species_trends_output", height = "70vh")
                                                               )
                                                        )
                                               ),
                                               fluidRow(style = "padding: 10px 30px 10px 14px;",
                                                        column(width = 6, style = "width: 65vw; padding-right: 14px;",
                                                               leafletOutput("trends_map", height = "50vh")
                                                        ),
                                                        column(width = 6, style = "width: 30vw;",
                                                               leafletOutput("association_map", height = "50vh")
                                                        )
                                               )
                                      ),
                                      tabPanel("INSIGHTS", height = "100%",
                                               fluidRow(style = "padding-left: 30px;",
                                                 h4("In the works...")
                                               )
                                      )
                        )
               ),
               absolutePanel(id = "elimia",
                             class = "panel panel-default",
                             top = 10, right = 0, left = "auto", bottom = "auto",
                             width = "3em",
                             height = "5em",
                             style = "padding: 0; border: none; box-shadow: none !important; border-bottom: none; border-color: transparent; background-color: transparent; z-index: 1000 !important; overflow-y: hidden !important; overflow-x: hidden;",
                             HTML("
                         <a href='https://elimia.io' target='_blank'><img src = 'elimia-logo-snail.png', height = '45%'></a>
                              ")
               )
           )
)
                          