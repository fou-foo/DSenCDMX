library(dplyr)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(reshape2)
library(plotly)
load(file='Data/estados_parser.rdata')
names(estados.parser) <- gsub(" ", ".", names(estados.parser))
shinyUI(
    fluidPage(theme = "style.css",
              div(style = "padding: 1px 0px; width: '100%'",
              titlePanel( title = "", windowTitle = "COVID-19 Mexico")),
              navbarPage(
                  # Application title.
                  title = div(span(img(src = "cimat.png"), "COVID-19: Mexico",
                                   style = "position: relative; top: 50%; transform: translateY(-50%);")),
                  # Demographics.
            tabPanel( "Simulacion ",
                      # One tab for each plot/table.
                      tabsetPanel(type = "tabs",
                      # Circle-packing plot of ethnicity and gender.
                        tabPanel( "Modelo 4 ",
                            # Sidebar panel for controls.
                            sidebarPanel(
                            tags$p(span("Large graphs (e.g., of the full dataset) may take a few seconds to render.  The first graph may take up to two minutes if the app is retrieving new data from Rebrickable.", style = "color:red")),
                            tags$p(HTML("<b>Hover</b> to see the part name.")),
                            tags$p(HTML("Each circle represents a <b>unique minifigure or minidoll head</b>.")),
                            tags$p(HTML("Area is proportional to the <b>number of pieces</b> across all sets.")),
                            tags$p(HTML("<b>\"Ethnicity\"</b> is the color of the piece.  Yes, it's silly.")),
                            tags$p(HTML("<b>Gender</b> is inferred from keywords in the part name (\"Male\", \"Female\", etc., plus references to facial hair).")),
                            tags$p("Some heads are not labeled male/female but contain the name of a character of known gender (e.g., \"Han Solo\").  Incorporating this information would require a hand-maintained list of character names and their genders; I haven't done this.") ) ),

                        column( width =3 ,
                            dropdownButton( size = 'default', tags$h3("Parametros"), 
                                sliderInput(inputId = 'days', label = 'Dias', value =60 , min = 30, max = 60, step = 30), # en realidad Lety me dijo 5.8 pero eso complica el rollo
                                sliderInput(inputId = 'sigma', label = 'Sigma', value = 0.22 , min = 0.18, max = 0.22, step = .02), # en realidad Lety me dijo 5.8 pero eso complica el rollo
                                sliderInput(inputId = 'gamma_s', label = 'Gamma_s', value = 0.95 , min = 0.55, max = .95, step = .20),
                                #sliderInput(inputId = 'gamma_a', label = 'Gamma_a', value = 0.75 , min = 0.55, max = .95, step = .20 ),
                                sliderInput(inputId = 'alpha', label = 'alpha', value = .5 , min = 0, max = 1, step = .5),
                                sliderInput(inputId = 'w', label = 'w', value = 1 , min = 0, max = 1, step = .5),
                            circle = TRUE, status = "danger", icon = icon("history"), width = "300px",
                            tooltip = tooltipOptions(title = "Cambia los parametros !") ),
                            verbatimTextOutput("beta")),
                        column(width = 4,
                               fluidRow( pickerInput( inputId = "estado", label = "Select/deselect all + format selected",
                                         choices = c('Todos', names(estados.parser)),options = list( `actions-box` = TRUE, size = 10,
                                                 `selected-text-format` = "count > 3"),
                                         multiple = TRUE)),
                    p(), # Main panel with plot.
                               fluidRow(plotlyOutput("ia_plot") %>% withSpinner() ),
                    fluidRow(plotlyOutput("is_plot") %>% withSpinner() ),
                    fluidRow(plotlyOutput("y_plot") %>% withSpinner() ) )) ) ,
                      # About and credits.
                      tabPanel( "About",type = "tabs", # Various tabs.
                          tabsetPanel(# General info.
                              tabPanel(
                                  "Overview",
                                  tags$h1("Scope"),
                                  tags$p(HTML("This collection of visualizations addresses the question, \"What is it like to live in the Lego world?\"  In other words, if you're Wyldstyle, what kinds of people do you meet?  How are they feeling?  What plants and animals do you find around you?")),
                                  tags$p(HTML("Think of each theme as an island on the Lego planet.  Each visualization can be faceted by theme, so you can compare fashion, flora and fauna, etc. across themes.")),
                                  tags$h1("Approach"),
                                  tags$p(HTML("Parts are labeled and categorized using three main sources of information:")),
                                  tags$ul(
                                      tags$li(HTML("The part category (e.g., \"Minifig Heads\" or \"Plants and Animals\") specified in the database")),
                                      tags$li(HTML("The hexadecimal part color specified in the database")),
                                      tags$li(HTML("Keywords in the part name"))   ),
                                  tags$p(HTML("The keywords that map part names to categories involve more-or-less hand-curated lists and some <i>very</i> basic text processing (mostly regular expressions).  The process is <b>not 100% accurate</b>; there are plenty of false positives and false negatives.  But it's good enough for a first pass.")),
                                  tags$h1("GitHub"),
                                  tags$p(HTML("Source code is available at <a href=\"https://github.com/kaplanas/Shiny-Lego\">https://github.com/kaplanas/Shiny-Lego</a>."))
                              ))
                          )
                  )
              )
)