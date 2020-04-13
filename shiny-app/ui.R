library(shiny)
library(gt)
source("helper/helper.R", local = TRUE)
#from helper
include = get_include()

shinyUI(
    navbarPage("Sea Level Rise",
                   tabPanel("Global Sea Level",
                          tags$div(
                             titlePanel("Global Sea Level Rise"), 
                            style="text-align:center;"),
                            fluidPage(
                              fluidRow(
                                column(6,
                                       htmlTemplate("html/index_1.html"), 
                                       htmlTemplate("html/index_2.html")), 
                                column(6, img(src = 'gmsl.gif', height = '500px', width = '500px'))
                              )
                            )
                            ),
                   tabPanel("Geo-Spatial",
                            tags$div(
                            titlePanel("Net Migration resulting from Sea Level Rise in US. Counties by 2100")
                            ,style="text-align:center;"),
                            fluidPage( 
                                      fluidRow(
                                        column(6,
                                                 htmlTemplate("html/geo_1.html")
                                        ), 
                                        column(6,
                                               htmlTemplate("html/geo_2.html")
                                        )
                                      ),
                              fluidRow(
                                column(4,
                                         gt_output("top_pos")
                                ), 
                                column(4,
                                       tags$div(
                                       selectInput("variable",
                                                   "Select a State:",
                                                   include)
                                       ,style="text-align:center;"),
                                       plotOutput("state")
                                ), 
                                column(4, 
                                       gt_output("top_neg")
                                       )
                                       
                              )
                            )
                            ),
                   tabPanel("Adjusted / Regressions",
                            tags$div(
                              titlePanel("Relationships between Population and Income"),
                               style="text-align:center;"),
                            fluidPage(
                     fluidRow(
                       column(6,
                              htmlTemplate("html/charts_1.html"), 
                              radioButtons("adjust", label = NULL,
                                           c("Non-Adjusted" = "non",
                                             "Adjusted" = "adj"), 
                                           inline = TRUE)
                            ), 
                       column(6, 
                              htmlTemplate("html/charts_2.html")
                              )
                     ),
                     fluidRow(column(6, 
                                     tabsetPanel(type = "tabs",
                                                 tabPanel("Population", plotOutput("pop")),
                                                 tabPanel("Table", tableOutput("pop_table")) 
                                            )
                                     ),
                              column(6,
                               tabsetPanel(type = "tabs",
                                           tabPanel("Population", plotOutput("Agg")),
                                           tabPanel("Median Income", plotOutput("Quart"))                              )
                            )
                        )
                     ) 
                   ), 
                   tabPanel("About",
                            tags$div(
                              titlePanel("About the Data / About the Dude"),
                              style="text-align:center;"),
                            fluidRow(
                              column(2),
                              column(8,
                            htmlTemplate("html/about.html")
                              )
                              )
                            ), 
                   fluid = TRUE, 
                   theme = "style.css"
               )
  )
