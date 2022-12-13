library(shiny)
library(shiny.router)
library(tidyverse)
library(shinythemes)
library(shinyjs)
library(leaflet)
library(htmlwidgets) #labels on interactive maps
library(tigris)     # geojoin - need
library(raster)
library(plyr)
library(dplyr)
library(plotly)
library(fresh) # fresh is used to create a custom bootstrap application with customized font

# Generates linebreaks when it is called. 
# N here refers to the amount of linebreaks
linebreaks <- function(n){HTML(strrep(br(), n))}

home_page <- div(
  tags$div(class="title-box",
           tags$div(class="information-box",
                    h2(id="title", "ASP",tags$sup(class="title",'3'),"IRE Children's Environmental Health Center"),
                    p(id="p-title", "Oregon State University's Advancing Science Practice, Programming and Policy in 
                      Research Translation for Children's Environment Health Center"))),
  tags$div(class="content",
           h3(class="title","Our Mission"),
           p(class="normal-p", "We are a dedicated team working together to improve 
                               children's health and wellbeing. Our efforts focus on
                               accelerating the adoption of evidence informed policies,
                               programs, and practices that can reduce harmful environmental
                               exposures where children live, play, and go to school."),
           tags$hr(),
           h3(class="center", "Projects")),
  div(class="flexbox-container",
      div(class="flexbox-item flexbox-item-1",
          div(class="photo-flexbox photo-flexbox-1"),
          h3(class="flexbox-title", 
             tags$a(class="flexbox-title-link",href=route_link("censusdata"), "Census Data")),
          p(class="flexbox-p",
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore 
                                     et dolore magna aliqua. Ut enim ad minim veniam."),
          tags$a(class="blue-link flex-link", href=route_link("censusdata"), "Read full information >>")),
      div(class="flexbox-item flexbox-item-2",
          div(class="photo-flexbox photo-flexbox-2"),
          h3(class="flexbox-title", 
             tags$a(class="flexbox-title-link",href=route_link("schooldistricts"), "Oregon School Districts")),
          p(class="flexbox-p",
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore 
                                     et dolore magna aliqua. Ut enim ad minim veniam."),
          tags$a(class="blue-link flex-link-2", href="", "Read full information >>")),
      div(class="flexbox-item flexbox-item-3",
          div(class="photo-flexbox photo-flexbox-3"),
          h3(class="flexbox-title", "Twitter Coding"),
          p(class="flexbox-p",
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore 
                                     et dolore magna aliqua. Ut enim ad minim veniam."),
          tags$a(class="blue-link flex-link", href="", "Read full information >>")),
      div(class="flexbox-item flexbox-item-4",
          div(class="photo-flexbox photo-flexbox-4"),
          h3(class="flexbox-title", "Holder"),
          p(class="flexbox-p",
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore 
                                     et dolore magna aliqua. Ut enim ad minim veniam."),
          tags$a(class="blue-link flex-link", href="", "Read full information >>")),
  )
)

censusdata <- div(
  tags$div(class="census-box",
           tags$div(class="information-box",
                    h2(id="title", "Oregon Census Data"))),
  tags$div(class="select-box",
           tags$div(class="center-select",
                    selectInput(inputId = "tab", 
                                label = "Choose a topic",
                                choices = c("Poverty Status", 
                                            "Health Insurance Coverage",
                                            "Population in Household by Age",
                                            "Resources"),
                                selected = "Poverty Status"))),
  conditionalPanel("input.tab == 'Poverty Status'",
                   tags$div(class="select-box",
                            div(class="input-middle",
                                selectInput("age", 
                                            label = "Choose a variable",
                                            choices = c("Under 5 years", 
                                                        "5 to 17 years",
                                                        "Under 18 years",
                                                        "Related children of householder under 18 years"),
                                            selected = "Under 5 years"))
                   ),
                   column(12, 
                          tags$div(class="data-content",
                                   linebreaks(2),
                          )),
                   column(12,
                          tags$div(class="center-plotly",
                                   plotlyOutput("povertyhist"))),
                   div(class="census-flex-container",
                       div(class="census-item census-item-1",
                           tags$img(src = "poverty_left_bottom.jpg", height = 170, class="img-topic"),
                           tags$div(class="inside-textbox",
                                    tags$h3(class="topic-title",
                                            "Poverty Status, Oregonian residents, 2021-2022"),
                                    tags$p(class="topic-p",
                                           "Map data are displayed based by county. Click on the desired area to see more information.
                                                    Poverty status is done by comparing a person’s or family’s income to a set poverty treshold 
                                                    or minimum amount of income needed to cover basic needs. The U.S. Census Bureau is the government 
                                                    agency in charge of measuring poverty. To do so, it uses two main measures, the official poverty 
                                                    measure and the Supplemental Poverty Measure."))),
                       div(class="census-item census-item-2",
                           tags$div(class="inside-textbox",
                                    tags$h3(class="topic-title",
                                            textOutput("poverty_variableTitle")),
                                    tags$p(class="topic-p",
                                           textOutput("poverty_variable")
                                    ))),
                   ),
                   column(12,
                          tags$div(class="source-list",
                                   tags$p(class="s-list",
                                          tags$i(class="fa fa-thin fa-globe fa-1x"),
                                          "Source",
                                          tags$hr(class="source"),
                                          tags$p(class="s-list", "Maps and histograms are data from the",
                                                 tags$a(class="blue-link", href="", "2020 US Census"),
                                                 "Line graphs show changes in ",
                                                 tags$a(class="blue-link", href="", "US Census over time.")))
                          ),
                   )
  ),
  conditionalPanel("input.tab == 'Health Insurance Coverage'",
                   tags$div(class="select-box",
                            tags$div(class="input-middle",
                                     selectInput("hc_age", 
                                                 label = "Choose a variable",
                                                 choices = c("Male under 6 years with health insurance coverage", 
                                                             "Male under 6 years without health insurance coverage",
                                                             "Male under 6 to 18 years with health insurance coverage",
                                                             "Male under 6 to 18 years without health insurance coverage"),
                                                 selected = "Male under 6 years with health insurance coverage")
                            )),
                   column(12, 
                          tags$div(class="data-content",
                                   linebreaks(2),
                          )),
                   column(12,
                          tags$div(class="center-plotly",
                                   plotlyOutput("healthcovhist"))),
                   div(class="census-flex-container",
                       div(class="census-item census-item-1",
                           tags$img(src = "census_healthcoverage_left.jpg", height = 170, class="img-topic"),
                           tags$div(class="inside-textbox",
                                    tags$h3(class="topic-title",
                                            "Health Coverage, Oregon, 2021-2022"),
                                    tags$p(class="topic-p",
                                           "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore 
                                     et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut 
                                     aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum 
                                     dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia 
                                     deserunt mollit anim id est laborum."))),
                       div(class="census-item census-item-2",
                           tags$div(class="inside-textbox",
                                    tags$h3(class="topic-title",
                                            textOutput("hc_title")),
                                    tags$p(class="topic-p",
                                           textOutput("hc_variable")))),
                   ),
                   column(12,
                          tags$div(class="source-list",
                                   tags$p(class="s-list",
                                          tags$i(class="fa fa-thin fa-globe fa-1x"),
                                          "Source",
                                          tags$hr(class="source"),
                                          tags$p(class="s-list", "Maps and histograms are data from the",
                                                 tags$a(class="blue-link", href="", "2020 US Census"),
                                                 "Line graphs show changes in ",
                                                 tags$a(class="blue-link", href="", "US Census over time.")))
                          ),
                   )),
  conditionalPanel("input.tab == 'Population in Household by Age'",
                   tags$div(class="select-box",
                            tags$div(class="input-middle",
                                     selectInput("hh_age", 
                                                 label = "Choose a variable",
                                                 choices = c("Children in households under 3 years old", 
                                                             "Children in households that are 3 to 4 years old",
                                                             "Children in households that are 5 years old",
                                                             "Children in households that are 6 to 8 years old", 
                                                             "Children in households that are 9 to 11 years old",
                                                             "Children in households that are 12 to 14 years old",
                                                             "Children in households that are 15 to 17 years old"),
                                                 selected = "Children in households under 3 years old")
                            )),
                   column(12, 
                          tags$div(class="data-content",
                                   linebreaks(2),
                          )),
                   column(12,
                          tags$div(class="center-plotly",
                                   plotlyOutput("hhist"))),
                   div(class="census-flex-container",
                       div(class="census-item census-item-1",
                           tags$img(src = "school_grades_left.jpg", height = 170, class="img-topic",
                                    tags$div(class="inside-textbox",
                                             tags$h3(class="topic-title",
                                                     textOutput("hhist_title")),
                                             tags$p(class="topic-p",
                                                    "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore 
                                     et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut 
                                     aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum 
                                     dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia 
                                     deserunt mollit anim id est laborum."))),
                           div(class="census-item census-item-2",
                               tags$div(class="inside-textbox",
                                        tags$h3(class="topic-title",
                                                textOutput("hhage_title")),
                                        tags$p(class="topic-p",
                                               textOutput("hhage_variable")))),
                       ),
                       column(12,
                              tags$div(class="source-list",
                                       tags$p(class="s-list",
                                              tags$i(class="fa fa-thin fa-globe fa-1x"),
                                              "Source",
                                              tags$hr(class="source"),
                                              tags$p(class="s-list", "Maps and histograms are data from the",
                                                     tags$a(class="blue-link", href="", "2020 US Census"),
                                                     "Line graphs show changes in ",
                                                     tags$a(class="blue-link", href="", "US Census over time.")))
                              ),
                       )
                   )),
  conditionalPanel("input.tab == 'Resources'",
                   tags$div(class="select-box",
                            tags$div(class="input-middle",
                                     selectInput("resource_type", 
                                                 label = "Choose a variable",
                                                 choices = c("Water", "Land"),
                                                 selected = "Water"))),
                   column(12, 
                          tags$div(class="data-content",
                                   linebreaks(2),
                          )),
                   column(12,
                          tags$div(class="center-plotly",
                                   plotlyOutput("resourcehist"))),
                   div(class="census-flex-container",
                       div(class="census-item census-item-1",
                           tags$img(src = "census_resources_left.jpg", height = 170, class="img-topic"),
                           tags$div(class="inside-textbox",
                                    tags$h3(class="topic-title",
                                            "Population in Household by Age, Oregonian residents, 2021-2022"),
                                    tags$p(class="topic-p",
                                           "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore 
                                     et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut 
                                     aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum 
                                     dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia 
                                     deserunt mollit anim id est laborum."))),
                       div(class="census-item census-item-2",
                           tags$div(class="inside-textbox",
                                    tags$h3(class="topic-title",
                                            textOutput("resources_title")),
                                    tags$p(class="topic-p",
                                           textOutput("resources_variable")))),
                   ),
                   column(12,
                          tags$div(class="source-list",
                                   tags$p(class="s-list",
                                          tags$i(class="fa fa-thin fa-globe fa-1x"),
                                          "Source",
                                          tags$hr(class="source"),
                                          tags$p(class="s-list", "Maps and histograms are data from the",
                                                 tags$a(class="blue-link", href="", "2020 US Census"),
                                                 "Line graphs show changes in ",
                                                 tags$a(class="blue-link", href="", "US Census over time.")))
                          ),
                   )
  ))

schooldistricts <- div(
  tags$div(class="content",
           h3(class="title", "Team Projects"),
           p(class="normal-p", "We are a dedicated team working together to improve 
                               children's health and wellbeing. Our efforts focus on
                               accelerating the adoption of evidence informed policies,
                               programs, and practices that can reduce harmful environmental
                               exposures where children live, play, and go to school."))
)
