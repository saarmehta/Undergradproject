# ======================================================================
## FIT5147 Data exploration and visualisation 
## Semester 1 2018
## Visualization project
## Author: Saaransh Mehta
## Date created: 29/05/2018
## Date modified: 4/06/2018
# ======================================================================

library(shiny)
library(shinydashboard)
### Using dashboard layout 
dashboardPage(skin = "blue",
              dashboardHeader(title = "Analysis of undergraduate project for Pune University", titleWidth = 550),
              dashboardSidebar(
                sidebarMenu(
                  ##Tab One
                  menuItem("Domain Analysis",tabName = "domain",icon = icon("bar-chart-o")),
                  ##Tab Two
                  menuItem("Word Cluster",tabName = "cluster",icon = icon("spinner")),
                  ##Tab Three
                  menuItem("Wordcloud",tabName = "wordcloud",icon = icon("cloud")),
                  ##Tab Four
                  menuItem("Raw data",tabName = "data",icon = icon("table"))
                )),
              
              dashboardBody(
                tabItems(
                  tabItem(tabName = "domain",
                          titlePanel("Domain Analysis"),
                          sidebarLayout(
                            sidebarPanel(
                              h4("Below is a dynamic layout, you can select multiple domains"),
                              uiOutput("selectoption"),
                              uiOutput("dmselect"),
                              uiOutput("ckbox2"),
                              uiOutput("sld2")),
                            mainPanel(plotOutput ("displot"))
                            )),
                          
                  tabItem(tabName = "cluster",
                              pageWithSidebar(
                              headerPanel('Word association & clustering'),
                              sidebarPanel(
                                numericInput('limit', 'Number of words', 10,
                                             min = 1, max = 2000)
                              ),
                              mainPanel(
                                plotOutput('plot1'),
                                plotOutput('plot2')
                              )
                            )
                            ),
                  
                  tabItem(tabName = "wordcloud",
                            # Application title
                            titlePanel("Project title wordcloud"),
                            
                            sidebarLayout(
                              # Sidebar with a slider and selection inputs
                              sidebarPanel(
                                actionButton("update", "Refresh"),
                                hr(),
                                sliderInput("freq",
                                            "Minimum Frequency:",
                                            min = 1,  max = 50, value = 15),
                                sliderInput("max",
                                            "Maximum Number of Words:",
                                            min = 1,  max = 300,  value = 100)
                              ),
                              
                              # Show Word Cloud
                              mainPanel(
                                plotOutput("plot")
                              )
                            )
                            ),
                  tabItem(tabName = "data",
                            DT::dataTableOutput("table")
                            )
                    
                  )
)
)