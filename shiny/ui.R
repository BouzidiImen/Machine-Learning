
library(shiny)
library(plotly)
library(shinydashboard)
library(dashboardthemes)

library(plotly)
library(caret)

shinyUI(dashboardPage(


  dashboardHeader(title = 'Diabetes in Tunisia'),
  ########## sider panel ####
  dashboardSidebar( width = 250,
                    sidebarMenu(


                      menuItem("Descriptive Analysis", tabName = "DA", icon = icon("connectdevelop"),
                               startExpanded = T,
                               menuSubItem("Univariate Analysis", tabName = "UA", icon = icon("boxes")),
                               menuSubItem("Bivariate analysis", tabName = "BA", icon = icon("megaport"))
                      ),
                      menuItem("Machine learning ", tabName = "Home", icon = icon("window-restore"))
                      ,
                      menuItem("About", tabName = "Home", icon = icon("info-circle"))


                    )),
  dashboardBody(


    shinyDashboardThemes( ### changing theme
      theme ='poor_mans_flatly'
    ),
    tabItems(
      ########## Home ######
      tabItem(  tabName = "UA",
        
        fluidRow(  box(title = "Visualization of each variable", solidHeader = T,status = "primary",  width = 4 , 
                        selectInput("Var", "Choose a variable :",
                               c("Area",'Genre','Age','Matrimonial_status','Profession','Physical_Activity'
                                 ,'Alcohol_consumption','Education_level','Diet','Tabagism','Diabetes'),selected = "Area")),
                   box( status = "primary",  width = 8 ,
                    tabsetPanel(
                      selected = "Plot",
                      
                      tabPanel("Plot",br(),plotlyOutput('plot')),
                      tabPanel("Summary",br(),verbatimTextOutput('sum'))
                    )),
                 tags$head(tags$style(" #cite {
                                      color: #949793;
                                      position: absolute;
                                      bottom: 10px;
                                      right: 10px;
                                      font-size: 12px;
                                      }
                                      "))),
       
        tags$div(id="cite",
                 '\U00A9 Imen Bouzidi'
        )),
      ########## Partional clustering ########
      tabItem(
        tabName = "BA",
        
        fluidRow(  box( title = "Distribution of Diabetes by each variable", solidHeader = T,status = "primary",  width = 4 , 
                        selectInput("Var2", "Choose a variable :",
                                    c("Area",'Genre','Age','Matrimonial_status','Profession','Physical_Activity'
                                      ,'Alcohol_consumption','Education_level','Diet','Tabagism'),selected = "Area")
                        ,h5('Each variable is presented to determine its dependence and correlation with having Diabetes.',align='left')
                        ),
                   box( status = "primary",  width = 8 ,
                        tabsetPanel(
                          selected = "Plot",
                          
                          
                          tabPanel("Plot",br(),plotlyOutput('plot2')),
                          tabPanel("Tests",br(),verbatimTextOutput('chisq')),
                          tabPanel('Data',br(),DT::dataTableOutput("tabl"), br(),downloadButton('x3', 'Download Data'))
                          
                        ))
      ),
              
              
        tags$div(id="cite",
                 '\U00A9 Imen Bouzidi'
        )


        ),
      ########## Hier clust #####################
      tabItem(
        
        tags$div(id="cite",
                 '\U00A9 Imen Bouzidi'
        )



            ),

      ########## Cluster validation ##################
      tabItem(tags$div(id="cite",
                   '\U00A9 Imen Bouzidi'
        )

        #,
        #fluidRow(
        # infoBoxOutput("progressBox",width = 6)
        #)
      ),


      ########## EPM #####################
      tabItem(tags$div(id="cite",
                   '\U00A9 Imen Bouzidi'
        )
      )
      )



    )





  )


  )




