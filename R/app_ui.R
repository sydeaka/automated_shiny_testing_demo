library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(fresh)
library(dplyr)
library(ggplot2)
library(plotly)


#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    fluidPage(
      # Application title
      titlePanel("Welcome to the Ice Cream App!"),
      
      sidebarLayout(
        
        # Ice cream flavor selection
        sidebarPanel(
          selectInput(inputId="flavor", label = h3("Select a flavor"), 
                      choices = list("Vanilla",  "Chocolate", "Strawberry", "Pistachio"), 
                      selected = "Vanilla"),
          
        # Toppings
        checkboxGroupInput(inputId="toppings", label = h3("Select one or more toppings"), 
                           choices = c("Peanuts", "Raisins", "Chocolate Syrup", "Chicken Nuggets"),
                           selected = "Peanuts"),
        
        # Size
        numericInput(inputId="serving_size", label = h3("Select a size (oz)"), value = 1),
        
        # Name
        textInput(inputId="recipe_name", label = h3("Provide a name for your recipe"), value = ""),
        
        width = 3
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(id='tabs',
            tabPanel(title='Main',
                     h3('Recipe summary'),
                     fluidRow(column(9, verbatimTextOutput("name_summary"))),
                     fluidRow(column(9, verbatimTextOutput("flavor_summary"))),
                     fluidRow(column(9, verbatimTextOutput("topping_summary"))),
                     fluidRow(column(9, verbatimTextOutput("size_summary"))),
                     hr(),
                     fluidRow(column(9, plotlyOutput("plot_calorie_contrib"))),
                     hr(),
                     h3('Calorie Contribution Summary'),
                     fluidRow(column(9, tableOutput("calorie_contributions_df")))
                     ),
            tabPanel('Nutrition Facts', hr(),
                     h3('Calories by Ice Cream Flavor'),
                     fluidRow(column(9, tableOutput("calories_flavor"))),
                     hr(), hr(),
                     h3('Calories by Topping'),
                     fluidRow(column(9, tableOutput("calories_by_topping")))
                     
                     ),
                     
          ), # end tabsetPanel
          width = 9
        )
      
    ) # end sidebarLayout
  ) # end fluidPage
  ) # end tagList
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'demo.shiny.test.app'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}






