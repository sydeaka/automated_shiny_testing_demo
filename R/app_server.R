#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  # source: https://www.fatsecret.com/calories-nutrition
  calories_by_flavor <- data.frame(
    flavor = c("Vanilla", "Chocolate", "Strawberry"),
    calories_per_oz = c(57, 61, 54)
  )
  
  # Sources:
  # - https://www.webmd.com/diet/health-benefits-peanuts#2-4
  # - https://www.webmd.com/diet/raisins-good-for-you#091e9c5e8200b8b7-1-2
  # - https://www.fatsecret.com/calories-nutrition/usda/chocolate-syrup?portionid=36732&portionamount=1.000
  # - https://www.fatsecret.com/calories-nutrition/generic/chicken-nuggets?portionid=5851&portionamount=1.000
  calories_by_topping <- data.frame(
    topping = c("Peanuts", "Raisins", "Chocolate Syrup", "Chicken Nuggets"),
    serving_size = c('0.25 cup', '0.25 cup', '2 tbsp', '3 nuggets'),
    calories_per_serving = c(207, 108, 109, 48*3)
  )

  
  output$name_summary <- renderText({paste('Recipe name:', input$recipe_name)})
  
  output$flavor_summary <- renderText({ paste("Flavor:", input$flavor) } )
  
  output$topping_summary <- renderText({ paste("Toppings:", paste(input$toppings, collapse=', ')) })
  
  output$size_summary <- renderText({paste("Serving size:", input$serving_size, 'ounces')})
  
  output$calories_flavor <- renderTable({calories_by_flavor})
  
  output$calories_by_topping <- renderTable({calories_by_topping})
  
  calorie_contributions <- reactiveVal(data.frame(ingredient=character(), 
                                                  calories_per_oz=integer(),
                                                  num_ounces=integer(),
                                                  calories_total=integer()
                                                  ))
  
  observe({
    toppings_df <- calories_by_topping %>% 
      filter(topping %in% input$toppings) %>%
      select(topping, calories_per_serving) %>%
      rename(ingredient = topping, calories_per_oz = calories_per_serving)
      
    flavors_df <- calories_by_flavor %>% 
      filter(flavor == input$flavor) %>%
      select(flavor, calories_per_oz) %>%
      rename(ingredient = flavor)
    
    res <- bind_rows(toppings_df, flavors_df) %>%
      mutate(num_ounces = input$serving_size,
             calories_total = calories_per_oz * input$serving_size)
    
    res_final <- bind_rows(res, 
                           data.frame(ingredient = 'Total', 
                                      calories_per_oz = sum(res$calories_per_oz),
                                      num_ounces = input$serving_size,
                                      calories_total = sum(res$calories_total))) %>%
      arrange(calories_total)
    #browser()
    calorie_contributions(res_final)
  })
  
  output$calorie_contributions_df <- renderTable({calorie_contributions()})
  
  output$plot_calorie_contrib <- renderPlotly({
    req(nrow(calorie_contributions()) > 0)
    
    #res_final <- read.csv('dev/xxx.csv')
    plot_dat <- #res_final %>% 
      calorie_contributions() %>% 
      bind_rows(., 
                data.frame(ingredient='None', calories_per_oz = 0, num_ounces=.$num_ounces[1], calories_total = 0) # 
                ) %>%
      arrange(calories_total) %>%
      mutate(cumsum = cumsum(calories_total),
             point_label = paste0(ingredient, ' (', calories_total, ' calories)')
             ) %>%
      filter(ingredient != 'Total'); plot_dat
    
    g <- ggplot(plot_dat, aes(x=seq_along(cumsum), y=cumsum, label=point_label)) +
      geom_step(direction='hv') + 
      geom_point() +
      geom_text(#hjust=0.7, vjust=-5.75
                nudge_x=0, nudge_y=30,
                ) +
      ylim(0, max(plot_dat$cumsum) + 100) +
      #xlim(1, max(seq_along(cumsum)) + 1) +
      xlab('') + ylab('') +
      ggtitle('Cumulative Calorie Counts (kcal)') + 
      #theme(axis.title.x=element_blank(),
      #      axis.text.x=element_blank(),
      #      axis.ticks.x=element_blank()) #+
      scale_x_discrete(name ="", 
                       limits=plot_dat$ingredient);
    g
    
    ggplotly(g)
  })
}
