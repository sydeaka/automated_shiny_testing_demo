library(shinytest2)
library(testthat)
library(shinytest2)
library(shinyvalidate)
library(rvest)

pause <- TRUE
pause_secs <- 0.5

test_that("{shinytest2} recording: automated_shiny_testing_demo", {
  app <- AppDriver$new(variant = platform_variant(), name = "automated_shiny_testing_demo", 
      height = 821, width = 1491)
  app$view()
  app$expect_values()
  if (pause) Sys.sleep(pause_secs)
  
  app$set_inputs(flavor = "Chocolate")
  expect_equal(app$get_value(input='flavor'), "Chocolate")
  if (pause) Sys.sleep(pause_secs)
  
  app$set_inputs(toppings = c("Peanuts", "Raisins", "Chicken Nuggets"))
  expect_equal(app$get_value(input='toppings'), c("Peanuts", "Raisins", "Chicken Nuggets"))
  if (pause) Sys.sleep(pause_secs)
  
  app$set_inputs(serving_size = 2)
  expect_equal(app$get_value(input='serving_size'), 2)
  if (pause) Sys.sleep(pause_secs)
  
  app$set_inputs(recipe_name = "Razzle Dazzle Frazzle")
  expect_equal(app$get_value(input='recipe_name'), "Razzle Dazzle Frazzle")
  if (pause) Sys.sleep(pause_secs)
  
  app$set_inputs(tabs = "Nutrition Facts")
  expect_equal(app$get_value(input='tabs'), "Nutrition Facts")
  if (pause) Sys.sleep(pause_secs)
  
  app$set_inputs(tabs = "Main")
  expect_equal(app$get_value(input='tabs'), "Main")
  if (pause) Sys.sleep(pause_secs)
  
  calorie_table_list <- read_html(app$get_value(output = "calorie_contributions_df")) %>% html_table
  expect_length(calorie_table_list, 1)
  expect_s3_class(calorie_table_list[[1]], 'data.frame')
  calorie_df <- calorie_table_list[[1]]
  expect_equal(ncol(calorie_df), 4)
  expect_equal(calorie_df[,4] %>% unlist %>% as.vector, 
               (app$get_value(input='serving_size') * calorie_df[,2] %>% unlist %>% as.vector) )
  if (pause) Sys.sleep(pause_secs)
  
  app$expect_screenshot()
  app$expect_values()
  
  
})
