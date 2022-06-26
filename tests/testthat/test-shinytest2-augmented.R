library(shinytest2)
library(testthat)
library(shinytest2)
library(shinyvalidate)
library(here)
library(logger)
library(dplyr)
library(magrittr)
library(xml2)
library(rvest)

log_dir <- here('shinytest-logs')
tests_dir <- here('tests')
if (!dir.exists(log_dir)) dir.create(log_dir)
if (!dir.exists(tests_dir)) dir.create(tests_dir)

# Set log level
log_threshold(TRACE, namespace = 'global')

# Enable automatic logging of errors, warnings, and messages
#log_errors()
#log_warnings()
#log_messages()

logfile_name <- paste0(log_dir, '/logfile_ice-cream-app_shinytest_', Sys.time(), '.log')
log_info("Log file saved to {logfile_name}")

# Logged events displayed in console and appended to log file
log_appender(appender_tee(logfile_name))

pause <- TRUE
pause_secs <- 0.5

test_that("{shinytest2} augmented test: `automated_shiny_testing_demo`", {
  log_info("\nStart the driver")
  app <- AppDriver$new(variant = platform_variant(), name = "automated_shiny_testing_demo", 
      height = 821, width = 1491)
  app$view()
  app$expect_values()
  #app$get_logs()
  if (pause) Sys.sleep(pause_secs)
  
  log_info('\nSelect flavor')
  app$set_inputs(flavor = "Chocolate")
  expect_equal(app$get_value(input='flavor'), "Chocolate")
  if (pause) Sys.sleep(pause_secs)
  
  log_info('\nSelect toppings')
  app$set_inputs(toppings = c("Peanuts", "Raisins", "Chicken Nuggets"))
  expect_equal(app$get_value(input='toppings'), c("Peanuts", "Raisins", "Chicken Nuggets"))
  if (pause) Sys.sleep(pause_secs)
  
  log_info('\nSet serving size')
  app$set_inputs(serving_size = 2)
  expect_equal(app$get_value(input='serving_size'), 2)
  if (pause) Sys.sleep(pause_secs)
  
  log_info('\nSet recipe name')
  app$set_inputs(recipe_name = "Razzle Dazzle Frazzle")
  expect_equal(app$get_value(input='recipe_name'), "Razzle Dazzle Frazzle")
  if (pause) Sys.sleep(pause_secs)
  
  log_info("\nSelect `Nutrition Facts` tab")
  app$set_inputs(tabs = "Nutrition Facts")
  expect_equal(app$get_value(input='tabs'), "Nutrition Facts")
  if (pause) Sys.sleep(pause_secs)
  
  log_info("\nSelect `Main` tab")
  app$set_inputs(tabs = "Main")
  expect_equal(app$get_value(input='tabs'), "Main")
  if (pause) Sys.sleep(pause_secs)
  
  #expect_equal(3, 3)
  
  log_info("\nCheck contents of calorie contributions dataframe")
  calorie_table_list <- read_html(app$get_value(output = "calorie_contributions_df")) %>% html_table
  expect_length(calorie_table_list, 1)
  expect_s3_class(calorie_table_list[[1]], 'data.frame')
  calorie_df <- calorie_table_list[[1]]
  expect_equal(ncol(calorie_df), 4)
  expect_equal(calorie_df[,4] %>% unlist %>% as.vector, 
               (app$get_value(input='serving_size') * calorie_df[,2] %>% unlist %>% as.vector) )
  if (pause) Sys.sleep(pause_secs)
  
  log_info("\nCheck contents of calorie toppings dataframe")
  calorie_topping_table_list <- read_html(app$get_value(output = "calories_by_topping")) %>% html_table
  calorie_topping_df <- calorie_topping_table_list[[1]]
  expect_equal(ncol(calorie_topping_df), 3)
  
  app$expect_screenshot()
  app$expect_values()
  app$get_logs() %>% print
  
})
