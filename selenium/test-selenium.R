# "Set parameters"
port <- 4545
app_url <- "https://auto-mate.shinyapps.io/automated_shiny_testing_demo/"
pause <- TRUE
pause_secs <- 0.5

# Load R packages
library(RSelenium)
library(logger)
library(magrittr)
library(testthat)

# Load helper functions
source(file.path('~', 'Documents', 'GitHub', 'automated_shiny_testing_demo', 'R', 'utils.R'))
source(file.path('~', 'Documents', 'GitHub', 'automated_shiny_testing_demo', 'R', 'selenium_utils.R'))

# Create folder to store artifacts created during this test run (log files, images, etc)
timestamp <- Sys.time() %>% make.names
test_result_parent_folder <- file.path('~', 'Documents', 'GitHub', 'automated_shiny_testing_demo', 
                                'selenium', 'test-results')
test_result_folder <- file.path(test_result_parent_folder, timestamp)
image_folder <- file.path(test_result_folder, 'images')
create_dir(test_result_parent_folder)
create_dir(test_result_folder)
create_dir(image_folder)

# Set log level
log_threshold(TRACE, namespace = 'global')

# Create name for log file
logfile_name <- file.path(test_result_folder, paste0('logfile_ice-cream-app_selenium_', Sys.time(), '.log'))
log_info("Log file saved to `{logfile_name}`")

# Configure logger so that logged events are displayed in console AND appended to log file
log_appender(appender_tee(logfile_name))

log_info("Start the Selenium server via Docker commands")
docker_hash <- system(paste0("docker run -d -p ", port, ":4444 selenium/standalone-firefox:2.53.1"), 
                      intern=TRUE)

log_info("Start Selenium session")
remDr <- remoteDriver(remoteServerAddr = "localhost", port = port, browserName = "firefox")
Sys.sleep(2)

log_info("Instantiate the browser")
remDr$open()

log_info("Navigate to the app URL: {app_url}")
remDr$navigate(app_url)
Sys.sleep(2)
take_screenshot(remDr, image_folder)


log_info("Use `test_that` to run the test")
test_that("{shinytest2} augmented test: `automated_shiny_testing_demo`", {
  log_info('\nSelect flavor')
  elem_flavor <- remDr$findElement(using = "xpath", './/select[@id="flavor"]/..') 
  elem_flavor$clickElement()
  take_screenshot(remDr, image_folder)
  
  elem_chocolate <- remDr$findElement(using = "xpath", './/div[@data-value="Chocolate"]') 
  elem_chocolate$clickElement()
  take_screenshot(remDr, image_folder)
  elem_flavor_option <- remDr$findElement(using = "xpath", './/select[@id="flavor"]/option') 
  expect_equal(elem_flavor_option$getElementAttribute('value')[[1]], "Chocolate")
  if (pause) Sys.sleep(pause_secs)
  
  log_info('\nSelect toppings')
  elem_toppings <- remDr$findElements(using = "xpath", './/input[@type="checkbox" and @name="toppings"]')
  for (elem in elem_toppings) elem$clickElement()
  Sys.sleep(0.5)
  take_screenshot(remDr, image_folder)
  checkbox_value <- sapply(elem_toppings, function(elem) elem$getElementAttribute("value")[[1]]) 
  checked_status <- sapply(elem_toppings, function(elem) elem$getElementAttribute("checked")) %>%
    sapply(., function(u) {
      if(length(u) == 0) {
        return('false')
      } else {
        u[[1]]
      }
    })
  names(checked_status) <- checkbox_value
  boxes_checked <- checked_status[checked_status == 'true']
  expect_setequal(names(boxes_checked), c("Raisins", "Chocolate Syrup", "Chicken Nuggets"))
  if (pause) Sys.sleep(pause_secs)
  
  log_info('\nSet serving size')
  elem_serving <- remDr$findElement(using = "id", "serving_size")
  elem_serving$clearElement()
  elem_serving$sendKeysToElement(list("2"))
  take_screenshot(remDr, image_folder)
  expect_equal(elem_serving$getElementAttribute('value')[[1]], "2")
  if (pause) Sys.sleep(pause_secs)
  
  log_info('\nSet recipe name')
  elem_recipe_name <- remDr$findElement(using = "id", "recipe_name")
  elem_recipe_name$clearElement()
  elem_recipe_name$sendKeysToElement(list("Razzle Dazzle Frazzle"))
  take_screenshot(remDr, image_folder)
  expect_equal(elem_recipe_name$getElementAttribute('value')[[1]], "Razzle Dazzle Frazzle")
  if (pause) Sys.sleep(pause_secs)
  
  log_info('\nSelect `Nutrition Facts` tab')
  elem_tabs <- remDr$findElement(using = "id", "tabs")
  
  
  
  log_info("\nCheck contents of calorie contributions dataframe")
  src <- remDr$getPageSource()
  calorie_table_list <- src[[1]] %>%
    minimal_html() %>%
    html_table() 
  expect_length(calorie_table_list, 1)
  expect_s3_class(calorie_table_list[[1]], 'data.frame')
  calorie_df <- calorie_table_list[[1]]
  expect_equal(ncol(calorie_df), 4)
  serving_size <- elem_serving$getElementAttribute('value')[[1]] %>% as.integer
  expect_equal(calorie_df[,4] %>% unlist %>% as.vector, 
               (serving_size * calorie_df[,2] %>% unlist %>% as.vector) )
  if (pause) Sys.sleep(pause_secs)

})

log_info("Close the browser session")
remDr$close()

log_info("Stop the Docker container")
docker_stop_result <- system(paste0("docker stop ", docker_hash), intern=TRUE)

log_info("Remove / delete the Docker container")
docker_rm_result <- system(paste0("docker rm ", docker_hash), intern=TRUE)
