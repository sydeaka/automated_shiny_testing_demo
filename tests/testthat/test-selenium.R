library(RSelenium)
library(logger)
library(magrittr)
library(testthat)

source(file.path('~', 'Documents', 'GitHub', 'automated_shiny_testing_demo', 'R', 'utils.R'))

## TO DO:
## - Convert to testthat::test_that  so it works with shinytest2::test_app
## - Figure out why remDr$open() won't work inside testthat::test_that
## - Save screenshots to test result folder, along with logs
## - Use helper functions to enhance readability
## - Use multiple deployments




log_dir <- file.path('~', 'Documents', 'GitHub', 'automated_shiny_testing_demo', 'selenium-logs')
create_dir(log_dir)

# Set log level
log_threshold(TRACE, namespace = 'global')

logfile_name <- paste0(log_dir, '/logfile_ice-cream-app_selenium_', Sys.time(), '.log')
log_info("Log file saved to `{logfile_name}`")

# Logged events displayed in console AND appended to log file
log_appender(appender_tee(logfile_name))

log_info("Set parameters")
port <- 4545
app_url <- "https://auto-mate.shinyapps.io/automated_shiny_testing_demo/"
pause <- TRUE
pause_secs <- 0.5

log_info("Start the Selenium server via Docker commands")
docker_hash <- system(paste0("docker run -d -p ", port, ":4444 selenium/standalone-firefox:2.53.1"), 
                      intern=TRUE)

log_info("Start Selenium session")
remDr <- remoteDriver(remoteServerAddr = "localhost", port = port, browserName = "firefox")

log_info("Instantiate the browser")
remDr$open()

log_info("Navigate to the app URL: {app_url}")
remDr$navigate(app_url)
Sys.sleep(2)
remDr$screenshot(display=TRUE)

test_that("{shinytest2} augmented test: `automated_shiny_testing_demo`", {
  log_info('\nSelect flavor')
  elem_flavor <- remDr$findElement(using = "xpath", './/select[@id="flavor"]/..') 
  elem_flavor$clickElement()
  remDr$screenshot(display=TRUE)
  
  elem_chocolate <- remDr$findElement(using = "xpath", './/div[@data-value="Chocolate"]') 
  elem_chocolate$clickElement()
  remDr$screenshot(display=TRUE)
  elem_flavor_option <- remDr$findElement(using = "xpath", './/select[@id="flavor"]/option') 
  expect_equal(elem_flavor_option$getElementAttribute('value')[[1]], "Chocolate")
  if (pause) Sys.sleep(pause_secs)
  
  log_info('\nSelect toppings')
  elem_toppings <- remDr$findElements(using = "xpath", './/input[@type="checkbox" and @name="toppings"]')
  for (elem in elem_toppings) elem$clickElement()
  Sys.sleep(1)
  remDr$screenshot(display=TRUE)
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
  expect_equivalent(names(boxes_checked), c("Raisins", "Chocolate Syrup", "Chicken Nuggets"))
  if (pause) Sys.sleep(pause_secs)
  
  log_info('\nSet serving size')
  elem_serving <- remDr$findElement(using = "id", "serving_size")
  elem_serving$clearElement()
  elem_serving$sendKeysToElement(list("2"))
  remDr$screenshot(display=TRUE)
  expect_equal(elem_serving$getElementAttribute('value')[[1]], "2")
  if (pause) Sys.sleep(pause_secs)
  
  log_info('\nSet recipe name')
  elem_recipe_name <- remDr$findElement(using = "id", "recipe_name")
  elem_recipe_name$clearElement()
  elem_recipe_name$sendKeysToElement(list("Razzle Dazzle Frazzle"))
  remDr$screenshot(display=TRUE)
  expect_equal(elem_recipe_name$getElementAttribute('value')[[1]], "Razzle Dazzle Frazzle")
  if (pause) Sys.sleep(pause_secs)

})

log_info("Close the browser session")
remDr$close()

log_info("Stop the Docker container")
docker_stop_result <- system(paste0("docker stop ", docker_hash), intern=TRUE)

log_info("Remove / delete the Docker container")
docker_rm_result <- system(paste0("docker rm ", docker_hash), intern=TRUE)
