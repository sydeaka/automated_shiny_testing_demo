# May have to run this first
# devtools::document()

# source helper scripts ----
library(here)
library(logger)
library(dplyr)
library(magrittr)
library(xml2)
library(rvest)
app_path <- here()
test_name <- "ice-cream-app-test-demo"
devtools::load_all()
#source(here::here("R", "shinytest_utils.R"))
#source(here::here("R", "shinytest_model_helper_fns.R"))
#source(here::here("R", "shinytest_parse_html.R"))
#source(here::here("R", "utils.R"))

options(shiny.testmode = TRUE)

# initialize shinytest driver ----
if (interactive()) {
  library(shiny)
  library(shinytest)
  #setup_shinytest()
  #Sys.sleep(2)
}


#record_test(".")


log_dir <- here('shinytest-logs')
tests_dir <- here('tests')
if (!dir.exists(log_dir)) dir.create(log_dir)
if (!dir.exists(tests_dir)) dir.create(tests_dir)

# Set log level
log_threshold(TRACE, namespace = 'global')

# Enable automatic logging of errors, warnings, and messages
log_errors()
log_warnings()
log_messages()

logfile_name <- paste0(log_dir, '/logfile_ice-cream-app_shinytest_', Sys.time(), '.log')
log_info("Log file saved to {logfile_name}")

# Logged events displayed in console and appended to log file
log_appender(appender_tee(logfile_name))

# set various shinytest options ----
log_info("set various shinytest options")
app <- ShinyDriver$new(path = app_path, loadTimeout = 50000)
#app <- AppDriver$new(name = "simple-app", height = 407, width = 348)
app$setWindowSize(1920, 1080)
app$snapshotInit(test_name)
snapshot_dir <- paste0(app$getSnapshotDir(), "-current")

# get one snapshot to create directory structure ----
log_info("get one snapshot to create directory structure")
app$snapshot(items = list(export = "ind"))
log_info("Shinytest setup complete")
Sys.sleep(2)

log_info('Set parameters')
timeout_secs <- 5000
wait_secs <- 3


# Welcome interface testing ----
log_info("take initial screenshot of the page after it loads")
app$takeScreenshot(file = file.path(snapshot_dir, "initial_view_on_load.png"))

ui_elements <- list()

ui_elements[['initial']] <- list()
ui_elements[['initial']][['Main']] <- get_ui_elements(app, 
                                                      timeout_secs, snapshot_dir,
                                                      input_parameters=list(), 
                                                      tab_id = 'Main') 
ui_elements[['initial']][['Nutrition Facts']] <- get_ui_elements(app, 
                                                                 timeout_secs, snapshot_dir,
                                                                 input_parameters=list(), 
                                                                 tab_id = 'Nutrition Facts') 

log_info('Specify input parameters')
input_parameters = list(
  `serving_size` = "3",
  `toppings` =  c('Peanuts', 'Chicken Nuggets'), 
  `flavor` = "Chocolate",
  `recipe_name` = "Razzle Dazzle Frazzle"
)
do.call(app$setInputs, c(input_parameters, timeout_ = timeout_secs))

ui_elements[['after_setting_inputs']] <- list()
ui_elements[['after_setting_inputs']][['Main']] <- get_ui_elements(app, 
                                                                   timeout_secs, snapshot_dir,
                                                                   input_parameters=list(), 
                                                                   tab_id = 'Main') 
ui_elements[['after_setting_inputs']][['Nutrition Facts']] <- get_ui_elements(app, 
                                                                              timeout_secs, snapshot_dir,
                                                                              input_parameters=list(), 
                                                                              tab_id = 'Nutrition Facts') 




























log_info('Stop the app')
# app$stop()


log_info('Test run completed.')