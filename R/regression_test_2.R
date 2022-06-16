rt_2 <- function() {
  tab_id = "Nutrition Facts"
  
  log_info("Get page source (HTML)")
  src <- app$getSource()
  
  log_info('Navigate to selected tab')
  navigate_to_tab(app, src, tab_id)
  
  log_info("Locate `{tab_id}` elements")
  xml_obj <- read_html(src) %>% xml2::xml_find_all(., glue::glue('.//div[@class="tab-pane active"]'))
  
  
  step1 <- function() {
    step_id = 'step 1'
    test_step_name <- paste("Regression Test #1", step_id)
    file_prefix = paste0('regression_test_1', step_id)
    action = "Examine the Calories by Ice Cream Flavor table"
    expected_res = "- Values should match values in flavor_calories.csv file
- Calories per ounce values should be numeric / integer
All values in the Flavor dropdown list should be represented in the table"
    log_info("Action: {action}")
    log_info("Expected result: {expected_res}")
    
    tryCatch({
      success <- FALSE
      
      
      log_eval(success, level=INFO)
      log_info('{test_step_name} successfully completed.')
    },
    
    error = function(e) {
      log_error('{test_step_name} failed!')
      log_error(as.character(e))
      e
    },
    
    finally = {
      log_info("Regression Test #1 {step_id} passed? {success} / {get_emoji(success)}")
      return(success)
    }) # end tryCatch
  } # end step1
  
  
  
  step2 <- function() {
    step_id = 'step 2'
    test_step_name <- paste("Regression Test #1", step_id)
    file_prefix = paste0('regression_test_1', step_id)
    action = "Examine the Calories by Topping table"
    expected_res = "- Values should match values in topping_calories.csv file
- Calories per serving values should be numeric / integer
- All values in the toppings checkbox group should be represented in the table"
    log_info("Action: {action}")
    log_info("Expected result: {expected_res}")
    
    tryCatch({
      success <- FALSE
      
      
      log_eval(success, level=INFO)
      log_info('{test_step_name} successfully completed.')
    },
    
    error = function(e) {
      log_error('{test_step_name} failed!')
      log_error(as.character(e))
      e
    },
    
    finally = {
      log_info("Regression Test #1 {step_id} passed? {success} / {get_emoji(success)}")
      return(success)
    }) # end tryCatch
  } # end step2
}

