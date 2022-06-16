



rt_1 <- function() {
  tab_id <- "Main"
  
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
    log_info('')
    action = "Inspect the Flavor dropdown menu"
    expected_res = "- Three options should include Vanilla, Chocolate, and Strawberry
  - Selected flavor should be visible in the Recipe Summary text box"
    log_info("Action: {action}")
    log_info("Expected result: {expected_res}")
    
    tryCatch({
      success <- FALSE
      
      log_info('Activate the dropdown menu')
      w <- app$findElement(xpath = './/select[@id="flavor"]/../div')
      w$sendKeys(webdriver::key$tab)
      app$takeScreenshot(file = file.path(snapshot_dir, paste0(file_prefix, "_activate_dropdown2.png")))
      w$sendKeys(webdriver::key$enter)
      
      log_info('Get the dropdown options')
      src <- app$getSource()
      flavor_dropdown_options <- read_html(src) %>% 
        xml2::xml_find_all(., './/*[@role="listbox"]') %>% 
        xml2::xml_find_all(., './/*[@role="option"]') %>%
        xml2::xml_text() %>% as.vector
      log_eval(flavor_dropdown_options, level=INFO)
      
      expected_result <- c("Vanilla", "Chocolate", "Strawberry")
      log_eval(expected_result, level=INFO)
      test1 <- all(flavor_dropdown_options %in% expected_result)
      
      
      
      # Selected flavor should be visible in the Recipe Summary text box
      displayed_flavor <- read_html(src) %>% 
        xml2::xml_find_all(., './/*[@id="flavor_summary"]') %>%
        xml2::xml_text(.[[1]]) %>%
        gsub(pattern="Flavor: ", replacement='', x=.)
      
      selected_flavor <- app$getAllValues()$input$flavor
      log_eval(displayed_flavor, level=INFO)
      log_eval(selected_flavor, level=INFO)
      test2 = displayed_flavor == selected_flavor
      success <- test1 & test2
      log_eval(success, level=INFO)
      log_info('Regression test #1 {step_id} successfully completed.')
    },
    
    error = function(e) {
      log_error('Regression Test #1 {step_id} failed!')
      log_error(as.character(e))
      e
    },
    
    finally = {
      log_info("Regression Test #1 {step_id} passed? {success} / {get_emoji(success)}")
      return(success)
    }
    
    ) # end tryCatch
    
  } # end step1
  
  
  
  
  
  
  step3 <- function() {
    step_id = 'step 3'
    test_step_name <- paste("Regression Test #1", step_id)
    file_prefix = paste0('regression_test_1', step_id)
    action = ""
    expected_res = ""
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
  } # end step3
  
  
  
  
  
  
  step6 <- function() {
    step_id = 'step 6'
    test_step_name <- paste("Regression Test #1", step_id)
    file_prefix = paste0('regression_test_1', step_id)
    action = ""
    expected_res = ""
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
  } # end step6
  
  
  
}
















