


test_model <- function(app, sidebar_id, model_label,
                       timeout_secs, snapshot_dir,
                       input_parameters, 
                       run_button_id,
                       module_id, max_polls = 60
) {
  
  log_info('Navigate to `{sidebar_id}` sidebar')
  app$setInputs(`left_sidebar` = sidebar_id, timeout_ = timeout_secs)
  app$takeScreenshot(file = file.path(snapshot_dir, glue::glue("navigate_to_{sidebar_id}_sidebar.png")))
  
  if (length(input_parameters) > 0) {
    log_info('Set {model_label} parameter values')
    do.call(app$setInputs, c(input_parameters, timeout_ = timeout_secs))
    Sys.sleep(wait_secs)
  } else {
    log_info('No inputs provided. Using default parameter values.')
  }
  app$takeScreenshot(file = file.path(snapshot_dir, glue::glue("set_{sidebar_id}_inputs.png")))
  
  log_info('Click the {model_label} run button to get predictions')
  
  click_run_args_list <- list(run_button_id = 'click', wait_ = FALSE, values_ = FALSE)
  names(click_run_args_list)[1] <- run_button_id
  do.call(app$setInputs, click_run_args_list)
  
  timer_start <- Sys.time()
  Sys.sleep(wait_secs)
  app$snapshot()
  app$takeScreenshot(file = file.path(snapshot_dir, glue::glue("click_{sidebar_id}_pred_button.png")))
  
  # Initialize snapshot of exported values
  vals_exported <- app$getAllValues(export=TRUE)$export
  status_id <- paste0(module_id, '-analysis_status')
  finished_id <- paste0(module_id, '-analysis_finished')
  status <- vals_exported[[status_id]]
  
  # Check every few seconds to see if the prediction is done
  app$takeScreenshot(file = file.path(snapshot_dir, glue::glue("{sidebar_id}_progress.png")))
  k <- 1
  while((!vals_exported[[finished_id]]) & (k <= max_polls) & !(status %in% c('', 'Finishing'))) {
    status <- vals_exported[[status_id]]
    log_info(glue::glue("{model_label} iteration {k} of {max_polls}: Status = {st}",
                        st = status))
    Sys.sleep(1)
    vals_exported <- app$getAllValues(export=TRUE)$export
    app$takeScreenshot(file = file.path(snapshot_dir, glue::glue("{sidebar_id}_progress_{k}.png")))
    k <- k + 1
  }
  
  # Calculate seconds/minutes elapsed
  secs_elapsed <- round(difftime(Sys.time(), timer_start, units='sec'), 2)
  mins_elapsed <- round(secs_elapsed / 60, 2)
  
  
  if (!(status %in% c('', 'Finishing'))) {
    # If no results found in alloted time, log the error
    log_error('No site {model_label} results found in {secs_elapsed} seconds ({mins_elapsed} minutes)')
  } else {
    # If results found, show success message
    log_info('{model_label} results successfully retrieved in {secs_elapsed} seconds ({mins_elapsed} minutes)')
    
    
  }
  
  return(app)
}








dismiss_shinyalert_trycatch <- function(app, alert_type = 'sweetalert') {
  tryCatch({
    success <- FALSE
    dismiss_alert(app, type = alert_type, keypress = 'escape')
    success <- TRUE
  }, 
  
  error = function(e) {
    log_warn(as.character(e))
    e
  }, 
  
  finally = {
    if (success) {
      log_info('Shiny Alert successfully dismissed.')
    } else {
      log_warn('Shiny alert could not be dismissed.')
    }
  }
  )
  
  return(app)
}











