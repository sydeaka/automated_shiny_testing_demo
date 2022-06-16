if (F) {
  # Site selection
  sidebar_id = 'ss_model'
  model_label = 'Site Selection'
  timeout_secs = timeout_secs
  snapshot_dir = snapshot_dir
  input_parameters = list(`study_scenario_ss_ui_1-ss_rand` = "Non-Randomized",
                          `study_scenario_ss_ui_1-ss_phase` = "III/IV",
                          `study_scenario_ss_ui_1-ss_immunology-ss_prim_ind` = "Lupus",
                          `study_scenario_ss_ui_1-ss_lilly_alias` = "lupus-alias")
  module_id = 'study_scenario_ss_ui_1'
  tab_id = "shiny-tab-ss_model"
  
  # Country Allocation
  sidebar_id = 'model' 
  model_label = 'Country Allocation'
  timeout_secs = timeout_secs 
  snapshot_dir = snapshot_dir
  input_parameters = list()
  module_id = 'study_scenario_ui_1' 
  tab_id = "shiny-tab-model"
  
  # Optimization
  sidebar_id = 'optimization'
  model_label = 'Optimization'
  timeout_secs = timeout_secs 
  snapshot_dir = snapshot_dir
  input_parameters = list(`optimization_ui_1-optimization_max_sites` = "61", 
                          `optimization_ui_1-optimization_lilly_alias` = "test-study-alias",
                          `optimization_ui_1-reset` = "click")
  run_button_id = 'optimization_ui_1-gobutton'
  module_id = 'optimization_ui_1'
  tab_id = "shiny-tab-optimization"
  
  # sidebar_id <- 'ss_model'
  # input_parameters <- list(`study_scenario_ss_ui_1-ss_rand` = "Non-Randomized",
  # rm(sidebar_id, input_parameters, model_label, run_button_id, module_id)
  
}


#input_parameters = list()
#tab_id = "Main"

navigate_to_tab <- function(app, src, tab_id) {
  log_info("Locate `{tab_id}` elements")
  ui_elem_attr <- read_html(src) %>% xml2::xml_find_all(., glue::glue('.//a[@data-value="{tab_id}"]')) %>% xml2::xml_attrs()
  if (ui_elem_attr[[1]]['aria-selected'] == 'false') {
    log_info('Switching to {`tab_id`}')
    app$setInputs(tabs = tab_id)
  } else {
    log_info("Tab `{tab_id}` is already selected.")
  }
}

get_ui_elements <- function(app, 
                            timeout_secs, snapshot_dir,
                            input_parameters=list(), 
                            tab_id=NULL) {
  
  log_info("Get page source (HTML)")
  src <- app$getSource()
  
  log_info('Navigate to selected tab')
  navigate_to_tab(app, src, tab_id)
  
  app$takeScreenshot(file = file.path(snapshot_dir, glue::glue("navigated_to_{tab_id}_tab.png")))
  
  if (length(input_parameters) > 0) {
    log_info('Set {tab_id} parameter values')
    tryCatch({
      success <- FALSE
      do.call(app$setInputs, c(input_parameters, timeout_ = timeout_secs))
      Sys.sleep(wait_secs)
      success. <- TRUE
      log_info('Input parameters successfully set.')
    },
    
    error = function(e) {
      log_error('Failed to set input parameters!')
      log_error(as.character(e))
      e
    }
    
    )
  } else {
    log_info('No inputs provided. Using default parameter values.')
  }
  
  log_info('Screenshot of {tab_id} inputs')
  app$takeScreenshot(file = file.path(snapshot_dir, glue::glue("set_{tab_id}_inputs.png")))
  
  
  
  # vals_output <- app$getAllValues()$output %>% .[startsWith(names(.), module_id)]
  # output_ids <- names(vals_output)[startsWith(names(vals_output), module_id)]
  # ui_elements_df <- lapply(output_ids, function(id) {
  #   vals_output[[id]]$html %>% parse_box_body()
  # }) %>% bind_rows
  
  
  log_info("Get page source (HTML)")
  src <- app$getSource()
  
  log_info("Locate `{tab_id}` elements")
  xml_obj <- read_html(src) %>% xml2::xml_find_all(., glue::glue('.//div[@class="tab-pane active"]'))
  
  log_info("Parse HTML")
  ui_elements <- list()
  ui_elements[['text']] <- get_text_values(xml_obj, xpath_str='.//pre') 
  ui_elements[['tables']] <- read_html(src) %>% html_table
  
  log_info("UI elements successfully retrieved for tab_id = `{tab_id}`")
  return(ui_elements)
}














# html_str = h
parse_box_body <- function(html_str=NULL, xml_obj=NULL) {
  if (is.null(xml_obj)) xml_obj <- xml2::read_html(html_str)
  bb_list <- xml2::xml_find_all(xml_obj, './/div[@class="box-body"]') 
  num_input_list <- xml2::xml_find_all(xml_obj, './/div[@class="shiny-html-output shiny-bound-output"]') 
  sic_list <- xml2::xml_find_all(xml_obj, './/div[@class="form-group shiny-input-container"]') 
  sdi_list <- xml2::xml_find_all(xml_obj, './/div[@class="shiny-date-input form-group shiny-input-container shiny-bound-input"]') 
  radio_list <- xml2::xml_find_all(xml_obj, './/div[@class="form-group shiny-input-radiogroup shiny-input-container shinyjs-resettable shiny-bound-input"]') 
  dt_list <- xml2::xml_find_all(xml_obj, './/table[@class="display dataTable no-footer"]') 
  
  
  
  ui_elements <- list()
  
  if (length(bb_list) == 0) {
    obj_button <- xml2::xml_find_all(xml_obj, './/button') 
    if (length(obj_button) > 0) {
      ui_elements[['label']] <- obj_button %>% xml_attr('id')
      ui_elements[['type']] <- obj_button %>% xml_attr('type')
      ui_elements[['values']] <- obj_button %>% xml_text()
    }
    
    return(ui_elements)
    
  }
  
  
  
  if (length(bb_list) > 0) {
    # Initialize UI element count
    num_elements <- length(ui_elements)
    
    log_info('Processing box-body list')
    for (k in 1:length(bb_list)) {
      log_info("Iteration {k} of {length(bb_list)}")
      bb = bb_list[[k]]; bb
      
      sic <- xml_find_all(bb, './/div[@class="form-group shiny-input-container"]')
      if (length(sic) > 0) {
        ui_elements[[num_elements + k]] <- parse_shiny_input_container(sic)
      }
      
      sicg <- xml_find_all(bb, './/div[@class="form-group shiny-input-checkboxgroup shiny-input-container"]')
      if (length(sicg) > 0) {
        ui_elements[[num_elements + k]] <- parse_shiny_input_checkboxgroup(sicg)
      }
      
      sicg <- xml_find_all(bb, './/div[@class="form-group shiny-input-checkboxgroup shiny-input-container shiny-bound-input shinyjs-resettable"]')
      if (length(sicg) > 0) {
        ui_elements[[num_elements + k]] <- parse_shiny_input_checkboxgroup(sicg)
      }
      
      sicg <- xml_find_all(bb, './/div[@class="form-group shiny-input-radiogroup shiny-input-container shinyjs-resettable shiny-bound-input"]')
      if (length(sicg) > 0) {
        ui_elements[[num_elements + k]] <- parse_shiny_input_checkboxgroup(sicg)
      }
      
      
      
    }
  }
  
  
  
  
  
  if (length(num_input_list) > 0) {
    # Update UI element count
    num_elements <- length(ui_elements)
    
    log_info('Processing numeric input list')
    for (k in 1:length(num_input_list)) {
      log_info("Iteration {k} of {length(num_input_list)}")
      ni <- num_input_list[[k]]; ni
      
      sic <- xml_find_all(ni, './/div[@class="form-group shiny-input-container"]'); sic
      if (length(sic) > 0) {
        ui_elements[[num_elements + k]] <- parse_shiny_input_container(sic)
      }
      
    }
  }
  
  
  
  if (length(sic_list) > 0) {
    # Update UI element count
    num_elements <- length(ui_elements)
    
    log_info('Processing shiny input container list')
    for (k in 1:length(sic_list)) {
      log_info("Iteration {k} of {length(sic_list)}")
      sic <- sic_list[[k]]; sic
      ui_elements[[num_elements + k]] <- parse_shiny_input_container(sic)
    }
  }
  
  
  
  
  
  if (length(sdi_list) > 0) {
    # Update UI element count
    num_elements <- length(ui_elements)
    
    log_info('Processing shiny date input list')
    for (k in 1:length(sdi_list)) {
      log_info("Iteration {k} of {length(sdi_list)}")
      sdi <- sdi_list[[k]]; sdi
      ui_elements[[num_elements + k]] <- parse_shiny_date_input(sdi)
    }
    
  } else {
    log_info("No shiny date inputs found.")
  }
  
  
  
  
  if (length(radio_list) > 0) {
    # Update UI element count
    num_elements <- length(ui_elements)
    
    log_info('Processing shiny radio box list')
    for (k in 1:length(radio_list)) {
      log_info("Iteration {k} of {length(radio_list)}")
      rad <- radio_list[[k]]; rad
      ui_elements[[num_elements + k]] <- parse_shiny_input_checkboxgroup(rad)
    }
    
  } else {
    log_info("No shiny radio button inputs found.")
  }
  
  
  
  
  
  if (length(dt_list) > 0) {
    # Update UI element count
    num_elements <- length(ui_elements)
    
    log_info('Processing shiny data table list')
    for (k in 1:length(dt_list)) {
      log_info("Iteration {k} of {length(dt_list)}")
      sdt <- dt_list[[k]]; sdt
      ui_elements[[num_elements + k]] <- parse_shiny_datatable(sdt)
    }
    
  } else {
    log_info("No shiny data table inputs found.")
  }
  
  
  
  
  return(ui_elements %>% bind_rows)
}




# sdi = xml_find_all(bb, './/div[@class="form-group shiny-input-checkboxgroup shiny-input-container"]')
parse_shiny_date_input <- function(sdi) {
  res <- list(label=NA_character_, type=NA_character_, values=NA_character_)
  
  if (length(xml_find_all(sdi, './/label[@class="control-label"]')) > 0) {
    res[['label']] <- xml_find_all(sdi, './/label[@class="control-label"]') %>% xml_text
  }
  
  if (length(xml_find_all(sdi, './/input[@class="form-control shinyjs-resettable"]')) > 0) {
    obj <- xml_find_all(sdi, './/input[@class="form-control shinyjs-resettable"]')
    res[['type']] <- 'date'
    att_names <- c('type', 'data-date-language', 'data-date-week-start', 'data-date-format', 'data-shinyjs-resettable-type', 'data-shinyjs-resettable-value')
    res[['values']] <- sapply(att_names, function(att_name) obj %>% xml_attr(att_name)) %>% t %>% data.frame
  }
  
  #print(res)
  
  res_df <- data.frame(
    label = res$label,
    type = res$type,
    values = jsonlite::toJSON(res$values) %>% as.character
  )
  
  return(res_df)
}







parse_shiny_datatable <- function(sdt) {
  table_id <- sdt %>% xml_attr('id')
  css_id <- paste0('#', table_id)
  
  lbl<- sdt %>% xml_parent()  %>% xml_parent()  %>%  xml_attr('id')
  
  tab <- sdt %>% 
    # rvest::html_node(css=css_id) %>% 
    rvest::html_table()
  
  res_df <- data.frame(
    label = lbl,
    type = 'datatable',
    values = jsonlite::toJSON(tab) %>% as.character
  )
  
  return(res_df)
}


# sicg = xml_find_all(bb, './/div[@class="form-group shiny-input-checkboxgroup shiny-input-container"]')
parse_shiny_input_checkboxgroup <- function(sicg) {
  res <- list(label=NA_character_, type=NA_character_, values=NA_character_)
  
  if (length(xml_find_all(sicg, './/label[@class="control-label"]')) > 0) {
    res[['label']] <- xml_find_all(sicg, './/label[@class="control-label"]') %>% xml_text
  }
  
  if (length(xml_find_all(sicg, './/input[@type="checkbox"]')) > 0) {
    obj <- xml_find_all(sicg, './/input[@type="checkbox"]')
    res[['type']] <- 'checkbox'
    res[['values']] <- obj %>% xml_attr('value')
  }
  
  if (length(xml_find_all(sicg, './/div[@class="shiny-options-group"]/div/input[@type="radio"]')) > 0) {
    obj <- xml_find_all(sicg, './/div[@class="shiny-options-group"]/div/input[@type="radio"]')
    res[['type']] <- 'radio-button'
    res[['values']] <- obj %>% xml_attr('value')
  }
  
  #print(res)
  
  res_df <- data.frame(
    label = res$label,
    type = res$type,
    values = jsonlite::toJSON(res$values) %>% as.character
  )
  
  return(res_df)
}



# parse_shiny_bound_output <- function(sbo) {
#   
#   
#   if (length(xml_find_all(sbo, './/input[@class="shiny-html-output shiny-bound-output"]')) > 0) {
#     res[['label']] <- xml_find_all(sbo, './/control-label') %>% xml_text
#   }
# }



# sic =  xml_find_all(bb, './/div[@class="form-group shiny-input-container"]')
parse_shiny_input_container <- function(sic) {
  res <- list(label='', type='', values='')
  res_df <- data.frame()
  
  lbl = ''
  if (length(xml_find_all(sic, './/label')) > 0) {
    lbl <- xml_find_all(sic, './/label') %>% xml_text
  } else {
    lbl <- xml_find_all(sic, './/select') %>% xml_attr('id')
  }
  
  res[['label']] <- ifelse(length(lbl) == 0, '', lbl)
  
  
  
  if (length(xml_find_all(sic, './/control-label')) > 0) {
    res[['label']] <- xml_find_all(sic, './/control-label') %>% xml_text
  }
  
  
  
  if (length(xml_find_all(sic, './/span[@class="irs-from"]')) > 0 & length(xml_find_all(sic, './/span[@class="irs-to"]')) > 0) {
    res[['type']] <- 'sliderInput'
    
    
    val_from <- xml_find_all(sic, './/span[@class="irs-from"]') %>% xml_text()
    val_to <- xml_find_all(sic, './/span[@class="irs-to"]') %>% xml_text()
    
    val_min <- xml_find_all(sic, './/span[@class="irs-min"]') %>% xml_text()
    val_max <- xml_find_all(sic, './/span[@class="irs-max"]') %>% xml_text()
    
    
    res[['values']] <-  data.frame(from=val_from, to=val_to, min=val_min, max=val_max)  %>% jsonlite::toJSON() %>% as.character
  }
  
  
  
  if (length(xml_find_all(sic, './/input[@class="form-control shiny-bound-input shinyjs-resettable"]')) > 0) {
    res[['type']] <- 'numericInput'
    
    res[['values']] <-  xml_find_all(sic, './/input[@class="form-control shiny-bound-input shinyjs-resettable"]')  %>% 
      xml_attr('value')
  }
  
  
  
  if (length(xml_find_all(sic, './/select[@class="shinyjs-resettable selectized shiny-bound-input"]')) > 0) {
    res[['type']] <- 'selectizedInput'
    
    res[['values']] <-  xml_find_all(sic, './/option')  %>% 
      xml_attr('value')
  }
  
  
  if (length(xml_find_all(sic, './/select[@class="selectized shiny-bound-input shinyjs-resettable"]')) > 0) {
    res[['type']] <- 'selectizedInput'
    
    res[['values']] <-  xml_find_all(sic, './/option')  %>% 
      xml_attr('value')
  }
  
  
  
  
  
  if (length(xml_find_all(sic, './/select[@class="selectpicker form-control"]')) > 0) {
    res[['type']] <- 'selectpicker'
    
    res[['values']] <-  xml_find_all(sic, './/select[@class="selectpicker form-control"]')  %>% 
      xml_children %>% xml_attr('value')
  }
  
  
  if (length(xml_find_all(sic, './/div[@class="dropdown bootstrap-select show-tick form-control bs3"]')) > 0) {
    res[['type']] <- 'dropdown'
    
    res[['values']] <-  xml_find_all(sic, './/div[@class="dropdown bootstrap-select show-tick form-control bs3"]//option')  %>% 
      xml_attr('value')
  }
  
  
  if (length(xml_find_all(sic, './/div[@class="dropdown bootstrap-select form-control bs3"]')) > 0) {
    res[['type']] <- 'dropdown'
    
    res[['values']] <-  xml_find_all(sic, './/div[@class="dropdown bootstrap-select form-control bs3"]//option')  %>% 
      xml_attr('value')
  }
  
  
  if (length(xml_find_all(sic, './/input[@class="sw-switchInput shinyjs-resettable shiny-bound-input"]')) > 0) {
    
    
    obj <- xml_find_all(sic, './/input[@class="sw-switchInput shinyjs-resettable shiny-bound-input"]') 
    
    if ( is.na(res[['label']]) | res[['label']] == '') res[['label']] <- obj %>% xml_attr('id')
    
    
    res[['type']] <- 'toggle-switch'
    
    
    res[['values']] <-  data.frame(`data-on-text` = obj %>% xml_attr('data-on-text'), `data-off-text` = obj %>% xml_attr('data-off-text'), check.names = FALSE)
  }
  
  
  
  
  if (length(xml_find_all(sic, './/input[@type="checkbox"]')) > 0) {
    obj <- xml_find_all(sic, './/input[@type="checkbox"]')
    res[['label']] <- obj %>% xml_attr('id')
    res[['type']] <- 'checkbox'
    vals_on <- obj %>% xml_attr('data-on-text')
    vals_off <- obj %>% xml_attr('data-off-text')
    stopifnot(length(vals_on) == length(vals_off))
    res[['values']] <- paste(vals_on, vals_off, sep=',') %>% sapply(., function(u) u %>% jsonlite::toJSON() %>% as.character)
    
    res_df <- data.frame(
      label = res$label,
      type = res$type,
      values = res$values
    )
  }
  
  #print(res)
  
  if (nrow(res_df) == 0) {
    res_df <- data.frame(
      label = res$label,
      type = res$type,
      values = jsonlite::toJSON(res$values) %>% as.character
    )
  }
  
  return(res_df)
}



if (F) {
  src <- app$getSource()
  tab_id <- "shiny-tab-ss_model"
  parse_pagesource <- function(src, tab_id) {
    xml_obj <- read_html(src) %>% xml2::xml_find_all(., glue::glue('.//div[@id="{tab_id}"]')) 
    p = parse_box_body(xml_obj=xml_obj)
    
    
  }
  
  
  x=xml2::read_html(h) %>% xml2::xml_find_all(., './/div[@class="box-body"]'); x 
  x=xml2::read_html(h) %>% xml2::xml_find_all(., './/div[@class="box-body"]//div[@class="form-group shiny-input-container"]'); x 
  # //label[@class="control-label"]
  paths <- xml_path(x)
  e = app$findElement(xpath="//html//body//div//div[1]//div//div//div[2]")
  e$getText()
  #x = xml2::read_html(h) %>% xml2::xml_find_all(., './/h3[@class="box-title"]') 
  xml_children(x[[1]])
  xml_contents(x[[1]])
  app$findElement(xpath = "//*[@class=\"row\"]")
  
}