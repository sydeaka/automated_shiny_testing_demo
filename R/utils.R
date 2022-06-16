library(logger)

create_dir <- function(path) {
  if (dir.exists(path)) {
    log_info('`{path}` path not created because it already exists')
    return(invisible(NULL))
  }
  
  dir.create(path)
  log_info('`{path}` path successfully created.')
}

#xpath_str <- './/pre[@class="shiny-text-output"]'

get_text_values <- function(xml_obj, xpath_str='.//pre') {
  txt = xml_obj %>%
    xml2::xml_find_all(., xpath_str) %>%
    xml_text
  
  ids = xml_obj %>%
    xml2::xml_find_all(., xpath_str) %>%
  xml2::xml_attrs() %>%
    sapply(., function(u) u['id'])
  
  data.frame(ids, txt)
}



get_emoji <- function(success) {
  if (success) {
    emo::ji("check")
  } else {
    emo::ji("x")
  }
}