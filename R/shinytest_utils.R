

# This script was copied from the MOATR repository, with permission from author Eric Nantz
# Source: https://github.com/EliLillyCo/moatr/blob/f260d7eb388224218b95e3e956cd9c4c9217a6a3/inst/app/tests/shinytest_utils.R



#' Load a linux module into the R session
#'
#' @param ... typically the string portion of a `module` command. For example,
#' if the command to load the R module is `module load R-qualified`, then the 
#' string that should be supplied is `"load R-qualified"`.
#'
#' @return nothing
#' @examples
#' \dontrun{
#' module("--ignore_cache load phantomjs")
#' }
module <- function(...){
  arglist <- as.list(match.call())
  ## change this from 'module' to 'r'
  arglist[1] <- 'r'
  args <- paste(arglist, collapse = ' ', sep = '')
  
  binary <- "/usr/local/lmod/lmod/libexec/lmod"
  
  cmd <- paste(binary, args, sep = ' ')
  
  #system(cmd, ignore.stderr = TRUE, ignore.stdout = TRUE)
  
  hndl <- pipe(cmd)
  eval(expr = parse(file = hndl))
  close(hndl)
  
  invisible(0)
}

#' Set up system environment for `shinytest`
#'
#' This simple function loads the `phantomjs` module so `shinytest` can 
#' perform as expected
#' 
#' @return `TRUE` invisibly
setup_shinytest <- function() {
  #source("/usr/local/lmod/lmod/init/R")
  if (!stringr::str_detect(Sys.getenv("PATH"), "phantomjs")) {
    res <- module("--ignore_cache load phantomjs")
  }
  invisible(TRUE)
}

#' Set up system environment for HPC simulation launch 
#'
#' This simple function loads the `cluster-setup` module so HPC simulates performed
#' on a compute node (typically within`shinytest` execution) can perform correctly 
#' 
#' @return `TRUE` invisibly
setup_hpc <- function() {
  #source("/usr/local/lmod/lmod/init/R")
  if (!stringr::str_detect(Sys.getenv("PATH"), "qsub")) {
    res <- module("--ignore_cache load cluster-setup")
  }
  Sys.sleep(2)
  invisible(TRUE)
}


#' Prepare a custom MOATR session directory appropriate for automated testing
#' 
#' This is a convenience function to initialize a MOATR session directory 
#' to `/lrlhps/users/<user_id>/.moatr_files_test` based on pre-made session
#' directories stored in the MOATR isilon location.
#'
#' @param testname name of the test used in the session. Specific settings for each of these
#'   are contained in the `session-config.yml` file. Choices are the following:
#'   * `test_welcome`
#'   * `test_design`
#'   * `test_simulate_upload`
#'   * `test_simulate_schemeconfig`
#'   * `test_simulate_hpc`
#'   * `test_postprocess_summary`
#'   * `test_postprocess_combo`
#'   * `test_postprocess_value`
#' @param user User ID
#' @param moatr_version string with MOATR version number
#' @param session_location Directory path to where the pre-made files that are used
#'   as setup files for each test are given.  
#' @param session_config: Name of session configuration file to import with `config` package.
#'   Default is `session-config.yml`.
#' @return invisible TRUE
#' @export
#'
#' @examples
prepare_testdir <- function(testname,
                            user = Sys.getenv("USER"), 
                            moatr_version = "2",
                            session_location = "/lrlhps/data/moatr_files",
                            session_config = "session-config.yml",
                            set_env = TRUE) {
  
  # set environment variable for active test session config if requested
  if (set_env) {
    Sys.setenv(MOATR_TEST_CONFIG_ACTIVE = testname)
  }
  
  # import the configuration information for the specified test name
  session_settings <- config::get(config = testname, file = here::here(session_config))
  testsession_path <- fs::path(session_settings$parent_dir, session_settings$session_dirname)
  
  # check if user already has a MOATR testing session directory. If so, move to temp dir
  if (fs::dir_exists(testsession_path)) {
    #message("removing previous version of test session specific files and directories")
    old_files <- fs::dir_ls(testsession_path, all = TRUE, regexp = "hpc_workdir_*", invert = TRUE)
    
    purrr::walk(old_files, ~fs::file_delete(.x))
  } else {
    # create the parent test session directory if it does not exist
    #message("no previous testing session dir found, creating new one.")
    fs::dir_create(testsession_path, recursive = TRUE)
  }
  
  # if the hpc_workdir already exists, then skip copying that one
  # otherwise, copy it from the session_location area
  workdir_path <- fs::path(testsession_path, glue::glue("hpc_workdir_{moatr_version}"))
  if (!fs::dir_exists(workdir_path)) {
    #message("no existing hpc_workdir found. Copying from session location dir. This could take a few minutes.")
    fs::dir_copy(
      fs::path(session_location, moatr_version, glue::glue("hpc_workdir_{moatr_version}")),
      fs::path(testsession_path)
    )
  } else {
    #message("existing hpc_workdir found, no copying performed.")
  }
  
  # list all of the files in the specified session directory to copy over
  sessionfiles_path <- fs::path(session_location, moatr_version, testname, session_settings$session_dirname)
  tmp_files <- fs::dir_ls(sessionfiles_path, all = FALSE)
  
  info <- fs::dir_info(sessionfiles_path)
  
  # copy the files within the source session directory to the new testing directory
  #message(glue::glue("begin copying of {testname} files"))
  
  purrr::walk2(info$path, as.character(info$type), ~{
    if (.y == "file") file.copy(.x, testsession_path, overwrite = TRUE)
    if (.y == "directory") fs::dir_copy(.x, testsession_path)
  })
  
  # update the key slots of r_data with user-specific paths
  update_rdata(testname)
  
  #message(glue::glue("finished copying for {testname}"))
  invisible(TRUE)
}

#' Customize settings for shinytest app object
#'
#' @param A `ShinyDriver` object
#' @param test_name String for the name of the test being performed
#'
#' @return list with the paths to the snapshot, screenshot, and log directories
configure_app <- function(app, test_name, test_files = NULL, verbose = FALSE) {
  app$setWindowSize(1920, 1080)
  app$snapshotInit(test_name, screenshot = FALSE)
  
  # establish directory paths and create when necessary
  snapshot_dir <- paste0(app$getSnapshotDir(), "-current")
  screenshot_dir <- paste0(app$getSnapshotDir(), "-screenshots")
  if (!fs::file_exists(screenshot_dir)) {
    fs::dir_create(screenshot_dir)
  } else {
    fs::dir_delete(screenshot_dir)
    fs::dir_create(screenshot_dir)
  }
  
  # establish logging directory and logging file ----
  log_dir <- paste0(app$getSnapshotDir(), "-logs")
  if (!fs::file_exists(log_dir)) {
    fs::dir_create(log_dir)
  } else {
    fs::dir_delete(log_dir)
    fs::dir_create(log_dir)
  }
  
  log_path <- file.path(log_dir, paste0(test_name, ".json"))
  if (fs::file_exists(log_path)) fs::file_delete(log_path)
  setLogFile(log_path, confirm = verbose)
  
  # create files with supplied files if requested
  file_dir  <- paste0(app$getSnapshotDir(), "-files")
  if (fs::file_exists(file_dir)) fs::file_delete(file_dir)
  fs::dir_create(file_dir)
  if (!is.null(test_files)) {
    purrr::walk(test_files, ~fs::file_copy(path = .x, new_path = fs::path(file_dir, fs::path_file(.x))))
  }
  
  # get one snapshot to create directory structure ----
  app$snapshot(items = list(export = "ind"))
  loggit("INFO", "Shinytest setup complete")
  Sys.sleep(2)
  
  # return directories created
  return(
    list(
      snapshot_dir = snapshot_dir,
      screenshot_dir = screenshot_dir,
      log_dir = log_dir,
      file_dir = file_dir
    )
  )
}

#' Print diagnostic log messages to test log file
#'
#' @param step_name String for name of step (typically formatted like `step1`)
#' @param session_settings list with customized test settings imported from `session_config.yml`
#' @param log_lvl Log level used by `loggit` default is `INFO`
#' @param ... Additional variables (scalars) to pass to `loggit` function
#'
#' @return Nothing
#' @export
#'
#' @examples
log_message <- function(step_name, session_settings, log_lvl = "INFO", ...) {
  res <- purrr::pluck(session_settings$test_steps, step_name)
  loggit(log_lvl = log_lvl, log_msg = res$title, step_name = step_name, ...)
}

take_screenshot <- function(app, step_name, session_settings, index = NULL) {
  screenshot_dir <- paste0(app$getSnapshotDir(), "-screenshots")
  res <- purrr::pluck(session_settings$test_steps, step_name)
  
  if (!is.null(index)) {
    image_file <- res$screenshot_file[index]
  } else {
    image_file <- res$screenshot_file
  }
  app$takeScreenshot(file = file.path(screenshot_dir, image_file))
}

#' Automatically dismiss an alert/modal dialog in shiny app
#' 
#' This function is meant to be used within a `shinytest` script when 
#' a popup alert or modal appears on the screen and it has the capability
#' of being dismissed via a keypress such as the escape key
#'
#' @param app A `ShinyDriver` object
#' @param type string indicating the type of alert to dismiss. Currently the
#' following types are supported:
#'   * `modal`: Modal dialog that is typically produced by `shiny::showModal` and there
#'              is not a custom input button to press other than "Dismiss"
#'   * `sweetalert`: Alert produced by the `shinyalert` package and there is not a 
#'                   custom input button to press other than "Ok" or something similar.
#' @param keypress which key to simulate pressing found in the `webdriver::key` list.
#'   Default is `escape` for the escape key.
#'
#' @return invisible TRUE
#' @export
#'
#' @examples
dismiss_alert <- function(app, type, keypress = 'escape') {
  
  # set up css value depending on type of dialog to dismiss
  css_sel <- switch(type,
                    'modal' = '#shiny-modal',
                    'sweetalert' = '.visible')
  
  web <- app$.__enclos_env__$private$web
  el <- web$findElement(css = css_sel)
  el$sendKeys(webdriver::key[[keypress]])
  invisible(TRUE)
}

#' Activate an existing session within the test mode of MOATR
#'
#' @param app A `ShinyDriver` object
#' @param session_name String with the name of the session to activate.
#' @param session_dirname String for directory name of MOATR session files.
#'   Defaults to `.moatr_files_test` since that is the custom directory
#'   used for testing the app in `shinytest`.
#' @param module_id Namespace ID of the welcome module. Default is `first`
#'
#' @return invisible TRUE
#' @export
#'
#' @examples
activate_session <- function(app, 
                             session_name, 
                             session_dirname = ".moatr_files_test", 
                             module_id = "first") {
  
  library(rlang)
  
  # ensure the welcome screen is in view
  app$setInputs(graphtest_shiny = "welcome")
  
  # select appropriate session
  # TODO: learn how to perform tidyeval so that we can dynamically
  # use the module_id param above to construct the proper input name
  #input_name <- paste(module_id, "sessiondatatable_rows_shinytest", sep = "-")
  #app$setInputs(!!enquo(input_name) := session_name)
  
  app$setInputs(`first-sessiondatatable_rows_shinytest` = session_name)
  Sys.sleep(1)
  
  # activate session
  app$setInputs(`first-activate_session` = "click", wait_ = TRUE, values_ = TRUE, timeout_ = 480000)
  
  invisible(TRUE)
}

#' Deactivate the session currently active in MOATR
#'
#' @param app A `ShinyDriver` object
#' @param session_dirname String for directory name of MOATR session files.
#'   Defaults to `.moatr_files_test` since that is the custom directory
#'   used for testing the app in `shinytest`.
#' @param module_id Namespace ID of the welcome module. Default is `first`
#'
#' @return
#' @export
#'
#' @examples
deactivate_session <- function(app, 
                               session_dirname = ".moatr_files_test", 
                               module_id = "first") {
  
  # ensure the welcome screen is in view
  app$setInputs(graphtest_shiny = "welcome")
  Sys.sleep(1)
  
  # deactivate session
  app$setInputs(`first-deactivate_session` = "click", wait_ = TRUE, values_ = TRUE, timeout_ = 480000)
  
  invisible(TRUE)
}

#' Update session directory and HPC workdir paths in pre-work test session directories
#' 
#' This function is necessary because if you use one of the test scripts interactive to
#' generate session files and copy those to the test session source directory, it will
#' have the session directory and HPC workdir path from that interactive session. When
#' using those files in a "new" test that builds upon those files, there will be a mismatch
#' in those directory paths. This function simply updates the appropriate slots in the 
#' `r_data` list object with the correct paths.
#'
#' @param testname String with name of test session directory
#' @param version_number MOATR version number
#' @param session_id String with the pre-saved session ID that is used in the test
#' @param session_config_file YAML config file with settings for each test name
#'
#' @return Nothing
#' @export
#'
#' @examples
#' library(purrr)
#' test_names <- c("test_design", "test_demo", "test_simulate_upload",
#' "test_simulate_schemeconfig", "test_simulate_hpc", "test_postprocess_summary",
#' "test_postprocess_combo", "test_postprocess_value")
#'
#'walk(test_names, ~update_rdata(.x))
update_rdata <- function(testname, 
                         version_number = "2",
                         session_id = "testsession1",
                         session_config_file = "session-config.yml") {
  
  message(glue::glue("updating {testname} session files..."))
  #testsession_source <- file.path("/lrlhps/data/moatr_files", version_number)
  
  # load settings from session-config.yml
  session_config <- config::get(config = testname, file = here::here(session_config_file))
  
  # define path to r_data file of test session source files
  testsession_rdata <- file.path(session_config$parent_dir, session_config$session_dirname, session_id, "settings.RData")
  
  # load existing settings file and update following slots:
  # - session_dir: "/lrlhps/users/c084511/.moatr_test_design/.moatr_files_test"
  # - hpc_workdir: "/lrlhps/users/c084511/.moatr_test_design/.moatr_files_test/hpc_workdir_1.3.4"
  
  load(testsession_rdata)
  
  # use session_config$parent_dir and session_config$session_dirname as beginning of path
  r_data$session_dir <- file.path(session_config$parent_dir, session_config$session_dirname)
  r_data$hpc_workdir <- file.path(session_config$parent_dir, session_config$session_dirname, paste0("hpc_workdir_", version_number))
  
  # re-save the updated version to settings file
  save(r_data, r_state, file = testsession_rdata)
  
  invisible(TRUE)
}

fill_teststep <- function(testname, 
                          step_name,
                          session_config = "session-config.yml",
                          template_file = "test_step_template.md") {
  
  
  # import the configuration information for the specified test name and step
  session_settings <- config::get(config = testname, file = here::here(session_config))
  res <- purrr::pluck(session_settings$test_steps, step_name)
  
  # define paths to key directories
  screenshot_dir <- here::here("tests", paste(testname, "screenshots", sep = "-"))
  log_dir <- here::here("tests", paste(testname, "logs", sep = "-"))
  file_dir <- here::here("tests", paste(testname, "files", sep = "-"))
  
  # load template file text
  template_string <- readChar(here::here("templates", template_file), file.info(here::here("templates", template_file))$size)
  
  # define snippet for screenshot insertion
  screenshot_filled <- ''
  
  if (res$screenshot_file[1] != '') {
    screenshot_snippet <- "
    ```{{r, eval=TRUE, echo=FALSE, out.width='100%', fig.align='center'}}
    knitr::include_graphics('{screenshot_file}')
    xfun::embed_file('{screenshot_file}', text = 'click to view original image file')
    ```

    "
    
    n_screenshots <- length(res$screenshot_file)
    
    screenshot_filled <- purrr::map_chr(1:n_screenshots, ~{
      glue::glue(screenshot_snippet, 
                 screenshot_file = fs::path(screenshot_dir, res$screenshot_file[.x]))
    })
    
    if (n_screenshots > 1) {
      screenshot_filled <- glue::collapse(screenshot_filled, sep = "\n")
    }
    screenshot_filled <- paste0("### Screenshots\n\n", screenshot_filled)
  }
  
  # define snippet for file insertion
  # see https://yihui.name/en/2018/07/embed-file/
  file_filled <- ''
  if ("download_file" %in% names(res)) {
    file_snippet <- "
    ### Attached Files

    ```{{r, echo=FALSE}}
    xfun::embed_file('{download_file}')
    ```

    "
    file_filled <- glue::glue(file_snippet, download_file = fs::path(file_dir, res$download_file))
  }
  
  # populate remaining portions of template
  input_env <- list2env(
    list(
      description = res$description,
      title = res$title,
      expected_result = res$expected_result,
      screenshot_snippet = screenshot_filled,
      file_snippet = file_filled
    )
  )
  
  template_filled <- glue::glue(template_string, .envir = input_env)
  
  return(template_filled)
}

fill_testreport <- function(testname, session_config = "session-config.yml") {
  # import the configuration information for the specified test name
  #browser()
  session_settings <- config::get(config = testname, file = here::here(session_config))
  test_label <- session_settings$test_label
  
  # extract the step names and make a simple data frame for use with purrr
  input_df <- tibble::tibble(
    testname = testname,
    step_name = names(session_settings$test_steps)
  )
  
  steps_filled <- purrr::pmap_chr(input_df, fill_teststep) %>%
    glue::collapse(., sep = "\n\n")
  
  # create snippet for template of all report steps filled
  all_snippet <- "
  # {test_label} {{.tabset .tabset-pills}}

  {steps_filled}
  "
  
  all_filled <- glue::glue(all_snippet)
  return(all_filled)
}

compile_testreport <- function(session_config = "session-config.yml", 
                               template_report_file = "test_report_template.txt",
                               version_number = "2",
                               verbose = FALSE) {
  
  library(magrittr)
  
  # set up temp file for R markdown report source
  report_file <- fs::file_temp(pattern = "moatr_test_", ext = ".Rmd")
  
  # import the configuration information
  session_settings <- config::get(file = here::here(session_config))
  
  # obtain all of the test names
  testnames_all <- session_settings$test_names
  
  # generate test report body content
  testcontent_filled <- purrr::map_chr(testnames_all, ~fill_testreport(testname = .x)) %>%
    glue::collapse(., sep = '\n\n')
  
  # load template file text
  template_string <- readChar(here::here("templates", template_report_file), 
                              file.info(here::here("templates", template_report_file))$size)
  
  # populate template report source file
  template_filled <- glue::glue(template_string)
  cat(template_filled, file = report_file)
  
  # use rmarkdown::render to compile report
  rmarkdown::render(report_file, 
                    output_format = "html_document", 
                    output_file = here::here("moatr_report.html"),
                    quiet = TRUE)
  
  if (verbose) message(glue::glue("report is ready to view!  Download moatr_report.html"))
  
}







# <button class="btn btn-box-tool" data-widget="collapse">
# Not finished!
expand_all <- function(app, xml_obj) {
  button_list <- xml2::xml_find_all(xml_obj, './/button[@class="btn btn-box-tool"]') 
  for (k in 1:length(button_list)) {
    click_run_args_list <- list(run_button_id = 'click', wait_ = FALSE, values_ = FALSE)
    
    names(click_run_args_list)[1] <- run_button_id
    do.call(app$setInputs, click_run_args_list)
  }
  
  return(app)
}

