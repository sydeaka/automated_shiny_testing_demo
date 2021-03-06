# Install renv
install.packages('renv')

# Initialize renv
renv::init(bare=TRUE)

# Install golem package
renv::install('golem')
renv::status()

# Create abd configure golem
package_name <- 'automated.testing.ice.cream'
project_path <- file.path(getwd())

golem::create_golem(path=project_path, 
                    package_name = package_name, 
                    check_name=FALSE,
                    overwrite = TRUE)
golem::set_golem_name(package_name)
golem::add_rstudioconnect_file()

list_packages <- c(
  'shiny', 
  'shinytest', 
  'shinytest2', 
  'shinyvalidate', 
  'dplyr', 
  'shinydashboard', 
  'shinydashboardPlus', 
  'fresh', 
  'ggplot2', 
  'plotly', 
  'logger', 
  'rvest', 
  'here', 
  'devtools', 
  'packrat', 
  'rsconnect', 
  "emo", # "hadley/emo", 
  'usethis', 
  'attachment', 
  #'covrpage', 
  'rhub', 
  'rmarkdown', 
  'RSelenium'#, 
  #'wdman' 
)


# Install packages
renv::install('shiny')
renv::install('shinytest')
renv::install('shinytest2')
renv::install('shinyvalidate')
renv::install('dplyr')
renv::install('shinydashboard')
renv::install('shinydashboardPlus')
renv::install('fresh')
renv::install('ggplot2')
renv::install('plotly')
renv::install('logger')
renv::install('rvest')
renv::install('here')
renv::install('devtools')
renv::install('packrat')
renv::install('rsconnect')
renv::install("hadley/emo")
renv::install('usethis')
renv::install('attachment')
renv::install('covrpage')
renv::install('rhub')
renv::install('rmarkdown')
renv::install('RSelenium')
renv::install('wdman')
#renv::install('rsconnect')
#renv::install('')
#renv::install('')

shinytest::installDependencies()

# Create a snapshot (update the lock file)
renv::snapshot()




for (pkg in list_packages) {
  usethis::use_package(pkg)
}

# Create directories for automated testing
source('R/utils.R')
create_dir('shinytest-logs')
create_dir('tests')
create_dir('inst/automated_testing')

# Create .Renviron
#usethis::edit_r_environ(scope = "project")

# Set GitHub Personal Access Token (PAT)
# Copy it to the clipboard and paste it when prompted
gitcreds::gitcreds_set()


