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

# Install packages
renv::install('shiny')
renv::install('shinytest')
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
#renv::install('')
#renv::install('')
#renv::install('')
#renv::install('')

# Create a snapshot (update the lock file)
renv::snapshot()

# Create directories for automated testing
source('R/utils.R')
create_dir('shinytest-logs')
create_dir('tests')
create_dir('inst/automated_testing')
