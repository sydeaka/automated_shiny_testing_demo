library(RSelenium)
#library(rvest)
#library(magrittr)

# Start the Selenium server via Docker commands
port = 4545
docker_hash <- system(paste0("docker run -d -p ", port, ":4444 selenium/standalone-firefox:2.53.1"), 
                      intern=TRUE)


# Start Selenium Session
remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = port,
  browserName = "firefox")


remDr$open()





remDr$close()