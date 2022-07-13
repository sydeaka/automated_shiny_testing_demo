

take_screenshot <- function(remDr, image_folder) {
  # display the image
  remDr$screenshot(display=TRUE)
  
  # save the image to disk
  nfiles <- length(list.files(image_folder)) + 1
  filename <- file.path(image_folder, paste0('screenshot_', sprintf("%03d", nfiles), '.png'))
  remDr$screenshot(file=filename)
}
