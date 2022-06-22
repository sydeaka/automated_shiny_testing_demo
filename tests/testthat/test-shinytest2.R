library(shinytest2)
library(testthat)

test_that("{shinytest2} recording: automated_shiny_testing_demo", {
  app <- AppDriver$new(variant = platform_variant(), name = "automated_shiny_testing_demo", 
      height = 821, width = 1491)
  app$view()
  app$set_inputs(flavor = "Chocolate")
  app$set_inputs(toppings = c("Peanuts", "Raisins", "Chicken Nuggets"))
  app$set_inputs(serving_size = 2)
  app$set_inputs(recipe_name = "Razzle Dazzle Frazzle")
  app$set_inputs(tabs = "Nutrition Facts")
  app$set_inputs(tabs = "Main")
  app$expect_screenshot()
})
