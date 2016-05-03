context("Settings files")

sname <- "sws.yml"

## Helper functions

# Use project path instead of test path
cp <- function(path){
  paste0("../../", path)
}

# Add .example to settings files
addEx <- function(name, extension = ".example"){
  paste0(name, extension)
}

# Check if settings file exists
setExists <- function(name){
  file.exists(cp(name))
}

# Test to see if example file is missing
settingHasNoExample <- function(file){
  setExists(file) && !setExists(addEx(file))
}

# Get names of fields in settings file
getSettingFields <- function(name){
  names(read.dcf(cp(name))[1,])
}

## TESTS

test_that("Settings file is never present without example", {
  expect_false(settingHasNoExample(sname))
})
