context("Settings files")

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
  expect_false(settingHasNoExample("settings_qa"))
  expect_false(settingHasNoExample("settings_prod"))
})

test_that("QA examples have same fields as actual settings", {
  if(!(setExists("settings_qa") & setExists(addEx("settings_qa")))){
    skip("QA settings not both available")
  }
  expect_equal(getSettingFields("settings_qa"), getSettingFields(addEx("settings_qa")))
})

test_that("Production examples have same fields as actual settings", {
  if(!(setExists("settings_prod") & setExists(addEx("settings_prod")))){
    skip("Production settings not both available")
  }
  expect_equal(getSettingFields("settings_prod"), getSettingFields(addEx("settings_prod")))
})
