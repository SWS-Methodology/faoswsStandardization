##'
##' **Author: Carlo Del Bello**
##'
##' **Description:**
##'
##' This module is designed to identify outliers in DES at sua balanced level
##' 
##' 
##' **Inputs:**
##'
##' * sua balanced


## load the libraries
library(faosws)
library(data.table)
library(faoswsUtil)
library(sendmailR)
library(dplyr)
library(faoswsFlag)


## set up for the test environment and parameters
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")

if(CheckDebug()){
  message("Not on server, so setting up environment...")
  
  library(faoswsModules)
  SETT <- ReadSettings("modules/outlierDetection/sws.yml")
  
  R_SWS_SHARE_PATH <- SETT[["share"]]  
  ## Get SWS Parameters
  SetClientFiles(dir = SETT[["certdir"]])
  GetTestEnvironment(
    baseUrl = SETT[["server"]],
    token = SETT[["token"]]
  )
}

startYear = 2013
endYear = 2016
geoM49 = swsContext.computationParams$geom49
stopifnot(startYear <= endYear)
yearVals = startYear:endYear

##' Get data configuration and session
sessionKey = swsContext.datasets[[1]]

sessionCountries =
  getQueryKey("geographicAreaM49", sessionKey)

geoKeys = GetCodeList(domain = "agriculture", dataset = "aproduction",
                      dimension = "geographicAreaM49")[type == "country", code]

top48FBSCountries = c(4,24,50,68,104,120,140,144,148,1248,170,178,218,320,
                      324,332,356,360,368,384,404,116,408,450,454,484,508,
                      524,562,566,586,604,608,716,646,686,762,834,764,800,
                      854,704,231,887,894,760,862,860)

# top48FBSCountries<-as.character(top48FBSCountries)
# 
# selectedCountries = setdiff(geoKeys,top48FBSCountries) #229
# 


# ##Select the countries based on the user input parameter
selectedGEOCode =
  switch(geoM49,
         "session" = sessionCountries,
         "all" = geoKeys)





#########################################
##### Pull from SUA unbalanced data #####
#########################################

message("Pulling SUA Unbalanced Data")

#take geo keys
geoDim = Dimension(name = "geographicAreaM49", keys = selectedGEOCode)

#Define element dimension. These elements are needed to calculate net supply (production + net trade)

eleKeys = GetCodeList(domain = "suafbs", dataset = "sua_balanced", "measuredElementSuaFbs")
eleKeys <-eleKeys[, code]

eleDim <- Dimension(name = "measuredElementSuaFbs", keys = eleKeys)


#Define item dimension

itemKeys = GetCodeList(domain = "suafbs", dataset = "sua_balanced", "measuredItemFbsSua")
itemKeys = itemKeys[, code]

itemDim <- Dimension(name = "measuredItemFbsSua", keys = itemKeys)


# Define time dimension

timeDim <- Dimension(name = "timePointYears", keys = as.character(yearVals))

#Define the key to pull SUA data
key = DatasetKey(domain = "suafbs", dataset = "sua_balanced", dimensions = list(
  geographicAreaM49 = geoDim,
  measuredElementSuaFbs = eleDim,
  measuredItemFbsSua = itemDim,
  timePointYears = timeDim
))



sua_balanced_data = GetData(key)

sua_balanced_data <- subset(sua_balanced_data, measuredElementSuaFbs %in% c("664"))

sua_balanced_data <- sua_balanced_data[,perc.change:= Value/shift(Value, type="lead")-1, by=c("geographicAreaM49","measuredItemFbsSua","measuredElementSuaFbs")]


sua_balanced_data <- subset(sua_balanced_data, (shift(Value, type="lead")>5 | Value>5) & abs(perc.change)>0.1 & timePointYears>2013)

sua_balanced_data<-nameData("suafbs","sua_balanced",sua_balanced_data)


#sua_balanced_data$measuredItemFbsSua <- paste0("'",sua_balanced_data$measuredItemFbsSua,sep = "")



bodySuaBALOutliers= paste("The Email contains a list of items where the caloric intakes increases more than 10% in abslolute value.",
                          "Consider it as a list of items where to start the validation.",
                          sep='\n')

sendMailAttachment(sua_balanced_data,"SuaBALOutliers",bodySuaBALOutliers)

