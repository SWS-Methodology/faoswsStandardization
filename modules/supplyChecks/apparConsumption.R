

## load the libraries
library(faosws)
library(data.table)
library(faoswsUtil)
library(sendmailR)
library(dplyr)
library(faoswsFlag)
library(faoswsStandardization)


## set up for the test environment and parameters
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")

if(CheckDebug()){
  message("Not on server, so setting up environment...")
  
  library(faoswsModules)
  SETT <- ReadSettings("modules/supplyChecks/sws.yml")
  
  R_SWS_SHARE_PATH <- SETT[["share"]]  
  ## Get SWS Parameters
  SetClientFiles(dir = SETT[["certdir"]])
  GetTestEnvironment(
    baseUrl = SETT[["server"]],
    token = SETT[["token"]]
  )
}

# Value below which supply must fall in order to be a case to be investigated
THRESHOLD <- -1000


#startYear = as.numeric(swsContext.computationParams$startYear)
#startYear = as.numeric(2014)

#endYear = as.numeric(swsContext.computationParams$endYear)
#endYear = as.numeric(2016)

startYear <- 2010
endYear <- 2017 # TODO: parameterise
geoM49 <- swsContext.computationParams$geom49
stopifnot(startYear <= endYear)
yearVals <- startYear:endYear

##' Get data configuration and session
sessionKey <- swsContext.datasets[[1]]

sessionCountries <- getQueryKey("geographicAreaM49", sessionKey)

geoKeys <- GetCodeList(domain = "agriculture", dataset = "aproduction",
                      dimension = "geographicAreaM49")[type == "country", code]

top48FBSCountries = c(4,24,50,68,104,120,140,144,148,1248,170,178,218,320,
                      324,332,356,360,368,384,404,116,408,450,454,484,508,
                      524,562,566,586,604,608,716,646,686,762,834,764,800,
                      854,704,231,887,894,760,862,860)

# top48FBSCountries<-as.character(top48FBSCountries)
# 
# selectedCountries = setdiff(geoKeys,top48FBSCountries) #229
# 

commodity_table <- ReadDatatable("fbs_commodity_definitions")

stopifnot(nrow(commodity_table) > 0)

# ##Select the countries based on the user input parameter
selectedGEOCode <-
  switch(
    geoM49,
    "session" = sessionCountries,
    "all"     = geoKeys
  )

#########################################
##### Pull from SUA unbalanced data #####
#########################################

#message("Pulling SUA Unbalanced Data")

# Take geo keys

geoDim <- Dimension(name = "geographicAreaM49", keys = selectedGEOCode)

# Define element dimension. These elements are needed to calculate net supply (production + net trade)

eleKeys <- GetCodeList(domain = "suafbs", dataset = "sua_unbalanced", "measuredElementSuaFbs")$code

eleDim <- Dimension(name = "measuredElementSuaFbs", keys = eleKeys)

# Define item dimension

itemKeys <- GetCodeList(domain = "suafbs", dataset = "sua_unbalanced", "measuredItemFbsSua")$code

itemDim <- Dimension(name = "measuredItemFbsSua", keys = itemKeys)

# Define time dimension

timeDim <- Dimension(name = "timePointYears", keys = as.character(yearVals))

# Define the key to pull SUA data

key <- DatasetKey(domain = "suafbs", dataset = "sua_unbalanced", dimensions = list(
  geographicAreaM49 = geoDim,
  measuredElementSuaFbs = eleDim,
  measuredItemFbsSua = itemDim,
  timePointYears = timeDim
))

sua_data_full <- GetData(key)

sua_data <-
  sua_data_full[
    measuredElementSuaFbs %in% c("5510","5610","5910","664")
  ][
    is.na(Value),
    Value := 0
  ][,
    oldprod := sum(Value[as.numeric(timePointYears) < 2014 & measuredElementSuaFbs == "5510"]),
    .(geographicAreaM49, measuredItemFbsSua, measuredElementSuaFbs)
  ][,
    drop := any(oldprod > 0),
    .(geographicAreaM49, measuredItemFbsSua)
  ][
    drop == FALSE & timePointYears > 2013
  ][,
    c("flagObservationStatus","flagMethod") := NULL
  ]


sua_data <-
  dcast.data.table(
    sua_data,
    geographicAreaM49 + measuredItemFbsSua + timePointYears ~ measuredElementSuaFbs,
    value.var = "Value",
    fill = 0
  )

setnames(sua_data, c("5510","5610","5910"), c("production","imports", "exports"))

sua_data[, supply := production + imports - exports]

## livestock cpc to exclude
livestock <- c("02111","02112","02121.01","02122","02123","02131","02132","02133","02140","02151","02152","02153","02154","02191","02194",
              "02196","02199.10","02199.20")

primary <- commodity_table[primary_commodity == "X", cpc]

apparentConsumption <-
  sua_data[
    timePointYears %in% 2014:endYear &
      !(measuredItemFbsSua %in% livestock) &
      supply < THRESHOLD &
      measuredItemFbsSua %in% primary
  ]

#apparentConsumption<- apparentConsumption[,c("geographicAreaM49","measuredItemFbsSua","timePointYears"),with=FALSE]

setcolorder(apparentConsumption, c("geographicAreaM49", "measuredItemFbsSua", "timePointYears", "supply", "production", "imports", "exports"))

apparentConsumption <- nameData("suafbs", "sua_unbalanced", apparentConsumption, except = "timePointYears")

#### send mail
bodyApparent <- paste("The Email contains a list of negative apparent consumptions",
                      "at sua unbalanced level. Negative values higher than",
                      THRESHOLD, "have been filtered out.")

sendMailAttachment(apparentConsumption, "apparentConsumption", bodyApparent)

#apparent consumption is the list of commodities,countries and years where supply is negative.


print("Module ran successfully. Check your e-mail.")
