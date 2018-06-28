#This script overwrites the food value for "food Residual"  only if the figures are unprotected. 


#First, it takes into consideration food values of food residual items in sua unbalanced table. 
#Then,it selects only the unprotected food figures with the help of the flag validation  table. 
#Next, it overwrites those food estimates with the new food estimates computed with the updated production and trade data as
#netSupply = (prodcution + import - export). Then, net supply becomes the new estimated food residual. Finally, these updated data are overwritten in sua unbalanced table.


## load the libraries
library(faosws)
library(data.table)
library(faoswsUtil)
library(sendmailR)


## set up for the test environment and parameters
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")

if(CheckDebug()){
  message("Not on server, so setting up environment...")
  
  library(faoswsModules)
  SETT <- ReadSettings("modules/FoodResidualComputation//sws.yml")
  
  R_SWS_SHARE_PATH <- SETT[["share"]]  
  ## Get SWS Parameters
  SetClientFiles(dir = SETT[["certdir"]])
  GetTestEnvironment(
    baseUrl = SETT[["server"]],
    token = SETT[["token"]]
  )
}

startYear = 2014
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

selectedCountries = setdiff(geoKeys,top48FBSCountries)



##Select the countries based on the user input parameter
selectedGEOCode =
  switch(geoM49,
         "session" = sessionCountries,
         "all" = selectedCountries)





#########################################
##### Pull from SUA unbalanced data #####
#########################################

message("Pulling SUA Unbalanced Data")

#take geo keys
geoDim = Dimension(name = "geographicAreaM49", keys = selectedGEOCode)

#Define element dimension. These elements are needed to calculate net supply (production + net trade)

eleDim <- Dimension(name = "measuredElementSuaFbs", keys = c("5510", "5610", "5910","5141"))


#Define item dimension

itemKeys = GetCodeList(domain = "suafbs", dataset = "sua_unbalanced", "measuredItemFbsSua")
itemKeys = itemKeys[, code]

itemDim <- Dimension(name = "measuredItemFbsSua", keys = itemKeys)


# Define time dimension

timeDim <- Dimension(name = "timePointYears", keys = as.character(yearVals))

#Define the key to pull SUA data
key = DatasetKey(domain = "suafbs", dataset = "sua_unbalanced", dimensions = list(
  geographicAreaM49 = geoDim,
  measuredElementSuaFbs = eleDim,
  measuredItemFbsSua = itemDim,
  timePointYears = timeDim
))



#Pull SUA Data

suaData = GetData(key)

#Pull only food data in order to create a dataframe with the CPC COdes for food
foodData=subset(suaData, measuredElementSuaFbs == "5141")
setnames(foodData,"Value", "food")



#Since the food classification table contains data for all countries, it is not necessary to pull data for all countries unless or otheriwse  selected geo codes are "all".
#Prepare geo codes to pull only food classification for the countries the user defines. 
#This saves a lot of time.

# geoCodes_classification <- copy(selectedGEOCode)%>%
#   as.character %>%
#     shQuote(type = "sh") %>%
#     paste0(collapse = ", ")





# Pull food  classifications only for the countries the user defines. 
# food_classification_country_specific <- ReadDatatable("food_classification_country_specific", where = paste0("geographic_area_m49 IN (", geoCodes_classification, ")"))
food_classification_country_specific <- ReadDatatable("food_classification_country_specific")


setnames(food_classification_country_specific, 
         old = c("geographic_area_m49", "measured_item_cpc", "food_classification"),
         new = c("geographicAreaM49", "measuredItemFbsSua", "foodClassification"))


#Merge food data and classification table

foodData <- merge(foodData, food_classification_country_specific, by = c("geographicAreaM49", "measuredItemFbsSua"),all.x = T)



setnames(foodData, "foodClassification", "type")

foodData <- foodData[type %in% c("Food Residual")]


keys = c("flagObservationStatus", "flagMethod")
foodDataMerge <- merge(foodData, flagValidTable, by = keys, all.x = T)


# Discussed in a meeting: change from M- to Mu
foodDataMerge[flagObservationStatus == "M" & flagMethod == "-", flagMethod := "u"]


## Checking countries with zero food figures
checkTotFood = foodDataMerge[, list(totFood = sum(food)), 
                             by = list(geographicAreaM49, timePointYears)]


checkTotFood = nameData("food", "fooddatafs", checkTotFood)
# checkTotFood[totFood == 0, .N, c("geographicAreaM49", "geographicAreaM49_description")]
# checkTotFood[timePointYears %in% referenceYearRange & totFood == 0, .N, geographicAreaM49]
excludeCountry = unique(checkTotFood[totFood == 0]$geographicAreaM49)
foodDataMerge = foodDataMerge[!(geographicAreaM49 %in% excludeCountry)]


## Create time series data set for the calculations 
timeSeriesData <- as.data.table(expand.grid(timePointYears = as.character(startYear:endYear),
                                            geographicAreaM49 = unique(foodDataMerge$geographicAreaM49),
                                            measuredItemFbsSua = unique(foodDataMerge$measuredItemFbsSua)))


timeSeriesData <- merge(timeSeriesData,food_classification_country_specific, by = c("geographicAreaM49","measuredItemFbsSua"), all.x = TRUE)


setnames(timeSeriesData, "foodClassification", "type")


timeSeriesData <- subset(timeSeriesData, type == "Food Residual")



timeSeriesData <- merge(timeSeriesData, foodDataMerge, all.x = T,
                        by = c("geographicAreaM49", "timePointYears", "measuredItemFbsSua", "type"))

timeSeriesData[, measuredElementSuaFbs := "5141"]



#pull trade data 
tradeData <- subset(suaData, measuredElementSuaFbs %in% c("5610","5910") )

tradeData[, c("flagObservationStatus","flagMethod"):= NULL]


tradeData <- dcast.data.table(tradeData, geographicAreaM49 + measuredItemFbsSua +
                                timePointYears ~ measuredElementSuaFbs, value.var = "Value")



setnames(tradeData, "5610", "imports")
setnames(tradeData, "5910", "exports")


tradeData[is.na(imports), imports := 0]
tradeData[is.na(exports), exports := 0]
tradeData[, netTrade := (imports - exports)]


## Merge timeseries data  and tradedata
keys <- c("geographicAreaM49", "timePointYears", "measuredItemFbsSua")
timeSeriesData <- merge(timeSeriesData, tradeData, by = keys, all.x = T)
timeSeriesData[is.na(netTrade), netTrade := 0]
timeSeriesData[is.na(imports), imports := 0]
timeSeriesData[is.na(exports), exports := 0]




#pull production data
productionData <- subset(suaData,measuredElementSuaFbs == "5510" )

productionData[, c("measuredElementSuaFbs", "flagObservationStatus", "flagMethod") := NULL]



#merge production and timeseries data

keys = c("geographicAreaM49", "measuredItemFbsSua", "timePointYears")
timeSeriesData = merge(timeSeriesData, productionData, by = keys,
                       all.x = T)
setnames(timeSeriesData, "Value", "production")

timeSeriesData[is.na(production), production := 0]
timeSeriesData[, netSupply := netTrade + production]

timeSeriesData[is.na(Protected), Protected := FALSE]


#Compute new food residual estimate based on food supply. The calcualtions are done only for the unprotected food figures. 

timeSeriesData[Protected == FALSE  & netSupply > 0, 
               foodHat:= netSupply]

timeSeriesData[Protected == FALSE  & netSupply <= 0, 
               foodHat := 0]

# Restructure and filter data to save in SWS 


cat("Restructure and filter data to save in SWS...\n")

dataTosave <- timeSeriesData[!is.na(foodHat)]

dataTosave <- dataTosave[,c("geographicAreaM49","measuredItemFbsSua", "timePointYears","measuredElementSuaFbs", "foodHat"),with = FALSE]

setnames(dataTosave, "foodHat", "Value")

dataTosave[ , flagObservationStatus := "I"]

dataTosave[, flagMethod := "i"]




setcolorder(dataTosave, c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemFbsSua",
                          "timePointYears", "Value", "flagObservationStatus", "flagMethod"))



# Save final data to SWS
cat("Save the final data...\n")

stats = SaveData(domain = "suafbs", dataset = "sua_unbalanced", data = dataTosave, waitTimeout = 1800)

paste0("Food Residual are over-written with the updated values!!! ")
