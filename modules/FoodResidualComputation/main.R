#This script overwrites the food value for "food Residual" (given the item is not a priamry item)  only if the figures are unprotected. Otherwise, figures are
#not touched. 


#First, it takes into consideration food values of food residual items in sua unbalanced table. 
#Then,it selects only the unprotected food figures with the help of the flag validation  table. 
#Next, it overwrites those food estimates with the new food estimates computed with the updated production, trade and utilization elements data as
#foodHat = (prodcution + import - export- (feed+seed+tourist+loss+industrial+stock)). Then, foodHat becomes the new estimated food residual. Finally, these updated data are overwritten in sua unbalanced table.


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
  SETT <- ReadSettings("modules/FoodResidualComputation//sws.yml")
  
  R_SWS_SHARE_PATH <- SETT[["share"]]  
  ## Get SWS Parameters
  SetClientFiles(dir = SETT[["certdir"]])
  GetTestEnvironment(
    baseUrl = SETT[["server"]],
    token = SETT[["token"]]
  )
}

startYear1 = 2004
startYear=2014
endYear = 2016
geoM49 = swsContext.computationParams$geom49
stopifnot(startYear <= endYear)
yearVals = startYear:endYear
yearVals1 = startYear1:endYear


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

top48FBSCountries<-as.character(top48FBSCountries)

selectedCountries = setdiff(geoKeys,top48FBSCountries) #229



# ##Select the countries based on the user input parameter
# selectedGEOCode =
#   switch(geoM49,
#          "session" = sessionCountries,
#          "all" = selectedCountries)

selectedGEOCode = sessionCountries



#########################################
##### Pull from SUA unbalanced data #####
#########################################

message("Pulling SUA Unbalanced Data")

#take geo keys
geoDim = Dimension(name = "geographicAreaM49", keys = selectedGEOCode)

#Define element dimension. These elements are needed to calculate net supply (production + net trade)

eleDim <- Dimension(name = "measuredElementSuaFbs", keys = c("5510", "5610", "5071", "5023",
                                                             "5910", "5016", "5165", "5520","5525","5164","5166","5141"))


#Define item dimension

itemKeys = GetCodeList(domain = "suafbs", dataset = "sua_unbalanced", "measuredItemFbsSua")
itemKeys = itemKeys[, code]

itemDim <- Dimension(name = "measuredItemFbsSua", keys = itemKeys)


# Define time dimension

timeDim <- Dimension(name = "timePointYears", keys = as.character(yearVals))
timeDim1 <- Dimension(name = "timePointYears", keys = as.character(yearVals1))


#Define the key to pull SUA data
key = DatasetKey(domain = "suafbs", dataset = "sua_unbalanced", dimensions = list(
  geographicAreaM49 = geoDim,
  measuredElementSuaFbs = eleDim,
  measuredItemFbsSua = itemDim,
  timePointYears = timeDim1
))



#Pull SUA Data

suaData = GetData(key)
suaData=setDT(suaData)
suaData[measuredElementSuaFbs==5164,Value:=0]
suaData[measuredElementSuaFbs==5164, flagObservationStatus:="E"]
suaData[measuredElementSuaFbs==5164, flagMethod:="e"]

touristData=suaData[measuredElementSuaFbs==5164]


## pull trade data and calculate net exports
shareData <- subset(suaData, (measuredElementSuaFbs %in% c("5610","5910","5141","5510")) & timePointYears<2014  )

shareData[, c("flagObservationStatus","flagMethod"):= NULL]


shareData <- dcast.data.table(shareData, geographicAreaM49 + measuredItemFbsSua +
                                timePointYears ~ measuredElementSuaFbs, value.var = "Value")



setnames(shareData, "5610", "imports")
setnames(shareData, "5910", "exports")
setnames(shareData, "5141", "food")
setnames(shareData, "5510", "production")




shareData[is.na(imports), imports := 0]
shareData[is.na(exports), exports := 0]
shareData[is.na(food), food := 0]

shareData[, netTrade := (exports - imports)]
shareData[,share:=food/imports]
shareData[is.infinite(share), share := NA]

shareData=setDT(shareData)
shareData[, medshare := median(share,na.rm = T), by=c("measuredItemFbsSua")]
shareData[, medfood := median(food,na.rm = T), by=c("measuredItemFbsSua")]
shareData[, noProd := median(production,na.rm = T), by=c("measuredItemFbsSua")]


shares=shareData[,list(share=unique(medshare), noProd=unique(noProd)), by=measuredItemFbsSua]



#Pull only food data in order to create a dataframe with the CPC COdes for food
foodData=subset(suaData, measuredElementSuaFbs == "5141" & timePointYears %in% yearVals)
setnames(foodData,"Value", "food")



#Since the food classification table contains data for all countries, it is not necessary to pull data for all countries unless or otheriwse  selected geo codes are "all".
#Prepare geo codes to pull only food classification for the countries the user defines. 
#This saves a lot of time.

geoCodes_classification <- copy(selectedGEOCode)%>%
  as.character %>%
    shQuote(type = "sh") %>%
    paste0(collapse = ", ")





# Pull food  classifications only for the countries the user defines. 
food_classification_country_specific <- ReadDatatable("food_classification_country_specific", where = paste0("geographic_area_m49 IN (", geoCodes_classification, ")"))
# food_classification_country_specific <- ReadDatatable("food_classification_country_specific")


setnames(food_classification_country_specific, 
         old = c("geographic_area_m49", "measured_item_cpc", "food_classification"),
         new = c("geographicAreaM49", "measuredItemFbsSua", "foodClassification"))




# Define all primary and proxy primaries. This includes no parent items, cutitems and zero proessing levels and also primaries among orphans.
#Further, orphans are defined as the items who are neither in tree (as a child or parent) nor cut items (proxy primary). But an orphan can be a primary.




# 
# 
# primaryProxyPrimary <- c("24220", "21529.03", "21523", "2351f", "23670.01", "01447", "2161", "2162", "21631.01", "21641.01", "21641.02", "2168", "21691.14", 
#   "2165", "2166", "21691.07", "2167", "21673", "21691.01", "21691.02", "21631.02", "21691.03", "21691.04", "21691.05", "21691.06", "21691.08", 
#   "21691.09", "21691.10", "21691.11", "21691.12", "21691.13", "21691.90", "23620", "34550", "21693.03", "24212.02", "24310.01", "24230.01", 
#   "24230.02", "24230.03", "24310.02", "24310.03", "24310.04", "22241.01","22241.02", "22242.01", "22242.02", "22249.01", "22249.02", "22120", 
#   "2413", "23991.01", "24110", "23511.02", "21700.01", "21700.02","34120", "21932.02", "0111", "0112", "0113", "0114", "0115", 
#   "0116", "0117", "0118", "01191", "01192", "01193", "01194", "01195","01199.02", "01199.90", "01211", "01212", "01213", "01215", "01216", 
#   "01221", "01231", "01232", "01233", "01234", "01235", "01241.01","01241.90", "01242", "01243", "01251", "01252", "01253.01", "01253.02", 
#   "01270", "01290.01", "01290.90", "01311", "01312", "01313", "01314","01315", "01316", "01318", "01319", "01321", "01322", "01323", 
#   "01324", "01329", "01330", "01341", "01342.01", "01342.02", "01343","01344.01", "01344.02", "01345", "01346", "01349.20", "01351.02", 
#   "01351.01", "01353.01", "01354", "01355.02", "01355.90", "01359.90","01371", "01372", "01374", "01375", "01376", "01377", "01379.90", 
#   "0141", "0142", "01441", "01442", "01443", "01444", "01445","01446", "01449.01", "01449.02", "01449.90", "01450", "01460", 
#   "01491.01", "01499.01", "01499.02", "01499.04", "01499.05", "01510","01520.01", "01530", "01550", "01599.10", "01610", "01640", "01691", 
#   "01701", "01702", "01703", "01704", "01705", "01707", "01709.01", "01709.90", "01801", "01802", "01809", "01921.01", "01930.02", 
#   "01950.01", "02211", "02212", "02291", "02292", "0231", "02910","02951.01", "02951.03", "02952.01", "02953", "02954", "21111.01", 
#   "21113.01", "21121", "21123", "21124", "21151", "21170.02", "21170.92", "21511.01", "21512", "21513", "21514", "21515", "01591", "01540", 
#   "01706", "01708", "01709.02", "01373", "01379.02", "01379.01","01499.03", "01448", "01214", "01219.01", "01254", "01239.01", 
#   "01356", "01349.10", "01355.01", "01229", "01359.01", "01359.02", "01352", "01317", "01620", "01630", "01651", "01652", "01656", 
#   "01658", "01655", "01653", "01654", "01657", "01699", "21112","21115", "21116", "21122", "21170.01", "21118.01", "21118.02", 
#   "21118.03", "21117.01", "21114", "21119.01", "21117.02", "02920","21152", "21155", "21156", "21153", "21160.03", "21159.01", "21159.02", 
#   "21519.02", "21519.03", "0232", "02293")
commDef=ReadDatatable("fbs_commodity_definitions")
primaryProxyPrimary=commDef$cpc[commDef[,proxy_primary=="X" | primary_commodity=="X"]]
primary=commDef$cpc[commDef[,primary_commodity=="X"]]


proxyPrimary=commDef$cpc[commDef[,proxy_primary=="X" | derived=="X"]]




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


checkTotFood = nameData("suafbs", "sua_unbalanced", checkTotFood)
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


################## CARLO

keys = c("measuredItemFbsSua")

timeSeriesData <- merge(timeSeriesData,shares, by=keys, all.x = TRUE)
timeSeriesData=setDT(timeSeriesData)

timeSeriesData[food==0 & noProd>0, food:=share*imports]  ## CARLO
timeSeriesData[food>0 & is.na(noProd) & netSupply<0, food:=0]  ## CARLO


# Pull other elements stock, feed,seed,loss,industrial and tourist


otherElements <-subset(suaData,measuredElementSuaFbs %in% c("5071","5525","5520","5016","5165","5164") )

otherElements[, c("flagObservationStatus","flagMethod") := NULL]


otherElements <- dcast.data.table(otherElements, geographicAreaM49 + measuredItemFbsSua +
                                   timePointYears ~ measuredElementSuaFbs, value.var = "Value")


setnames(otherElements, c("5016","5071","5164","5165","5520","5525"),
         c("loss","stock","tourist","industrial","feed","seed"))


#merge other elements to the time series table 


keys = c("geographicAreaM49", "measuredItemFbsSua", "timePointYears")

timeSeriesData <- merge(timeSeriesData,otherElements, by=keys, all.x = TRUE)



timeSeriesData[is.na(Protected), Protected := FALSE]



timeSeriesData[, primary := ifelse(measuredItemFbsSua %in% primary, "primary", "not-primary")]



#compute food for non-primary items

#Assign zero for NA before the calculation


cols= c("stock","loss","industrial","feed","seed","tourist")

for (j in cols)
  set(timeSeriesData,which(is.na(timeSeriesData[[j]])),j,0)


timeSeriesData[primary == "not-primary", foodHat_nonprimary := netSupply - (stock+feed+seed+loss+industrial+tourist)]


##########################



#Compute new food residual estimate . The calcualtions are done only for the unprotected food figures. 


# timeSeriesData[Protected == FALSE & primary == "primary" & netSupply > 0, 
#                foodHat:= netSupply]
# 
timeSeriesData[Protected == FALSE & primary == "primary"  & netSupply <= 0, 
               foodHat := food] # changed by carlo


timeSeriesData[Protected == FALSE & primary == "not-primary" & foodHat_nonprimary > 0 , 
               foodHat := foodHat_nonprimary]


timeSeriesData[Protected == FALSE & primary == "not-primary"  & foodHat_nonprimary <= 0, 
               foodHat := food] # changed by carlo

# Restructure and filter data to save in SWS 


cat("Restructure and filter data to save in SWS...\n")

dataTosave <- timeSeriesData[!is.na(foodHat)]

dataTosave <- dataTosave[,c("geographicAreaM49","measuredItemFbsSua", "timePointYears","measuredElementSuaFbs", "foodHat"),with = FALSE]

setnames(dataTosave, "foodHat", "Value")

dataTosave[ , flagObservationStatus := "I"]

dataTosave[, flagMethod := "i"]

dataTosave=rbind(dataTosave,touristData)


setcolorder(dataTosave, c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemFbsSua",
                          "timePointYears", "Value", "flagObservationStatus", "flagMethod"))



# Save final data to SWS
cat("Save the final data...\n")

stats = SaveData(domain = "suafbs", dataset = "sua_unbalanced", data = setDT(dataTosave), waitTimeout = 1800)

paste0("Food Residual are over-written with the updated values!!! ")

