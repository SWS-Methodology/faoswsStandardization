

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
  SETT <- ReadSettings("modules/supplyChecks/sws.yml")
  
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


suaBalancedData_DES <- subset(sua_balanced_data, measuredElementSuaFbs %in% c("5510","5610","5910","664"))

suaBalancedData_DES[, c("flagObservationStatus","flagMethod") := NULL]

suaBalancedData_DES <- dcast.data.table(suaBalancedData_DES, geographicAreaM49 + measuredItemFbsSua +
                          timePointYears ~ measuredElementSuaFbs, value.var = "Value")



setnames(suaBalancedData_DES, c("5510","5610","5910","664"),c("production","imports", "exports","calories"))


suaBalancedData_DES[is.na(imports), imports := 0]
suaBalancedData_DES[is.na(exports), exports := 0]
suaBalancedData_DES[is.na(production), production := 0]
suaBalancedData_DES[is.na(calories), calories := 0]


suaBalancedData_DES[,supply := production+imports-exports]



#print apparent consumption



apparentConsumption <- copy (suaBalancedData_DES)


apparentConsumption <- subset(apparentConsumption, timePointYears %in% c(2014:2016))


apparentConsumption <- subset(apparentConsumption, supply < 0 & supply>1000)

apparentConsumption<- apparentConsumption[,c("geographicAreaM49","measuredItemFbsSua","timePointYears"),with=FALSE]

apparentConsumption <-nameData("suafbs","sua_unbalanced",apparentConsumption)

apparentConsumption[,timePointYears_description := NULL]

#### send mail
bodyApparent= paste("The Email contains a list of negative apparent consumptions at sua balanced level. It is advisable to check them all one by one and decide course of action.",
                      sep='\n')

sendMailAttachment(apparentConsumption,"apparentConsumption",bodyApparent)


#apparent consumption is the list of commodities,countries and years where supply is negative.




#create lag supply and calories



suaBalancedData_DES = split(suaBalancedData_DES, f = suaBalancedData_DES$geographicAreaM49, drop = TRUE)
suaBalancedData_DES = lapply(suaBalancedData_DES, data.table)

suaBalancedData_lag = list()



for (i in 1:length(suaBalancedData_DES)){
  
  data_lag= data.table(suaBalancedData_DES[[i]])
 
  
  suaBalancedData_lag[[i]] <- data_lag[order(timePointYears),supply_lag:= shift(supply), by= c("measuredItemFbsSua")] 
  suaBalancedData_lag[[i]] <- data_lag[order(timePointYears),calories_lag:= shift(calories), by= c("measuredItemFbsSua")] 
  
  
}




data_lag_final = do.call("rbind",suaBalancedData_lag)


#Assign zero for NA



data_lag_final[is.na(supply_lag), supply_lag := 0]

data_lag_final[is.na(calories_lag), calories_lag := 0]




#Eliminiate year 2013

data_lag_final <- subset(data_lag_final, timePointYears %in% c(2014:2016))



#compute delta supply and delta calories. Ex:  delta_supply_2015 = supply_2015 - supply_2014


data_lag_final[, delta_supply := supply - supply_lag]


data_lag_final[, delta_calories := calories - calories_lag ]



#take cases where delta supply is negative and delta calories are positive



printData <- subset(data_lag_final, delta_supply < 0 & delta_calories > 2 & calories>1 ) 


printcases <- printData[,c("geographicAreaM49","measuredItemFbsSua","timePointYears"),with=FALSE]


printcases <- nameData("suafbs","sua_unbalanced",printcases)

printcases[, timePointYears_description := NULL]

bodySupplyCondition= paste("The Email contains a list of cases where consumption is increasing despite a decrease in supply.",
                           "It is advisable to adjust the figures in order to have a more realistic consumption behavior.",
                    sep='\n')

sendMailAttachment(printcases,"supplyCondition",bodySupplyCondition)


