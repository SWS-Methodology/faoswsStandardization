

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



#startYear = as.numeric(swsContext.computationParams$startYear)
#startYear = as.numeric(2014)

#endYear = as.numeric(swsContext.computationParams$endYear)
#endYear = as.numeric(2016)

startYear = 2010
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

eleKeys = GetCodeList(domain = "suafbs", dataset = "sua_unbalanced", "measuredElementSuaFbs")
eleKeys <-eleKeys[, code]

eleDim <- Dimension(name = "measuredElementSuaFbs", keys = eleKeys)


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



sua_data_full = GetData(key, omitna = F)


sua_data <- subset(sua_data_full, measuredElementSuaFbs %in% c("5510","5610","5910","664"))

sua_data[is.na(Value), Value := 0]


sua_data<- sua_data %>% group_by(geographicAreaM49, measuredItemFbsSua, measuredElementSuaFbs) %>% 
   mutate(oldprod=sum(Value[timePointYears<2014 & measuredElementSuaFbs=="5510"]))

sua_data<- sua_data %>% group_by(geographicAreaM49, measuredItemFbsSua) %>% mutate(keep=any(oldprod>0))
sua_data=as.data.table(sua_data)

sua_data <- subset(sua_data, keep==T)


sua_data[, c("flagObservationStatus","flagMethod") := NULL]

sua_data <- subset(sua_data, timePointYears>2013)


sua_data <- dcast.data.table(sua_data, geographicAreaM49 + measuredItemFbsSua +
                          timePointYears ~ measuredElementSuaFbs, value.var = "Value")



setnames(sua_data, c("5510","5610","5910"),c("production","imports", "exports"))


sua_data[is.na(imports), imports := 0]
sua_data[is.na(exports), exports := 0]
sua_data[is.na(production), production := 0]


sua_data[,supply := production+imports-exports]


## livestock cpc to exclude
'%!in%' <- function(x,y)!('%in%'(x,y))

livestock<- c("02111","02112","02121.01","02122","02123","02131","02132","02133","02140","02151","02152","02153","02154","02191","02194",
              "02196","02199.10","02199.20")


#print apparent consumption



apparentConsumption <- copy (sua_data)


apparentConsumption <- subset(apparentConsumption, timePointYears %in% c(2014:2016))


apparentConsumption <- subset(apparentConsumption, measuredItemFbsSua %!in% livestock)

apparentConsumption <- subset(apparentConsumption, supply < -1000 )

apparentConsumption<- apparentConsumption[,c("geographicAreaM49","measuredItemFbsSua","timePointYears"),with=FALSE]

apparentConsumption <-nameData("suafbs","sua_unbalanced",apparentConsumption)

apparentConsumption[,timePointYears_description := NULL]

#### send mail
bodyApparent= paste("The Email contains a list of negative apparent consumptions at sua unbalanced level. Negative values higher than -1000 have been filtered out",
                      sep='\n')

sendMailAttachment(apparentConsumption,"apparentConsumption",bodyApparent)


#apparent consumption is the list of commodities,countries and years where supply is negative.




