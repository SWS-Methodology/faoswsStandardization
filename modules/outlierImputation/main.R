##' # Pull data from different domains to sua 
##'
##' **Author: Carlo Del Bello**
##'
##' **Description:**
##'
##' This module is designed to identify outliers in feed, seed and loss figures at sua unbalanced level
##' 
##' 
##' **Inputs:**
##'
##' * sua unbalanced


##'
##' **Flag assignment:**
##'
##' I e



## load the library
library(faosws)
library(data.table)
library(faoswsUtil)
library(sendmailR)
library(dplyr)
library(faoswsUtil)
#library(faoswsStandardization)
library(faoswsFlag)


# oldProductionCode = c("51","11","21","131") ## 51: prod, other for animals
# foodCode = "5141"
# importCode = "5610"
# exportCode = "5910"
# oldFeedCode = "101"
# oldSeedCode = "111"
# 
# #oldLossCode = "121"
# lossCode = "5016"
# industrialCode = "5165"
# touristCode = "100"
# suaTouristCode = "5164"
# # Convert tourism units to tonnes
# # touristConversionFactor = -1/1000
# touristConversionFactor = 1
# # warning("Stocks is change in stocks, not absolute! This needs to be changed")
# stocksCode = c("5071","5113")
# 
# ## set up for the test environment and parameters
# R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")

if(CheckDebug()){
  message("Not on server, so setting up environment...")
  
  library(faoswsModules)
  SETT <- ReadSettings("modules/outlierImputation/sws.yml")
  
  R_SWS_SHARE_PATH <- SETT[["share"]]  
  ## Get SWS Parameters
  SetClientFiles(dir = SETT[["certdir"]])
  GetTestEnvironment(
    baseUrl = SETT[["server"]],
    token = SETT[["token"]]
  )
}

#startYear = as.numeric(swsContext.computationParams$startYear)
startYear = as.numeric(2011)

#endYear = as.numeric(swsContext.computationParams$endYear)
endYear = as.numeric(2016)

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

eleKeys <- GetCodeList(domain = "suafbs", dataset = "sua_unbalanced", "measuredElementSuaFbs")
eleKeys = eleKeys[, code]

eleDim <- Dimension(name = "measuredElementSuaFbs", keys = c("5510","5610","5071","5023","5910","5016","5165","5520","5525","5164","5166","5141","664","5113"))

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



data = data = GetData(key,omitna=F)


################# get protected flags
protectedFlags<-ReadDatatable(table = "valid_flags")

keys = c("flagObservationStatus", "flagMethod")

###########################################################
##### calculate historical ratios                     #####
###########################################################

sua_unb<- data %>% group_by(geographicAreaM49,measuredItemFbsSua,measuredElementSuaFbs) %>% mutate(Meanold=mean(Value[timePointYears<2014],na.rm=T))
sua_unb$Meanold[is.na(sua_unb$Meanold)]<-0
sua_unb$Value[is.na(sua_unb$Value)]<-0
sua_unb <- merge(sua_unb, protectedFlags, by = keys)




##
sua_unb<- sua_unb %>% group_by(geographicAreaM49,measuredItemFbsSua,timePointYears) %>% mutate(prod=sum(Value[measuredElementSuaFbs==5510]))
sua_unb$prod[is.na(sua_unb$prod)]<-0
sua_unb<- sua_unb %>% group_by(geographicAreaM49,measuredItemFbsSua,timePointYears) %>% mutate(imp=sum(Value[measuredElementSuaFbs==5610]))
sua_unb$imp[is.na(sua_unb$imp)]<-0
sua_unb<- sua_unb %>% group_by(geographicAreaM49,measuredItemFbsSua,timePointYears) %>% mutate(exp=sum(Value[measuredElementSuaFbs==5910]))
sua_unb$exp[is.na(sua_unb$exp)]<-0
sua_unb<- sua_unb %>% group_by(geographicAreaM49,measuredItemFbsSua,timePointYears) %>% mutate(supply=prod+imp-exp)
sua_unb<- sua_unb %>% group_by(geographicAreaM49,measuredItemFbsSua,measuredElementSuaFbs,timePointYears) %>% mutate(ratio=Value/supply)
sua_unb<- sua_unb %>% group_by(geographicAreaM49,measuredItemFbsSua,measuredElementSuaFbs) %>% mutate(mean_ratio=mean(ratio[timePointYears<2014 & timePointYears>2010],na.rm=T))


## feed
feed<-sua_unb %>% filter(measuredElementSuaFbs==5520 & abs(supply)>0)
feed<-feed %>% mutate(outl=abs(ratio-mean_ratio)>0.1)
outl_feed<-feed %>% filter((outl==T & timePointYears>2013) )
impute_feed<-outl_feed %>% filter(supply>=0 & mean_ratio>=0 & mean_ratio<=1 & Protected==F )
#outl_feed %>% filter(supply<0 | mean_ratio<0 | mean_ratio>1) %>% write.csv( file = "feed_to_check.csv")
impute_feed<-impute_feed %>% mutate(impute=supply*mean_ratio)
#impute_feed %>% write.csv( file = "feed_outliers.csv")
colnames(impute_feed)[7]<-"pippo"
colnames(impute_feed)[18]<-"Value"
impute_feed<-impute_feed[,colnames(data)] 
impute_feed[,6]<-c("E")
impute_feed[,7]<-c("e")


## seed
seed<-sua_unb %>% filter(measuredElementSuaFbs==5525)
seed<-seed %>% mutate(outl=abs(ratio-mean_ratio)>0.05)
outl_seed<-seed %>% filter((outl==T & timePointYears>2013) )
impute_seed<-outl_seed %>% filter(supply>=0 & mean_ratio>=0 & mean_ratio<=1 & Protected==F )
#outl_feed %>% filter(supply<0 | mean_ratio<0 | mean_ratio>1) %>% write.csv( file = "feed_to_check.csv")
impute_seed<-impute_seed %>% mutate(impute=supply*mean_ratio)
#impute_feed %>% write.csv( file = "feed_outliers.csv")
colnames(impute_seed)[7]<-"pippo"
colnames(impute_seed)[18]<-"Value"
impute_seed<-impute_seed[,colnames(data)] 
impute_seed[,6]<-c("E")
impute_seed[,7]<-c("e")




## loss careful supply definition changes
sua_unb2<- sua_unb %>% group_by(geographicAreaM49,measuredItemFbsSua,timePointYears) %>% mutate(supply=prod+imp)
sua_unb2<- sua_unb2 %>% group_by(geographicAreaM49,measuredItemFbsSua,measuredElementSuaFbs,timePointYears) %>% mutate(ratio=Value/supply)
sua_unb2<- sua_unb2 %>% group_by(geographicAreaM49,measuredItemFbsSua,measuredElementSuaFbs) %>% mutate(mean_ratio=mean(ratio[timePointYears<2014 & timePointYears>2010],na.rm=T))


loss<-sua_unb2 %>% filter(measuredElementSuaFbs==5016)
loss<-loss %>% mutate(outl=abs(ratio-mean_ratio)>0.05)
outl_loss<-loss %>% filter((outl==T & timePointYears>2013) )
impute_loss<-outl_loss %>% filter(supply>=0 & mean_ratio>=0 & mean_ratio<=1 & Protected==F)
impute_loss<-impute_loss %>% mutate(impute=supply*mean_ratio)
impute_loss<-impute_loss %>% mutate(diff=Value-impute)


colnames(impute_loss)[7]<-"pippo"
colnames(impute_loss)[18]<-"Value"
impute_loss<-impute_loss[,colnames(data)] 
impute_loss[,6]<-c("E")
impute_loss[,7]<-c("e")





################################################################
#####  save data                                           #####
################################################################
impute=rbind(impute_feed,impute_seed,impute_loss)
out<-as.data.table(impute)

stats = SaveData(domain = "suafbs", dataset = "sua_unbalanced", data = out, waitTimeout = 2000000)
paste0(stats$inserted, " observations written, ",
       stats$ignored, " weren't updated, ",
       stats$discarded, " had problems.")

################################################################
#####  send Email with notification of correct execution   #####
################################################################

## Initiate email
from = "sws@fao.org"
to = swsContext.userEmail
subject = "PullDataToSua plug-in has correctly run"
body = "The plug-in has saved the SUAs in your session"

sendmail(from, to, subject , body)
paste0("Email sent to ", swsContext.userEmail)

