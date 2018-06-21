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
library(faoswsStandardization)
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
  SETT <- ReadSettings("modules/pullDataToSUA/sws.yml")
  
  R_SWS_SHARE_PATH <- SETT[["share"]]  
  ## Get SWS Parameters
  SetClientFiles(dir = SETT[["certdir"]])
  GetTestEnvironment(
    baseUrl = SETT[["server"]],
    token = SETT[["token"]]
  )
}

startYear = as.numeric(swsContext.computationParams$startYear)
#startYear = as.numeric(2014)

endYear = as.numeric(swsContext.computationParams$endYear)
#endYear = as.numeric(2016)

geoM49 = swsContext.computationParams$geom49
stopifnot(startYear <= endYear)
yearVals = startYear:endYear

##' Get data configuration and session
sessionKey = swsContext.datasets[[1]]

sessionCountries =
  getQueryKey("geographicAreaM49", sessionKey)

geoKeys = GetCodeList(domain = "agriculture", dataset = "aproduction",
                      dimension = "geographicAreaM49")[type == "country", code]

##' Select the countries based on the user input parameter
selectedGEOCode =
  switch(geoM49,
         "session" = sessionCountries,
         "all" = geoKeys)

################################################
##### Harvest from Agricultural Production #####
################################################
message("Reading SUA data...")

data = data = GetData(sessionKey,omitna=F)

###########################################################
##### calculate historical ratios                     #####
###########################################################

sua_unb<- data %>% group_by(geographicAreaM49,measuredItemFbsSua,measuredElementSuaFbs) %>% mutate(Meanold=mean(Value[timePointYears<2014],na.rm=T))
sua_unb$Meanold[is.na(sua_unb$Meanold)]<-0
sua_unb$Value[is.na(sua_unb$Value)]<-0



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
feed<-sua_unb %>% filter(measuredElementSuaFbs==5520)
feed<-feed %>% mutate(outl=abs(ratio-mean_ratio)>0.1)
outl_feed<-feed %>% filter((outl==T & timePointYears>2013) )
impute_feed<-outl_feed %>% filter(supply>=0 & mean_ratio>=0 & mean_ratio<=1)
#outl_feed %>% filter(supply<0 | mean_ratio<0 | mean_ratio>1) %>% write.csv( file = "feed_to_check.csv")
impute_feed<-impute_feed %>% mutate(impute=supply*mean_ratio)
#impute_feed %>% write.csv( file = "feed_outliers.csv")
impute_feed<-impute_feed[,c(1:7,16)] 
impute_feed<-impute_feed[,c(1,2,3,4,8,6,7)] 
colnames(impute_feed)[5]<-"Value"
impute_feed[,6]<-c("E")
impute_feed[,7]<-c("e")


## seed
seed<-sua_unb %>% filter(measuredElementSuaFbs==5525)
seed<-seed %>% mutate(outl=abs(ratio-mean_ratio)>0.05)
outl_seed<-seed %>% filter((outl==T & timePointYears>2013) )
impute_seed<-outl_seed %>% filter(supply>=0 & mean_ratio>=0 & mean_ratio<=1)
#outl_feed %>% filter(supply<0 | mean_ratio<0 | mean_ratio>1) %>% write.csv( file = "feed_to_check.csv")
impute_seed<-impute_seed %>% mutate(impute=supply*mean_ratio)
#impute_feed %>% write.csv( file = "feed_outliers.csv")
impute_seed<-impute_seed[,c(1:7,16)] 
impute_seed<-impute_seed[,c(1,2,3,4,8,6,7)] 
colnames(impute_seed)[5]<-"Value"
impute_seed[,6]<-c("E")
impute_seed[,7]<-c("e")




## loss careful supply definition changes
sua_unb2<- sua_unb %>% group_by(geographicAreaM49,measuredItemFbsSua,timePointYears) %>% mutate(supply=prod+imp)
sua_unb2<- sua_unb2 %>% group_by(geographicAreaM49,measuredItemFbsSua,measuredElementSuaFbs,timePointYears) %>% mutate(ratio=Value/supply)
sua_unb2<- sua_unb2 %>% group_by(geographicAreaM49,measuredItemFbsSua,measuredElementSuaFbs) %>% mutate(mean_ratio=mean(ratio[timePointYears<2014 & timePointYears>2010],na.rm=T))


loss<-sua_unb %>% filter(measuredElementSuaFbs==5016)
loss<-loss %>% mutate(outl=abs(ratio-mean_ratio)>0.05)
outl_loss<-loss %>% filter((outl==T & timePointYears>2013) )
impute_loss<-outl_loss %>% filter(supply>=0 & mean_ratio>=0 & mean_ratio<=1)
impute_loss<-impute_loss %>% mutate(impute=supply*mean_ratio)
impute_loss<-impute_loss[,c(1:7,16)] 
impute_loss<-impute_loss[,c(1,2,3,4,8,6,7)] 
colnames(impute_loss)[5]<-"Value"
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

