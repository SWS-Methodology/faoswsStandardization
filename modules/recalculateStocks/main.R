##' # Pull data from different domains to sua 
##'
##' **Author: Carlo Del Bello**
##'
##' **Description:**
##'
##' This module is designed to recalculate stocks after validation of stocks variation and check if the resulting opening stocks are realistic
##' 
##' 
##' **Inputs:**
##'
##' * sua unbalanced


##'
##' **Flag assignment:**
##'
##' E e



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
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")

if(CheckDebug()){
  message("Not on server, so setting up environment...")
  
  library(faoswsModules)
  SETT <- ReadSettings("modules/recalculateStocks/sws.yml")
  
  R_SWS_SHARE_PATH <- SETT[["share"]]  
  ## Get SWS Parameters
  SetClientFiles(dir = SETT[["certdir"]])
  GetTestEnvironment(
    baseUrl = SETT[["server"]],
    token = SETT[["token"]]
  )
}

#startYear = as.numeric(swsContext.computationParams$startYear)
startYear = as.numeric(2014)

#endYear = as.numeric(swsContext.computationParams$endYear)
endYear = as.numeric(2016)

#geoM49 = swsContext.computationParams$geom49
stopifnot(startYear <= endYear)
yearVals = startYear:endYear

##' Get data configuration and session
sessionKey = swsContext.datasets[[1]]

sessionCountries =
  getQueryKey("geographicAreaM49", sessionKey)

geoKeys = GetCodeList(domain = "agriculture", dataset = "aproduction",
                      dimension = "geographicAreaM49")[type == "country", code]

#' Select the countries based on the user input parameter
##Select the countries based on the user input parameter
# selectedGEOCode =
#   switch(geoM49,
#          "session" = sessionCountries,
#          "all" = selectedCountries)

selectedGEOCode = sessionCountries


################################################
##### Harvest from sua #####
################################################
message("Reading SUA data...")

suaData = GetData(sessionKey,omitna=F)

###########################################################
##### calculate historical ratios                     #####
###########################################################
# 
# sua_unb<- suaData %>% group_by(geographicAreaM49,measuredItemFbsSua,measuredElementSuaFbs) %>% mutate(Meanold=mean(Value[timePointYears<2014],na.rm=T))
# sua_unb$Meanold[is.na(sua_unb$Meanold)]<-0
# sua_unb$Value[is.na(sua_unb$Value)]<-0
# 
# 
# # 
# ##
# sua_unb<- sua_unb %>% group_by(geographicAreaM49,measuredItemFbsSua,timePointYears) %>% mutate(prod=sum(Value[measuredElementSuaFbs==5510]))
# sua_unb$prod[is.na(sua_unb$prod)]<-0
# sua_unb<- sua_unb %>% group_by(geographicAreaM49,measuredItemFbsSua,timePointYears) %>% mutate(imp=sum(Value[measuredElementSuaFbs==5610]))
# sua_unb$imp[is.na(sua_unb$imp)]<-0
# sua_unb<- sua_unb %>% group_by(geographicAreaM49,measuredItemFbsSua,timePointYears) %>% mutate(exp=sum(Value[measuredElementSuaFbs==5910]))
# sua_unb$exp[is.na(sua_unb$exp)]<-0
# sua_unb<- sua_unb %>% group_by(geographicAreaM49,measuredItemFbsSua,timePointYears) %>% mutate(supply=prod+imp-exp)
# sua_unb<- sua_unb %>% group_by(geographicAreaM49,measuredItemFbsSua,measuredElementSuaFbs,timePointYears) %>% mutate(ratio=Value/supply)
# sua_unb<- sua_unb %>% group_by(geographicAreaM49,measuredItemFbsSua,measuredElementSuaFbs) %>% mutate(mean_ratio=mean(ratio[timePointYears<2014 & timePointYears>2010],na.rm=T))


## mutate(pchange2=(Value/lag(Value))-1)
stocksvar<-suaData %>% dplyr::filter((measuredElementSuaFbs=="5071" | measuredElementSuaFbs=="5113") & timePointYears>2013)
#stocksvar <- stocksvar %>% group_by(geographicAreaM49,measuredItemFbsSua) %>% arrange(timePointYears) %>% mutate(cumstocks=cumsum(Value))
stockswide<- stocksvar %>% dcast(geographicAreaM49+measuredItemFbsSua+timePointYears~measuredElementSuaFbs,value.var="Value")
stockswide$`5071`[is.na(stockswide$`5071`)]<-0
stockswide["stocksnew"]<-stockswide["5113"]



###########
esum <- function(x,y) mapply("+", x,y)
esum2 <- function(x,y) x+y


#stockswide <- stockswide %>% group_by(geographicAreaM49,measuredItemFbsSua)  %>%  mutate(stocksnew=lag(stocksnew)+lag(`5071`))
#stockswide <- stockswide %>% group_by(geographicAreaM49,measuredItemFbsSua)  %>%  mutate(stocksnew=Reduce(sum, `5071`, init = 0, accumulate = TRUE)[-1])
#stockswide <- stockswide %>% group_by(geographicAreaM49,measuredItemFbsSua)  %>%  mutate(stocksnew=Reduce(esum, list(lag(stocksnew),lag(`5071`)),accumulate = T))
stockswide <- stockswide %>% dplyr::group_by(geographicAreaM49,measuredItemFbsSua)  %>%  dplyr::mutate(stocksnew =  cumsum(ifelse(is.na(lag(`5071`)), 0, lag(`5071`))) +`5113`[timePointYears==2014])


stockswide <- stockswide %>%   dplyr::mutate(changedStock=(abs(`5113`-stocksnew)>10 & stocksnew>0))
stockswide<- stockswide %>%  dplyr::filter(changedStock==T)
stockswide[,"5113"]<-stockswide[,"stocksnew"]

stockswide=as.data.table(stockswide)

stockswide[,c("stocksnew","changedStock","5071"):=NULL]


stockslong<-melt(stockswide, id=c("geographicAreaM49","measuredItemFbsSua", "timePointYears"))
colnames(stockslong)[4]<-"measuredElementSuaFbs"
colnames(stockslong)[5]<-"Value"

stockslong[,"flagObservationStatus":="E"] 
stockslong[,"flagMethod":="e"]
stockslong[,measuredElementSuaFbs:=as.character(measuredElementSuaFbs)] 

setcolorder(stockslong, colnames(suaData))
#stockslong<-stockslong[,colnames(suaData)]


################################################################
#####  save data                                           #####
################################################################
impute=stockslong
out<-as.data.table(impute)
out$measuredElementSuaFbs<-as.character(out$measuredElementSuaFbs)

stats = SaveData(domain = "suafbs", dataset = "sua_unbalanced", data = out, waitTimeout = 2000000)

paste0(stats$inserted, " observations written, ",
       stats$ignored, " weren't updated, ",
       stats$appended, " were updated, ",
       stats$discarded, " had problems.")

################################################################
#####  send Email with notification of correct execution   #####
################################################################

## Initiate email
from = "sws@fao.org"
to = swsContext.userEmail
subject = "Stocks recalculation plug-in has correctly run"
body = "The plug-in has saved the SUAs in your session"

sendmailR::sendmail(from, to, subject , body)
paste0("Email sent to ", swsContext.userEmail)

