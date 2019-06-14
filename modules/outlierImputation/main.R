##' # Pull data from different domains to sua
##'
##' **Author: Cristina Valdivia**
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




if(CheckDebug()){
  message("Not on server, so setting up environment...")
  
  library(faoswsModules)
  SETT <- ReadSettings("C:/Users/modules/outlierImputation")
  
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
endYear = as.numeric(2017)

#years to consider for outlier detections

startYearOutl = as.numeric(swsContext.computationParams$startYearoutl)
endYearOutl = as.numeric(swsContext.computationParams$endYearoutl)
yearValsOutl = startYearOutl:endYearOutl

#endYear = as.numeric(swsContext.computationParams$endYear)
endYear = as.numeric(2017)


geoM49 = swsContext.computationParams$geom49
stopifnot(startYear <= endYear)
yearVals = startYear:endYear

##' Get data configuration and session
sessionKey = swsContext.datasets[[1]]

sessionCountries =
  getQueryKey("geographicAreaM49", sessionKey)

geoKeys = GetCodeList(domain = "agriculture", dataset = "aproduction",
                      dimension = "geographicAreaM49")[type == "country", code]


selectedGEOCode =sessionCountries


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




data = GetData(key,omitna = F, normalized=T)

#write.csv(data, "data_before_outlier_plugin.csv", row.names = F)

##adding missing lines

data2014_2017<-data %>% dplyr::filter(timePointYears>=2014 & timePointYears<=2017)

data2011_2013<-data %>% dplyr::filter(timePointYears>=2011 & timePointYears<=2013)





data2<-dcast(data, formula = geographicAreaM49+  measuredElementSuaFbs+ measuredItemFbsSua ~ timePointYears, value.var = "Value")
data3<-melt(data2, id.vars = c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemFbsSua"))
data3$variable<-as.character(data3$variable)
data3<-data3 %>% dplyr::filter(is.na(value) & variable>=2014 & variable<=2017 & (measuredElementSuaFbs==5520|measuredElementSuaFbs==5525|measuredElementSuaFbs==5016|measuredElementSuaFbs==5165))
data3[,"coladd"]<-"E"
data3[,"coladd2"]<-"e"


data3<-as.data.table(data3)
data4<-data3[,colnames(data)] 
setnames(data3, data4)
data2014_2017<-data2014_2017 %>% dplyr::filter(!is.na(Value))


datafinal<-rbind(data2011_2013, data2014_2017, data3)
data<-datafinal

#miss<-data %>% filter(measuredElementSuaFbs==5520|measuredElementSuaFbs==5525|measuredElementSuaFbs==5165)
#miss<- miss %>% group_by(geographicAreaM49,measuredItemFbsSua) %>% mutate(Meanold=mean(Value[timePointYears<2014],na.rm=T))
#miss$Meanold[is.na(miss$Meanold)]<-0
#miss<-miss %>% filter(Meanold!=0)

#miss<- miss %>% group_by(geographicAreaM49,measuredItemFbsSua, measuredElementSuaFbs) %>% mutate(sumyears=sum(as.integer(timePointYears[timePointYears>=2014])))


################# get protected flags
protectedFlags<-ReadDatatable(table = "valid_flags")

keys = c("flagObservationStatus", "flagMethod")


commDef=ReadDatatable("fbs_commodity_definitions")
# 
primaryProxyPrimary=commDef$cpc[commDef[,proxy_primary=="X" | primary_commodity=="X"]]
primary=commDef$cpc[commDef[,primary_commodity=="X"]]
ProxyPrimary=commDef$cpc[commDef[,proxy_primary=="X"]]
food=commDef$cpc[commDef[,food_item=="X"]]

###########################################################
##### calculate historical ratios                     #####
###########################################################

sua_unb<- data %>% group_by(geographicAreaM49,measuredItemFbsSua,measuredElementSuaFbs) %>% dplyr::mutate(Meanold=mean(Value[timePointYears<2014],na.rm=T))
sua_unb$Meanold[is.na(sua_unb$Meanold)]<-0
sua_unb$Value[is.na(sua_unb$Value)]<-0
sua_unb$flagObservationStatus[is.na(sua_unb$Value)]<-0

sua_unb <- merge(sua_unb, protectedFlags, by = keys)

##Added by Valdivia
attach(sua_unb)
sua_unb$Protected[flagObservationStatus=="E" & flagMethod %in% c("e","f")]<-FALSE

##
sua_unb<- sua_unb %>% group_by(geographicAreaM49,measuredItemFbsSua,timePointYears) %>% dplyr::mutate(prod=sum(Value[measuredElementSuaFbs==5510]))
sua_unb$prod[is.na(sua_unb$prod)]<-0
sua_unb<- sua_unb %>% group_by(geographicAreaM49,measuredItemFbsSua,timePointYears) %>% dplyr::mutate(imp=sum(Value[measuredElementSuaFbs==5610]))
sua_unb$imp[is.na(sua_unb$imp)]<-0
sua_unb<- sua_unb %>% group_by(geographicAreaM49,measuredItemFbsSua,timePointYears) %>% dplyr::mutate(exp=sum(Value[measuredElementSuaFbs==5910]))
sua_unb$exp[is.na(sua_unb$exp)]<-0
sua_unb<- sua_unb %>% group_by(geographicAreaM49,measuredItemFbsSua,timePointYears) %>% dplyr::mutate(supply=prod+imp-exp)

sua_unb<- sua_unb %>% group_by(geographicAreaM49,measuredItemFbsSua,measuredElementSuaFbs,timePointYears) %>% dplyr::mutate(ratio=Value/supply)

sua_unb$ratio[sua_unb$ratio<0]<-NA 

sua_unb<- sua_unb %>% group_by(geographicAreaM49,measuredItemFbsSua,measuredElementSuaFbs) %>% dplyr::mutate(mean_ratio=mean(ratio[timePointYears<2014 & timePointYears>2010],na.rm=T))
sua_unb<- sua_unb %>% dplyr::mutate(Diff_val=Value-Meanold)


sua_unb<-as.data.table(sua_unb)
sua_unb[, mean_ratio2 :=  ifelse(mean_ratio>1, 1, mean_ratio)]

utiLARGERsup<-sua_unb %>% dplyr::filter(mean_ratio>1 & (measuredElementSuaFbs==5520|measuredElementSuaFbs==5525|measuredElementSuaFbs==5165|measuredElementSuaFbs==5016))

## feed
feed<-sua_unb %>% dplyr::filter(measuredElementSuaFbs==5520 & abs(supply)>0)

feed=data.table(feed)



##following row changed by Valdivia to balance items where feed is the only utilization
feed <-
  feed %>%
  dplyr::mutate(
    outl =
      abs(ratio-mean_ratio) > 0.1 |
      mean_ratio==1 |
      mean_ratio>1|
      (mean_ratio!=0 & ratio==0) |
      ((Value>2*Meanold | Value<0.5*Meanold) & (Diff_val > 10000 | Diff_val < -10000))
  )



outl_feed<-feed %>% dplyr::filter((outl==T & timePointYears%in%yearValsOutl) )
impute_feed<-outl_feed %>% dplyr::filter(supply>=0 & mean_ratio>=0 & Protected==F )
#outl_feed %>% filter(supply<0 | mean_ratio<0 | mean_ratio>1) %>% write.csv( file = "feed_to_check.csv")


impute_feed<-impute_feed %>% dplyr::mutate(impute=supply*mean_ratio2)

#impute_feed %>% write.csv( file = "feed_outliers.csv")
colnames(impute_feed)[7]<-"pippo"
colnames(impute_feed)[20]<-"Value"
impute_feed<-impute_feed[,colnames(data)] 

if (nrow(impute_feed)> 0) {
  impute_feed[,6]<-c("E")
  impute_feed[,7]<-c("e")
  
}



## seed careful ratio of seed is measured over production
sua_unb2<- sua_unb %>% group_by(geographicAreaM49,measuredItemFbsSua,timePointYears) %>% dplyr::mutate(supply=prod)
sua_unb2<- sua_unb2 %>% group_by(geographicAreaM49,measuredItemFbsSua,measuredElementSuaFbs,timePointYears) %>% dplyr::mutate(ratio=Value/supply)
sua_unb2<- sua_unb2 %>% group_by(geographicAreaM49,measuredItemFbsSua,measuredElementSuaFbs) %>% dplyr::mutate(mean_ratio=mean(ratio[timePointYears<2014 & timePointYears>2010],na.rm=T))

seed<-sua_unb2 %>% dplyr::filter(measuredElementSuaFbs==5525)

seed <-
  seed %>%
  dplyr::mutate(
    mean_ratio2 = ifelse(mean_ratio > 1, 1, mean_ratio),
    outl =
      abs(ratio-mean_ratio) > 0.05 |
      mean_ratio==1 |
      mean_ratio>1 |
      (mean_ratio!=0 & ratio==0) |
      ((Value>2*Meanold | Value<0.5*Meanold) & (Diff_val > 10000 | Diff_val < -10000))
  )


outl_seed<-seed %>% dplyr::filter((outl==T & timePointYears%in%yearValsOutl) )
impute_seed<-outl_seed %>% dplyr::filter(supply>=0 & mean_ratio>=0 & Protected==F )
#outl_feed %>% filter(supply<0 | mean_ratio<0 | mean_ratio>1) %>% write.csv( file = "feed_to_check.csv")
impute_seed<-impute_seed %>% dplyr::mutate(impute=supply*mean_ratio2)
#impute_feed %>% write.csv( file = "feed_outliers.csv")
colnames(impute_seed)[7]<-"pippo"
colnames(impute_seed)[20]<-"Value"
impute_seed<-impute_seed[,colnames(data)] 

if (nrow(impute_seed)> 0) {
  impute_seed[,6]<-c("E")
  impute_seed[,7]<-c("e")
  
}


rm(sua_unb2)



## loss careful supply definition changes
sua_unb2<- sua_unb %>% group_by(geographicAreaM49,measuredItemFbsSua,timePointYears) %>% dplyr::mutate(supply=prod+imp)
sua_unb2<- sua_unb2 %>% group_by(geographicAreaM49,measuredItemFbsSua,measuredElementSuaFbs,timePointYears) %>% dplyr::mutate(ratio=Value/supply)
sua_unb2<- sua_unb2 %>% group_by(geographicAreaM49,measuredItemFbsSua,measuredElementSuaFbs) %>% dplyr::mutate(mean_ratio=mean(ratio[timePointYears<2014 & timePointYears>2010],na.rm=T))


loss<-sua_unb2 %>% dplyr::filter(measuredElementSuaFbs==5016)

loss <-
  loss %>%
  dplyr::mutate(
    mean_ratio2 = ifelse(mean_ratio > 1, 1, mean_ratio),
    outl =
      abs(ratio-mean_ratio) > 0.05 |mean_ratio>1|
      ((Value>2*Meanold | Value<0.5*Meanold) & (Diff_val > 10000 | Diff_val < -10000))
  )

outl_loss<-loss %>% dplyr::filter((outl==T & timePointYears%in%yearValsOutl) )
impute_loss<-outl_loss %>% dplyr::filter(supply>=0 & mean_ratio>0 & Protected==F & measuredItemFbsSua %in% food & measuredItemFbsSua %in% primaryProxyPrimary)
impute_loss<-impute_loss %>% dplyr::mutate(impute=supply*mean_ratio2)


colnames(impute_loss)[7]<-"pippo"
colnames(impute_loss)[20]<-"Value"
impute_loss<-impute_loss[,colnames(data)] 

if (nrow(impute_loss)> 0) {
  impute_loss[,6]<-c("E")
  impute_loss[,7]<-c("e")
  
}


##### INDUSTRIAL
## ind
ind<-sua_unb %>% dplyr::filter(measuredElementSuaFbs==5165)


ind <-
  ind %>%
  dplyr::mutate(
    outl =
      abs(ratio-mean_ratio) > 0.05 |
      mean_ratio==1 |
      mean_ratio>1|
      (mean_ratio!=0 & ratio==0) |
      ((Value>2*Meanold | Value<0.5*Meanold) & (Diff_val > 10000 | Diff_val < -10000))
  )


outl_ind<-ind %>% dplyr::filter((outl==T & timePointYears%in%yearValsOutl) )
impute_ind<-outl_ind %>% dplyr::filter(supply>=0 & mean_ratio>=0 & Protected==F )
#outl_feed %>% filter(supply<0 | mean_ratio<0 | mean_ratio>1) %>% write.csv( file = "feed_to_check.csv")
impute_ind<-impute_ind %>% dplyr::mutate(impute=supply*mean_ratio2)
#impute_feed %>% write.csv( file = "feed_outliers.csv")
colnames(impute_ind)[7]<-"pippo"
colnames(impute_ind)[20]<-"Value"
impute_ind<-impute_ind[,colnames(data)] 

if (nrow(impute_loss)> 0) {
  impute_ind[,6]<-c("E")
  impute_ind[,7]<-c("e")
  
}

impute_feed<-as.data.table(impute_feed)
impute_seed<-as.data.table(impute_seed)
impute_loss<-as.data.table(impute_loss)
impute_ind<-as.data.table(impute_ind)

################################################################
#####  save data                                           #####
################################################################
impute=rbind(impute_feed,impute_seed,impute_loss,impute_ind)
impute<-impute %>% dplyr::filter(Value!=0)
out<-as.data.table(impute)

################################################################
#####  save data                                           #####
################################################################

utiLARGERsup$timePointYears<-NULL
utiLARGERsup$Value<-NULL
utiLARGERsup$prod<-NULL
utiLARGERsup$imp<-NULL
utiLARGERsup$exp<-NULL
utiLARGERsup$supply<-NULL
utiLARGERsup$ratio<-NULL
utiLARGERsup$Diff_val<-NULL
utiLARGERsup$mean_ratio2<-NULL
utiLARGERsup$Valid<-NULL
utiLARGERsup$Protected<-NULL
utiLARGERsup$Meanold<-NULL
utiLARGERsup$flagMethod<-NULL
utiLARGERsup$flagObservationStatus<-NULL

utiLARGERsup<-unique(utiLARGERsup)



if (nrow(out) > 0) {
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
  subject = "Outlier plug-in has correctly run"
  body = "The plug-in has replaced outliers and saved imputed values in your session"
  
  sendmailR::sendmail(from, to, subject , body)
  paste0("Email sent to ", swsContext.userEmail)
  
} else {
  print("No outliers were detected and replaced.")
}


setDT(utiLARGERsup)


bodyOutliers= paste("The Email contains a list of items with one or more utilizations that historically were larger than supply.",
                    sep='\n')

sendMailAttachment(utiLARGERsup,"Util_larger_supply",bodyOutliers)

