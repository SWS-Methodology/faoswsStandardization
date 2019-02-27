##' #Balanced SUA element outliers detection
##' 
##'  
##'
##' **Author: Cristina Valdivia**
##'
##' **Description:**
##'
##' This module is designed to detect outliers in SUA balanced for all elements.
##' 
##' **Inputs:**
##'
##' * sua balanced


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
library(faoswsStandardization)

if(CheckDebug()){
  message("Not on server, so setting up environment...")
  
  library(faoswsModules)
  ## SETT <- ReadSettings("modules/outlierImputation/sws.yml")
  SETT <- ReadSettings("C:/Users/VALDIVIACR/Desktop/FAO/R/version faostandardization october 2018/faoswsStandardization/modules/fullStandardizationAndBalancing/sws.yaml")
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

stopifnot(startYear <= endYear)
yearVals = startYear:endYear

##' Get data configuration and session
sessionKey = swsContext.datasets[[1]]

sessionCountries =
  getQueryKey("geographicAreaM49", sessionKey)

geoKeys = GetCodeList(domain = "agriculture", dataset = "aproduction",
                      dimension = "geographicAreaM49")[type == "country", code]




##Select the countries based on the user input parameter
selectedGEOCode =sessionCountries


#########################################
##### Pull from SUA unbalanced and balanced data #####
#########################################

message("Pulling SUA unbalanced Data")

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


data_unbal = data = GetData(key,omitna=F)




#########################################
##### Pull from SUA balanced data #####
#########################################

timeDim <- Dimension(name = "timePointYears", keys = as.character(yearVals))

#Define the key to pull SUA data
key = DatasetKey(domain = "suafbs", dataset = "sua_balanced", dimensions = list(
  geographicAreaM49 = geoDim,
  measuredElementSuaFbs = eleDim,
  measuredItemFbsSua = itemDim,
  timePointYears = timeDim
))


data_bal = data = GetData(key,omitna=F)


#Identify cpc codes of food and primary commodities

commDef = ReadDatatable("utilization_table_2018")

food=commDef$cpc[commDef[,food_item=="X"]] 
primary=commDef$cpc[commDef[,primary_item=="X"]] 

#TWO SIMULTANEOUS CRITERIA FOR DETECTING OUTLIERS

#1.THE RATIO BETWEEN VALUE AND HISTORIAL VALUE (RELATIVE TO YEARS 2011-2013) HAS TO BE WITHIN 2 AND 0.5
#2.RATIO BETWEEN VALUE AND SUPPLY HAS TO BE WITHIN 15 PERCENTAGE POINTS DIFFERENCE FROM HISTORICAL RATIO OF VALUE TO SUPPLY (RELATIVE TO YEARS 2011-2013)

#Keep only data for years 2011-2013 to calculate historical values using data from sua unbalanced

data_unbal=data_unbal[timePointYears>=2011 & timePointYears<=2013]

data_unbal <-data_unbal %>% group_by(geographicAreaM49,measuredItemFbsSua, measuredElementSuaFbs) %>% dplyr::mutate(Meanold=mean(Value,na.rm=T))

data_unbal<- data_unbal %>% group_by(geographicAreaM49,measuredItemFbsSua,timePointYears) %>% dplyr::mutate(prod=sum(Value[measuredElementSuaFbs==5510]))
data_unbal$prod[is.na(data_unbal$prod)]<-0
data_unbal<- data_unbal %>% group_by(geographicAreaM49,measuredItemFbsSua,timePointYears) %>% dplyr::mutate(imp=sum(Value[measuredElementSuaFbs==5610]))
data_unbal$imp[is.na(data_unbal$imp)]<-0
data_unbal<- data_unbal %>% group_by(geographicAreaM49,measuredItemFbsSua,timePointYears) %>% dplyr::mutate(exp=sum(Value[measuredElementSuaFbs==5910]))
data_unbal$exp[is.na(data_unbal$exp)]<-0
data_unbal<- data_unbal %>% group_by(geographicAreaM49,measuredItemFbsSua,timePointYears) %>% dplyr::mutate(stock=sum(Value[measuredElementSuaFbs==5071]))
data_unbal$stock[is.na(data_unbal$stock)]<-0
data_unbal<- data_unbal %>% group_by(geographicAreaM49,measuredItemFbsSua,timePointYears) %>% dplyr::mutate(supply=prod+imp-exp-stock)
data_unbal<- data_unbal %>% group_by(geographicAreaM49,measuredItemFbsSua,measuredElementSuaFbs,timePointYears) %>% dplyr::mutate(ratio_old=Value/supply)
data_unbal<- data_unbal %>% group_by(geographicAreaM49,measuredItemFbsSua,measuredElementSuaFbs) %>% dplyr::mutate(mean_ratio=mean(ratio_old,na.rm=T))


data_unbal=data_unbal[,c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemFbsSua", "Meanold", "mean_ratio")]

data_unbal<-unique(data_unbal)


data = merge(data_bal, data_unbal, by =c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemFbsSua") )
data=data[measuredItemFbsSua%in%food]

data$Meanold[is.na(data$Meanold)]<-0
data$Value[is.na(data$Value)]<-0
data$mean_ratio[is.na(data$mean_ratio)]<-0

data<-data%>%  dplyr::mutate(ratio_val=(Value/Meanold))

data<- data%>%   dplyr::mutate(outlier= (ratio_val>2 | ratio_val<0.5) & timePointYears>2013)


data<-  data%>% group_by(geographicAreaM49,measuredItemFbsSua,timePointYears) %>% dplyr::mutate(prod=sum(Value[measuredElementSuaFbs==5510]))
data$prod[is.na(data$prod)]<-0
data<- data %>% group_by(geographicAreaM49,measuredItemFbsSua,timePointYears) %>% dplyr::mutate(imp=sum(Value[measuredElementSuaFbs==5610]))
data$imp[is.na(data$imp)]<-0
data<- data %>% group_by(geographicAreaM49,measuredItemFbsSua,timePointYears) %>% dplyr::mutate(exp=sum(Value[measuredElementSuaFbs==5910]))
data$exp[is.na(data$exp)]<-0
data<- data %>% group_by(geographicAreaM49,measuredItemFbsSua,timePointYears) %>% dplyr::mutate(stock=sum(Value[measuredElementSuaFbs==5071]))
data$stock[is.na(data$stock)]<-0
data<- data %>% group_by(geographicAreaM49,measuredItemFbsSua,timePointYears) %>% dplyr::mutate(supply=prod+imp-exp-stock)
data<- data %>% group_by(geographicAreaM49,measuredItemFbsSua,measuredElementSuaFbs,timePointYears) %>% dplyr::mutate(ratio_supp=Value/supply)

data<-data%>% dplyr::mutate(outl_on_supp=(abs(ratio_supp-mean_ratio)>=0.1))

data=as.data.table(data)
data=data[measuredElementSuaFbs!=5610 & measuredElementSuaFbs!=5910 & measuredElementSuaFbs!=5071 & measuredElementSuaFbs!=664]
data=data[!(measuredElementSuaFbs==5510 & measuredItemFbsSua%in%primary) ]
data=data[!(measuredElementSuaFbs==5016 & Value!=0 & Meanold==0) ]


setDT(data)
#imbToSend=subset(data,timePointYears>=2014 & ((outlier==T & outl_on_supp==T & (measuredElementSuaFbs==5520|measuredElementSuaFbs==5525|measuredElementSuaFbs==5016|measuredElementSuaFbs==5023|measuredElementSuaFbs==5165|measuredElementSuaFbs==5141))|(outlier==T & (measuredElementSuaFbs==5510))|(Value==0 & Meanold!=0)|(Value!=0 & Meanold==0)|(abs(Meanold-Value)>50000 & outlier==T)))
imbToSend=subset(data,timePointYears>=2014 & ((outlier==T & outl_on_supp==T & (measuredElementSuaFbs==5520|measuredElementSuaFbs==5525|measuredElementSuaFbs==5016|measuredElementSuaFbs==5023|measuredElementSuaFbs==5165|measuredElementSuaFbs==5141) & (abs(Meanold-Value)>10000))|(outlier==T & (measuredElementSuaFbs==5510) & (abs(Meanold-Value)>10000))))
setDT(imbToSend)
imbToSend<-imbToSend%>% dplyr::mutate(keep=(Value==0 & Meanold!=0))
imbToSend<-imbToSend%>% dplyr::mutate(keep2=(Value!=0 & Meanold==0))
imbToSend=subset(imbToSend, (Value>10000 & keep==FALSE & keep2==FALSE))


if (nrow(imbToSend) > 0) {
  setnames(imbToSend,"Meanold","Historical_value2011_2013")

  imbToSend=imbToSend[,c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemFbsSua", "timePointYears", "Value", "flagObservationStatus", "flagMethod", "Historical_value2011_2013")]

  setDT(imbToSend)


  #data.table::setorder(imbToSend,-perc.imbalance)
  imbToSend=nameData("suafbs","sua_balanced",imbToSend)

  bodyImbalances= paste("The Email contains the list of outliers for the following elements: Production, Food, Feed, Seed, Loss, Processed, Industrial Use. Please check them.",
                      sep='\n')

  sendMailAttachment(setDT(imbToSend),"Outliers_elements_SUA_bal",bodyImbalances)
  print("Outliers found, please check email.")
} else {
  print("No outliers found.")
}
