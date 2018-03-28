
## load the library
library(faosws)
library(faoswsUtil)
library(faoswsBalancing)
library(faoswsStandardization)
library(faoswsFlag)

message("libraries loaded")

library(data.table)
library(igraph)
library(stringr)
library(dplyr)
# library(dtplyr)
library(MASS) 
library(lattice)
library(reshape2)
library(sendmailR)

if(packageVersion("faoswsStandardization") < package_version('0.1.0')){
  stop("faoswsStandardization is out of date")
}

## set up for the test environment and parameters
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")

if (CheckDebug()) {
  library(faoswsModules)
  message("Not on server, so setting up environment...")
  
  # Read settings file sws.yml in working directory. See 
  # sws.yml.example for more information
  PARAMS <- ReadSettings("modules/export missing connections/sws.yml")
  message("Connecting to server: ", PARAMS[["current"]])
  
  R_SWS_SHARE_PATH = PARAMS[["share"]]
  apiDirectory = "./R"
  
  ## Get SWS Parameters
  SetClientFiles(dir = PARAMS[["certdir"]])
  GetTestEnvironment(
    baseUrl = PARAMS[["server"]],
    token = PARAMS[["token"]]
  )
  
  
  batchnumber = 000 # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! SET IT   
  
} else {
  batchnumber = 000 # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! SET IT   
  message("Running on server, no need to call GetTestEnvironment...")
  
}


#User name is what's after the slash
SWS_USER = regmatches(swsContext.username, 
                      regexpr("(?<=/).+$", swsContext.username, perl = TRUE))

# instead of replace the existing 
# (for example save example files for different batches)
# put the name in the .yml file
# default is NULL

if(CheckDebug()){
  SUB_FOLDER = paste0(PARAMS[["subShare"]],batchnumber) 
}

message("Getting parameters/datasets...")

# start and end year for standardization come from user parameters
startYear = swsContext.computationParams$startYear
endYear = swsContext.computationParams$endYear
geoM49 = swsContext.computationParams$geom49
stopifnot(startYear <= endYear)
yearVals = as.character(startYear:endYear)

##  Get data configuration and session
sessionKey_suaUnb = swsContext.datasets[[1]]



sessionCountries =
  getQueryKey("geographicAreaM49", sessionKey_suaUnb)

geoKeys = GetCodeList(domain = "agriculture", dataset = "aproduction",
                      dimension = "geographicAreaM49")[type == "country", code]

# Select the countries based on the user input parameter
selectedGEOCode =
  switch(geoM49,
         "session" = sessionCountries,
         "all" = geoKeys)
areaKeys = selectedGEOCode


##############################################################
############ DOWNLOAD AND VALIDATE TREE ######################
##############################################################

treeelemKeys = c("5423", "5431")
treeitemPKeys = GetCodeList(domain = "suafbs", dataset = "ess_fbs_commodity_tree2", "measuredItemParentCPC_tree")
treeitemPKeys = treeitemPKeys[, code]

treeitemCKeys = GetCodeList(domain = "suafbs", dataset = "ess_fbs_commodity_tree2", "measuredItemChildCPC_tree")
treeitemCKeys = treeitemCKeys[, code]

treekey = faosws::DatasetKey(domain = "suafbs", dataset = "ess_fbs_commodity_tree2", dimensions = list(
  geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = areaKeys),
  measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", keys = treeelemKeys),
  measuredItemParentCPC = Dimension(name = "measuredItemParentCPC_tree", keys = treeitemPKeys),
  measuredItemChildCPC = Dimension(name = "measuredItemChildCPC_tree", keys = treeitemCKeys),
  timePointYears = Dimension(name = "timePointYears", keys = yearVals)
))

## Extract the specific tree

tree = faosws::GetData(treekey,omitna = FALSE)
if("flag_obs_status_v2"%in%colnames(tree)){
  setnames(tree,"flag_obs_status_v2","flagObservationStatus")
}



# missingTree=data.table(data.frame(tree[measuredElementSuaFbs=="extractionRate"&Value==0]))
missingTree=data.table(data.frame(tree[measuredElementSuaFbs=="5423"]))

missingTree[,sumEr:=sum(Value),by=c("geographicAreaM49","measuredItemChildCPC_tree","timePointYears")]
missingTree=missingTree[sumEr==0]

missingTree_in=data.table(data.frame(missingTree[,sumEr:=NULL]))


##############################################################
############ Update params for specific dataset ##############
##############################################################

params = defaultStandardizationParameters()
params$itemVar = "measuredItemSuaFbs"
params$mergeKey[params$mergeKey == "measuredItemCPC"] = "measuredItemSuaFbs"
params$elementVar = "measuredElementSuaFbs"
params$childVar = "measuredItemChildCPC"
params$parentVar = "measuredItemParentCPC"
params$productionCode = "production"
params$importCode = "imports"
params$exportCode = "exports"
params$stockCode = "stockChange"
params$foodCode = "food"
params$feedCode = "feed"
params$seedCode = "seed"
params$wasteCode = "loss"
params$industrialCode = "industrial"
params$touristCode = "tourist"
params$foodProcCode = "foodManufacturing"
params$residualCode = "residual"
params$createIntermetiateFile= "TRUE"
params$protected = "Protected"
params$official = "Official"

##############################################################
#################### SET KEYS FOR DATA #######################
##############################################################

elemKeys=c("5510", "5610", "5071", "5023", "5910", "5016", "5165", "5520","5525","5164","5166","5141")

desKeys = c("664","674","684")

## Starting reading SUAs
message("Reading SUA data...")


itemKeys = GetCodeList(domain = "suafbs", dataset = "sua_unbalanced", "measuredItemFbsSua")
itemKeys = itemKeys[, code]



key = DatasetKey(domain = "suafbs", dataset = "sua_unbalanced", dimensions = list(
  geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = areaKeys),
  measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", keys = elemKeys),
  measuredItemFbsSua = Dimension(name = "measuredItemFbsSua", keys = itemKeys),
  timePointYears = Dimension(name = "timePointYears", keys = yearVals)
))



##############################################################
########## DOWNLOAD AND FIX DATA FOR SUGAR CODES #############
##############################################################
# load(file.path(PARAMS$localFiles, "dataTESTNewStand.RData")) 

# data=data[geographicAreaM49=="1248"&timePointYears%in%yearVals]
# data=suppressWarnings(dataDownloadFix(key=key,p=params))


data = elementCodesToNames(data = GetData(key), itemCol = "measuredItemFbsSua",
                           elementCol = "measuredElementSuaFbs")

message("elementCodesToNames ok")

setnames(data, "measuredItemFbsSua", "measuredItemSuaFbs")
data[measuredElementSuaFbs=="stock_change",measuredElementSuaFbs:="stockChange"]
data[measuredElementSuaFbs=="stock",measuredElementSuaFbs:="stockChange"]

data=data[timePointYears%in%yearVals]
data=data[!is.na(measuredElementSuaFbs)]

##############################################################
######### SUGAR RAW CODES TO BE CONVERTED IN 2351F ###########
##############################################################
#######################################
datas=data[measuredItemSuaFbs %in% c("23511.01","23512","2351f")]
datas[,Value2:=sum(Value*ifelse(measuredItemSuaFbs=="2351f",0,1)),by=c("geographicAreaM49","timePointYears","measuredElementSuaFbs")]
sugarComb = datas[, .N, by = c("geographicAreaM49", "timePointYears","measuredElementSuaFbs")]

# Filter all the rows for which only one row exists
filterA=sugarComb[N==1]
filterA=filterA[,N:=NULL]
datasA=datas[filterA, ,on=c("geographicAreaM49", "timePointYears","measuredElementSuaFbs")]
# check if some of this rows are not 2351f and change the flag, in that case
datasA[measuredItemSuaFbs!="2351f",flagObservationStatus:=ifelse(flagObservationStatus!="I","I",flagObservationStatus)]
datasA[measuredItemSuaFbs!="2351f",flagMethod:=ifelse(!(flagMethod%in%c("e","s")),"s",flagMethod)]
datasA[,measuredItemSuaFbs:="2351f"]
datasA[,Value2:=NULL]



filterB=sugarComb[N>1]
filterB=filterB[,N:=NULL]
datasC=datas[filterB, ,on=c("geographicAreaM49", "timePointYears","measuredElementSuaFbs")]
datasC[,s2351f:=max(Value2,Value),by=c("geographicAreaM49","timePointYears","measuredElementSuaFbs")]

datasC[,c("Value","Value2"):=NULL]
datasC[,Value:=s2351f*ifelse(measuredItemSuaFbs=="2351f",1,0),by=c("geographicAreaM49","timePointYears","measuredElementSuaFbs")]
datasB=datasC[Value!=0]

sugarComb2=datasB[,.N,by=c("geographicAreaM49","timePointYears","measuredElementSuaFbs")][,N:=NULL]
datasD=datasC[sugarComb2,,on=c("geographicAreaM49","timePointYears","measuredElementSuaFbs")]

datasD=setdiff(datasC,datasD)

datasD[,Value:=sum(s2351f*ifelse(measuredItemSuaFbs=="2351f",0,1)),by=c("geographicAreaM49","timePointYears","measuredElementSuaFbs")]
datasD=unique(datasD,by=c("geographicAreaM49","timePointYears","measuredElementSuaFbs"))
datasD[,measuredItemSuaFbs:="2351f"]

datasB=datasB[,colnames(datasA),with=FALSE]
datasD=datasD[,colnames(datasA),with=FALSE]
dataTorBind=rbind(datasA,datasB,datasD)

data=data[!(measuredItemSuaFbs %in% c("23511.01","23512","2351f"))]
data=rbind(data,dataTorBind)

# ########################################
#
# data=data[, list(Value = sum(Value, na.rm = TRUE)),
#           by = c("measuredItemSuaFbs","measuredElementSuaFbs", "geographicAreaM49", "timePointYears","flagObservationStatus","flagMethod")]
data=left_join(data,flagValidTable,by=c("flagObservationStatus","flagMethod"))%>%
  data.table

data[flagObservationStatus%in%c("","T"),Official:=TRUE]
# data[flagObservationStatus%in%c(""),Official:=TRUE]
data[is.na(Official),Official:=FALSE]
# data=data[,mget(c("measuredItemSuaFbs","measuredElementSuaFbs", "geographicAreaM49", "timePointYears","Value","Protected","Official"))]
# ######### ######### #########
#######################################
dataFlags = copy(data)
##############################################################
data=data[,.(geographicAreaM49,measuredItemSuaFbs,measuredElementSuaFbs,timePointYears,Value)]

data[,Value:="1"]

data=data[,.N,by=c("geographicAreaM49","measuredItemSuaFbs","timePointYears")]

data[,N:=NULL]
data=unique(data)
setnames(data,"measuredItemSuaFbs","measuredItemChildCPC_tree")
data[,Av:="Y"]

missingTree_in=data.table(left_join(missingTree_in,data,by=c("geographicAreaM49","measuredItemChildCPC_tree","timePointYears")))
missingTree_in=missingTree_in[!is.na(Av)]
missingTree_in[,Av:=NULL]


if(dim(missingTree_in)[1]>0){
  
  if(!CheckDebug()){
    # Create the body of the message
    
    FILETYPE = ".csv"

    basename <- "data"
    basedir <- tempfile()
    dir.create(basedir)
    destfile <- file.path(basedir, paste0(basename, FILETYPE))
    
    # create the csv in a temporary foldes   
    write.csv(missingTree_in, destfile, row.names = FALSE)  
    # define on exit strategy
    on.exit(file.remove(destfile))    

    body = paste(paste0("Tree missing connections for", areaKeys),
                 " ",
                 "DO NOT CHANGE THE FILE-NAME:",
                 " ",
                 "PLEASE FOLLOW THE FOLLOWING INSTRUCTIONS:",
                 " ",
                 "=================================================",
                 "1. OPEN THE FILE WITH NOTEPAD OR NOTEPAD++",
                 " ",
                 "2. CHANGE THE VALUES",
                 " ",
                 "3. CHANGE THE FLAG CONBINATION IS (E,f)",
                 " ",
                 "4. OPEN A SESSION OF E COMMODITY TREE FOR THE SESSION COUNTIES",
                 " ",
                 "UPLOAD THE VALUES IN THE SWS USING THE FOLLOWING SPECIFICATIONS:",
                 " ",
                 "Scope: Data (please unselect Metadata and Block Metadata",
                 "Structure: Normalized",
                 "Key Content: Code" ,
                 "=================================================",
                 sep='\n')
    
    sendmailR::sendmail(from = "sws@fao.org",
                        to = swsContext.userEmail,
                        subject = paste0("Missing Connections", areaKeys),
                        msg = list(strsplit(body,"\n")[[1]], 
                                   sendmailR::mime_part(destfile, 
                                                        name = paste0(basename, FILETYPE)
                                   )
                        )
    )
    paste0("Email sent to ", swsContext.userEmail)
  }else{
    if(file.exists(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"__99_missingConnections.csv"))){
      file.remove(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"__99_missingConnections.csv"))
    }
    dir.create(paste0(PARAMS$debugFolder,"/Batch_",batchnumber), showWarnings = FALSE,recursive=TRUE)
    write.csv(missingTree_in, paste0(PARAMS$debugFolder,"/Batch_",batchnumber,"/B",batchnumber,"__99_missingConnections.csv"), row.names = FALSE)  
    message(paste0("Csv file with missing connections saved in: ", paste0(PARAMS$debugFolder,"/Batch_",batchnumber,"/B",batchnumber,"__99_missingConnections.csv")))
    
  }
  
}else{  # End of case for which flags and/or figures are invalid
  if(!CheckDebug()){
    body = paste("No Missing Connections",
                 sep='\n')
    sendmailR::sendmail(from = "sws@fao.org",
                        to = swsContext.userEmail,
                        subject = sprintf("No missing connections detected"),
                        msg = strsplit(body,"\n")[[1]])
    
  }else{
    message("No Missing Connections")
    
  }
}
