
## load the library
library(faosws)
library(faoswsUtil)
library(faoswsBalancing)
library(faoswsStandardization)
library(faoswsFlag)

library(data.table)
library(igraph)
library(stringr)
library(dplyr)
library(MASS) 
library(lattice)
library(reshape2)

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
  PARAMS <- ReadSettings("modules/standardization/sws.yml")
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

# If one want to create sub-folder in the share drive,
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
sessionKey = swsContext.datasets[[1]]

sessionCountries =
  getQueryKey("geographicAreaM49", sessionKey)

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

# load("C://Users/muschitiello/Desktop/tree.RData")
# 
# tree=tree[geographicAreaM49 %in% areaKeys & timePointYears %in% yearVals]

# ptm <- proc.time()
# tree3=getCommodityTreeNewMethod(areaKeys,yearVals)
# message((proc.time() - ptm)[3])

ptm <- proc.time()
tree=getCommodityTreeNewMethod(areaKeys,yearVals)
message((proc.time() - ptm)[3])


validateTree(tree)

# stop("prova")

# NA ExtractionRates are recorded in the sws dataset as 0
# for the standardization, we nee them to be treated as NA
# therefore here we are re-changing it

tree[Value==0,Value:=NA]

##############################################################
################### MARK OILS COMMODITY ######################
##############################################################

oilFatsCPC=c("2161", "2162", "21631.01", "21641.01", "21641.02", "2168", 
             "21691.14", "2165", "34120", "21932.02", "2166", "21691.07", 
             "2167", "21673", "21691.01", "21691.02", "21691.03", "21691.04", 
             "21691.05", "21691.06", "21631.02", "21691.08", "21691.09", "21691.10", 
             "21691.11", "21691.12", "21691.13", "21691.90", "23620", "21700.01", 
             "21700.02", "21693.02", "34550", "F1275", "21512", "21512.01", 
             "21513", "21514", "F0994", "21515", "21511.01", "21511.02", "21521", 
             "21511.03", "21522", "21519.02", "21519.03", "21529.03", "21529.02", 
             "21932.01", "21523", "F1243", "F0666")

tree[(measuredItemParentCPC%in%oilFatsCPC|measuredItemChildCPC%in%oilFatsCPC),oil:=TRUE]

##############################################################
################ CLEAN NOT OFFICIAL SHARES ###################
################   & SHARES EXCEPT  OILS   ###################
##############################################################

## (E,f) have to be kept
## any other has to ve cleaned except the oils 
tree[,checkFlags:=paste0("(",flagObservationStatus,",",flagMethod,")")]


tree[measuredElementSuaFbs=="share"&(checkFlags=="(E,f)"|oil==TRUE),keep:=TRUE]
tree[measuredElementSuaFbs=="share"&is.na(keep),Value:=NA]

tree[,checkFlags:=NULL]
tree[,oil:=NULL]
tree[,keep:=NULL]


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
elemKeys = GetCodeTree(domain = "suafbs", dataset = "sua_unbalanced", "measuredElementSuaFbs")

#    code              description
# 1:   51                   Output
# 2:   61              Inflow (Qt)
# 3:   71 Variation Intial Exstenc
# 4:   91             Outflow (Qt)
# 5:  101     Use For Animals (Qt)
# 6:  111     Use For Same Product
# 7:  121                   Losses
# 8:  131 Reemployment Same Sector  (remove it)

fs_elements <- c("51", "61", "71", "91", "101", "111", "121", "131")

elemKeys = elemKeys[parent %in% fs_elements,
                    paste0(children, collapse = ", ")]

# code                  description
# 1: 5141                     Food [t]
# 2: 5164 Tourist consumption [1000 t]
# 3: 5165     Industrial uses [1000 t]


sws_elements <- c("5141", "5164", "5165")

elemKeys = unique(c(strsplit(elemKeys, ", ")[[1]], sws_elements))
itemKeys = GetCodeList(domain = "suafbs", dataset = "sua_unbalanced", "measuredItemFbsSua")
itemKeys = itemKeys[, code]

key = DatasetKey(domain = "suafbs", dataset = "sua_unbalanced", dimensions = list(
  geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = areaKeys),
  measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", keys = elemKeys),
  measuredItemFbsSua = Dimension(name = "measuredItemFbsSua", keys = itemKeys),
  timePointYears = Dimension(name = "timePointYears", keys = yearVals)
))

message("Reading SUA data...")


##############################################################
########## DOWNLOAD AND FIX DATA FOR SUGAR CODES #############
##############################################################
# load(file.path(PARAMS$localFiles, "dataTESTNewStand.RData")) 

# data=data[geographicAreaM49=="1248"&timePointYears%in%yearVals]
data=suppressWarnings(dataDownloadFix(key=key,p=params))

# I save a copy of the data with the flag. I'll need this for saving the 
# Forced production lines at the end of the process.
# 
dataFlags = copy(data)
##############################################################

#protected data
#### CRISTINA: after havig discovered that for crops , official food values are Wrong and have to be deleted. 
# now we have to delete all the wrong values:
# THE FOLLOWING STEPS HAVE BEEN COMMENTED BECAUSE THEY SHOULD NOT BE NEEDED
# the data might have to be corrected from the questionnaires

cropsOfficialFood = c("0111","0112","0113","0115","0116","0117","01199.02","01801","01802")
data[!geographicAreaM49%in%c("604")&get(params$itemVar)%in%cropsOfficialFood
     &get(params$elementVar)==params$foodCode
     ,Value:=NA]
# only for Japan, delete also Food of Rice Milled.
data[geographicAreaM49=="392"&get(params$elementVar)==params$foodCode&get(params$itemVar)=="23161.02",Value:=0]

#########################

# For DERIVED select only the protected and the estimation (coming from the submodule)

level = findProcessingLevel(tree, from = params$parentVar,
                            to = params$childVar, aupusParam = params)
primaryEl = level[processingLevel == 0, get(params$itemVar)]

# I have to select Protected and Estimation (I,e)
# For all the others delete the production value
# this will leave the Sua Filling creting prodcution, where needed

data[!(get(params$protected)=="TRUE"|(flagObservationStatus=="I"&flagMethod=="e"))
     &get(params$elementVar)==params$productionCode
     &!(get(params$itemVar) %in% primaryEl),Value:=NA]

##############################################################
##################   RECALCULATE SHARE   #####################
##########   FOR COMMODITIES MANUALLY ENTERED   ##############
##############################################################

p=params

### Compute availability and SHARE  

data[, availability := sum(ifelse(is.na(Value), 0, Value) *
                             ifelse(get(p$elementVar) == p$productionCode, 1,
                                    ifelse(get(p$elementVar) == p$importCode, 1,
                                           ifelse(get(p$elementVar) == p$exportCode, -1,
                                                  ifelse(get(p$elementVar) == p$stockCode, 0,
                                                         ifelse(get(p$elementVar) == p$foodCode, 0,
                                                                ifelse(get(p$elementVar) == p$foodProcCode, 0,
                                                                       ifelse(get(p$elementVar) == p$feedCode, 0,
                                                                              ifelse(get(p$elementVar) == p$wasteCode, 0,
                                                                                     ifelse(get(p$elementVar) == p$seedCode, 0,
                                                                                            ifelse(get(p$elementVar) == p$industrialCode, 0,
                                                                                                   ifelse(get(p$elementVar) == p$touristCode, 0,
                                                                                                          ifelse(get(p$elementVar) == p$residualCode, 0, 0))))))))))))),
     by = c(p$mergeKey)]



mergeToTree = data[, list(availability = mean(availability)),
                   by = c(p$itemVar)]
setnames(mergeToTree, p$itemVar, p$parentVar)
plotTree = copy(tree)

tree2=copy(plotTree)

tree2shares=tree[measuredElementSuaFbs=="share"]
tree2shares[,share:=Value]
tree2shares[,c("measuredElementSuaFbs","Value"):=NULL]

tree2exRa=tree[measuredElementSuaFbs=="extractionRate"]
tree2exRa[,extractionRate:=Value]
tree2exRa[,c("measuredElementSuaFbs","Value"):=NULL]


tree2=data.table(left_join(tree2shares,tree2exRa[,c("geographicAreaM49","measuredItemParentCPC","measuredItemChildCPC",
                                                    "timePointYears","extractionRate"),with=FALSE],by=c("geographicAreaM49","measuredItemParentCPC","measuredItemChildCPC",
                                                                                                        "timePointYears")))

tree2=tree2[!is.na(extractionRate)]
tree2 = merge(tree2, mergeToTree, by = p$parentVar, all.x = TRUE)


#### SHAREs 1 
# share are the proportion of availability of each parent
# on the total availability by child
# Function checkShareValue() checks the validity of the shares,
# change wrong values
# return a severity table 
# and the values to be saved back in the session of the TRee

tree2[,checkFlags:=paste0("(",flagObservationStatus,",",flagMethod,")")]

tree2[,availability.child:=availability*get(p$extractVar)]

tree2[,shareSum:=sum(share),by=c("measuredItemChildCPC", "timePointYears")]

# Create eventual Errors HEre for testing the checkshares function 

uniqueShares2change = tree2[checkFlags=="(E,f)"&(round(shareSum,3)!=1|is.na(shareSum)), .N, by = c("measuredItemChildCPC", "timePointYears")]
uniqueShares2change[, N := NULL]


tree2[,newShare:=NA]
tree2[,severity:=NA]
tree2[,message:=NA]
tree2[,newShare:=as.numeric(newShare)]
tree2[,severity:=as.integer(severity)]
tree2[,message:=as.character(message)]

tree2change = vector(mode = "list", length = nrow(uniqueShares2change))


for (i in seq_len(nrow(uniqueShares2change))) {
  filter = uniqueShares2change[i, ]
  tree2Subset = tree2[filter, , on = c("measuredItemChildCPC", "timePointYears")]
  
  tree2change[[i]] = checkShareValue(tree2Subset)
  
}

tree2change = rbindlist(tree2change)

tree2merge=copy(tree2change)

# Before sending it via email, change flags

if(dim(tree2merge)[1]>0){
  tree2merge[checkFlags!="(E,f)",flagObservationStatus:="I"]
  tree2merge[checkFlags!="(E,f)",flagMethod:="i"]
  tree2merge[,c("checkFlags","availability.child","shareSum","availability","extractionRate"):=NULL]
}

sendMail4shares(tree2merge)

##############################################################
# The following rows were constructed as an attempt of creating a table to be later put in 
#  inside a save Validation function. Unfortunately the saveValidation does not allow to save on a different
#  dataset in a different session.
#  As I don't have the possibility to develop such a function in this moment. These rows are unuseful.
# If in the future, there will be time fot his, The saveValidation can start from here.

# if(nrow(tree2change)){
#   
#   invalidTable=copy(tree2change[,c("geographicAreaM49", "measuredItemParentCPC", "measuredItemChildCPC", 
#                                    "timePointYears","message","severity"),with=FALSE])
#   invalidTable[,measuredElementSuaFbs:="share"]
#   
#   setcolorder(invalidTable,c("geographicAreaM49","measuredElementSuaFbs", "measuredItemParentCPC", "measuredItemChildCPC", 
#                              "timePointYears","message","severity"))
#   
#   setnames(invalidTable,c("message","severity"),c("Description","Severity"))
# }

# stop()
##############################################################

##############################################################

# IF SHARES ARE VALID ( and Tree2change does not exists), The " tree" is the one to use 
# onwards

# nothing has to be done in this case

# IF THERE IS A tree2change, after it has been sent, it has to be integrated in the tree
# before going on

if(dim(tree2merge)[1]>0){
  setnames(tree2merge,"share","Value")
  tree2merge[,c("severity","message"):=NULL]
  tree2merge[,measuredElementSuaFbs:="share"]
  setcolorder(tree2merge,c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemParentCPC", 
                           "measuredItemChildCPC", "timePointYears", "Value", "flagObservationStatus", 
                           "flagMethod"))
  uniquecomb = tree2merge[, .N, by = c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemParentCPC", 
                                       "measuredItemChildCPC", "timePointYears")]
  uniquecomb[,N := NULL]
  
  tree=rbind(tree[!uniquecomb, ,on=c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemParentCPC", 
                                     "measuredItemChildCPC", "timePointYears")],tree2merge)
  
}

##############################################################
################ CHANGE TREE SHAPE & SAVE  ###################
################  TREE TO BE RE-EXPORTED   ###################
##############################################################

# This Tree is saved with a different name with all the flags. 
# From now on Flags will be removed and the tree will be used as usual, but using 
# eventual manually corrected extraction rates and Shares. 

# This is done because the tree has been modifyed in a very subsequent moment from the 
# time the module was created, therefore functions are designed to be used with a structure of the 
# tree different from the one of the Dataset Commodity tree

# is therefore, very important that this order of the things is not changed
tree2beReExported = copy(tree)

tree[,c("flagObservationStatus","flagMethod"):=NULL]
tree=data.table(dcast(tree,geographicAreaM49 + measuredItemParentCPC + measuredItemChildCPC + timePointYears
                      ~ measuredElementSuaFbs,value.var = "Value"))

##############################################################
##################  LAST FIXES ON TREE   #####################
##############################################################

tree=tree[!is.na(extractionRate)]
tree=tree[!is.na(measuredItemChildCPC)]


##############################################################
##############################################################
##############################################################
#######################################################################################
## Update tree by setting some edges to "F", computing average extraction rates
## when missing, and bringing in extreme extraction rates

FPCommodities <- c( "01499.06", "01921.01")

# These commodities are forwards processed instead of backwards processed:
#        code             description type
# 3: 01499.06      Kapokseed in shell CRNP
# 4: 01921.01   Seed cotton, unginned CRPR  

tree[, target := ifelse(measuredItemParentCPC %in% FPCommodities,
                        "F", "B")]

# MERGE the TREE with the item Map fpr future manipulation

itemMap = GetCodeList(domain = "agriculture", dataset = "aproduction", "measuredItemCPC")
itemMap = itemMap[, c("code", "type"), with = FALSE]
setnames(itemMap, "code", "measuredItemSuaFbs")
data = merge(data, itemMap, by = "measuredItemSuaFbs")
setnames(itemMap, "measuredItemSuaFbs", "measuredItemParentCPC")
tree = merge(tree, itemMap, by = "measuredItemParentCPC")

## Remove missing elements
data = data[!is.na(measuredElementSuaFbs), ]


#######################################################
# save the initial data locally for future reports
if(CheckDebug()){
  dir.create(paste0(PARAMS$debugFolder,"/Batch_",batchnumber), showWarnings = FALSE,recursive=TRUE)
}
if(CheckDebug()){
  initialSua = data
  save(initialSua,file=paste0(PARAMS$debugFolder,"/Batch_",batchnumber,"/B",batchnumber,"_01_InitialSua_BeforeCB.RData"))
}
#######################################################
data=data[,mget(c("measuredItemSuaFbs","measuredElementSuaFbs", "geographicAreaM49", "timePointYears","Value","Official","Protected","type"))]


#############################################################
##########    LOAD NUTRIENT DATA AND CORRECT    #############
#############################################################
message("Loading nutrient data...")

itemKeys = GetCodeList("agriculture", "aupus_ratio", "measuredItemCPC")[, code]

# Nutrients are:
# 1001 Calories
# 1003 Proteins
# 1005 Fats
nutrientCodes = c("1001", "1003", "1005")

nutrientData = getNutritiveFactors(measuredElement = nutrientCodes,
                                   timePointYears = yearVals)
setnames(nutrientData, c("measuredItemCPC", "timePointYearsSP"),
         c("measuredItemSuaFbs", "timePointYears"))

# It has been found that some Nutrient Values are wrong in the Nutrient Data Dataset

######### CREAM SWEDEN 

nutrientData[geographicAreaM49=="752"&measuredItemSuaFbs=="22120"&measuredElement=="1001",Value:=195]
nutrientData[geographicAreaM49=="752"&measuredItemSuaFbs=="22120"&measuredElement=="1003",Value:=3]
nutrientData[geographicAreaM49=="752"&measuredItemSuaFbs=="22120"&measuredElement=="1005",Value:=19]

### MILK SWEDEN
nutrientData[geographicAreaM49%in%c("756","300","250","372","276")&measuredItemSuaFbs=="22251.01"&measuredElement=="1001",Value:=387]
nutrientData[geographicAreaM49%in%c("756","300","250","372","276")&measuredItemSuaFbs=="22251.01"&measuredElement=="1003",Value:=26]
nutrientData[geographicAreaM49%in%c("756","300","250","372","276")&measuredItemSuaFbs=="22251.01"&measuredElement=="1005",Value:=30]

nutrientData[geographicAreaM49=="300"&measuredItemSuaFbs=="22253"&measuredElement=="1001",Value:=310]
nutrientData[geographicAreaM49=="300"&measuredItemSuaFbs=="22253"&measuredElement=="1003",Value:=23]
nutrientData[geographicAreaM49=="300"&measuredItemSuaFbs=="22253"&measuredElement=="1005",Value:=23]

#################################################################
#################################################################
#################################################################

message("Defining vectorized standardization function...")

standardizationVectorized = function(data, tree, nutrientData,batchnumber,
                                     utilizationTable){
  
  # record if output is being sunk and at what level
  sinkNumber <- sink.number()
  # Prevent sink staying open if function is terminated prematurely (such as
  # in debugging of functions in standardization)
  on.exit(while (sink.number() > sinkNumber) sink())
  
  if (nrow(data) == 0) {
    message("No rows in data, nothing to do")
    return(data)
  }
  
  # If printCodes is length 0, neither the .md files nor plots are created
  # If it has a non-zero value, those are the codes which will have file outputs
  
  printCodes = character()
  
  # printCodes = c("0112")
  # printCodes = getChildren(commodityTree = tree,
  # parentColname = params$parentVar,
  # childColname = params$childVar,
  # topNodes = printCodes)
  
  
  dir.create(paste0(R_SWS_SHARE_PATH, "/", SWS_USER, "/", SUB_FOLDER, "/standardization/")
             , showWarnings = FALSE,recursive = TRUE
  )
  sink(paste0(R_SWS_SHARE_PATH, "/", SWS_USER,"/", SUB_FOLDER, "/standardization/",
              data$timePointYears[1], "_",
              data$geographicAreaM49[1], "_sample_test.md"),
       split = TRUE)
  
  out = standardizationWrapper(data = data, tree = tree, fbsTree = fbsTree, 
                               standParams = params, printCodes = printCodes,
                               nutrientData = nutrientData,
                               debugFile = params$createIntermetiateFile
                               ,batchnumber = batchnumber,
                               utilizationTable = utilizationTable)
  return(out)
}

## Split data based on the two factors we need to loop over
uniqueLevels = data[, .N, by = c("geographicAreaM49", "timePointYears")]
uniqueLevels[, N := NULL]

parentNodes = getCommodityLevel(tree, parentColname = "measuredItemParentCPC",
                                childColname = "measuredItemChildCPC")

parentNodes = parentNodes[level == 0, node]

message("Beginning actual standardization process...")

aggFun = function(x) {
  if (length(x) > 1)
    stop("x should only be one value!")
  return(sum(x))
}

standData = vector(mode = "list", length = nrow(uniqueLevels))

# Create Local Temporary File for Intermediate Savings
if(CheckDebug()){
  basedir=getwd()
}else{
  basedir <- tempfile()
}

if(params$createIntermetiateFile){
  if(file.exists(paste0(basedir,"\\debugFile\\Batch_",batchnumber,"\\B",batchnumber,"_00a_AfterSuaFilling1.csv"))){
    file.remove(paste0(basedir,"\\debugFile\\Batch_",batchnumber,"\\B",batchnumber,"_00a_AfterSuaFilling1.csv"))
  }
  if(file.exists(paste0(basedir,"\\debugFile\\Batch_",batchnumber,"\\B",batchnumber,"_00b_AfterFoodProc.csv"))){
    file.remove(paste0(basedir,"\\debugFile\\Batch_",batchnumber,"\\B",batchnumber,"_00b_AfterFoodProc.csv"))
  }
  if(file.exists(paste0(basedir,"\\debugFile\\Batch_",batchnumber,"\\B",batchnumber,"_02_AfterSuaFilling_BeforeST.csv"))){
    file.remove(paste0(basedir,"\\debugFile\\Batch_",batchnumber,"\\B",batchnumber,"_02_AfterSuaFilling_BeforeST.csv"))
  }
  if(file.exists(paste0(basedir,"\\debugFile\\Batch_",batchnumber,"\\B",batchnumber,"_03_AfterST_BeforeFBSbal.csv"))){
    file.remove(paste0(basedir,"\\debugFile\\Batch_",batchnumber,"\\B",batchnumber,"_03_AfterST_BeforeFBSbal.csv"))
  }
  if(file.exists(paste0(basedir,"\\debugFile\\Batch_",batchnumber,"\\B",batchnumber,"_10_ForcedProduction.csv"))){
    file.remove(paste0(basedir,"\\debugFile\\Batch_",batchnumber,"\\B",batchnumber,"_10_ForcedProduction.csv"))
  }
}


##  Run all the standardization and balancig for combination of country/year
ptm <- proc.time()
for (i in seq_len(nrow(uniqueLevels))) {
  filter = uniqueLevels[i, ]
  dataSubset = data[filter, , on = c("geographicAreaM49", "timePointYears")]
  treeSubset = tree[filter, , on = c("geographicAreaM49", "timePointYears")]
  treeSubset[, c("geographicAreaM49", "timePointYears") := NULL]
  subNutrientData = nutrientData[filter, , on = c("geographicAreaM49",
                                                  "timePointYears")]
  subNutrientData = dcast(measuredItemSuaFbs ~ measuredElement,
                          data = subNutrientData, value.var = "Value",
                          fun.agg = aggFun)
  setnames(subNutrientData, nutrientCodes,
           c("Calories", "Proteins", "Fats"))
  utilizationTableSubset = utilizationTable[get(params$geoVar)==as.character(uniqueLevels[i,1,with=FALSE])]
  
  standData[[i]] = standardizationVectorized(data = dataSubset,
                                             tree = treeSubset,
                                             nutrientData = subNutrientData,
                                             batchnumber = batchnumber,
                                             utilizationTable = utilizationTableSubset
  )
  
  standData[[i]] <- rbindlist(standData[[i]])
  names(standData[[i]])[grep("^fbsID", names(standData[[i]]))] <- params$itemVar
  standData[[i]][,(params$itemVar):= paste0("S", get(params$itemVar))] 
  
}


message((proc.time() - ptm)[3])

message("Combining standardized data...")
standData = rbindlist(standData)


##  Save the StandData LOCALLY
if(CheckDebug()){
  save(standData,file=paste0(PARAMS$temporaryStandData,"/standDatabatch",batchnumber,".RData"))
}

###################################
## AFTER SUA FILLING 1 (No intermediate saving)
if(CheckDebug()){
  ptm <- proc.time()
  if(file.exists(paste0(basedir,"\\debugFile\\Batch_",batchnumber,"\\B",batchnumber,"_00a_AfterSuaFilling1.csv"))){
    AfterSuaFilling1 = read.table(paste0(basedir,"\\debugFile\\Batch_",batchnumber,"\\B",batchnumber,"_00a_AfterSuaFilling1.csv"),
                                  header=FALSE,sep=";",col.names=c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemFbsSua",
                                                                   "timePointYears","Value","flagObservationStatus","flagMethod"),
                                  colClasses = c("character","character","character","character","character","character","character"))
    AfterSuaFilling1 = data.table(AfterSuaFilling1)
    message((proc.time() - ptm)[3])
    
    # Save these data LOCALLY
    if(CheckDebug()){
      save(AfterSuaFilling1,file=paste0(PARAMS$debugFolder,"/Batch_",batchnumber,"/B",batchnumber,"_00a_AfterSuaFilling1.RData"))
    }
  }
}
###################################


###################################
## AFTER FOOD PROCESSING (No intermediate saving)
if(CheckDebug()){
  ptm <- proc.time()
  if(file.exists(paste0(basedir,"\\debugFile\\Batch_",batchnumber,"\\B",batchnumber,"_00b_AfterFoodProc.csv"))){
    AfterFoodProc = read.table(paste0(basedir,"\\debugFile\\Batch_",batchnumber,"\\B",batchnumber,"_00b_AfterFoodProc.csv"),
                               header=FALSE,sep=";",col.names=c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemFbsSua",
                                                                "timePointYears","Value","flagObservationStatus","flagMethod"),
                               colClasses = c("character","character","character","character","character","character","character"))
    AfterFoodProc = data.table(AfterFoodProc)
    message((proc.time() - ptm)[3])
    
    # Save these data LOCALLY
    if(CheckDebug()){
      save(AfterFoodProc,file=paste0(PARAMS$debugFolder,"\\Batch_",batchnumber,"\\B",batchnumber,"_00b_AfterFoodProc.RData"))
    }
  }
}


###################################
## FORCED COMMODITIES IN THE SUA FILLING PROCESS (to be sent by mail)

if(file.exists(paste0(basedir,"\\debugFile\\Batch_",batchnumber,"\\B",batchnumber,"_10_ForcedProduction.csv"))){
  FORCED_PROD = read.table(paste0(basedir,"\\debugFile\\Batch_",batchnumber,"\\B",batchnumber,"_10_ForcedProduction.csv"),
                           header=FALSE,sep=";",col.names=c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemFbsSua",
                                                            "timePointYears","Value","flagObservationStatus","flagMethod"),
                           colClasses = c("character","character","character","character","character","character","character"))
  FORCED_PROD = data.table(FORCED_PROD)
  message=(paste0( length(FORCED_PROD[,unique(measuredItemFbsSua)])," commodities have a FORCED Official Production"))
  
  
  setnames(FORCED_PROD,"measuredItemFbsSua","measuredItemSuaFbs")
  setnames(dataFlags,"Value","ValueOld")
  setnames(FORCED_PROD,"Value","ValueForced")
  
  ForcedProd2send=data.table(left_join(FORCED_PROD[,c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemSuaFbs",
                                                         "timePointYears","ValueForced"),with=FALSE],dataFlags,by=c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemSuaFbs",
                                                                                                              "timePointYears")))
  
  ForcedProd2send[measuredElementSuaFbs!=params$productionCode,ValueForced:="-"]

  # Save these data LOCALLY
  if(CheckDebug()){
    save(FORCED_PROD,file=paste0(PARAMS$debugFolder,"/Batch_",batchnumber,"/B",batchnumber,"_10_ForcedProduction.RData"))
  }else{
    # or send them by email
    if(dim(ForcedProd2send)[1]>0){
    sendMail4forced(ForcedProd2send)
    }
  }
}



###################################
### FIRST INTERMEDIATE SAVE

ptm <- proc.time()
AfterSuaFilling = read.table(paste0(basedir,"\\debugFile\\Batch_",batchnumber,"\\B",batchnumber,"_02_AfterSuaFilling_BeforeST.csv"),
                             header=FALSE,sep=";",col.names=c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemFbsSua",
                                                              "timePointYears","Value","flagObservationStatus","flagMethod"),
                             colClasses = c("character","character","character","character","character","character","character"))
AfterSuaFilling = data.table(AfterSuaFilling)
SaveData(domain = "suafbs", dataset = "sua_balanced", data = AfterSuaFilling, waitTimeout = 20000)
message((proc.time() - ptm)[3])

# Save these data LOCALLY
if(CheckDebug()){
  save(AfterSuaFilling,file=paste0(PARAMS$debugFolder,"/Batch_",batchnumber,"/B",batchnumber,"_02_AfterSuaFilling_BeforeST.RData"))
}

###################################
## IMBALANCE ANALYSIS SAVE 3
### SECOND INTERMEDIATE SAVE
ptm <- proc.time()
AfterST_BeforeFBSbal = read.table(paste0(basedir,"\\debugFile\\Batch_",batchnumber,"\\B",batchnumber,"_03_AfterST_BeforeFBSbal.csv"),
                                  header=FALSE,sep=";",col.names=c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemFbsSua", 
                                                                   "timePointYears","Value","flagObservationStatus","flagMethod"),
                                  colClasses = c("character","character","character","character","character","character","character"))
AfterST_BeforeFBSbal = data.table(AfterST_BeforeFBSbal)
SaveData(domain = "suafbs", dataset = "fbs_standardized", data = AfterST_BeforeFBSbal, waitTimeout = 20000)
message((proc.time() - ptm)[3])

# Save these data LOCALLY
if(CheckDebug()){
  save(AfterST_BeforeFBSbal,file=paste0(PARAMS$debugFolder,"/Batch_",batchnumber,"/B",batchnumber,"_03_AfterST_BeforeFBSbal.RData"))
}


# Remove all intermediate Files Create in Temp Folder
if(!CheckDebug){
  unlink(basedir)
}else{
  unlink(paste0(basedir,"\\debugFile\\Batch_",batchnumber))
}


###################################
# TREE TO BE RESAVED

###  Merge the Tree with the new Values

tree=tree[,c("geographicAreaM49","measuredItemParentCPC","measuredItemChildCPC","timePointYears","extractionRate","share"),with=FALSE]

tree2melt=melt(tree,id.vars = c("geographicAreaM49","measuredItemParentCPC","measuredItemChildCPC","timePointYears"),
               variable.name = "measuredElementSuaFbs",value.name = "Value")

tree2beReExported2=tree2beReExported[,c("geographicAreaM49","measuredItemParentCPC","measuredItemChildCPC","timePointYears","measuredElementSuaFbs","flagObservationStatus","flagMethod"),with=FALSE]
# newTree=data.table(left_join(tree2beReExported2,tree2melt,by=c("geographicAreaM49","measuredItemParentCPC","measuredItemChildCPC","timePointYears","measuredElementSuaFbs")))

newTree=merge(tree2beReExported2,tree2melt,
              by=c("geographicAreaM49","measuredItemParentCPC","measuredItemChildCPC",
                   "timePointYears","measuredElementSuaFbs"),all = TRUE)

###  Change Flags of Recalculated Shares in the Commodity Tree
# Combination not to be touched are 

newTree[measuredElementSuaFbs=="share"&(measuredItemParentCPC%in%oilFatsCPC|measuredItemChildCPC%in%oilFatsCPC),flagFix:=T]
newTree[flagObservationStatus=="E"&flagMethod=="f",flagFix:=T]
# Flags to be assigned are those of the Shares which have been calculated during the Standardization
newTree[measuredElementSuaFbs=="share"&is.na(flagFix),flagObservationStatus:="I"]
newTree[measuredElementSuaFbs=="share"&is.na(flagFix),flagMethod:="i"]

tree2saveBack=newTree[,c("geographicAreaM49","measuredItemParentCPC","measuredItemChildCPC",
                         "timePointYears","measuredElementSuaFbs","flagObservationStatus",
                         "flagMethod","Value"),with=FALSE]

### Before Saving Bach NA have to be changed to zero 
tree2saveBack[is.na(Value),Value:=0]



tree2saveBack[measuredElementSuaFbs=="extractionRate",measuredElementSuaFbs:="5423",]
tree2saveBack[measuredElementSuaFbs=="share",measuredElementSuaFbs:="5431"]

ptm <- proc.time()
SaveData(domain = "suafbs", dataset = "ess_fbs_commodity_tree2", data = tree2saveBack, waitTimeout = 20000)
message((proc.time() - ptm)[3])



###################################
### FINAL SAVE
fbs_sua_conversion2 <- data.table(measuredElementSuaFbs=c("Calories", "Fats", "Proteins","DESfoodSupply_kCd","proteinSupplyQt_gCd","fatSupplyQt_gCd", "exports", "feed", "food",
                                                          "foodManufacturing", "imports", "loss", "production",
                                                          "seed", "stockChange", "residual","industrial", "tourist"),
                                  code=c("261", "281", "271","664","674","684","5910", "5520", "5141",
                                         "5023", "5610", "5016", "5510",
                                         "5525", "5071", "5166","5165", "5164"))


standData = merge(standData, fbs_sua_conversion2, by = "measuredElementSuaFbs")
standData[,`:=`(measuredElementSuaFbs = NULL)]
setnames(standData, "code", "measuredElementSuaFbs")

standData[, standardDeviation := NULL]

## Assign flags: I for imputed (as we're estimating/standardizing) and s for
## "sum" (aggregate)
standData[, flagObservationStatus := "I"]
standData[, flagMethod := "s"]

setcolorder(standData, c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemSuaFbs", "timePointYears",
"Value", "flagObservationStatus", "flagMethod"))

# Remove NA Values
standData <- standData[!is.na(Value),]






message("Attempting to save standardized data...")

setnames(standData, "measuredItemSuaFbs", "measuredItemFbsSua")

ptm <- proc.time()
out = SaveData(domain = "suafbs", dataset = "fbs_balanced", data = standData, waitTimeout = 2000000)
cat(out$inserted + out$ignored, " observations written and problems with ",
    out$discarded, sep = "")
paste0(out$inserted + out$ignored, " observations written and problems with ",
       out$discarded)
message((proc.time() - ptm)[3])




