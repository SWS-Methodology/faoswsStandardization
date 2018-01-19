
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
startYear = as.numeric(swsContext.computationParams$startYear)
endYear = as.numeric(swsContext.computationParams$endYear)
stopifnot(startYear <= endYear)

yearVals = as.character(startYear:endYear)


##############################################################
################## DOWNLOAD TREE FROM SWS ####################
##############################################################


areaKeys = GetCodeList(domain = "suafbs", dataset = "sua_unbalanced", "geographicAreaM49")
areaKeys = areaKeys[type == "country", code]

elemKeys = GetCodeTree(domain = "suafbs", dataset = "sua_unbalanced", "measuredElementSuaFbs")
elemKeys = elemKeys[parent =="41",
                    paste0(children, collapse = ", ")]

elemKeys = C("5423")
elemKeys = c(strsplit(elemKeys, ", ")[[1]], sws_elements)
itemKeys = GetCodeList(domain = "suafbs", dataset = "sua_unbalanced", "measuredItemFbsSua")
itemKeys = itemKeys[, code]

treekey = DatasetKey(domain = "suafbs", dataset = "sua_unbalanced", dimensions = list(
  geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = areaKeys),
  measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", keys = elemKeys),
  measuredItemFbsSua = Dimension(name = "measuredItemFbsSua", keys = itemKeys),
  timePointYears = Dimension(name = "timePointYears", keys = yearVals)
))


tree = GetData(treekey)

##############################################################
################## SOME TREE CORRECTIONS #####################
##############################################################
tree[,timePointYearsSP:=as.character(timePointYearsSP)]

oilFatsCPC=c("2161", "2162", "21631.01", "21641.01", "21641.02", "2168", 
             "21691.14", "2165", "34120", "21932.02", "2166", "21691.07", 
             "2167", "21673", "21691.01", "21691.02", "21691.03", "21691.04", 
             "21691.05", "21691.06", "21631.02", "21691.08", "21691.09", "21691.10", 
             "21691.11", "21691.12", "21691.13", "21691.90", "23620", "21700.01", 
             "21700.02", "21693.02", "34550", "F1275", "21512", "21512.01", 
             "21513", "21514", "F0994", "21515", "21511.01", "21511.02", "21521", 
             "21511.03", "21522", "21519.02", "21519.03", "21529.03", "21529.02", 
             "21932.01", "21523", "F1243", "F0666")

tree[!(measuredItemParentCPC%in%oilFatsCPC|measuredItemChildCPC%in%oilFatsCPC),share:=NA]
tree=tree[timePointYearsSP%in%yearVals]
tree=tree[!is.na(extractionRate)]
tree=tree[!is.na(measuredItemChildCPC)]


##############################################################
################## DOWNLOAD DATA FROM SWS ####################
##############################################################

areaKeys = GetCodeList(domain = "suafbs", dataset = "sua_unbalanced", "geographicAreaM49")
areaKeys = areaKeys[type == "country", code]
areaKeys=c("1248","454","686","360","392","484","716")

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

elemKeys = c(strsplit(elemKeys, ", ")[[1]], sws_elements)
itemKeys = GetCodeList(domain = "suafbs", dataset = "sua_unbalanced", "measuredItemFbsSua")
itemKeys = itemKeys[, code]

key = DatasetKey(domain = "suafbs", dataset = "sua_unbalanced", dimensions = list(
  geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = areaKeys),
  measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", keys = elemKeys),
  measuredItemFbsSua = Dimension(name = "measuredItemFbsSua", keys = itemKeys),
  timePointYears = Dimension(name = "timePointYears", keys = yearVals)
))

message("Reading SUA data...")

## This gets the values for all countries, all elements which are children of the
## element classes listed above, all CPCs in suafbs and all years between those
## specified by the user.

data = elementCodesToNames(data = GetData(key), itemCol = "measuredItemFbsSua",
elementCol = "measuredElementSuaFbs")
setnames(data, "measuredItemFbsSua", "measuredItemSuaFbs")
data[measuredElementSuaFbs=="stock",measuredElementSuaFbs:="stockChange"]

data=data[timePointYears%in%yearVals]
data=data[!is.na(measuredElementSuaFbs)]

## Update params for specific dataset
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



# Remove Feed for SUgar refined

data[measuredItemSuaFbs=="23520"&measuredElementSuaFbs=="feed",Value:=0]

########################################
# Final Changes in the data files for sugar
### temporary change in the data for accounting for corrections in sugar Tree

#######################################
datas=data[measuredItemSuaFbs %in% c("23511.01","23512","2351f")]
datas[measuredElementSuaFbs=="tourist",measuredItemSuaFbs:="2351f"]
datas[measuredElementSuaFbs=="stockChange"&geographicAreaM49=="705",measuredItemSuaFbs:="2351f"]
datas[measuredElementSuaFbs=="food"&geographicAreaM49%in%c("422","28","308"),measuredItemSuaFbs:="2351f"]
datas[,s2351f:=sum(Value*
                     ifelse(measuredElementSuaFbs == "production"&measuredItemSuaFbs=="2351f",1,0),na.rm = TRUE),
      by=c("geographicAreaM49","timePointYears")]
dataTorBind = unique(datas[measuredElementSuaFbs=="production",list(geographicAreaM49,timePointYears,measuredElementSuaFbs,s2351f,flagObservationStatus,flagMethod)])
datas[,s2351f:=NULL]
datas=datas[!(measuredItemSuaFbs%in%c("23511.01","23512"))]
dataTorBind = dataTorBind[,measuredItemSuaFbs:="2351f"]

setnames(dataTorBind,"s2351f","Value")
dataTorBind=dataTorBind[,c(7,3,1,2,4:6),with=FALSE]

datas=rbind(datas[!measuredElementSuaFbs=="production"],dataTorBind)
data=data[!(measuredItemSuaFbs %in% c("23511.01","23512","2351f"))]

data=rbind(data,datas)

########################################

data=data[, list(Value = sum(Value, na.rm = TRUE)),
          by = c("measuredItemSuaFbs","measuredElementSuaFbs", "geographicAreaM49", "timePointYears","flagObservationStatus","flagMethod")]
data=left_join(data,flagValidTable,by=c("flagObservationStatus","flagMethod"))%>%
  data.table

data[flagObservationStatus%in%c("","T"),Official:=TRUE]
data[is.na(Official),Official:=FALSE]

#######################################

#protected data
#### CRISTINA: after havig discovered that for crops , official food values are Wrong and have to be deleted. 
# now we have to delete all the wrong values:
# THE FOLLOWING STEPS HAVE BEEN COMMENTED BECAUSE THEY SHOULD NOT BE NEEDED
# the data might have to be corrected from the questionnaires

cropsOfficialFood = c("0111","0112","0113","0115","0116","0117","01199.02","01801","01802")
data[!geographicAreaM49%in%c("604")&get(params$itemVar)%in%cropsOfficialFood
     &get(params$elementVar)==params$foodCode
     ,Value:=NA]

# modify utilization Table for: 
# 1. Not taking into account food for the non food primary item
# 2. Consider Loss only for the item in the list of the loss dataset
utilizationTable=utilizationTable[!(measuredElementSuaFbs=="food"&measuredItemSuaFbs%in%cropsOfficialFood)]
utilizationTable=utilizationTable[!(measuredElementSuaFbs=="loss"&measuredItemSuaFbs%in%fbsTree[fbsID4%in%c("2605"),measuredItemSuaFbs])]

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


#######################################################################################
## Update tree by setting some edges to "F", computing average extraction rates
## when missing, and bringing in extreme extraction rates

FPCommodities <- c( "01499.06", "01921.01")
##


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

data[,.N,by=c("geographicAreaM49","timePointYears","measuredElementSuaFbs","measuredItemSuaFbs")][N>1]


##### This is a temporary correction for Batch 107,108,109
##### Code 2351f Has to be checked... prodcution & flags)
data=unique(data,by=c("geographicAreaM49","timePointYears","measuredElementSuaFbs","measuredItemSuaFbs"))
########################
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

data[geographicAreaM49=="392"&measuredElementSuaFbs=="food"&measuredItemSuaFbs=="23161.02",Value:=0]
########################
########################
########################






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

message("Defining vectorized standardization function...")

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

##################################################################!!!!!!!!!!!!!!!!!!!!
##################################################################!!!!!!!!!!!!!!!!!!!!
##################################################################!!!!!!!!!!!!!!!!!!!!


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
 # printCodes = c(fbsTree[fbsID3=="2914",measuredItemSuaFbs],fbsTree[fbsID4%in%c("2737"),measuredItemSuaFbs])
 # printCodes = fbsTree[fbsID4%in%c("2542","2543"),measuredItemSuaFbs]
 # printCodes = c("21641.01","21641.02","2161","2165",fbsTree[fbsID4=="2586",measuredItemSuaFbs])
 # printCodes = c("21641.01","21641.02","2161","2165","0115","24320")
 # printCodes = c(fbsTree[fbsID4=="2805",measuredItemSuaFbs],"0113")
 # printCodes = c("24490","F0665","F1061","22254","01324","01620","01355.90","01211","21512","2165",
 #                "01318","01234","39120.04","39130.04","23220.02","22130.01","22110.02","02211",
 #                "02212","02291","02292")
 # printCodes = c("0112")
 # printCodes = fbsTree[fbsID4%in%c("2605"),measuredItemSuaFbs]

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
setnames(tree, "timePointYearsSP", "timePointYears")
uniqueLevels = data[, .N, by = c("geographicAreaM49", "timePointYears")]
uniqueLevels[, N := NULL]
elementGroup = ReadDatatable("element_codes")


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

### ONLY CONSIDER FAOSTAT COUNTRIES
uniqueLevels=uniqueLevels[geographicAreaM49 %in% c("4", "8", "12", "24", "28", "32", "51", "36", "40", "31", "44", 
                                                   "50", "52", "112", "56", "84", "204", "60", "68", "70", "72", 
                                                   "76", "96", "100", "854", "132", "116", "120", "124", "140", 
                                                   "148", "152", "344", "446", "1248", "158", "170", "178", "188", 
                                                   "384", "191", "192", "196", "203", "408", "208", "262", "212", 
                                                   "214", "218", "818", "222", "233", "231", "242", "246", "250", 
                                                   "258", "266", "270", "268", "276", "288", "300", "308", "320", 
                                                   "324", "624", "328", "332", "340", "348", "352", "356", "360", 
                                                   "364", "368", "372", "376", "380", "388", "392", "400", "398", 
                                                   "404", "296", "414", "417", "418", "428", "422", "426", "430", 
                                                   "440", "442", "450", "454", "458", "462", "466", "470", "478", 
                                                   "480", "484", "496", "499", "504", "508", "104", "516", "524", 
                                                   "528", "530", "540", "554", "558", "562", "566", "578", "512", 
                                                   "586", "591", "600", "604", "608", "616", "620", "410", "498", 
                                                   "642", "643", "646", "659", "662", "670", "882", "678", "682", 
                                                   "686", "688", "891", "694", "703", "705", "90", "710", "724", 
                                                   "144", "729", "736", "740", "748", "752", "756", "762", "764", 
                                                   "807", "626", "768", "780", "788", "792", "795", "800", "804", 
                                                   "784", "826", "834", "840", "858", "860", "548", "862", "704", 
                                                   "887", "894", "716")]
# Countries not Existing
uniqueLevels=uniqueLevels[!geographicAreaM49 %in% c("728","886","654"),]
# Countries with mapping problems and deleted for the moment
uniqueLevels=uniqueLevels[!geographicAreaM49 %in% c("499","736","729","891","688","720","887","616","530"),]
# to see all the countries having problems, run the following code: 
# nameData("suafbs","sua_unbalanced",data.table(geographicAreaM49=c("499","736","729","891","688","720","887","616","530","728","886","654")))
# Batch with the 45 priority countries
# uniqueLevels=uniqueLevels[geographicAreaM49 %in% c("1248", "356", "586", "231", "50", "360", "834", "608", "566",
                                                   # "408", "704", "800", "404", "4", "368", "450", "104", "894",
#                                                    "508", "887", "332", "484", "716", "764", "144", "148", "170",
#                                                    "646", "854", "454", "24", "762", "384", "320", "604", "140",
#                                                    "524", "116", "218", "686", "120", "324", "562", "68", "178"),]

########################
uniqueLevels=uniqueLevels[geographicAreaM49 %in% c("1248","454","686","360","392","484","716"),]  ### TEST 6
uniqueLevels=uniqueLevels[order(geographicAreaM49,timePointYears)]
########################
# uniqueLevels=uniqueLevels[geographicAreaM49 %in% c("716"),]

if(params$createIntermetiateFile){
  if(file.exists(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_00a_AfterSuaFilling1.csv"))){
    file.remove(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_00a_AfterSuaFilling1.csv"))
  }
  if(file.exists(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_00b_AfterFoodProc.csv"))){
    file.remove(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_00b_AfterFoodProc.csv"))
  }
  if(file.exists(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_02_AfterSuaFilling_BeforeST.csv"))){
    file.remove(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_02_AfterSuaFilling_BeforeST.csv"))
  }
  if(file.exists(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_03_AfterST_BeforeFBSbal.csv"))){
    file.remove(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_03_AfterST_BeforeFBSbal.csv"))
  }
  if(file.exists(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_10_ForcedProduction.csv"))){
    file.remove(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_10_ForcedProduction.csv"))
  }
}




# ## IMBALANCE ANALYSIS 1: INITIAL SUA IMBALANCE 
# # save the initial data locally for future reports
# if(CheckDebug()){
#   dir.create(paste0(PARAMS$debugFolder,"/Batch_",batchnumber), showWarnings = FALSE,recursive=TRUE)
# }
# if(CheckDebug()){
# initialSua = data
# save(initialSua,file=paste0(PARAMS$debugFolder,"/Batch_",batchnumber,"/B",batchnumber,"_01_InitialSua_BeforeCB.RData"))
# }


# Run all the standardization and balancig for combination of country/year
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

# Save the StandData
# save(standData,file=("debugFile/standData.RData"))

# 

# Save the StandData LOCALLY
if(CheckDebug()){
  save(standData,file=paste0(PARAMS$temporaryStandData,"/standDatabatch",batchnumber,".RData"))
}
#################################################################
###################################
## INITIAL IMBALANCE
  # write.table(initialSua, paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_000_InitialSua.csv"))



###################################
## AFTER SUA FILLING 1 (No intermediate saving)
ptm <- proc.time()
if(file.exists(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_00a_AfterSuaFilling1.csv"))){
  AfterSuaFilling1 = read.table(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_00a_AfterSuaFilling1.csv"),
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
###################################


###################################
## AFTER FOOD PROCESSING
ptm <- proc.time()
if(file.exists(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_00b_AfterFoodProc.csv"))){
  AfterFoodProc = read.table(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_00b_AfterFoodProc.csv"),
                           header=FALSE,sep=";",col.names=c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemFbsSua",
                                                            "timePointYears","Value","flagObservationStatus","flagMethod"),
                           colClasses = c("character","character","character","character","character","character","character"))
  AfterFoodProc = data.table(AfterFoodProc)
  message((proc.time() - ptm)[3])
  
  # Save these data LOCALLY
  if(CheckDebug()){
    save(AfterFoodProc,file=paste0(PARAMS$debugFolder,"/Batch_",batchnumber,"/B",batchnumber,"_00b_AfterFoodProc.RData"))
  }
}
###################################

###################################
## FORCED COMMODITIES IN THE SUA FILLING PROCESS
ptm <- proc.time()
if(file.exists(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_10_ForcedProduction.csv"))){
FORCED_PROD = read.table(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_10_ForcedProduction.csv"),
                              header=FALSE,sep=";",col.names=c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemFbsSua",
                                                               "timePointYears","Value","flagObservationStatus","flagMethod"),
                              colClasses = c("character","character","character","character","character","character","character"))
FORCED_PROD = data.table(FORCED_PROD)
message((proc.time() - ptm)[3])
message(paste0( length(FORCED_PROD[,unique(measuredItemFbsSua)])," Commodity have a FORCED supply element"))

# Save these data LOCALLY
if(CheckDebug()){
  save(FORCED_PROD,file=paste0(PARAMS$debugFolder,"/Batch_",batchnumber,"/B",batchnumber,"_10_ForcedProduction.RData"))
 }
}
###################################


###################################
## IMBALANCE ANALYSIS SAVE 2
### FIRST INTERMEDIATE SAVE

ptm <- proc.time()
AfterSuaFilling = read.table(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_02_AfterSuaFilling_BeforeST.csv"),
                     header=FALSE,sep=";",col.names=c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemFbsSua",
                                                      "timePointYears","Value","flagObservationStatus","flagMethod"),
                     colClasses = c("character","character","character","character","character","character","character"))
AfterSuaFilling = data.table(AfterSuaFilling)
# SaveData(domain = "suafbs", dataset = "sua_balanced", data = AfterSuaFilling, waitTimeout = 20000)
message((proc.time() - ptm)[3])

# Save these data LOCALLY
if(CheckDebug()){
  save(AfterSuaFilling,file=paste0(PARAMS$debugFolder,"/Batch_",batchnumber,"/B",batchnumber,"_02_AfterSuaFilling_BeforeST.RData"))
}

###################################
## IMBALANCE ANALYSIS SAVE 3
### SECOND INTERMEDIATE SAVE
ptm <- proc.time()
AfterST_BeforeFBSbal = read.table(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_03_AfterST_BeforeFBSbal.csv"),
                       header=FALSE,sep=";",col.names=c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemFbsSua", 
                                                        "timePointYears","Value","flagObservationStatus","flagMethod"),
                       colClasses = c("character","character","character","character","character","character","character"))
AfterST_BeforeFBSbal = data.table(AfterST_BeforeFBSbal)
# SaveData(domain = "suafbs", dataset = "fbs_standardized", data = AfterST_BeforeFBSbal, waitTimeout = 20000)
message((proc.time() - ptm)[3])

# Save these data LOCALLY
if(CheckDebug()){
  save(AfterST_BeforeFBSbal,file=paste0(PARAMS$debugFolder,"/Batch_",batchnumber,"/B",batchnumber,"_03_AfterST_BeforeFBSbal.RData"))
}

# ###################################
# ### FINAL SAVE
# 
# warning("The below is a rough hack to convert codes back. In truth, I'm almost
#         certain that the units are not the same. A unit conversion needs to happen at
#         the beginning and at this step.")
# 
# fbs_sua_conversion <- data.table(measuredElementSuaFbs=c("Calories", "Fats", "Proteins", "exports", "feed", "food",
#                                                          "foodManufacturing", "imports", "loss", "production",
#                                                          "seed", "stockChange", "residual","industrial", "tourist",
#                                                          "DES_calories","DES_proteins","DES_fats", "population"),
#                                  code=c("261", "281", "271", "5910", "5520", "5141",
#                                         "5023", "5610", "5016", "5510",
#                                         "5525", "5071", "5166","5165", "5164","664","674","684","5215"))
# 
# ##standData[measuredElementSuaFbs %in% c(params$touristCode, params$industrialCode),
# ##  `:=`(measuredElementSuaFbs = "other",
# ##       Value = sum(Value, na.rm=TRUE)),
# ##  by=c(params$mergeKey)]
# 
# standData = merge(standData, fbs_sua_conversion, by = "measuredElementSuaFbs")
# standData[,`:=`(measuredElementSuaFbs = NULL)]
# setnames(standData, "code", "measuredElementSuaFbs")
# 
# standData[, standardDeviation := NULL]
# 
# ## Assign flags: I for imputed (as we're estimating/standardizing) and s for
# ## "sum" (aggregate)
# standData[, flagObservationStatus := "I"]
# standData[, flagMethod := "s"]
# 
# setcolorder(standData, c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemSuaFbs", "timePointYears",
# "Value", "flagObservationStatus", "flagMethod"))
# 
# # Remove NA Values
# standData <- standData[!is.na(Value),]
# 
# message("Attempting to save standardized data...")
# 
# 
# setnames(standData, "measuredItemSuaFbs", "measuredItemFbsSua")
# 
# ptm <- proc.time()
# out = SaveData(domain = "suafbs", dataset = "fbs_balanced_", data = standData, waitTimeout = 2000000)
# cat(out$inserted + out$ignored, " observations written and problems with ",
#     out$discarded, sep = "")
# paste0(out$inserted + out$ignored, " observations written and problems with ",
#        out$discarded)
# message((proc.time() - ptm)[3])
# 
# 


