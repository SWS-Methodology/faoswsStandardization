

suppressMessages({
    library(data.table)
    library(faosws)
    library(faoswsFlag)
    library(faoswsUtil)
    library(faoswsImputation)
    library(faoswsProduction)
    library(faoswsProcessing)
    library(faoswsEnsure)
    library(magrittr)
    library(dplyr)
    library(sendmailR)
    library(faoswsStandardization)
  library(stringr)
    
})
# 
top48FBSCountries = c(4,24,50,68,104,120,140,144,148,1248,170,178,218,320,
                      324,332,356,360,368,384,404,116,408,450,454,484,508,
                      524,562,566,586,604,608,716,646,686,762,834,764,800,
                      854,704,231,887,894,760,862,860)

R_SWS_SHARE_PATH <- Sys.getenv("R_SWS_SHARE_PATH")

if(CheckDebug()){
    
    library(faoswsModules)
    SETTINGS = ReadSettings("modules/treeCheck/sws.yml")
    
    ## If you're not on the system, your settings will overwrite any others
    R_SWS_SHARE_PATH = SETTINGS[["share"]]
    
    ## Define where your certificates are stored
    SetClientFiles(SETTINGS[["certdir"]])
    
    ## Get session information from SWS. Token must be obtained from web interface
    GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                       token = SETTINGS[["token"]])
    
    dir=paste0("C:/Work/SWS/FBS/Production/DerivedProduction/Output/")
    
}
sessionKey = swsContext.datasets[[1]]

###########################################################################################################
##'This are those commodity that are pubblished on FAOSTAT


##'I build the key, always the same in the PRODUCTION sub modules:
##completeImputationKey=getCompleteImputationKey("production")
FBScountries=ReadDatatable("fbs_countries")[,code]
FBSItems=ReadDatatable("fbs_tree")[,item_sua_fbs]
fclmapping=ReadDatatable("fcl_2_cpc")
countrymapping=ReadDatatable("fal_2_m49")


##----------------------------------------------------------------------------------------------------------
##'  Get default parameters
params = defaultProcessedItemParams()
processedCPC=ReadDatatable("processed_item")[,measured_item_cpc]
toBePubblished=ReadDatatable("processed_item")[faostat==TRUE,measured_item_cpc]

#############################################################################################################
##'  Get the commodity tree from the TREE DATASET:
##'  The country dimention depends on the session: 
geoImputationSelection = swsContext.computationParams$geom49
sessionCountry=getQueryKey("geographicAreaM49", sessionKey)
selectedCountry =
    switch(geoImputationSelection,
           "session" = sessionCountry,
           "all" = FBScountries)
#selectedCountry = "233"


##---------------------------------------------------------------------------------------------------------
##'  Get the list of processed items to impute
#ItemImputationSelection = swsContext.computationParams$Items
#sessionItems=getQueryKey("measuredItemFbsSua", sessionKey)
sessionItems = FBSItems

##'  The year dimention depends on the session: 
startYear=2000
# imputationStartYear = startYear
#imputationStartYear=swsContext.computationParams$startImputation
endYear=2013

areaKeys=selectedCountry

##'Check on the consistency of startYear, andYear

if(startYear>=endYear){
    stop("You must select startYear lower than endYear")
}


timeKeys=as.character(c(2000:2013))

# if(!(imputationStartYear>=startYear & imputationStartYear<=endYear)){
#     stop("imputationStartYear must lie in the time window between startYear and EndYear")
# }



##'  I have to pull all the items in the parent and child columns:

itemKeysParent=GetCodeList(domain = "suafbs", dataset = "ess_fbs_commodity_tree2", "measuredItemParentCPC_tree")[,code]
itemKeysChild=GetCodeList(domain = "suafbs", dataset = "ess_fbs_commodity_tree2", "measuredItemChildCPC_tree")[,code]
##'  To save time I pull just the only element I need: "extraction rate" whose code is 5423
elemKeys=c("5423")

#allCountries=GetCodeList(domain = "suafbs", dataset = "ess_fbs_commodity_tree2", "geographicAreaM49")[,code]
##'  Seven Pilot countries
# allCountries=c("454", "686", "1248", "716","360", "392", "484")
# allCountries=c("56")

allLevels=list()
forValidationFile=list()
#logConsole1=file("modules/processedItems/logProcessed.txt",open = "w")
#sink(file = logConsole1, append = TRUE, type = c( "message"))
itemKeysChild[itemKeysChild] #%in% processedCPC

 
keyTree = DatasetKey(domain = "suafbs", dataset = "ess_fbs_commodity_tree2", dimensions = list(
  geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = areaKeys),
  measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", keys = elemKeys),
  measuredItemParentCPC_tree = Dimension(name = "measuredItemParentCPC_tree", keys = itemKeysParent),
  measuredItemChildCPC_tree = Dimension(name = "measuredItemChildCPC_tree", keys = itemKeysChild ),
  timePointYears = Dimension(name = "timePointYears", keys = timeKeys)
))

tree = GetData(keyTree)
oldtree=subset(tree,Value==0)
body=paste("The Email contains a backup of the old tree with zero values",
           sep='\n')
sendMailAttachment(oldtree,"old_tree_backup",body)


tree=subset(tree,timePointYears<2014)


tempor=dcast(tree, geographicAreaM49 + measuredElementSuaFbs+ measuredItemParentCPC_tree + measuredItemChildCPC_tree~ timePointYears, value.var="Value")
tree=melt(tempor,id.vars = 1:4)
colnames(tree)[colnames(tree)=="variable"] <- "timePointYears"
colnames(tree)[colnames(tree)=="value"] <- "Value"
tree=subset(tree,Value==0)


countryKeysOld=GetCodeList(domain = "FAOStat1", dataset = "updated_sua_2013_data",dimension="geographicAreaFS")[,code]
elemKeysOld=GetCodeList(domain = "FAOStat1", dataset = "updated_sua_2013_data",dimension="measuredElementFS")[,code]
itemKeysOld=GetCodeList(domain = "FAOStat1", dataset = "updated_sua_2013_data",dimension="measuredItemFS")[,code]
timeKeysOld=as.character(c(2000:2013))

countryKeysOld=countrymapping$fal[countrymapping$m49==areaKeys]

keyOld =  DatasetKey(domain = "FAOStat1", dataset = "updated_sua_2013_data", dimensions = list(
  geographicAreaFS = Dimension(name = "geographicAreaFS", keys =countryKeysOld),
  measuredElementFS = Dimension(name = "measuredElementFS", keys = "41"),
  measuredItemFS = Dimension(name = "measuredItemFS", keys = itemKeysOld),
  timePointYears = Dimension(name = "timePointYears", keys = timeKeysOld)
))

sua2013= GetData(keyOld)

sua2013[,measuredItemFS:=str_pad(measuredItemFS,4,side="left", pad="0")]
sua2013[,measuredItemSuaFbs:=fcl2cpc(measuredItemFS)]
#sua2013=subset(sua2013,measuredItemSuaFbs) #%in% tree$measuredItemChildCPC_tree 
#sua2013=subset(sua2013,measuredItemSuaFbs ) #%in% processedCPC

tempor=dcast(sua2013, geographicAreaFS + measuredElementFS+ measuredItemSuaFbs ~ timePointYears, value.var="Value", fun.aggregate = sum)
sua2013=melt(tempor,id.vars = 1:3)

colnames(sua2013)[colnames(sua2013)=="variable"] <- "timePointYears"
colnames(sua2013)[colnames(sua2013)=="value"] <- "oldEr"

alltree=merge(tree,sua2013,by.x=c("measuredItemChildCPC_tree","timePointYears"),by.y=c("measuredItemSuaFbs","timePointYears"), all.y=T)

alltree=data.table(alltree)
alltree[,newER:=ifelse(is.na(Value)==T,oldEr,Value)]
alltree[,newER:=pmax(Value,oldEr/10000, na.rm=T)]

#alltree[,medEr:=median(oldEr/10000), by=c("geographicAreaM49","measuredElementSuaFbs","measuredItemParentCPC_tree", "measuredItemChildCPC_tree")]

newtree=alltree[,c(colnames(tree),"newER"),with=F]
newtree=subset(newtree,is.na(geographicAreaM49)==F)
newtree=nameData(domain="suafbs", dataset="ess_fbs_commodity_tree2",newtree)
body=paste("The Email contains a list of extraction rates that have been modified taking into account old 41 element from Faostat1.",
           "Column Value reports the old value, newER the new one.",
           sep='\n')
sendMailAttachment(newtree,"Changed_extraction_rates",body)

newtree2=copy(newtree)
newtree2=newtree2[,Value:=newER]
newtree2=newtree2[,newER:=NULL]


newtreewide=dcast(newtree2, geographicAreaM49 + measuredElementSuaFbs+ measuredItemParentCPC_tree + measuredItemChildCPC_tree ~ timePointYears, value.var="Value")
newtreewide=data.table(newtreewide)
newtreewide[!(geographicAreaM49 %in% top48FBSCountries),`2014`:=`2013`]
newtreewide[!(geographicAreaM49 %in% top48FBSCountries),`2015`:=`2013`]
newtreewide[!(geographicAreaM49 %in% top48FBSCountries),`2016`:=`2013`]

newtree2=melt(newtreewide,id.vars = 1:4,
              measure.vars = 5:21)
newtree2=data.table(newtree2)


colnames(newtree2)[colnames(newtree2)=="variable"] <- "timePointYears"
colnames(newtree2)[colnames(newtree2)=="value"] <- "Value"
newtree2[,timePointYears:=as.character(timePointYears)] 
newtree2[,flagObservationStatus:="E"] 
newtree2[,flagMethod:="f"]
newtree2=subset(newtree2,is.na(Value)==F)

newtree_named=nameData(domain="suafbs", dataset="ess_fbs_commodity_tree2",newtree2)

SaveData(domain = "suafbs", dataset = "ess_fbs_commodity_tree2", newtree2,
         waitMode = "wait", waitTimeout = 600000, chunkSize = 50000)
