##' # Imputation  Processed Commodities
##'
##' **Author: Francesca Rosa**
##' Modified by NW to include al SUA components for availability and reverted function elementToCodes2
##' back to its original version
##'
##' **Description:**
##'
##'
##'
##' **Inputs:**
##' 
##' 1. commodity tree
##' 2. SUA data (restricted to the components I need: Prod, Trade, Stock, Seed)
##'
##'
##'
##' **Steps:**
##' 
##' 1. Build the imputation key and pull the data necessary 
##' 
##' 
##' 2. Compute Share "DownUp"
##'
##' 3. Compute Processing share
##'
##'  **Data scope**
##'
##' * GeographicAreaM49: The user can decide if apply the procedure to the country queried in the session
##'   or to "all". "all" picks its Value from the production completeImputationKey (stored in the data.table:
##'   fbs_production_comm_codes)
##'
##' * measuredItemCPC: the list of item involved in this process is stored in the SWS, in the table 
##'   processed_item
##'
##' * timePointYears: All years specified in tha range selected by the user start_year-end_year.
##'   It is requested also to specifi the first year for which we need to produce imputations
##'   The module authomatically protecte all the figures up to this year
##' 
##' ---

##' Initialisation
##'

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
  
})
# 
# top48FBSCountries = c(4,24,50,68,104,120,140,144,148,1248,170,178,218,320,
#                       324,332,356,360,368,384,404,116,408,450,454,484,508,
#                       524,562,566,586,604,608,716,646,686,762,834,764,800,
#                       854,704,231,887,894,760,862,860)


R_SWS_SHARE_PATH <- Sys.getenv("R_SWS_SHARE_PATH")

if(CheckDebug()){
  
  library(faoswsModules)
  SETTINGS = ReadSettings("modules/productionDerivedCalculation/sws.yml")
  
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
##Create a new directories in the share env to support the validation:


#dir_to_save <- "//hqfile4/ESS/Team_working_folder/B_C/3. SUA_FBS/Validation/Output/"
dir_to_save <- file.path(R_SWS_SHARE_PATH, "processedItem_SUA",paste0("validation_", gsub("/", "_",substr(swsContext.username,11, nchar(swsContext.username)))))

if(!file.exists(dir_to_save)){
  dir.create(dir_to_save, recursive = TRUE)
}


# dir_to_save_plot <- file.path(R_SWS_SHARE_PATH, "processedItem", paste0("validation", gsub("/", "_",swsContext.username)),"plot")
dir_to_save_plot <- file.path(R_SWS_SHARE_PATH, "processedItem_SUA", paste0("validation_", gsub("/", "_",substr(swsContext.username,11, nchar(swsContext.username)))),"plot")
# dir_to_save_plot <- "//hqfile4/ESS/Team_working_folder/B_C/3. SUA_FBS/Validation/Plot"

if(!file.exists(dir_to_save_plot)){
  dir.create(dir_to_save_plot, recursive = TRUE)
}


# dir_to_save_plot <- file.path(R_SWS_SHARE_PATH, "processedItem", paste0("validation", gsub("/", "_",swsContext.username)),"plot")
dir_to_save_neg <- file.path(R_SWS_SHARE_PATH, "processedItem_SUA", paste0("validation_", gsub("/", "_",substr(swsContext.username,11, nchar(swsContext.username)))),"neg")
# dir_to_save_plot <- "//hqfile4/ESS/Team_working_folder/B_C/3. SUA_FBS/Validation/Plot"

if(!file.exists(dir_to_save_neg)){
  dir.create(dir_to_save_neg, recursive = TRUE)
}


dir_to_save_shares <- file.path(R_SWS_SHARE_PATH, "processedItem_SUA", paste0("validation_", gsub("/", "_",substr(swsContext.username,11, nchar(swsContext.username)))),"shares")

if(!file.exists(dir_to_save_shares)){
  dir.create(dir_to_save_shares, recursive = TRUE)
}




# dir_to_save_output <- file.path(R_SWS_SHARE_PATH, "processedItem",paste0("validation", gsub("/", "_",swsContext.username)), "output")
#dir_to_save_output <- "//hqfile4/ESS/Team_working_folder/B_C/3. SUA_FBS/Validation/Output/"

dir_to_save_output <- file.path(R_SWS_SHARE_PATH, "processedItem_SUA",paste0("validation_", gsub("/", "_",substr(swsContext.username,11, nchar(swsContext.username)))), "output")

if(!file.exists(dir_to_save_output)){
  dir.create(dir_to_save_output, recursive = TRUE)
}



# dir_to_save_recovery<- file.path(R_SWS_SHARE_PATH, "processedItem",paste0("validation", gsub("/", "_",swsContext.username)), "recovery")
# dir_to_save_recovery <- "//hqfile4/ESS/Team_working_folder/B_C/3. SUA_FBS/Validation/Recovery/"
dir_to_save_recovery<- file.path(R_SWS_SHARE_PATH, "processedItem_SUA",paste0("validation_", gsub("/", "_",substr(swsContext.username,11, nchar(swsContext.username)))), "recovery")

if(!file.exists(dir_to_save_recovery)){
  dir.create(dir_to_save_recovery, recursive = TRUE)
}
###########################################################################################################
##'This are those commodity that are pubblished on FAOSTAT


##'I build the key, always the same in the PRODUCTION sub modules:
##completeImputationKey=getCompleteImputationKey("production")
FBScountries=ReadDatatable("fbs_countries")[,code]
FBSItems=ReadDatatable("fbs_tree")[,item_sua_fbs]

##----------------------------------------------------------------------------------------------------------
##'  Get default parameters
params = defaultProcessedItemParams()
processedCPC=ReadDatatable("processed_item")[,measured_item_cpc]
toBePubblished=ReadDatatable("processed_item")[faostat==TRUE,measured_item_cpc]

#############################################################################################################
##'  Get the commodity tree from the TREE DATASET:
##'  The country dimention depends on the session: 
geoImputationSelection = swsContext.computationParams$imputation_country_selection
sessionCountry=getQueryKey("geographicAreaM49", sessionKey)
selectedCountry =
  switch(geoImputationSelection,
         "session" = sessionCountry,
         "all" = FBScountries)
# selectedCountry = selectedCountry[!selectedCountry%in%top48FBSCountries]

Validation_Param = swsContext.computationParams$Validation

##---------------------------------------------------------------------------------------------------------
##'  Get the list of processed items to impute
ItemImputationSelection = swsContext.computationParams$Items
sessionItems=getQueryKey("measuredItemFbsSua", sessionKey)
sessionItems =
  switch(ItemImputationSelection,
         "session" = sessionItems,
         "all" = FBSItems)

##'  The year dimention depends on the session: 
startYear=as.integer(swsContext.computationParams$startYear)
# imputationStartYear = startYear
imputationStartYear=as.integer(swsContext.computationParams$startImputation)
endYear = as.integer(swsContext.computationParams$endYear)
endYear=2017
areaKeys=selectedCountry



##'Check on the consistency of startYear, andYear

if(startYear>=endYear){
  stop("You must select startYear lower than endYear")
}


timeKeys=as.character(c(startYear:endYear))

if(!(imputationStartYear>=startYear & imputationStartYear<=endYear)){
  stop("imputationStartYear must lie in the time window between startYear and EndYear")
}



##'  I have to pull all the items in the parent and child columns:

itemKeysParent=GetCodeList(domain = "suafbs", dataset = "ess_fbs_commodity_tree2", "measuredItemParentCPC_tree")[,code]
itemKeysChild=GetCodeList(domain = "suafbs", dataset = "ess_fbs_commodity_tree2", "measuredItemChildCPC_tree")[,code]
##'  To save time I pull just the only element I need: "extraction rate" whose code is 5423
elemKeys=c("5423")

allCountries=areaKeys
##'  Seven Pilot countries
# allCountries=c("454", "686", "1248", "716","360", "392", "484")
# allCountries=c("56")

allLevels=list()
forValidationFile=list()
#logConsole1=file("modules/processedItems/logProcessed.txt",open = "w")
#sink(file = logConsole1, append = TRUE, type = c( "message"))

##'  Loop by country 
##'  
start.time <- Sys.time()
for(geo in   seq_along(allCountries)){
  
  finalByCountry=list()
  
  
  currentGeo=allCountries[geo]
  ##'  Subset the data (SUA)    
  
  message(paste0("Pull commodity tree for country: ", currentGeo)  )  
  
  keyTree = DatasetKey(domain = "suafbs", dataset = "ess_fbs_commodity_tree2", dimensions = list(
    geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = currentGeo),
    measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", keys = elemKeys),
    measuredItemParentCPC_tree = Dimension(name = "measuredItemParentCPC_tree", keys = itemKeysParent),
    measuredItemChildCPC_tree = Dimension(name = "measuredItemChildCPC_tree", keys = itemKeysChild),
    timePointYears = Dimension(name = "timePointYears", keys = timeKeys)
  ))
  
  tree = GetData(keyTree)
  #tree = try(setTimeOut({GetData(keyTree);},
  #                   timeout=180, onTimeout="error"), silent = TRUE)
  
  #if(!inherits(tree, "try-error")) {
  
  if(nrow(tree)<1)
  {message(paste0("The commodity-tree does not exist for the country: "), currentGeo)}else{
    
    
    ##'  I modify the structure of the tree to make it compliant with the rest of the routine:
    setnames(tree, c("measuredItemParentCPC_tree", "measuredItemChildCPC_tree"),c("measuredItemParentCPC", "measuredItemChildCPC"))
    setnames(tree, "Value", "extractionRate")
    tree[,flagObservationStatus:=NULL ]
    tree[,flagMethod:=NULL ]
    tree[,measuredElementSuaFbs:=NULL ]
    
    ##'  I delete all the connection where no extraction  rate is available:
    tree=tree[!is.na(extractionRate)]
    tree=tree[extractionRate!=0]
    if(nrow(tree)<1)
    {message(paste0("The commodity-tree conteins only ZERO extraction rates in the country: "), currentGeo)}else{
      ##'  I associate the level to each parent-child combination, this operation is done by country-year combinations
      ##'  because the commodity tree may change across countries and over time. 
      
      ##'  The uniqueLevel represents all the country-year comb.
      uniqueLevels = tree[, .N, by = c("geographicAreaM49", "timePointYears")]
      uniqueLevels[, N := NULL]
      levels=list()
      treeLevels=list()
      for (i in seq_len(nrow(uniqueLevels))) {
        filter=uniqueLevels[i, ]
        treeCurrent=tree[filter, , on = c("geographicAreaM49", "timePointYears")]
        levels=findProcessingLevel(treeCurrent,"measuredItemParentCPC","measuredItemChildCPC")
        setnames(levels, "temp","measuredItemParentCPC")
        treeLevels[[i]]= merge(treeCurrent, levels, by=c("measuredItemParentCPC"), all.x=TRUE)
      }
      tree=rbindlist(treeLevels)
      
      tree = tree %>%
        dplyr::group_by_("geographicAreaM49","timePointYears","measuredItemChildCPC") %>%
        dplyr::mutate(processingLevel = max(processingLevel,na.rm=TRUE))
      
      tree = as.data.table(tree)
      ##-------------------------------------------------------------------------------------------------------------------------------------
      ############################################################################################################################
      ##'  Select all the commodity involved (what I need is just the dependence parent-child,
      ##'  all the possibilities, no country-year specific)
      
      treeRestricted=tree[,.(measuredItemParentCPC,measuredItemChildCPC,processingLevel)]
      treeRestricted=treeRestricted[with(treeRestricted, order(measuredItemChildCPC))]
      treeRestricted=treeRestricted[!duplicated(treeRestricted)]
      
      if(ItemImputationSelection=="session") {
        processedCPC = processedCPC[processedCPC%in%sessionItems]
      }
      
      ##'  Get all the primary starting from the processed item (stored in the data.table: processed_item )
      primaryInvolved=getPrimary(processedCPC, treeRestricted, params)
      
      ##'  Get all the children of primary commodities involved (all levels)
      primaryInvolvedDescendents=getChildren( commodityTree = treeRestricted,
                                              parentColname ="measuredItemParentCPC",
                                              childColname = "measuredItemChildCPC",
                                              topNodes =primaryInvolved )
      
      ##' For some countris (not included in the FBS country list) there are no processed commodities to be imputed,
      ##' and the next steps cannot be run.
      if(length(primaryInvolvedDescendents)==0)
      {message("No primary commodity involved in this country")}else{
        
        ##'  Find the so-called secondLoop commodities:
        ##'  select all the unique combination of country-year-commodity-level:
        ##'  
        multipleLevels=unique(tree[,.( geographicAreaM49,timePointYears, measuredItemChildCPC, processingLevel)])
        
        ##'  count all the repeated country-commodity combinations.
        multipleLevels[,n:=.N, by=c("measuredItemChildCPC", "geographicAreaM49", "timePointYears")]
        ##'  the n>1 it means that that commodity appears at least for one country in more tham one level.
        ##'  If this situation occurs just for some countries (and not for all), it is not a problem, because at the end of the process we decide
        ##'  to keep only ONE production-contribution and in particular the contribution of the highest level in the tree- 
        ##'  hierachy (if it is just one level, we are properly considering ALL the production components)
        ##'  see row 560 of this script.
        
        secondLoop=unique(multipleLevels[n!=1,measuredItemChildCPC])
        
        ############################################################################################################################
        ##'  Get SUA data from QA: all components are stored in the SWS
        ##'  Note Restricted means that I am pulling only the components I need in this submodule: PRODUCTION, STOCK, SEED, TRADE
        
        
        # dataSuaRestricted=getSUADataRestricted()
        
        ############################################################################################################################
        ## Get from SUA
        importCode = "5610"
        exportCode = "5910"
        productionCode="5510"
        seedCode="5525"
        stocksCode = "5071"
        FeedCode = "5520"
        LossCode = "5016"
        
        message("Pulling data from SUA_unbalanced")
        
        geoDim = Dimension(name = "geographicAreaM49", keys = currentGeo)
        eleDim = Dimension(name = "measuredElementSuaFbs", keys = c("5510",
                                                                    "5610",
                                                                    "5071",
                                                                    "5023",
                                                                    "5910",
                                                                    "5016",
                                                                    "5165",
                                                                    "5520",
                                                                    "5525",
                                                                    "5164",
                                                                    "5166",
                                                                    "5141",
                                                                    "5113"))
        
        itemKeys = primaryInvolvedDescendents
        itemDim = Dimension(name = "measuredItemFbsSua", keys = itemKeys)
        timeDim = Dimension(name = "timePointYears", keys = timeKeys)
        sua_un_key = DatasetKey(domain = "suafbs", dataset = "sua_unbalanced",
                                dimensions = list(
                                  geographicAreaM49 = geoDim,
                                  measuredElementSuaFbs = eleDim,
                                  measuredItemFbsSua = itemDim,
                                  timePointYears = timeDim)
        )
        sua_unbalancedData = GetData(sua_un_key)
        if(nrow(sua_unbalancedData[measuredElementSuaFbs=="5510",]) > 1) {
          ############################################################################################################################            
          # dataSuaRestricted=dataSuaRestricted[timePointYears<2014]
          # dataSuaRestricted=rbind(dataSuaRestricted,sua_unbalancedData )
          ############################################################################################################################
          
          
          ##'  I am using elementCodesToNames2!!!!! because it has not been deployed yet the standardization package 
          ##'  This function is upload into the package folder when I have created the plugin
          ##'  
          ##'  NW: changed back to elementCodestoNames (apparently function has been updated)
          
          # data = elementCodesToNames(data = dataSuaRestricted, itemCol = "measuredItemFbsSua",
          #                             elementCol = "measuredElementSuaFbs")
          data = elementCodesToNames(data = sua_unbalancedData, itemCol = "measuredItemFbsSua",
                                     elementCol = "measuredElementSuaFbs")
          ##'  Add all the missing PRODUCTION row: if the production of a derived product does not exist it even if it is created by this 
          ##'  routine cannot be stored in the SUA table and consequently all the commodities that belongs to its descendents are not estimates
          ##'  or are estimated using only the TRADE and neglecting an important part of the supply components.
          
          prod=data[measuredElementSuaFbs=="production", .( geographicAreaM49, timePointYears ,measuredItemFbsSua)]
          ##all=unique(data[ ,.( geographicAreaM49, timePointYears ,measuredItemParentCPC)])
          
          ##------------------------------------------------------------
          ##'  all time point years
          timePointYears=timeKeys
          ##'  all countries
          geographicAreaM49=currentGeo
          
          ##'  I have to build table containing the complete data key ( I have called it: all) 
          all1=merge(timePointYears, primaryInvolvedDescendents)
          setnames(all1, c("x","y"),c("timePointYears","measuredItemFbsSua"))
          
          all2=merge(geographicAreaM49, primaryInvolvedDescendents)
          setnames(all2, c("x","y"),c("geographicAreaM49","measuredItemFbsSua"))
          all1=data.table(all1)
          all2=data.table(all2)
          
          all=merge(all1, all2, by="measuredItemFbsSua", allow.cartesian = TRUE)
          ##'   The object "all" contais a complete key (all countries, all years, all items) for the production element
          ##'  To avoid duplicates I select just those row that are not already included into the SUA table
          
          ##'  Note that all the columns of the all datatable have class=factor
          all[,measuredItemFbsSua:=as.character(measuredItemFbsSua)]
          all[,timePointYears:=as.character(timePointYears)]
          all[,geographicAreaM49:=as.character(geographicAreaM49)]
          
          
          noProd=setdiff(all,prod)
          ##'  I add the value and flags columns:
          if(nrow(noProd)>0){
            noProd=data.table(noProd, measuredElementSuaFbs="production",Value=NA,flagObservationStatus="M",flagMethod="u" )
            
            
            ##'  I add this empty rows into the SUA table
            data=rbind(data,noProd)
          }
          ############################################################################################################################
          ##--------------------------------------------------------------------------------------------------------------------------
          ##'   Processed data (OUTPUT). I pull the data from the aproduction domain. Baasically I am pulling the already existing 
          ##'   production data,
          
          ##'   Restrict the complete imputation keys in ordert to pull only PRODUCTION:
          ##  dataProcessedImputationKey = DatasetKey(domain = "agriculture", dataset = "aproduction",
          ##                     dimensions = list(
          ##                         geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = selectedCountry),
          ##                         measuredElement = Dimension(name = "measuredElement", keys = c(productionCode)),
          ##                         measuredItemCPC =  Dimension(name = "measuredItemCPC", keys = primaryInvolvedDescendents),
          ##                         timePointYears = Dimension(name = "timePointYears", keys = timeKeys))
          ##  )
          ##
          ## 
          ## dataProcessed=GetData(dataProcessedImputationKey)
          
          dataProcessed=sua_unbalancedData[measuredElementSuaFbs=="5510"]
          setnames(dataProcessed,c("measuredElementSuaFbs","measuredItemFbsSua"),c("measuredElement","measuredItemCPC"))
          dataProcessed[,timePointYears:=as.numeric(timePointYears)]
          dataProcessed=dataProcessed[,benchmark_Value:=Value]
          
          ##'  Artificially protect production data between the time window external to the range I have to produce imputations:
          ##dataProcessed[timePointYears<imputationStartYear,flagObservationStatus:="E"] 
          ##dataProcessed[timePointYears<imputationStartYear,flagMethod:="h"] 
          dataProcessed=expandYear(dataProcessed,newYear=as.numeric(endYear))
          dataProcessed=removeInvalidFlag(dataProcessed, "Value", "flagObservationStatus", "flagMethod", normalised = TRUE)
          dataProcessed=removeNonProtectedFlag(dataProcessed, "Value", "flagObservationStatus", "flagMethod", normalised = TRUE, keepDataUntil =imputationStartYear)
          
          setnames(dataProcessed, "measuredItemCPC", "measuredItemChildCPC")
          ##'  At the moment we have SUA data for the time range 2000-2015 (I pulled data from QA)
          ##'   data:SUA (if I pulled data from SWS (the name of the ITEM column is different: measuredItemFbsSua))
          
          ##'  I keep only the item theat I need: processed Items and all those items in them trees
          data=data[measuredItemFbsSua %in% primaryInvolvedDescendents]
          data=data[!is.na(measuredElementSuaFbs),]
          
          ##'  change the name of the ITEM columns: parents
          setnames(data,"measuredItemFbsSua","measuredItemParentCPC")
          
          ##-----------------------------------------------------------------------------------------------------------------------
          
          ##'   the loop works on processing levels:
          
          levels=unique(tree[, processingLevel])
          
          data[,timePointYears:=as.numeric(timePointYears)]
          tree[,timePointYears:=as.character(timePointYears)]
          tree[,timePointYears:=as.numeric(timePointYears)]
          
          
          
          #data=data[geographicAreaM49==currentGeo]
          ##'  Tresform TIME in a numeric variabible:
          ## data[,timePointYears:=as.numeric(timePointYears)]
          
          ##'   Subset PRODUCTION data
          ##dataProcessed=dataProcessed[geographicAreaM49==currentGeo]
          #dataProcessed[,timePointYears:=as.numeric(timePointYears)]
          data$PG1 = NA
          for(lev in (seq_along(levels)-1))  {
            ##'  Loop by level
            treeCurrentLevel=tree[processingLevel==lev]
            message("Processing country ", currentGeo, " - level ", lev )
            
            
            ##'  To compute the PRODUCTION I need to evaluate how much (which percentage) of 
            ##'  the parent commodity availability is allocated in the productive process associate
            ##'  to a specific child item.
            
            ##'  Calculate share down up. Please note that currentData contains the SUA table.
            ##'  NW: use all SUA components
            dataMergeTree=calculateShareDownUpFBS(data=data,tree=treeCurrentLevel,
                                                  params=params, printDirectory = dir_to_save_neg,
                                                  printNegativeAvailability=TRUE,useAllSUAcomponents=FALSE)
            
            
            ##'  Here I merge the SUA table (already merged with tree), with the PRODUCTION DATA
            ##'  This merge produces many NA because dataProcessed contains all the derived item (and not just
            ##'   those at the level of the loop)
            inputOutputdata= merge(dataMergeTree,dataProcessed, by=c("geographicAreaM49","measuredItemChildCPC","timePointYears"),all.y=TRUE) 
            
            
            inputOutputdata[shareDownUp=="NaN",shareDownUp:=0]
            inputOutputdata$PG1 = NA
            inputOutputdata=calculateProcessingShareFBS_NW(inputOutputdata, param=params)
            
            ##-------------------------------------------------------------------------------------------------------------------------------------    
            
            ##inputOutputdata[, meanProcessingShare:=mean(processingShare, na.rm = TRUE), by=c("geographicAreaM49","measuredItemChildCPC","measuredItemParentCPC")]
            
            ##inputOutputdata[is.na(processingShare)&!is.na(meanProcessingShare),processingShare:=meanProcessingShare ]
            inputOutputdata[, newImputation:=availability*processingShare*extractionRate]
            
            
            
            finalByCountry[[lev+1]]=inputOutputdata
            
            ##'   Update production in data in order to add the just computed production
            ##'   at each loop we compute production for the following level, this prodution
            ##'   should be used in the following loop to compute the availabilities
            
            updateData=inputOutputdata[,.(geographicAreaM49, timePointYears, measuredItemChildCPC, newImputation, PG1)]
            updateData[, newImputation:=sum(newImputation,na.rm = TRUE), by=c("geographicAreaM49", "timePointYears", "measuredItemChildCPC")]
            updateData=unique(updateData)
            
            
            ##'  I change the column names bacause the commodities that now are children will be parent in the next loop
            setnames(updateData,"measuredItemChildCPC","measuredItemParentCPC")
            
            ##'  I merge the new results into the SUA table
            data=merge(data,updateData, by=c("geographicAreaM49", "timePointYears", "measuredItemParentCPC"), all.x=TRUE)
            data$PG1.x[is.na(data$PG1.x)] = data$PG1.y[is.na(data$PG1.x)]
            data$PG1= data$PG1.x
            data[,PG1.x:=NULL]
            data[,PG1.y:=NULL]
            ##'   Olnly non-protected production figures have to be overwritten:
            
            
            data[,flagComb:=paste(flagObservationStatus,flagMethod,sep=";")]   
            
            flagValidTable=copy(flagValidTable)
            flagValidTable=flagValidTable[Protected==TRUE,]
            protected=flagValidTable[,protectedComb:=paste(flagObservationStatus,flagMethod,sep=";")]
            protected=protected[,protectedComb]
            
            
            data[geographicAreaM49==currentGeo &
                   !(flagComb %in% protected)&
                   measuredElementSuaFbs=="production" &
                   !is.na(newImputation) &
                   !(measuredItemParentCPC %in% secondLoop) &
                   timePointYears>=imputationStartYear,
                 ":="(c("Value","flagObservationStatus","flagMethod"),list(newImputation,"I","e"))]
            
            
            data[,newImputation:=NULL]
            data[,flagComb:=NULL]
          }
          
          
          
          allLevelsByCountry=rbindlist(finalByCountry)
          
          secondLoopChildren=getChildren( commodityTree = treeRestricted,
                                          parentColname ="measuredItemParentCPC",
                                          childColname = "measuredItemChildCPC",
                                          topNodes =secondLoop )
          
          ##' Note that the so called "secondLoop" items are computed for each country separately:
          secondLoop=secondLoopChildren
          
          ##'   This passage is to sum up the productions of a derived commodities coming from more
          ##'   than one parent (those commodities  are flagged as second-loop). To identify the highest 
          ##'   level of the hierarchy I have to choose the min value in the processingLevel column.
          
          allLevelsByCountry[, minProcessingLevel:=NA_real_]
          
          ##'   The min of the processing-level is not only item-specific, but also country-time specific, since the 
          ##'   the commodity-tree may change over the time and the countries.
          # allLevelsByCountry = as.data.frame(allLevelsByCountry)
          # 
          # allLevelsByCountry[measuredItemChildCPC %in% secondLoop, minProcessingLevel:=min(processingLevel), by=c("geographicAreaM49","timePointYears", "measuredItemChildCPC")]
          # 
          # allLevelsByCountry[measuredItemChildCPC %in% secondLoop & extractionRate!=0, minProcessingLevel:=min(processingLevel),
          #                    by=c("geographicAreaM49","timePointYears", "measuredItemChildCPC")]
          
          
          # NW:  This is not just removing contributions from lower items in the same hierarchy, but also contributions at lower levels 
          # in the production chain coming from another branch
          ##'   I remove (just for those commodities classified as secondLoop) the production-contributions coming from parents 
          ##'   lower in the hierachy (it means with an higher processing-level)
          # allLevelsByCountry=allLevelsByCountry[(is.na(minProcessingLevel) | processingLevel==minProcessingLevel) & extractionRate!=0]
          # Here either sum across parents by child or send email.
          
          
          #finalOutput = output[!(measuredItemChildCPC %in% secondLoop),  list(newImputation = sum(newImputation, na.rm = TRUE)),
          #                        by =c ("geographicAreaM49","measuredItemChildCPC","timePointYears")]
          
          
          allLevels[[geo]]=allLevelsByCountry
          
          
          
          
          
          
          
          
          ## create an intermediate set of data to be saved in a shared folder 
          
          forValidationFile[[geo]]=    allLevels[[geo]][,.(geographicAreaM49, measuredItemChildCPC, timePointYears, 
                                                           measuredItemParentCPC ,extractionRate  ,processingLevel,processingShare,
                                                           availability ,shareDownUp ,newImputation, minProcessingLevel, benchmark_Value)]  
          
          setnames(forValidationFile[[geo]], c("measuredItemChildCPC","measuredItemParentCPC"), c("measuredItemChildCPC_tree","measuredItemParentCPC_tree"))
          
          forValidationFile[[geo]]= nameData("suafbs", "ess_fbs_commodity_tree2", forValidationFile[[geo]])
          # forValidationFile[[geo]][,timePointYears:=as.numeric(timePointYears)]
          
          dataLabel=copy(data)
          dataLabel=dataLabel[geographicAreaM49==currentGeo]
          setnames(dataLabel,  "measuredItemParentCPC", "measuredItemChildCPC_tree")
          dataLabel=nameData("suafbs", "ess_fbs_commodity_tree2", dataLabel)
          dataLabel[,timePointYears_description:=NULL]
          setnames(dataLabel, c("measuredElementSuaFbs", "Value"), c("measuredElementSuaFbs_child", "Value_child"))
          
          
          forValidationFile[[geo]]=merge(dataLabel,forValidationFile[[geo]], by=c("geographicAreaM49","measuredItemChildCPC_tree",
                                                                                  "measuredItemChildCPC_tree_description",
                                                                                  "geographicAreaM49_description",
                                                                                  "timePointYears") , allow.cartesian =  TRUE)
          
          
          
          
          forValidationFile[[geo]]=forValidationFile[[geo]][,.(geographicAreaM49,geographicAreaM49_description,timePointYears,
                                                               measuredItemChildCPC_tree,measuredItemChildCPC_tree_description , measuredItemParentCPC_tree,measuredItemParentCPC_tree_description,
                                                               measuredElementSuaFbs_child ,Value_child, flagObservationStatus, flagMethod, availability ,
                                                               extractionRate,  processingLevel,benchmark_Value, newImputation, 
                                                               shareDownUp,processingShare,minProcessingLevel)]
          
          
          ## Save all the intermediate output country by country:
          
          
          if(!CheckDebug()){
            
            
            try(write.csv(allLevels[[geo]], file=file.path(dir_to_save_recovery,paste0(currentGeo,".csv")), row.names = FALSE), silent = TRUE)    
            
            try(write.csv(forValidationFile[[geo]], file=file.path(dir_to_save_output,paste0(currentGeo,".csv")), row.names = FALSE), silent = TRUE)
            
            
            
          }
        }
      }
    }
  } 
  
  
  
  #}else{message(paste0("TimeOut issues to GetData for country: ",currentGeo))}
  
  
}

end.time <- Sys.time()
#-------------------------------------------------------------------------------------------------------------------------------------    
#-------------------------------------------------------------------------------------------------------------------------------------    
output=rbindlist(allLevels)


###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################

#Sumeda: Modified the code in order to adjust the shares of coprudcuts. There are cases where the co-products have different
#processing shares that is not acceptable. Tomarsz provided a table that indicates the relationship between the co-products.
#With the help of this table, a new processing share has been adjusted for the coproducts in order to maintain simillar shares.



coproduct_table <- ReadDatatable('zeroweight_coproducts')


coproduct_table <- coproduct_table[,c("measured_item_child_cpc","branch"), with = FALSE]

coproduct_table <- setNames(coproduct_table, c("measuredItemChildCPC","branch"))


# if (nrow(coproduct_table[measuredItemChildCPC == branch]) > 0) {
#   stop("There's an error in the co-product table.")
# }


# Can't do anything if this information if missing, so remove these cases
coproduct_table <-
  coproduct_table[!is.na(branch)] %>%
  tbl_df() %>%
  # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  # XXX removing these cases as '22242.01' appears as zero-weight for main product = '22110.04' X
  # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  dplyr::filter(branch != '22242.01 + 22110.04')




# If TRUE it means that "+" or "or" cases are involved
fmax <- function(child, main, share, plusor = FALSE) {
  main <- unique(main)
  
  if (plusor) {
    found <- sum(sapply(child, function(x) grepl(x, main), USE.NAMES = FALSE))
    
    if (found == 0) {
      return(max(share, na.rm = TRUE))
    } else if (found == 1) {
      return(share[(1:length(child))[sapply(child, function(x) grepl(x, main), USE.NAMES = FALSE)]])
    } else { # should be 2
      return(max(share[(1:length(child))[sapply(child, function(x) grepl(x, main), USE.NAMES = FALSE)]], na.rm = TRUE))
    }
  } else {
    if (sum(grepl(main, child)) > 0) {
      share[child == main]
    } else {
      max(share, na.rm = TRUE)
    }
  }
}


################################# Easy cases

coproduct_table_easy <-
  coproduct_table %>%
  dplyr::filter(!grepl('\\+|or', branch))

coproduct_table_easy <-
  bind_rows(
    coproduct_table_easy,
    coproduct_table_easy %>% dplyr::mutate(measuredItemChildCPC = branch)
  ) %>%
  distinct()


output_easy <-
  output[measuredItemChildCPC %in% unique(coproduct_table_easy$measuredItemChildCPC)] %>%
  tbl_df()

if (nrow(output_easy) > 0) {
  output_easy <-
    output_easy %>%
    left_join(coproduct_table_easy, by = 'measuredItemChildCPC') %>%
    group_by(geographicAreaM49, timePointYears, branch,measuredItemParentCPC) %>%
    dplyr::mutate(
      reference_share = fmax(measuredItemChildCPC, branch, processingShare, FALSE),
      # Ask whether rounding by 2 is fine
      reimpute = round(processingShare, 2) != round(reference_share, 2)
    ) %>%
    ungroup()
}


################################# / Easy cases



################################# Plus cases

coproduct_table_plus <-
  coproduct_table %>%
  dplyr::filter(grepl('\\+', branch))


coproduct_table_plus <-
  bind_rows(
    coproduct_table_plus,
    coproduct_table_plus %>%
      tidyr::separate(branch, into = c('main1', 'main2'), remove = FALSE, sep = ' *\\+ *') %>%
      dplyr::select(measuredItemChildCPC = main1, branch) %>%
      distinct(),
    coproduct_table_plus %>%
      tidyr::separate(branch, into = c('main1', 'main2'), remove = FALSE, sep = ' *\\+ *') %>%
      dplyr::select(measuredItemChildCPC = main2, branch) %>%
      distinct()
  ) %>%
  distinct()


output_plus <-
  output[measuredItemChildCPC %in% unique(coproduct_table_plus$measuredItemChildCPC)] %>%
  tbl_df()


if (nrow(output_plus) > 0) {
  output_plus <-
    output_plus %>%
    left_join(coproduct_table_plus, by = 'measuredItemChildCPC') %>%
    group_by(geographicAreaM49, timePointYears, branch,measuredItemParentCPC) %>%
    dplyr::mutate(
      reference_share = fmax(measuredItemChildCPC, branch, processingShare, TRUE),
      # Ask whether rounding by 2 is fine
      reimpute = round(processingShare, 2) != round(reference_share, 2)
    ) %>%
    ungroup()
}



################################# / Plus cases




################################# Or cases
coproduct_table_or <-
  coproduct_table %>%
  dplyr::filter(grepl('or', branch))

coproduct_table_or <-
  bind_rows(
    coproduct_table_or,
    coproduct_table_or %>%
      tidyr::separate(branch, into = c('main1', 'main2'), remove = FALSE, sep = ' *\\or *') %>%
      dplyr::select(measuredItemChildCPC = main1, branch) %>%
      distinct(),
    coproduct_table_or %>%
      tidyr::separate(branch, into = c('main1', 'main2'), remove = FALSE, sep = ' *\\or *') %>%
      dplyr::select(measuredItemChildCPC = main2, branch) %>%
      distinct()
  ) %>%
  distinct()

output_or <-
  output[measuredItemChildCPC %in% unique(coproduct_table_or$measuredItemChildCPC)] %>%
  tbl_df()


if (nrow(output_or) > 0) {
  output_or <-
    output_or %>%
    left_join(coproduct_table_or, by = 'measuredItemChildCPC') %>%
    group_by(geographicAreaM49, timePointYears, branch, measuredItemParentCPC) %>%
    dplyr::mutate(
      reference_share = fmax(measuredItemChildCPC, branch, processingShare, TRUE),
      # Ask whether rounding by 2 is fine
      reimpute = round(processingShare, 2) != round(reference_share, 2)
    ) %>%
    ungroup()
}


################################# / Or cases



output_to_check <-
  bind_rows(
    output_easy,
    output_plus,
    output_or
  ) %>%
  dplyr::filter(timePointYears >= imputationStartYear) %>%
  dplyr::mutate(
    processingShare = ifelse(reimpute, reference_share, processingShare),
    # Status I = Imputed
    processingShareFlagObservationStatus = ifelse(reimpute, 'I', processingShareFlagObservationStatus),
    # Method c = copied from somewhere else.
    processingShareFlagMethod = ifelse(reimpute, 'c', processingShareFlagMethod),
    # Redoing this, as the formula holds before and should 
    # also hold after assigning reference_share
    newImputation   = availability * processingShare * extractionRate
  )


if (nrow(output_to_check) > 0) {
  output <-
    bind_rows(
      anti_join(
        output,
        dplyr::select(output_to_check, -branch, -reference_share, -reimpute),
        by = c('geographicAreaM49', 'measuredItemParentCPC',
               'measuredItemChildCPC', 'timePointYears')
      ),
      dplyr::select(output_to_check, -branch, -reference_share, -reimpute)
    ) %>%
    as.data.table()
}







###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################




##'   Those commodities that appear in more than one level of the commodity-tree hierachy are considered children 
##'   of the highest item in the hierachy.

##'   I have to expand the list of secondLoop commodities: I impose that all the descentents of a second-loop
##'   commodities are classified as "second-loop":



#-------------------------------------------------------------------------------------------------------------------------------------    
#-------------------------------------------------------------------------------------------------------------------------------------    
##'   The following lines create the dataset useful for validation purposes:
##'   I want to sum the newImputation into the totNewImputation, but I want to keep
##'   all the constributions to double check the final result
BadProcessingShares = output[PG1==TRUE,]
BadProcessingShares = BadProcessingShares[BadProcessingShares$Year>=imputationStartYear,]

if(CheckDebug()){
  
  output[,  totNewImputation := sum(newImputation, na.rm = TRUE),
         by =c ("geographicAreaM49","measuredItemChildCPC","timePointYears")]
  
  ##'   outPutforValidation is the file created to validate the output from team B/C
  outPutforValidation=copy(output)
  
  
  
  outPutforValidation=outPutforValidation[,.(geographicAreaM49,measuredItemChildCPC, timePointYears, measuredItemParentCPC, extractionRate,
                                             processingLevel ,availability,shareDownUp,processingShare,newImputation, totNewImputation)]
  
  # directory="//hqfile4/ESS/Team_working_folder/B_C/3. SUA_FBS/Validation/Output/"
  # dir.create(directory)
  # fileForValidation2(outPutforValidation,SUAdata=data ,  dir=directory)
  write.csv(outPutforValidation,"C:/Work/SWS/FBS/Production/DerivedProduction/Output/outPutforValidation.csv")
  if (nrow(BadProcessingShares)>1) {
    write.csv(BadProcessingShares,"C:/Work/SWS/FBS/Production/DerivedProduction/Output/BadProcessingShares.csv")
  }
  
  ##'  For validation purposes it is extremly important to produce validation files filtered for those 47
  ##'  commodies plut flours (that are upposed to be pubblished)
  
}

#-------------------------------------------------------------------------------------------------------------------------------------    
#-------------------------------------------------------------------------------------------------------------------------------------    
##'   Here I fisically sum by child the newImputation (that now are disaggregated by parent-contribution)
finalOutput=output[,  list(newImputation = sum(newImputation, na.rm = TRUE)),
                   by =c ("geographicAreaM49","measuredItemChildCPC","timePointYears")]

#output[,.(geographicAreaM49,measuredItemChildCPC,timePointYears,
#         measuredItemParentCPC,availability,processingShare,Value,
#         flagObservationStatus,flagMethod,benchmark_Value,newImputation, processingLevel)]

#-------------------------------------------------------------------------------------------------------------------------------------    
##'   I subset the output datatable in order to make comparison between  benchmark_Value (basically data already stored in the SWS)
##'    and newImputations (I keep just the Value and benchmark_Value columns)
orig=output[,.(geographicAreaM49,measuredItemChildCPC,timePointYears,Value,benchmark_Value,flagObservationStatus,flagMethod)]
##'   This subset of output is full of duplicated rows because each child can have more than one parents, and new imputations have been preduced
##'   for each of its parent. Each new imputations represent the contribution of one single parent item to the production of the derived item.
orig=orig[!duplicated(orig)]

##'   finalOutput contains all the newImputation (olready aggregated with respect to all the parent contribuions)
imputed=merge(finalOutput,orig, by=c("geographicAreaM49", "measuredItemChildCPC","timePointYears"), allow.cartesian = TRUE)

##' Only M,u figures are overwritten by the new imputations.
imputed[flagObservationStatus=="M" & flagMethod=="u" & !is.na(newImputation), ":="(c("Value", "flagObservationStatus", "flagMethod"), list(newImputation,"I","e"))]

#-------------------------------------------------------------------------------------------------------------------------------------    
##' The column PROTECTED is created just to PLOT the output and distinguish between new imputations and protected figures.
imputed[,flagComb:=paste(flagObservationStatus,flagMethod,sep=";")]   
imputed[, PROTECTED:=FALSE]
imputed[flagComb %in% protected, PROTECTED:=TRUE]
imputed[PROTECTED==TRUE,newImputation:=benchmark_Value]
# imputed[geographicAreaM49%in%top48FBSCountries]
toPlot=imputed
##'   This table is saved just to produce comparisond between batches: 

if(CheckDebug()){
  write.csv(toPlot, "C:/Work/SWS/FBS/Production/DerivedProduction/Output/toPlot.csv", row.names=FALSE)
  res_plot = try(plotResult(toPlot, toPubblish=toBePubblished, pathToSave= "C:/Work/SWS/FBS/Production/DerivedProduction/Output/Plots"))
}

##Plots goes directly to the shared folder

if(!CheckDebug()){
  
  if(nrow(BadProcessingShares) > 0) {
    
    try(write.csv(BadProcessingShares, file=file.path(dir_to_save_shares,"BadShares.csv"), row.names = FALSE), silent = TRUE)
    
  }
  
}

if(!CheckDebug() & swsContext.computationParams$Validation=="1"){
  
  res_plot = try(plotResult(toPlot, toPubblish=toBePubblished, pathToSave= dir_to_save_plot))
  
}



#-------------------------------------------------------------------------------------------------------------------------------------    
##'   Save back

##'   Remove the auxiliary columns created in the process
imputed[,newImputation:=NULL]
imputed[,benchmark_Value:=NULL]
imputed[,flagComb:=NULL]
imputed[,PROTECTED:=NULL]

imputed[,measuredElement:="5510"]

imputed= removeInvalidDates(data = imputed, context = sessionKey)
imputed= postProcessing(data =  imputed) 

imputed=imputed[flagObservationStatus=="I" & flagMethod=="e"]
imputed=imputed[,.(measuredElement,geographicAreaM49, measuredItemChildCPC,
                   timePointYears,Value,flagObservationStatus,flagMethod)]

##' ensure that just data after the imputationStartYear are saved back and I am not overwriting 
##' protected figures
##' 
imputed=imputed[timePointYears>=imputationStartYear]
ensureProtectedDataFBS(imputed, returnData = FALSE)
setnames(imputed, "measuredElement", "measuredElementSuaFbs")
setnames(imputed, "measuredItemChildCPC", "measuredItemFbsSua")

SaveData(domain = sessionKey@domain,
         dataset = sessionKey@dataset,
         data =  imputed)

if (nrow(BadProcessingShares) > 0) {
  from = "sws@fao.org"
  to = swsContext.userEmail
  subject = "Derived-Products imputation plugin has correctly run"
  body = paste0("The plug-in has saved the Production imputation in your session.",
                "  However, some shares are greater than 1.  Please check these at: ",
                dir_to_save_shares)
  sendmailR::sendmail(from = from, to = to, subject = subject, msg = body)
  paste0("Email sent to ", swsContext.userEmail)
  
}
if (nrow(BadProcessingShares) == 0){
  print("The plugin ran successfully!")
  ## Initiate email
  from = "sws@fao.org"
  to = swsContext.userEmail
  subject = "Derived-Products imputation plugin has correctly run"
  body = paste0("The plug-in has saved the Production imputation in your session.",
                "You can browse charts and intermediate csv files in the shared folder: ",
                dir_to_save)
  
  sendmailR::sendmail(from = from, to = to, subject = subject, msg = body)
  paste0("Email sent to ", swsContext.userEmail)
  
}