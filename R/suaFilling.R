##' This function replaces the old BalanceResidual
##' 
##' This function forces a the filling of empty elements in the pulled SUA
##' by allocating the "imbalance" according to a Ranking of the possible 
##' Uitlizazions for each combination of country/commodity
##' 
##' - If supply < utilization
##'   * the imbalance is assigned to production, if it is not official
##'   * if is official it is checked if the commodity is a stock commodity
##'     i.e. if it does make sense to allocate stock. If it is so, stock is created (negative)
##'   * if production is official and is not a stocj comodity, 
##'     Utilizations are proportionally reduced,if the difference S-U <=10% Utilizations
##'   * if none of these operations is possible, a manual check of the data is needed
##'     (for the moment production is increased anyway and an external file is produced for check)
##' - if supply > utilization a Multiple filler approach is used: 
##'   * If all the ranked utilizations are present 
##'     these are proportionally incremented
##'   * if one is empty, imbalance goes to this commodity
##'   * if more than one is empty, Utilization are created based on a ranking of possible Utilization 
##'     coming from an external source.
##'     The approach used is of the Inverse Ranking. 
##'     (CRISTINA: for the moment we are trying with the Approach2: using old SUA)
##' 
##' Trade and Food Processing is never created in this function:
##' - trade is supposed to be "protected", 
##' - food processing is created outside this function 
##'   using a specific function and is not replaced here
##' 
##' @param data The data.table containing the full dataset for standardization.
##' @param p The parameters for standardization.  These parameters 
##'   provide information about the columns of data and tree, specifying (for 
##'   example) which columns should be standardized, which columns represent 
##'   parents/children, etc.
##' @param primaryCommodities Primary level commodities (such as wheat, oranges,
##'   sweet potatoes, etc.) should not be balanced at this step but rather by
##'   the balancing algorithm.  This argument allows the user to specify a
##'   character vector with these primary element codes.
##' @param stockCommodities This list specify if the commodity can be allocated to stock
##' @param utilizationTable is the external utilizataion table
##' @param imbalanceThreshold The size that the imbalance must be in order for 
##'   an adjustment to be made.
##' @param cut these are primary equivalent commodities.
##' @param tree this is the sub tree used in the function.
##' @return the Value column of the passed data.table is updated 
##'   

suaFilling = function(data, p = p, tree=tree,
                      primaryCommodities = c(), stockCommodities = c(),
                      debugFile= NULL,
                      utilizationTable=c(),imbalanceThreshold = 10){

    # The commodities that have to be crude balanced are the NON PRIMARY
    
    stopifnot(imbalanceThreshold > 0)

    eleToExclude = c(p$productionCode,p$exportCode,p$importCode,p$foodProcCode)
 
  
#############################
   # STEP 1: create production for subsequent food processing calculation:
############################# 

  ## Supply-Utilization = imbalance
  
  data[, imbalance := sum(ifelse(is.na(Value), 0, Value) *
                          ifelse(get(p$elementVar) == p$productionCode, 1,
                          ifelse(get(p$elementVar) == p$importCode, 1,
                          ifelse(get(p$elementVar) == p$exportCode, -1,
                          ifelse(get(p$elementVar) == p$stockCode, -1,
                          ifelse(get(p$elementVar) == p$foodCode, -1,
                          ifelse(get(p$elementVar) == p$foodProcCode, -1,
                          ifelse(get(p$elementVar) == p$feedCode, -1,
                          ifelse(get(p$elementVar) == p$wasteCode, -1,
                          ifelse(get(p$elementVar) == p$seedCode, -1,
                          ifelse(get(p$elementVar) == p$industrialCode, -1,
                          ifelse(get(p$elementVar) == p$touristCode, -1,
                          ifelse(get(p$elementVar) == p$residualCode, -1, 
                          NA))))))))))))),
       by = c(p$mergeKey)]
  
  # we need the sum of Utilizations
  
  data[, sumUtils := sum(ifelse(is.na(Value), 0, Value) *
                         ifelse(get(p$elementVar) == p$productionCode, 0,
                         ifelse(get(p$elementVar) == p$importCode, 0,
                         ifelse(get(p$elementVar) == p$exportCode, 0,
                         ifelse(get(p$elementVar) == p$stockCode, 1,
                         ifelse(get(p$elementVar) == p$foodCode, 1,
                         ifelse(get(p$elementVar) == p$foodProcCode, 0,
                         ifelse(get(p$elementVar) == p$feedCode, 1,
                         ifelse(get(p$elementVar) == p$wasteCode, 1,
                         ifelse(get(p$elementVar) == p$seedCode, 1,
                         ifelse(get(p$elementVar) == p$industrialCode, 1,
                         ifelse(get(p$elementVar) == p$touristCode, 1,
                         ifelse(get(p$elementVar) == p$residualCode, 1, 
                         NA))))))))))))),
       by = c(p$mergeKey)]
  
  # the following line added otherwise some cases were not treated at all 
  data[is.na(officialProd),officialProd:="FALSE"]
  
  # I serparate the different blocks of data for trating them separately 
  dataPrimary = data[(get(p$itemVar) %in% primaryCommodities)]
  dataNoPrimary = data[!(get(p$itemVar) %in% primaryCommodities)]
  
  dataNoImb = dataNoPrimary[imbalance<=imbalanceThreshold&imbalance>=(-imbalanceThreshold)]
  
  
  dataNegImb = dataNoPrimary[imbalance < (-imbalanceThreshold)]
  dataPosImb = dataNoPrimary[imbalance > imbalanceThreshold]

  ## Supply < utilization (= imbalance < -imbalanceThreshold)
  
  # if production is not official, create production
  dataNegImb[officialProd=="FALSE" & get(p$elementVar)==p$productionCode,
             newValue:=ifelse(is.na(Value),-imbalance,Value-imbalance)]
  
  # if production is official and is a stock commodity, Create negative stock

  dataNegImb[officialProd=="TRUE" & get(p$itemVar) %in% stockCommodities&get(p$elementVar) == p$stockCode,
             newValue:= ifelse(is.na(Value),imbalance,Value+imbalance),
             by=c(p$mergeKey)]
  
    # if production is official and is NOT a stock commodity and abs(imbalance)<90% of sum(Utilization),
  # reduce utilization proportionally to their values
  # this is for nod deleting completely the existing utilization, allowing a reduction that will still leave
  # an amount of utilization
  
  dataNegImb[officialProd=="TRUE" & !(get(p$itemVar) %in% stockCommodities)&(abs(imbalance)<=(0.6*sumUtils)),
                    newValue:=(1-(abs(imbalance)/sumUtils))*
                      ifelse(get(p$elementVar)%in%eleToExclude,NA,
                             ifelse(is.na(Value),NA,Value))]
                    
  # if production is official and is NOT a stock commodity
  # and abs(imbalance)>90% of sum(Utilization),
  # these cases have to be revised and should NOT bebalanced
  # I'm forcing the balancing for the moment but saving externally the 
  # data table with this information
  NoFillable=dataNegImb[officialProd=="TRUE" & !(get(p$itemVar) %in% stockCommodities)&
               (abs(imbalance)>(0.6*sumUtils))]
             
             
  dataNegImb[officialProd=="TRUE" & !(get(p$itemVar) %in% stockCommodities)&
               (abs(imbalance)>(0.6*sumUtils))& get(p$elementVar)==p$productionCode,
             newValue:=ifelse(is.na(Value),-imbalance,Value-imbalance)]

  ## Supply > utilization (= imbalance > imbalanceThreshold)
  
  
  actualCommodities = dataPosImb[,unique(measuredItemSuaFbs)]
  
  for (i in actualCommodities){
    # If none of the utilization is activable based in the utilization Table
    if(length(dataPosImb[measuredItemSuaFbs==i
                         &!(get(p$elementVar)%in%eleToExclude)
                         &!is.na(rank),Value])==0){
    # conventionally put all on food (As was in the previous version of the new module)
    # this a very rare case but can happen
      dataPosImb[measuredItemSuaFbs==i
                 &!(get(p$elementVar)%in%eleToExclude)&
                 get(p$elementVar)==p$foodCode,newValue:=imbalance]
    }else{
      # Se tutti i Value sono popolati
    if(length(dataPosImb[measuredItemSuaFbs==i
                         &!(get(p$elementVar)%in%eleToExclude)
                         &!is.na(rank)&(is.na(Value)|Value==0),Value])==0){
      # distribuisci inbalance proporzionalmente ai value stessi
      
      sumV=sum(dataPosImb[measuredItemSuaFbs==i
                          &!(get(p$elementVar)%in%eleToExclude)
                          &!is.na(rank),Value],na.rm=TRUE)
      
      dataPosImb[measuredItemSuaFbs==i
                 &!(get(p$elementVar)%in%eleToExclude)
                 &!is.na(rank),newValue:=imbalance*(Value/sumV)]
    }else{
      #se un valore non 'e popolato 
      if(length(dataPosImb[measuredItemSuaFbs==i
                           &!(get(p$elementVar)%in%eleToExclude)
                           &!is.na(rank)&(is.na(Value)|Value==0),Value])==1){
        # metti tutto l' imbalance in questo elemento
        
        dataPosImb[measuredItemSuaFbs==i
                   &!(get(p$elementVar)%in%eleToExclude)&!is.na(rank)
                   &(is.na(Value)|Value==0),
                   newValue:=imbalance]
        
      }else{
        # se c'e piu' di un elemento non popolato
        if(length(dataPosImb[measuredItemSuaFbs==i
                             &!(get(p$elementVar)%in%eleToExclude)
                             &!is.na(rank)&(is.na(Value)|Value==0),Value])>1){
          # allora in base alla seguente funzione dei rank e rank inversi:
          sumRank = sum(dataPosImb[measuredItemSuaFbs==i
                                   &!(get(p$elementVar)%in%eleToExclude)
                                   &!is.na(rank)&(is.na(Value)|Value==0),rank])
          dataPosImb[measuredItemSuaFbs==i
                     &!(get(p$elementVar)%in%eleToExclude)
                     &!is.na(rank)&(is.na(Value)|Value==0),newValue:=imbalance*(rankInv/sumRank)]
        }
      }
    }
    
  }
  
}
  
  
  ###    ###    ###    ###    
  ### EXTERNAL SAVING OF FORCED INFORMATION

  if(dim(NoFillable)[1]>0){
    message(paste0("There are ",dim(NoFillable)[1]," row with supply<Utilization where Official Production has been incremented"))
  
    NoFillable[,noF:=NULL]
    setnames(NoFillable, "measuredItemSuaFbs", "measuredItemFbsSua")
    fbs_sua_conversion <- data.table(measuredElementSuaFbs=c("Calories", "Fats", "Proteins", "exports", "feed", "food", 
                                                             "foodManufacturing", "imports", "loss", "production", 
                                                             "seed", "stockChange", "residual","industrial", "tourist"),
                                     code=c("261", "281", "271", "5910", "5520", "5141", 
                                            "5023", "5610", "5016", "5510",
                                            "5525", "5071", "5166","5165", "5164"))
    
    standData = merge(NoFillable, fbs_sua_conversion, by = "measuredElementSuaFbs")
    standData[,`:=`(measuredElementSuaFbs = NULL)]
    setnames(standData, "code", "measuredElementSuaFbs")
    
    standData=standData[,.(geographicAreaM49, measuredElementSuaFbs, measuredItemFbsSua, timePointYears, 
                           Value)]
    standData <- standData[!is.na(Value),]
    
    standData[, flagObservationStatus := "I"]
    standData[, flagMethod := "x"]
    
    if(!is.null(debugFile)){
      
      saveFBSItermediateStep(directory=paste0("debugFile/Batch_",batchnumber),
                             fileName=paste0("B",batchnumber,"_10_ForcedProduction"),
                             data=standData)
    }

  }
  ###    ###    ###    ###    
  
  if(dim(dataNegImb)[1]==0){
    if(dim(dataPosImb)[1]==0){
      data=rbind(dataPrimary,dataNoImb)
    }else{
      dataPosImb[!is.na(newValue),Value:=newValue]
      data=rbind(dataPrimary,dataNoImb,dataPosImb[,1:14,with=FALSE])
    }
  }else{
    dataNegImb[!is.na(newValue),Value:=newValue]
  if(dim(dataPosImb)[1]==0){
    data=rbind(dataPrimary,dataNoImb,dataNegImb[,1:14,with=FALSE])
  }else{
  dataPosImb[!is.na(newValue),Value:=newValue]
  data=rbind(dataPrimary,dataNoImb,dataNegImb[,1:14,with=FALSE],dataPosImb[,1:14,with=FALSE])
  } 
}
  data[, c("imbalance","sumUtils") := NULL]   
  
}
    


   
