##' This function replaces the old BalanceResidual
##' 
##' This function forces a the filling of empty elements in the pulled SUA
##' by allocating the "imbalance" according to a Ranking of the possible 
##' Uitlizazions for each combination of country/commodity
##' 
##' It works in loops:
##' 
##' LOOP1: 
##' 
##' - If supply < utilization
##'   * the imbalance is  assigned to production, if it is not official
##'   * if is official Utilizations are proportionally reduced, 
##'     if the difference S-U <=10% Utilizations
##'   * if none of these operations is possible, a manual check of the data is needed
##' - if supply > utilization a Multiple filler approach is usde: 
##'   * Utilization are created based on a ranking of possible Utilization 
##'     coming from an external source
##'     (CRISTINA: for the moment we are trying with the Approach2: using old SUA)
##' 
##' LOOP2: 
##' (Now production has been allocated and some utilization)
##' Food Processing is computed
##' This will create more utilizations and So it will be needed to go back to the 
##' Loop 1
##' Negative Stock is created in this step
##' 
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
##' @param foodProc indicates if food processing have been already calculated or not. On this information depends the 
##' sum of utils annd the fact that this element is considered or not
##' @return the Value column of the passed data.table is updated 
##'   

suaFillingold = function(data, p = p, tree=tree,
                      primaryCommodities = c(), stockCommodities = c(),
                      utilizationTable=c(),imbalanceThreshold = 10,foodProc=FALSE){

    # The commodities that have to be crude balanced are the NON PRIMARY
    
    stopifnot(imbalanceThreshold > 0)

  if(foodProc==FALSE){
    eleToExclude = c(p$productionCode,p$exportCode,p$importCode,p$foodProcCode)
  }else{
    eleToExclude = c(p$productionCode,p$exportCode,p$importCode)
    }
  
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
                         ifelse(get(p$elementVar) == p$foodProcCode, 1,
                         ifelse(get(p$elementVar) == p$feedCode, 1,
                         ifelse(get(p$elementVar) == p$wasteCode, 1,
                         ifelse(get(p$elementVar) == p$seedCode, 1,
                         ifelse(get(p$elementVar) == p$industrialCode, 1,
                         ifelse(get(p$elementVar) == p$touristCode, 1,
                         ifelse(get(p$elementVar) == p$residualCode, 1, 
                         NA))))))))))))),
       by = c(p$mergeKey)]
  
  # Supply have to be created. 
  # Step 1 and 2 are parallel
  for (j in 1:dim(data)[1]){
    ## Supply < utilization (= imbalance < -imbalanceThreshold)
    if(data[j,!(get(p$itemVar) %in% primaryCommodities)]){
    if(data[j,imbalance < (-imbalanceThreshold)]){
      # if production is not official, Create production
      if(data[j,!officialProd & get(p$elementVar)==p$productionCode]){
        data[j,newValue:=ifelse(is.na(Value),-imbalance,Value-imbalance)]
      }else{
        # if production is official and is a stock commodity, Create negative stock
        if(data[j,officialProd & get(p$itemVar) %in% stockCommodities 
                &get(p$elementVar)%in%p$stockCode]){
          data[j,newValue:=ifelse(is.na(Value),-imbalance,Value-imbalance)]
        }else{
          # if production is official and is NOT a stock commodity and abs(imbalance)<90% of sum(Utilization),
          # reduce utilization proportionally to their values
          # this is for nod deleting completely the existing utilization, allowing a reduction that will still leave
          # an amount of utilization
          if(data[j,officialProd & !(get(p$itemVar) %in% stockCommodities)&
                  !(get(p$elementVar)%in%c(p$productionCode,p$importCode))& abs(imbalance)<=(0.6*sumUtils)]){
            data[j,newValue:=Value-(abs(imbalance)*Value/sumUtils)]
          }else{
            # if production is official and is NOT a stock commodity
            # and abs(imbalance)>90% of sum(Utilization),
            # these cases have to be revised and should NOT bebalanced
            # I'm forcing the balancing for the moment but saving externally the 
            # data table with this information
            if(data[j,officialProd & !(get(p$itemVar) %in% stockCommodities)&
                    abs(imbalance)>(0.6*sumUtils)]){
              data[j,newValue:=ifelse(is.na(Value),-imbalance,Value-imbalance)]
              data[j,noF:=1]
              if(data[j,!is.na(rank)&is.na(Value)]){
                data[j,Value:=0]
              }
            }
          }
        }
      }
    }else{
      ## Supply > utilization (= imbalance > imbalanceThreshold)
      # in this step Trade has to be excluded. Values for import export 
      # will not be touched
      if(data[j,imbalance > imbalanceThreshold]){
        data[j,toB:=1]
      }
    }
  } 
}
  ## Supply > utilization (= imbalance > imbalanceThreshold)
  
  dataToBind = data[toB==1]
  
  data=data[is.na(toB)]
  
  actualCommodities = dataToBind[,unique(measuredItemSuaFbs)]
  
  for (i in actualCommodities){
    # Se tutti i Value sono popolati
    if(length(dataToBind[measuredItemSuaFbs==i
                         &!(get(p$elementVar)%in%eleToExclude)
                         &!is.na(rank)&(is.na(Value)|Value==0),Value])==0){
      # distribuisci inbalance proporzionalmente ai value stessi
      
      sumV=sum(dataToBind[measuredItemSuaFbs==i
                          &!(get(p$elementVar)%in%eleToExclude)
                          &!is.na(rank),Value],na.rm=TRUE)
      
      dataToBind[measuredItemSuaFbs==i
                 &!(get(p$elementVar)%in%eleToExclude)
                 &!is.na(rank),newValue:=imbalance*(Value/sumV)]
    }else{
      #se un valore non 'e popolato 
      if(length(dataToBind[measuredItemSuaFbs==i
                           &!(get(p$elementVar)%in%eleToExclude)
                           &!is.na(rank)&(is.na(Value)|Value==0),Value])==1){
        # metti tutto l' imbalance in questo elemento
        
        dataToBind[measuredItemSuaFbs==i
                   &!(get(p$elementVar)%in%eleToExclude)&!is.na(rank),
                   newValue:=imbalance]
        
      }else{
        # se c'e piu' di un elemento non popolato
        if(length(dataToBind[measuredItemSuaFbs==i
                             &!(get(p$elementVar)%in%eleToExclude)
                             &!is.na(rank)&(is.na(Value)|Value==0),Value])>1){
          # allora in base alla seguente funzione dei rank e rank inversi:
          sumRank = sum(dataToBind[measuredItemSuaFbs==i
                                   &!(get(p$elementVar)%in%eleToExclude)
                                   &!is.na(rank)&(is.na(Value)|Value==0),rank])
          dataToBind[measuredItemSuaFbs==i
                     &!(get(p$elementVar)%in%eleToExclude)
                     &!is.na(rank)&(is.na(Value)|Value==0),newValue:=imbalance*(rankInv/sumRank)]
        }
      }
    }
    
  }
  
  
  data=rbind(data,dataToBind)
  
  
  ###    ###    ###    ###    
  ### EXTERNAL SAVING OF FORCED INFORMATION
  NoFillable=data["noF"==1]
  
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
  
  
  
  data[!is.na(newValue),Value:=newValue]
  data[, c("imbalance", "newValue","sumUtils","noF","toB") := NULL]   
  
  
}
    


   
