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
##' @return Nothing is returned, but the Value column of the passed data.table 
##'   is updated.
##'   

suaFilling = function(data, p, feedCommodities = c(), tree=tree,
                      primaryCommodities = c(), stockCommodities = c(),
                      utilizationTable=c(),imbalanceThreshold = 10){
    p = standParams

    # The commodities that have to be crude balanced are the NON PRIMARY
    
    stopifnot(imbalanceThreshold > 0)
  
    
    
##### LOOP 1:
 
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
  
   
   # We will need the information of total utilization
   
   data[, sumUtils := sum(ifelse(is.na(Value), 0, Value) *
                           ifelse(get(p$elementVar) == p$productionCode, 0,
                           ifelse(get(p$elementVar) == p$importCode, 0,
                           ifelse(get(p$elementVar) == p$exportCode, 1,
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
   
   # we need to know if production if official
   
   data[, officialProd := any(get(p$elementVar) == p$productionCode & 
                                get(p$official)== "TRUE" & Value > 0),
        by = c(p$itemVar)]
   
   # we also need to add the external information of UTilization percentage
   
   data=data.table(full_join(data,utilizationTable,
                             by=c("measuredItemSuaFbs","measuredElementSuaFbs")))
   
  ## Supply=Utilization? (=-threshold)
   
    ## YES -> go out of loop
    ## NO: 
   
      ## Different Treatment for the two cases: 
              
        ## Supply < utilization (= imbalance < -imbalanceThreshold)
         # Supply have to be created. 
 
   # if production is not official, Create production
   data[imbalance < -imbalanceThreshold & !officialProd & !(get(p$itemVar) %in% primaryCommodities)&get(p$elementVar)==p$productionCode,
        newValue:=ifelse(is.na(Value),-imbalance,Value-imbalance)]
   
   # if production is official and is a stock commodity, Create negative stock
   data[imbalance < -imbalanceThreshold & officialProd & get(p$itemVar) %in% stockCommodities & !(get(p$itemVar) %in% primaryCommodities)
        &get(p$elementVar)%in%p$stockCode,
        newValue:=ifelse(is.na(Value),-imbalance,Value-imbalance)]
   
   # if production is official and is NOT a stock commodity and abs(imbalance)<90% of sum(Utilization),
   # reduce utilization proportionally to their values
   # this is for nod deleting completely the existing utilization, allowing a reduction that will still leave
   # an amount of utilization
   
   data[imbalance < -imbalanceThreshold & officialProd & !(get(p$itemVar) %in% stockCommodities) & !(get(p$itemVar) %in% primaryCommodities)&
          !(get(p$elementVar)%in%c(p$productionCode,p$importCode))& abs(imbalance)<=(0.9*sumUtils),
        newValue:=Value-(abs(imbalance)*Value/sumUtils)]
  
   # if production is official and is NOT a stock commodity 
   # and abs(imbalance)>90% of sum(Utilization),
   # these cases have to be revised and are NOT balanced
   
   data[imbalance < -imbalanceThreshold & officialProd & !(get(p$itemVar) %in% stockCommodities) & !(get(p$itemVar) %in% primaryCommodities)&
          !(get(p$elementVar)%in%c(p$productionCode,p$importCode))& abs(imbalance)>(0.9*sumUtils)]
   
       ## Supply > utilization (= imbalance > imbalanceThreshold)
   
   
   # data[imbalance < -imbalanceThreshold & !officialProd &
   #        (!get(p$itemVar) %in% NotTobeBalanced) &
   #        get(p$elementVar) == p$productionCode ,
   #      Value := -newValue]
   # data[, c("imbalance", "newValue") := NULL]
   
    
    ## Supply > Utilization: assign difference to food, feed, etc.  Or, if 
    ## production is official, force a balance by adjusting food, feed, etc.
   data[(imbalance > imbalanceThreshold & officialProd )
    # data[(imbalance > imbalanceThreshold)
             & (!get(p$itemVar) %in% NotTobeBalanced) ,
         ## Remember, data is currently in long format.  This condition is ugly, but the idea is
         ## that Value should be replaced with newValue if the particular row of interest
         ## corresponds to the food variable and if the commodity should have it's residual
         ## allocated to food.  Likewise, if the row corresponds to feed and if the commodity
         ## should have it's residual allocated to feed, then Value is updated with newValue.
         Value := ifelse(
            (get(p$elementVar) == p$feedCode & get(p$itemVar) %in% feedCommodities) |
            (get(p$elementVar) == p$foodProcCode & get(p$itemVar) %in% foodProcessCommodities) |
            (get(p$elementVar) == p$industrialCode & get(p$itemVar) %in% indCommodities) |
            (get(p$elementVar) == p$stockCode & get(p$itemVar) %in% stockCommodities) |
            (get(p$elementVar) == p$wasteCode & get(p$itemVar) %in% lossCommodities) |
            (get(p$elementVar) == p$seedCode & get(p$itemVar) %in% seedCommodities) |
            #food have been defined outside the Balance Residual function (Cristina)
            (get(p$elementVar) == p$foodCode & get(p$itemVar) %in% foodCommodities),
            newValue, Value)]
    
  ##  data[(imbalance > imbalanceThreshold & officialProd )
  ##       & (!get(p$itemVar) %in% primaryCommodities) ,
  ##       ## Remember, data is currently in long format.  This condition is ugly, but the idea is
  ##       ## that Value should be replaced with newValue if the particular row of interest
  ##       ## corresponds to the food variable and if the commodity should have it's residual
  ##       ## allocated to food.  Likewise, if the row corresponds to feed and if the commodity
  ##       ## should have it's residual allocated to feed, then Value is updated with newValue.
  ##       Value := ifelse(get(p$elementVar) == p$foodProcCode,newValue, Value)]
    

}
