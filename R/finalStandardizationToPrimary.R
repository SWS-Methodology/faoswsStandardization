##' Final Standardization to Primary Equivalent
##' 
##' After the full SUA has been balanced, all the commodities need to be rolled 
##' up to their primary equivalents.  This function does this, aggregating up 
##' trade and food.
##' 
##' @param data The data.table containing the full dataset for standardization.
##' @param tree The commodity tree which provides the edge structure.  Note that
##'   this tree may be different than the processing tree, in particular if some
##'   commodities will be standardized into a different aggregate.
##' @param standParams The parameters for standardization.  These parameters 
##'   provide information about the columns of data and tree, specifying (for 
##'   example) which columns should be standardized, which columns represent 
##'   parents/children, etc.
##' @param sugarHack Logical.  See standardizeTree for details.
##' @param specificTree Logical.  Is a country/year specific commodity tree 
##'   being provided?
##' @param additiveElements Column names of data which should be
##'   aggregated/standardized via simple addition.
##'   
##' @return A data.table with the aggregated primary commodities.
##'   

finalStandardizationToPrimary = function(data, tree, standParams,
                                         sugarHack = TRUE, specificTree = TRUE,
                                         additiveElements = c()){
    
    ## Note: food processing amounts should be set to zero for almost all
    ## commodities (as food processing shouldn't be standardized, generally). 
    ## However, if a processed product is standardized in a different tree, then
    ## a balanced SUA line will NOT imply a (roughly, i.e. we still must
    ## optimize) balanced FBS.  Thus, the food processing for grafted
    ## commodities should be rolled up into the parents as "Food Processing" or
    ## "Food Manufacturing".
    foodProcElements = tree[!is.na(get(standParams$standParentVar)),
                            unique(get(standParams$childVar))]
    data[get(standParams$elementVar) == standParams$foodProcCode &
             !get(standParams$itemVar) %in% foodProcElements, Value := 0]
    ## Assign production of these commodities to their food processing element
    ## so that we can roll that up.
    toMerge = data[get(standParams$itemVar) %in% foodProcElements &
                       get(standParams$elementVar) == "5510", ]
    toMerge[, c(standParams$elementVar) := standParams$foodProcCode]
    toMerge = toMerge[, c(standParams$mergeKey, standParams$elementVar, "Value"), with = FALSE]
    data = merge(data, toMerge, by = c(standParams$mergeKey, standParams$elementVar), all.x = TRUE,
                 suffixes = c("", ".new"))
    data[!is.na(Value.new), Value := Value.new]
    
    ## Now, we must adjust the commodity tree for the pruned elements.  We want 
    ## all elements to roll up to the new parent ID except for the food 
    ## processing.  Thus, we must keep both parents in the tree and use new 
    ## codes to identify the two cases.  The nodes rolling into new parentIDs
    ## get new_ prefixes.
    data[get(standParams$itemVar) %in% foodProcElements & get(standParams$elementVar) == standParams$foodProcCode,
         c(standParams$itemVar) := paste0("f???_", get(standParams$itemVar))]
    tree[get(standParams$childVar) %in% foodProcElements,
         c(standParams$childVar) := paste0("f???_", get(standParams$childVar))]
    ## Originally I thought we needed to roll up the new elements into a new
    ## commodity.  But, that's not the case; if we leave those edges off the
    ## tree entirely, they'll standardize to themselves.
#     addToTree = tree[!is.na(get(standParams$standParentVar)), ]
#     addToTree[, c("groupID", "availability") := NULL]
#     addToTree[, share := 1]
#     addToTree[, c(standParams$parentVar) := get(standParams$standParentVar)]
#     addToTree[, c(standParams$extractVar) := get(standParams$standExtractVar)]
#     addToTree[, c(standParams$childVar) := paste0("new_", get(standParams$childVar))]
#     addToTree = unique(addToTree)
#     tree = rbindlist(list(tree, addToTree), fill = TRUE)
    
    keyCols = standParams$mergeKey[standParams$mergeKey != standParams$itemVar]
    if(!specificTree){
        if(nrow(data[, .N, by = c(standParams$geoVar, standParams$yearVar)]) > 1)
            stop("If not using a specificTree, there should only be one ",
                 "country and year!")
        keyCols = keyCols[!keyCols %in% c(standParams$geoVar, standParams$yearVar)]
        tree[, c(standParams$yearVar) := data[, get(standParams$yearVar)][1]]
        tree[, c(standParams$geoVar) := data[, get(standParams$geoVar)][1]]
    }
    standTree = collapseEdges(edges = tree, keyCols = keyCols,
                              parentName = standParams$parentVar,
                              childName = standParams$childVar,
                              extractionName = standParams$extractVar)
    localParams = standParams
    localParams$elementPrefix = ""
    out = data[, standardizeTree(data = .SD, tree = standTree,
                                 standParams = localParams, elements = "Value",
                                 sugarHack = sugarHack),
               by = c(standParams$elementVar)]
    if(length(additiveElements) > 0){
        additiveTree = copy(standTree)
        additiveTree[, c(standParams$extractVar) := 1]
        nutrients = lapply(additiveElements, function(nutrient){
            temp = data[get(standParams$elementVar) == standParams$foodCode,
                        standardizeTree(data = .SD, tree = additiveTree,
                                        standParams = localParams, elements = nutrient,
                                        sugarHack = sugarHack)]
            temp[, Value := get(nutrient)]
            temp[, get(standParams$elementVar) := nutrient]
            temp[, c(nutrient) := NULL]
            temp
        })
        out = rbind(out, do.call("rbind", nutrients))
    }

    ## Add on the primary value for use in some special cases of
    ## standardization.
    out = merge(out, data[, c(standParams$mergeKey, standParams$elementVar, "Value"),
                          with = FALSE],
                by = c(standParams$mergeKey, standParams$elementVar),
                suffixes = c("", ".old"), all.x = TRUE)
    
#     ## Standardizing food values is complicated.  The value reported under
#     ## "food" for the primary commodity includes the quantity eaten as such plus
#     ## the quantity of the primary commodity allocated to processing.  Thus, we
#     ## must determine (a) how much of the primary commodity is eaten as such and
#     ## (b) the standardized food values of just the processed products.
#     ## 
#     ## To determine (a), we compute the standardized production value and 
#     ## subtract from it the food value of the primary commodity.
#     ## 
#     ## To determine (b), we compute the standardized food values of the 
#     ## processed products.
#     ## 
#     ## Thus, standardized food is computed as:
#     ## Food(primary) - (Production(standardized) - Production(primary)) +
#     ## Food(standardized) - Food(primary) =
#     ## Production(primary) - Production(standardized) + Food(standardized)
#     correctFood = out[, .SD[element == standParams$productionCode, Value.old] -
#                         .SD[element == standParams$productionCode, Value] +
#                         .SD[element == standParams$foodCode, Value],
#                       by = c(standParams$mergeKey)]
#     setnames(correctFood, "V1", "Value")
#     correctFood[, element := standParams$foodCode]
#     out = merge(out, correctFood, suffixes = c("", ".new"),
#                 by = c(standParams$mergeKey, "element"), all.x = TRUE)
#     out[!is.na(Value.new), Value := Value.new]
#     out[, Value.new := NULL]
    
    ## Production should never be standardized. Instead, take the primary value 
    ## directly.  But, the elements in the food processing tree won't have a
    ## previously assigned production, so don't do this for them.
    foodProcParents = tree[grepl("^f\\?\\?\\?_", get(standParams$childVar)),
                           unique(get(standParams$parentVar))]
    foodProcParents = c()
    out[get(standParams$elementVar) %in% c(standParams$productionCode) &
            !get(standParams$itemVar) %in% foodProcParents,
        Value := Value.old]
    warning("The standardization approach may not work for production in ",
            "the case of grafted trees IF those grafted trees have more than ",
            "one level.  This likely won't occur often, but we'll need to ",
            "check and confirm.")
    out[, Value.old := NULL]
    
    return(out)
}