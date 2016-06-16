##' Aggregate Availability
##' 
##' In order to determine shares for standardization, we have to calculate 
##' availability of parent commodities.  For example, if fruit juice is produced
##' from both apples and oranges, and the country has 400 tonnes of apples and 
##' 100 tonnes of oranges, then we should standardize 80% of fruit juice values 
##' to apples and 20% to oranges.  This becomes more complicated when you 
##' consider the multiple levels of the tree, and that there may be trade of 
##' flour, for example, which influences the availability of wheat.
##' 
##' Note that availability becomes complicated with complicated graphs.  For 
##' example, if A is a parent of B and C, and B and C are both parents of D, 
##' what is the availability of A for standardizing D?  There is no clear best 
##' approach, but we decided to compute availability of A for D in this case by 
##' computing the availability of A, B, and C for D (i.e. aggregating the 
##' imbalances over all parents in the path).  In the case of A and B are 
##' parents of C and C is a parent of D, we have a different problem. Imbalances
##' in C shouldn't be double counted in the imbalances of A and B, so we should
##' split C's imbalance into A and B according to availability of A and B.
##' 
##' @param tree The commodity tree, specified as a data.table object.  It should
##'   have columns childVar (the commodity code of the child), parentVar (the 
##'   commodity code of the parent), extractionVar (numeric value specifying the
##'   extraction rate), and possibly shareVar (numeric value specifying how the 
##'   commodity should be split up), all of which are specified in standParams.
##' @param standParams The parameters for standardization.  These parameters 
##'   provide information about the columns of data and tree, specifying (for 
##'   example) which columns should be standardized, which columns represent 
##'   parents/children, etc.
##'   
##' @return A data.table with columns parentVar, childVar, and the availability 
##'   from that process.  Thus, if beer could be standardized into wheat, maize 
##'   or barley (and there is availability in all three products) our final 
##'   table will have three rows (beer/wheat, beer/maize, beer/barley).
##'   

calculateAvailability = function(tree, standParams){
    
    ## Since we'll be editing the tree, make a copy so we don't overwrite
    ## things.
    origTree = copy(tree[, c(standParams$parentVar, standParams$childVar,
                               standParams$extractVar, "availability"),
                           with = FALSE])
    level = findProcessingLevel(origTree, from = standParams$parentVar,
                        to = standParams$childVar, aupusParam = standParams)
    setnames(level, standParams$itemVar, standParams$parentVar)
    origTree = merge(origTree, level)
    origTree[processingLevel == 0,
             availability := availability * get(standParams$extractVar)]
        
    ## The initial availabilities should be reported for parents
    ## Parent child combinations should only have one output, but we get
    ## the mean just in case
    output = origTree[processingLevel == 0,
                      list(availability = mean(availability)),
                      by = c(standParams$parentVar, standParams$childVar)]
    
    editedTree = copy(origTree)
    ## Stop if we have a very flat tree:
    if (max(level$processingLevel) > 1) {
        for (i in 1:(max(level$processingLevel) - 1)) {
            ## Roll down availability
            copyTree = copy(origTree)
            # Rename parent as child and child as 'newChild'
            setnames(copyTree, c(standParams$parentVar, standParams$childVar),
                     c(standParams$childVar, "newChild"))
            # Extraction rate comes from copyTree, so deleted here
            editedTree[, c("extractionRate", "processingLevel") := NULL]
            # Merge original tree to get child availabilities
            editedTree = merge(editedTree, copyTree, by = standParams$childVar,
                           suffixes = c("", ".child"), allow.cartesian = TRUE)
            # Sum parent availability with child availability multiplied by
            # extraction rate
            editedTree[, availability := mean(
                (sapply(availability, na2zero) + sapply(availability.child, na2zero)) *
                    get(standParams$extractVar)),
                by = c(standParams$parentVar, "newChild")]
            editedTree[, c(standParams$childVar) := newChild]
            editedTree[, c("newChild", "availability.child") := NULL]
            
            output = rbind(output,
                editedTree[processingLevel == i, c(standParams$parentVar,
                                                   standParams$childVar,
                                                   "availability"), with = FALSE])
        }
    }
    ## Because we extract edges at each level, we could possibly still get
    ## multiple edges (i.e. A is a parent of B and C, but C is also a parent of
    ## B).  It's a weird case, but it exists and we need to handle it.  So, just
    ## average the availabilities in those cases.
    output = output[, list(availability = mean(availability)),
                    by = c(standParams$childVar, standParams$parentVar)]
    output[availability < 0, availability := 0]
    return(output)
}