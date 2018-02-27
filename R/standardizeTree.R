##' Standardize Tree
##' 
##' This function takes an input dataset and commodity tree and standardizes the
##' data according to the tree.
##' 
##' @param data A data.table object containing the data of interest.
##' @param tree The commodity tree, specified as a data.table object.  The 
##'   columns should be childVar (the commodity code of the child), parentVar 
##'   (the commodity code of the parent), extractionVar (numeric value 
##'   specifying the extraction rate), and shareVar (numeric value specifying 
##'   how the commodity should be split up).  There are also two optional 
##'   columns: targetVar (either "T", "B" or "F" indicating if that commodity is
##'   a target commodity, should be backward standardized, or should be forward 
##'   standardized) and standDev (containing the standard deviation estimates 
##'   which should be aggregated as well).  If the target is missing, everything
##'   is assumed to be backward standardized.  If no standDev is provided, a 
##'   deviation of 0 is assumed.  The actual names of the columns are specified 
##'   in standParams.
##' @param elements The element codes for nodes that should be standardized. 
##'   These correspond to the different "elements" of the FBS, such as 
##'   production, imports, exports, etc.
##' @param standParams The parameters for standardization.  These parameters 
##'   provide information about the columns of data and tree, specifying (for 
##'   example) which columns should be standardized, which columns represent 
##'   parents/children, etc.
##' @param sugarHack Logical.  Indicates if the commodity tree should be edited 
##'   by this program to give the correction standardization of sugar?  This is 
##'   a hack and should be fixed, but for now it is generally necessary.
##' @param zeroWeight Vector. Vector containg the items that must have a zero weights
##' because they represent co-byproducts. (see the example wheat, wheat flour, wheat germ..)  
##'   
##'   
##' @import data.table  
##' 
##' @return A data.table with the commodities standardized to the highest level 
##'   in the tree.
##'   

standardizeTree = function(data, tree, elements, standParams,zeroWeight=c(),
                           sugarHack = TRUE){

    ## Assign parameters
    geoVar = standParams$geoVar
    yearVar = standParams$yearVar
    itemVar = standParams$itemVar
    elementPrefix = standParams$elementPrefix
    childVar = standParams$childVar
    parentVar = standParams$parentVar
    extractVar = standParams$extractVar
    shareVar = standParams$shareVar
    protected = standParams$protected
    
    ## Data Quality Checks
    stopifnot(is(data, "data.table"))
    stopifnot(is(tree, "data.table"))
    stopifnot(c(geoVar, yearVar, itemVar, paste0(elementPrefix, elements)) %in%
                  colnames(data))
    stopifnot(c(geoVar, yearVar, childVar, parentVar, extractVar, shareVar)
              %in% colnames(tree))
    if(!all(sapply(data[, paste0(elementPrefix, c(elements)), with = FALSE],
                  is.numeric))){
        stop("Some of the elements passed are not numeric!")
    }
    if(!"target" %in% colnames(tree)){
        tree[, target := "B"]
    }
    stopifnot(all(tree[, target] %in% c("B", "T", "F")))
    if(!"standDev" %in% colnames(tree)){
        returnStandDev = FALSE
        tree[, standDev := 0]
    } else {
        returnStandDev = TRUE
    }
    stopifnot(all(tree[, standDev] >= 0))
    
    
    
    
    
    elements = paste0(elementPrefix, elements)
    
    ## Restructure the data for easier standardization
    standardizationData = data.table::melt.data.table(
        data = data, measure.vars = elements,
        id.vars = c(geoVar, yearVar, itemVar,protected),
        variable.name = "measuredElement", value.name = "Value")
    standardizationData[, measuredElement :=
                            gsub(elementPrefix, "", measuredElement)]
    
    ## To ensure commodities are standardized up multiple levels, we have to
    ## collapse the tree (otherwise if A -> B -> C in the tree, C may be
    ## standardized only to B and not to A, as desired).
    standKey = standParams$mergeKey[standParams$mergeKey != standParams$itemVar]
    if(dim(tree)[1]!=0){
    tree = collapseEdges(edges = tree, parentName = standParams$parentVar,
                         childName = standParams$childVar,
                         extractionName = standParams$extractVar,
                         keyCols = standKey)
    
    ## Merge the tree with the node data
    tree[, c(parentVar, childVar, yearVar, geoVar) :=
             list(as.character(get(parentVar)), as.character(get(childVar)),
                  as.character(get(yearVar)), as.character(get(geoVar)))]
    }
    setnames(standardizationData, itemVar, childVar)
    standardizationData[, c(childVar, yearVar, geoVar) :=
                            list(as.character(get(childVar)),
                                 as.character(get(yearVar)),
                                 as.character(get(geoVar)))]
    
    ## To deal with joint byproducts

    standardizationData = merge(standardizationData, tree,
                                by = c(yearVar, geoVar, childVar),
                                all.x = TRUE, allow.cartesian = TRUE)
    
    ##' If an element is not a child in the tree, then "standardize" it to
    ##' itself with a rate of 1 and a share of 1.
     standardizationData[is.na(get(parentVar)),
                        c(parentVar, extractVar, shareVar) :=
                          list(get(childVar), 1, 1)]
    
    
    standardizationData[,weight:=1]
    standardizationData[measuredItemChildCPC %in% zeroWeight , weight:=0]
    
    ## Standardizing backwards is easy: we just take the value, divide by the 
    ## extraction rate, and multiply by the shares.  However, we don't 
    ## standardize the production element (because production of flour is 
    ## derived from the production of wheat already).  We standardize everything
    ## backwards, and then edges marked as forwards (i.e. target == "F") get
    ## standardized down.

    # Extraction rates of zero will cause us to divide by zero, so we must
    # remove them
    extract0 <- standardizationData[abs(get(extractVar)) < .Machine$double.eps ^ .5]
    if(nrow(extract0) > 0){
      # Check for tricky floating point issues, but checking if equal to 0
      warning(sprintf("Extraction rates of 0 present in commodity codes: {%s} in country {%s}.
                      Ignoring all extraction rates of 0 in backwards standardization",
              paste0(unique(extract0[, get(parentVar)]), collapse = ", "), unique(extract0[, get(geoVar)])))
      standardizationData[abs(get(extractVar)) < .Machine$double.eps^.5, Value := NA]
    }
    output = standardizationData[, list(
       Value = sum( Value  *    weight   /get(extractVar)*get(shareVar), na.rm = TRUE)),
       by = c(yearVar, geoVar,
              "measuredElement", parentVar)]
   
   forwardEdges = tree[target == "F", ]
    
  

    ## Reshape to put back into the same shape as the passed data
    setnames(output, parentVar, itemVar)
    output[, measuredElement := paste0(elementPrefix,
                                       measuredElement)]
    form = as.formula(paste(yearVar, "+", geoVar, "+", itemVar, "~ measuredElement"))
    output = dcast.data.table(data = output, formula = form, value.var = "Value",
                              fun.aggregate = mean, na.rm = TRUE)
    return(output)
}
