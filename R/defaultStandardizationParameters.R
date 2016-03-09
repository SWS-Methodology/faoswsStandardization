##' Default Standardization Parameters
##' 
##' Provides an object which contains the standardization parameters.  This 
##' allows for easy passing into functions.  The meaning of most variables is 
##' fairly clear, but a few obscure ones are described below:
##' 
##' \itemize{
##' 
##' \item target The column name of the commodity tree data.frame which contains
##' directions on how to process the tree.  In particular, this column should be
##' either "B" or "F" (for "backwards" or "forward" processing).  Almost 
##' everything should be "backwards" standardized, except for some bizarre cases
##' such as sugar.  For sugar cane/beets, the quantities should be
##' "standardized" to raw sugar immediately, so the standardization proceedure
##' handles these "F"/"B" cases differently.
##' 
##' }
##' 
##' @return A list with the standardization parameters.
##'   
##' @export
##' 

defaultStandardizationParameters = function(){
    geoVar = "geographicAreaM49"
    yearVar = "timePointYears"
    itemVar = "measuredItemCPC"
    list(
        geoVar = geoVar,
        yearVar = yearVar,
        itemVar = itemVar,
        elementVar = "measuredElement",
        mergeKey = c(geoVar, yearVar, itemVar), # For merging with the main data
        elementPrefix = "Value_measuredElement_",
        childVar = "childID",
        parentVar = "parentID",
        standParentVar = "standParentID",
        extractVar = "extractionRate",
        standExtractVar = "standExtractionRate",
        shareVar = "share",
        targetVar = "target",
        productionCode = "production",
        yieldCode = "yield",
        areaHarvCode = "areaHarvested",
        importCode = "imports",
        exportCode = "exports",
        stockCode = "stockChange",
        foodCode = "food",
        foodProcCode = "foodManufacturing",
        feedCode = "feed",
        wasteCode = "loss",
        seedCode = "seed",
        industrialCode = "industrial",
        touristCode = "tourist",
        residualCode = "residual"
    )
}