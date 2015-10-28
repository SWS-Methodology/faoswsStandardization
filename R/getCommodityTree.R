##' Get Commodity Tree
##' 
##' This function pulls the commodity trees in the new CPC system.
##' 
##' @param geographicAreaM49 A character vector of area codes.  The trees 
##'   returned are specific to country and year; thus, providing this parameter 
##'   limits which trees are pulled.  If NULL, all are used.
##' @param timePointYears A character vector of years.  See geographicAreaM49.
##'   
##' @return A data.table object containing the commodity tree.  The dimension 
##'   columns correspond to the country, year, parent, and child commodity.  Two
##'   value columns are available: extraction rate and share.  The logic of the
##'   old system MAY have been that missing extraction rates were assumed to be
##'   1.
##' 
##' @export
##' 

getCommodityTree = function(geographicAreaM49 = NULL, timePointYears = NULL){
    ## Data Quality Checks
    if(!exists("swsContext.datasets")){
        stop("No swsContext.datasets object defined.  Thus, you probably ",
             "won't be able to read from the SWS and so this function won't ",
             "work.")
    }
    
    ## Define constants
    extractionCode = "5423"
    shareCode = "1"
    
    ## Define the dimensions and check for input errors
    allAreaCodes = GetCodeList(domain = "agriculture", dataset = "aupus_ratio",
                               dimension = "geographicAreaM49")
    allAreaCodes = allAreaCodes[type == "country", code]
    allYears = GetCodeList(domain = "agriculture", dataset = "aupus_ratio",
                               dimension = "timePointYears")[, code]
    allItemCodes = GetCodeList(domain = "agriculture", dataset = "aupus_ratio",
                               dimension = "measuredItemCPC")[, code]
    if(!is.null(geographicAreaM49)){
        stopifnot(geographicAreaM49 %in% allAreaCodes)
        allAreaCodes = geographicAreaM49
    }
    if(!is.null(timePointYears)){
        stopifnot(timePointYears %in% allYears)
        allYears = timePointYears
    }
    
    ## Extract the specific tree
    ratioKey = DatasetKey(
        domain = "agriculture", dataset = "aupus_ratio",
        dimensions = list(
            geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = allAreaCodes),
            measuredElement = Dimension(name = "measuredElement", keys = extractionCode),
            measuredItemCPC = Dimension(name = "measuredItemCPC", keys = allItemCodes),
            timePointYearsSP = Dimension(name = "timePointYearsSP", keys = allYears)
        ))
    ratioData = GetData(ratioKey)
    ratioData[, c("measuredElement", "flagRatio") := NULL]
    shareKey = DatasetKey(
        domain = "agriculture", dataset = "aupus_share",
        dimensions = list(
            geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = allAreaCodes),
            measuredElement = Dimension(name = "measuredShare", keys = shareCode),
            measuredItemChildCPC = Dimension(name = "measuredItemChildCPC", keys = allItemCodes),
            measuredItemParentCPC = Dimension(name = "measuredItemParentCPC", keys = allItemCodes),
            timePointYearsSP = Dimension(name = "timePointYearsSP", keys = allYears)
        ))
    shareData = GetData(shareKey)
    shareData[, c("measuredShare", "flagShare") := NULL]
    ## Merge together the trees
    setnames(ratioData, "measuredItemCPC", "measuredItemChildCPC")
    ## Kinda a hack here...  If no rows are returned, faosws and data.table will
    ## return a data.table with 0 rows and classes of type logical instead of
    ## character.
    ratioData[, geographicAreaM49 := as.character(geographicAreaM49)]
    ratioData[, measuredItemChildCPC := as.character(measuredItemChildCPC)]
    ratioData[, timePointYearsSP := as.character(timePointYearsSP)]
    shareData[, geographicAreaM49 := as.character(geographicAreaM49)]
    shareData[, measuredItemChildCPC := as.character(measuredItemChildCPC)]
    shareData[, timePointYearsSP := as.character(timePointYearsSP)]
    tree = merge(ratioData, shareData,
                 by = c("geographicAreaM49", "measuredItemChildCPC",
                        "timePointYearsSP"), all = TRUE)
    setnames(tree, "Value.x", "extractionRate")
    setnames(tree, "Value.y", "share")
    
    ## Extract the tree without specific years
    ratioKey@dimensions$timePointYearsSP@keys = "0"
    ratioData = GetData(ratioKey)
    ratioData[, c("measuredElement", "flagRatio") := NULL]
    shareKey@dimensions$timePointYearsSP@keys = "0"
    shareData = GetData(shareKey)
    shareData[, c("measuredShare", "flagShare") := NULL]
    ## Merge together the trees
    setnames(ratioData, "measuredItemCPC", "measuredItemChildCPC")
    ## Kinda a hack here...  If no rows are returned, faosws and data.table will
    ## return a data.table with 0 rows and classes of type logical instead of
    ## character.
    ratioData[, geographicAreaM49 := as.character(geographicAreaM49)]
    ratioData[, measuredItemChildCPC := as.character(measuredItemChildCPC)]
    ratioData[, timePointYearsSP := as.character(timePointYearsSP)]
    shareData[, geographicAreaM49 := as.character(geographicAreaM49)]
    shareData[, measuredItemChildCPC := as.character(measuredItemChildCPC)]
    shareData[, timePointYearsSP := as.character(timePointYearsSP)]
    treeYear = merge(ratioData, shareData,
                 by = c("geographicAreaM49", "measuredItemChildCPC",
                        "timePointYearsSP"), all = TRUE)
    setnames(treeYear, "Value.x", "extractionRate")
    setnames(treeYear, "Value.y", "share")
    treeYear[, timePointYearsSP := NULL]

    ## Extract the tree without specific years
    ratioKey@dimensions$geographicAreaM49@keys = "0"
    ratioData = GetData(ratioKey)
    ratioData[, c("measuredElement", "flagRatio") := NULL]
    shareKey@dimensions$geographicAreaM49@keys = "0"
    shareData = GetData(shareKey)
    shareData[, c("measuredShare", "flagShare") := NULL]
    ## Merge together the trees
    setnames(ratioData, "measuredItemCPC", "measuredItemChildCPC")
    ## Kinda a hack here...  If no rows are returned, faosws and data.table will
    ## return a data.table with 0 rows and classes of type logical instead of
    ## character.
    ratioData[, geographicAreaM49 := as.character(geographicAreaM49)]
    ratioData[, measuredItemChildCPC := as.character(measuredItemChildCPC)]
    ratioData[, timePointYearsSP := as.character(timePointYearsSP)]
    shareData[, geographicAreaM49 := as.character(geographicAreaM49)]
    shareData[, measuredItemChildCPC := as.character(measuredItemChildCPC)]
    shareData[, timePointYearsSP := as.character(timePointYearsSP)]
    treeGeneric = merge(ratioData, shareData,
                 by = c("geographicAreaM49", "measuredItemChildCPC",
                        "timePointYearsSP"), all = TRUE)
    setnames(treeGeneric, "Value.x", "extractionRate")
    setnames(treeGeneric, "Value.y", "share")
    treeGeneric[, c("timePointYearsSP", "geographicAreaM49") := NULL]
    
    ## Merge together all three trees, keeping the most specific information
    ## possible
    allEdges = rbind(tree[, c("measuredItemChildCPC", "measuredItemParentCPC"),
                          with = FALSE],
                     treeYear[, c("measuredItemChildCPC", "measuredItemParentCPC"),
                          with = FALSE],
                     treeGeneric[, c("measuredItemChildCPC", "measuredItemParentCPC"),
                          with = FALSE])
    allEdges = unique(allEdges)
    allEdges = allEdges[!is.na(measuredItemParentCPC), ]
    finalTree = expand.grid(geographicAreaM49 = allAreaCodes,
                            timePointYearsSP = allYears)
    finalTree = merge(finalTree, allEdges)
    finalTree = data.table(finalTree)
    finalTree = merge(finalTree, tree, by = c("geographicAreaM49",
                                              "timePointYearsSP",
                                              "measuredItemChildCPC",
                                              "measuredItemParentCPC"),
                      all.x = TRUE)
    finalTree = merge(finalTree, treeYear, by = c("geographicAreaM49",
                                                  "measuredItemChildCPC",
                                                  "measuredItemParentCPC"),
                      all.x = TRUE, suffixes = c("", ".new"))
    finalTree[is.na(share), share := share.new]
    finalTree[is.na(extractionRate), extractionRate := extractionRate.new]
    finalTree[, c("share.new", "extractionRate.new") := NULL]
    finalTree = merge(finalTree, treeGeneric, by = c("measuredItemChildCPC",
                                                     "measuredItemParentCPC"),
                      all.x = TRUE, suffixes = c("", ".new"))
    finalTree[is.na(share), share := share.new]
    finalTree[is.na(extractionRate), extractionRate := extractionRate.new]
    finalTree[, c("share.new", "extractionRate.new") := NULL]
    
    setcolorder(finalTree, c("geographicAreaM49", "timePointYearsSP",
                             "measuredItemParentCPC", "measuredItemChildCPC",
                             "extractionRate", "share"))
    finalTree[, extractionRate := extractionRate / 100]
    finalTree[, share := share / 100]
    finalTree
}