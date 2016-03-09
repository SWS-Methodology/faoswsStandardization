##' Get FBS Tree
##' 
##' This function creates the tree for aggregation to FBS commodities.
##' 
##' @return A data.table object with the FBS tree.
##' 
##' @import faoswsUtil fcl2cpc
##' 
##' @export
##' 

getFBSTree = function(){
    tree = fread("/home/josh/Documents/Github/faoswsAupus/data/item_tree_final.csv")
    tree = tree[, list(itemCode, fbsCode)]
    tree[, measuredItemCPC :=
             faoswsUtil::fcl2cpc(formatC(tree$itemCode, width = 4, flag = "0"))]
    tree = tree[!is.na(measuredItemCPC), list(measuredItemCPC, fbsCode)]
    tree = tree[!is.na(fbsCode), ]
    fbsLevels = fread("~/Documents/Github/faoswsAupus/documentation/annex7.csv")
    setnames(fbsLevels, paste0("fbsID", 1:4))
    tree = merge(fbsLevels, tree, by.x = "fbsID4", by.y = "fbsCode")
    setcolorder(tree, c("measuredItemCPC", paste0("fbsID", 4:1)))
    tree
}