##' Plot Commodity Trees
##' 
##' This function generates plots for each of the commodity trees defined
##' in a commodity tree object.  A separate plot is given for each node that
##' has no parent, and this should allow for the production of a wheat tree,
##' a milk tree, etc.
##' 
##' @param commodityTree A data.table with parent and child node IDs
##' (corresponding to the IDs in nodes) which specify the commodity tree
##' structure.  Additionally, there should be a column with extraction rate
##' data and a column with shares data.
##' @param parentColname The column name of commodityTree which contains the ID
##' of the parent node.
##' @param childColname The column name of commodityTree which contains the ID
##' of the child node.
##' @param extractionColname The column name of commodityTree which contains
##' the extraction rate data.
##' @param dir The directory where the commodity trees should be saved.
##' @param prefix The prefix to add to the name of the plot(s) which will be
##' saved to dir.  One plot is created for each commodity tree.
##' @param adjExtractionRates Logical.  Typically, the extraction rates are
##' stored as percentages multiplied by 10000, i.e. 5% is represented as 
##' 0.05*10000 = 500.  If TRUE, the function converts them to percentages.
##' 
##' @return No object is returned, but image files of the plots are written
##' out to .png files.  The input parameter dir specifies where these files
##' will be placed.
##' 
##' @export
##' 

plotCommodityTrees = function(commodityTree, parentColname, childColname,
                              extractionColname, dir = getwd(),
                              prefix = "commodity_tree",
                              adjExtractionRates = TRUE){
    
    ## Data Quality Checks
    stopifnot(is(commodityTree, "data.table"))
    stopifnot(c(parentColname, childColname, extractionColname) %in%
                  colnames(commodityTree))
    if(any(commodityTree[, .N, by = c(parentColname, childColname)][, N] > 1))
        stop("This function is not designed to work for multiple countries, ",
             "years, etc.  Please subset the data and loop to generate ",
             "multiple such trees.")
    
    if(adjExtractionRates)
        commodityTree[, c(extractionColname) := get(extractionColname)/10000]
    
    ## Find the top nodes.
    topNodes = setdiff(commodityTree[[parentColname]],
                       commodityTree[[childColname]])
    
    ## Now, we'll assume that each topNode corresponds ot a unique commodity
    ## tree.
    ##
    ## Assign the edges to the approriate tree.  Any child node receives the
    ## treeID of it's parent, if available.  Some children may be children
    ## of multiple parents and hence (possibly) multiple treeID's.  In those
    ## cases, we'll need to group treeID's into the same group.
    commodityTree[, treeID := NA_character_]
    commodityTree[get(parentColname) %in% topNodes,
                  treeID := as.character(get(parentColname))]
    while(any(is.na(commodityTree[, treeID]))){
        ids = commodityTree[!is.na(treeID), .N, by = c(childColname, "treeID")]
        ids[, N := NULL]
        setnames(ids, c(childColname, "treeID"), c(parentColname, "newTreeID"))
        commodityTree = merge(commodityTree, ids, by = parentColname,
                              all.x = TRUE, allow.cartesian = TRUE)
        commodityTree[is.na(treeID), treeID := newTreeID]
        commodityTree[, newTreeID := NULL]
    }
    
    for(currentTreeID in unique(commodityTree$treeID)){
        png(paste0(dir, "/", prefix, "_commodity_tree_", currentTreeID, ".png"),
            width = 10, height = 10, units = "in", res = 400)
        plotSingleTree(commodityTree[treeID == currentTreeID, ], parentColname,
                       childColname, extractionColname)
        dev.off()
    }
}