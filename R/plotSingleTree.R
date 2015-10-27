##' Plot Single Tree
##' 
##' This function generates a plot for one commodity trees defined with an edge 
##' list.
##' 
##' @param edges A data.table with parent and child node IDs (corresponding to 
##'   the IDs in nodes) which specify the commodity tree structure. 
##'   Additionally, there should be a column with extraction rate data and a 
##'   column with shares data.
##' @param parentColname The column name of commodityTree which contains the ID 
##'   of the parent node.
##' @param childColname The column name of commodityTree which contains the ID 
##'   of the child node.
##' @param extractionColname The column name of commodityTree which contains the
##'   extraction rate data.
##' @param dashInf Should extraction rates of infinity be represented with
##'   dashes?  This generally makes sense because such processing should be
##'   shown on the tree but not considered in standardization.
##' @param ... Additional plotting parameers for diagram::plotmat.
##'   
##' @return Generates a plot of the commodity tree as specified by edges.
##'   
##' @export
##' 

plotSingleTree = function(edges, parentColname, childColname,
                              extractionColname, dashInf = TRUE, ...){
    nodes = unique(c(edges[[parentColname]], edges[[childColname]]))
    
    ## Get the level for each node so we know which group to plot it in.
    level = getCommodityLevel(commodityTree = edges,
                              parentColname = parentColname,
                              childColname = childColname,
                              returnMinLevel = FALSE)
    level = level[order(level), ]
    levelCounts = level[order(level), .N, by = "level"]
    
    ## Create an adjacency matrix to define the plotting structure.
    A = matrix(0, nrow = length(nodes), ncol = length(nodes))
    colnames(A) = nodes
    rownames(A) = nodes
    indices = as.matrix(edges[, list(get(childColname), get(parentColname))])
    indices = apply(indices, c(1, 2), as.character)
    A[indices] = round(edges[[extractionColname]], 2)*100
    
    ## Create a matrix specifying curvature.  Default value should be
    curve = merge.data.frame(level, level, by = NULL)
    curve$curvature = ifelse(curve$level.y >= curve$level.x, .2, 0)
    curve = reshape::cast(curve, node.x ~ node.y, value = "curvature")
    rownames(curve) = curve$node.x
    curve$node.x = NULL
    
    ## Reorder A based on the levels
    A = A[as.character(level$node), as.character(level$node)]
    curve = curve[as.character(level$node), as.character(level$node)]

    ## Configure plotting arguments
    plotArgs = list(...)
    if(!"relsize" %in% names(plotArgs)) plotArgs$relsize = 1
    if(!"box.size" %in% names(plotArgs)) plotArgs$box.size = 0.03
    if(!"box.type" %in% names(plotArgs)) plotArgs$box.type = "rect"
    if(!"shadow.size" %in% names(plotArgs)) plotArgs$shadow.size = 0
    plotArgs$A = A
    plotArgs$pos = levelCounts$N
    plotArgs$curve = curve
    
    ## Plot it!
    plotmat = diagram::plotmat
    do.call(plotmat, plotArgs)
}
