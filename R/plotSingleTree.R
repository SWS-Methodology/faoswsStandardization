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
##'   
##' @return Generates a plot of the commodity tree as specified by edges.
##'   

plotSingleTree = function(edges, parentColname, childColname,
                              extractionColname){
    nodes = unique(c(edges[[parentColname]], edges[[childColname]]))
    
    ## Create an adjacency matrix to define the plotting structure.
    A = matrix(0, nrow = length(nodes), ncol = length(nodes))
    colnames(A) = nodes
    rownames(A) = nodes
    indices = as.matrix(edges[, list(get(childColname), get(parentColname))])
    indices = apply(indices, c(1, 2), as.character)
    A[indices] = round(edges[[extractionColname]], 2)*100
    
    ## Get the level for each node so we know which group to plot it in.
    level = getCommodityLevel(commodityTree = edges,
                              parentColname = parentColname,
                              childColname = childColname)
    level = level[order(level), ]
    
    ## Reorder A based on the levels
    A = A[as.character(level$node), as.character(level$node)]

    ## Plot it!
    levelCounts = level[order(level), .N, by = "level"]
    diagram::plotmat(A, pos = levelCounts$N, curve = 0, relsize = 1,
                     box.size = 0.01, box.type = "rect", shadow.size = 0)
}
