##' Find Processing Level
##' 
##' The function finds the processing level of the item in relation
##' to the rest of the commodity items.  The processing level is zero for all
##' nodes which have no inputs (i.e. they can be processed in the first round).
##' A node with inputs has it's processing level defined as 1 greater than the
##' max processing level of all of it's input nodes.  Thus, nodes at level 0
##' are processed first, as their processing doesn't depend on any other
##' commodites.  Then, nodes at processing level 1 are processed because all
##' their inputs (level 0 nodes) have been computed.  All nodes at a fixed
##' processing level can thus be computed in one step.
##'
##' @param edgeData The edge data, typically from the function buildEdges
##' @param from The column name of edgeData corresponding to the from node.
##' @param to The column name of edgeData corresponding to the target node.
##' @param plot Logical, indicates of the graph should be plotted.
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' 
##' @return A data.table providing the processing level for each of the items.
##'
##' @export
##' 

findProcessingLevel = function(edgeData, from, to, plot = FALSE,
                               aupusParam = list(itemVar = "temp"),
                               errorOnLoop = TRUE){
    e = edgeData[, c(from, to), with = FALSE]
    v = unique(unlist(edgeData[, c(from, to), with = FALSE]))
    processingGraph = graph.data.frame(d = e, vertices = v, directed = TRUE)
    if(plot == TRUE)
            plot(processingGraph, vertex.size = 6, edge.arrow.size = 0.5)    
    root = names(which(degree(processingGraph, mode = "in") == 0 &
                           degree(processingGraph, mode = "out") > 0))

    processingLevel =
        shortest.paths(processingGraph, v = V(processingGraph),
                       to = V(processingGraph)[c(root)], mode = "in")

    ## Take the finite maximum level from processing level
    finalLevels = apply(processingLevel, 1,
        FUN = function(x) max(x[is.finite(x)], na.rm = TRUE))

    finalLevels.dt = data.table(names(finalLevels), finalLevels)
    setnames(finalLevels.dt, c(aupusParam$itemVar, "processingLevel"))
    
    if(errorOnLoop & finalLevels.dt[, min(processingLevel) == -Inf]){
        loopNodes = finalLevels.dt[processingLevel == -Inf,
                                   get(aupusParam$itemVar)]
        graph = graph.data.frame(edges[get(from) %in% loopNodes |
                                         get(to) %in% loopNodes, ])
        plot(graph)
        stop("Loops detected in commodity tree!  Check vertices:\n",
             paste(loopNodes, collapse = "\n"))
    }
    return(finalLevels.dt[order(processingLevel), ])
}
