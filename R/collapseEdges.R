##' Collapse Edges
##' 
##' This function takes a "edge" data.table (i.e. a data.table with parent/child
##' columns and an extraction rate column) and condenses the parent/child 
##' relationships.  For example, wheat may be processed into flour which is in 
##' turn processed into bread.  Thus, we can standardize bread to flour and then
##' the wheat, but it'd be easier to just standardize straight to bread.  This 
##' function condenses the edges by multiplying the extraction rates so that you
##' can take bread straight to wheat.
##' 
##' @param edges A data.table containing three columns (corresponding to a 
##'   parent, a child, and an extraction rate).
##' @param parentName The column name of the parent column in edges.
##' @param childName The column name of the child column in edges.
##' @param extractionName The column name of the extraction rate column in 
##'   edges.
##' @param keyCols The column name(s) of the columns of edges which should be
##'   considered as keys.  These columns will thus be included in any joins to
##'   ensure we don't get duplicates.  If there is no key, this can be "".
##'   
##' @return A data.table of the same structure as edges, but with intermediate 
##'   steps removed.
##'   

collapseEdges = function(edges, parentName = "parentID",
                         childName = "childID",
                         extractionName = "extractionRate",
                         keyCols = c("timePointYearsSP", "geographicAreaFS")){
    ## Data quality checks
    stopifnot(is(edges, "data.table"))
    stopifnot(c(parentName, childName, extractionName) %in% colnames(edges))
    if(max(edges[[extractionName]][edges[[extractionName]] < Inf]) > 100)
        stop("Extraction rates larger than 100 indicate they are probably ",
             "expressed in different units than on [0,1].  This will cause ",
             "huge problems when multiplying, and should be fixed.")
    ## Test for loops
    findProcessingLevel(edgeData = edges, from = parentName,
                        to = childName)
    
    targetNodes = setdiff(edges[[parentName]], edges[[childName]])
    edgesCopy = copy(edges[, c(parentName, childName, extractionName,
                               keyCols), with = FALSE])
    setnames(edgesCopy, c(parentName, childName, extractionName),
             c("newParent", parentName, "extractionMult"))
    finalEdges = edges[get(parentName) %in% targetNodes, ]
    currEdges = edges[!get(parentName) %in% targetNodes, ]
    while(nrow(currEdges) > 0){
        currEdges = merge(currEdges, edgesCopy, by = c(parentName, keyCols),
                      all.x = TRUE, allow.cartesian = TRUE)
        ## Update edges table with new parents/extraction rates.  For edges that
        ## didn't get changed, we keep the old parent name and extraction rate.
        currEdges[, c(parentName) := ifelse(is.na(newParent), get(parentName),
                                        newParent)]
        currEdges[, c(extractionName) := get(extractionName) *
                  ifelse(is.na(extractionMult), 1, extractionMult)]
        currEdges[, c("newParent", "extractionMult") := NULL]
        finalEdges = rbind(finalEdges, currEdges[get(parentName) %in% targetNodes, ])
        currEdges = currEdges[!get(parentName) %in% targetNodes, ]
    }
    finalEdges = unique(finalEdges,by=colnames(finalEdges))

    return(finalEdges)
}
