#' FBS Tree
#' 
#' This dataset contains the aggregation rules for commodities up into FBS
#' aggregates.  Ultimately, this table should probably exist on the SWS.
#' 
#' \itemize{
#'      \item measuredItemSuaFbs The commodity code, in CPC format.
#'      \item fbsID4 The lowest FBS aggregate code.  On the disseminated food
#'      balance sheets, data is provided at the level of this code, as well as
#'      aggregated levels of this code.  For example, see 
#'      http://faostat3.fao.org/download/FB/FBS/E.
#'      \item fbsID3 The third-level FBS code.
#'      \item fbsID2 The second-level FBS code.
#'      \item fbsID1 The top-level FBS code.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @name fbsTree
#' @usage data(fbsTree)
#' @format A data table with 264 rows and 5 variables
NULL