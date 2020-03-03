##' Converts sugar codes 23511.01 and 23512 to 2351f
##' 
##' 
##' This function harmonize sugar codes. 
##' Raw cane and beet sugar are considered as separate codes in some domain, like trade, because sugar raw can be traded
##' as beet raw, cane raw or just raw (23511.01, 23512 or 2351f), 
##' but when one has to go from the processed product to the primary product, 
##' is not possible to know if a code 2351f has to be standardized to cane or beet, therefore 
##' in the standardization process cane and beet have to be considered as a unique code (2351f)
##' This function makes checks and harmonize sugar codes 
##'  
##' 
##' @param data the downloaded data from sua
##' @return A data.table with the data fixed for sugar
##' @export
##' 

convertSugarCodes_new <- function(data) {
  d <- copy(data)

  stock_elem_lab <-
    unique(d$measuredElementSuaFbs)[grep("5071|stock_change|stockChange", unique(d$measuredElementSuaFbs))]

  # Keep name as it can be measuredItemFbsSua or measuredItemFbsSua (why????)
  item_name <- names(d)[grepl("Item", names(d))]

  setnames(d, item_name, "item")

  sugar <- d[item %in% c('23511.01', '23512', '2351f') & measuredElementSuaFbs != stock_elem_lab]

  if (nrow(sugar) == 0) {
    return(d)
  }

  sugar[,
      n := .N,
      by = c('geographicAreaM49', 'measuredElementSuaFbs', 'timePointYears')
  ]

  sugar[,
      Value_max := max(Value[item == "2351f"], sum(Value[item %in% c('23511.01', '23512')])),
      by = c('geographicAreaM49', 'measuredElementSuaFbs', 'timePointYears')
  ]

  data("flagWeightTable", package = "faoswsFlag")

  sugar[
    n > 0,
    flag :=
      ifelse(
        "2351f" %in% item[Value == Value_max],
        flagObservationStatus[Value == Value_max],
        faoswsFlag::aggregateObservationFlag(flagObservationStatus[item != "2351f"])
      ),
      by = c('geographicAreaM49', 'measuredElementSuaFbs', 'timePointYears')
  ]

  sugar_summ <-
    sugar[,
      .(
        Value = max(Value[item == "2351f"], sum(Value[item %in% c('23511.01', '23512')])),
        flagObservationStatus = unique(flag),
        flagMethod = ifelse(.N > 1, 's', flagMethod),
        item = '2351f'
      ),
      by = c('geographicAreaM49', 'measuredElementSuaFbs', 'timePointYears')
    ]

  res <-
    rbind(
      d[!(item %in% c('23511.01', '23512', '2351f') & measuredElementSuaFbs != stock_elem_lab)],
      sugar_summ
    )

  setnames(res, "item", item_name)

  return(res)
}
