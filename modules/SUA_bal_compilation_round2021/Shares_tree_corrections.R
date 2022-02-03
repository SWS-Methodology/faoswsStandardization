library(faosws)
library(faoswsUtil)
library(faoswsBalancing)
library(faoswsStandardization)
library(dplyr)
library(data.table)
library(tidyr)
library(openxlsx)

# The only parameter is the string to print
# COUNTRY is taken from environment (parameter)
dbg_print <- function(x) {
  message(paste0("NEWBAL (", COUNTRY, "): ", x))
}

start_time <- Sys.time()

R_SWS_SHARE_PATH <- Sys.getenv("R_SWS_SHARE_PATH")

if (CheckDebug()) {
  R_SWS_SHARE_PATH <- "//hqlprsws1.hq.un.fao.org/sws_r_share"
  
  mydir <- "modules/SUA_bal_compilation_round2021"
  
  SETTINGS <- faoswsModules::ReadSettings(file.path(mydir, "sws.yml"))
  
  SetClientFiles(SETTINGS[["certdir"]])
  
  GetTestEnvironment(baseUrl = SETTINGS[["server"]], token = SETTINGS[["token"]])
}


COUNTRY <- as.character(swsContext.datasets[[1]]@dimensions$geographicAreaM49@keys)

COUNTRY_NAME <-
  nameData(
    "suafbs", "sua_unbalanced",
    data.table(geographicAreaM49 = COUNTRY))$geographicAreaM49_description

dbg_print("parameters")

USER <- regmatches(
  swsContext.username,
  regexpr("(?<=/).+$", swsContext.username, perl = TRUE)
)

#if (!file.exists(file.path(R_SWS_SHARE_PATH, USER))) {
#  dir.create(file.path(R_SWS_SHARE_PATH, USER))
#}


startYear <- as.numeric(swsContext.computationParams$startYear)
#startYear <- 2014
endYear <- as.numeric(swsContext.computationParams$endYear)
#endYear<- 2019
# 2000 rimane hard coded , 2019 in YEARS diventa endYear da input utente
YEARS <- as.character(2000:endYear)
#rimane hard coded
coeff_stock_year <- 2013
#diventa funzione dell ' input utente il reference year 2009:2013
reference_years <- (startYear-5):(startYear-1)
#focus interval è il principale input del'utente 2014-2019
focus_interval <- startYear:endYear
#NOTES for future complete parametrization. Where opening stock 2014 table is took in consideration, 2014 year is hard coded

Item_Parent <- as.character(swsContext.computationParams$Item_Parent)
#Item_Parent <- "02211"



# Always source files in R/ (useful for local runs).
# Your WD should be in faoswsStandardization/
sapply(dir("R", full.names = TRUE), source)


p <- defaultStandardizationParameters()

p$itemVar <- "measuredItemSuaFbs"
p$mergeKey[p$mergeKey == "measuredItemCPC"] <- "measuredItemSuaFbs"
p$elementVar <- "measuredElementSuaFbs"
p$childVar <- "measuredItemChildCPC"
p$parentVar <- "measuredItemParentCPC"
p$createIntermetiateFile <- "TRUE"
p$protected <- "Protected"
p$official <- "Official"

#to be corrected
shareDownUp_file <-
  file.path(R_SWS_SHARE_PATH, USER, paste0("shareDownUp_", COUNTRY, ".csv"))


tourist_cons_table <- ReadDatatable("keep_tourist_consumption")

stopifnot(nrow(tourist_cons_table) > 0)

TourismNoIndustrial <- tourist_cons_table[small == "X"]$tourist


dbg_print("define functions")

######### FUNCTIONS: at some point, they will be moved out of this file. ####

# Replacement for merge(x, y, by = VARS, all.x = TRUE) that do not set keys
# By default it behaves as dplyr::left_join(). If nomatch = 0, non-matching
# rows will not be returned
dt_left_join <- function(x, y, by = NA, allow.cartesian = FALSE,
                         nomatch = NA) {
  if (anyNA(by)) {
    stop("'by' is required")
  }
  
  if (any(!is.data.table(x), !is.data.table(y))) {
    stop("'x' and 'y' should be data.tables")
  }
  
  res <- y[x, on = by, allow.cartesian = allow.cartesian, nomatch = nomatch]
  
  setcolorder(res, c(names(x), setdiff(names(y), names(x))))
  
  res
}

dt_full_join <- function(x, y, by = NA) {
  if (anyNA(by)) {
    stop("'by' is required")
  }
  
  if (any(!is.data.table(x), !is.data.table(y))) {
    stop("'x' and 'y' should be data.tables")
  }
  
  res <- merge(x, y, by = by, all = TRUE)
  
  # merge sets the key to `by`
  setkey(res, NULL)
  
  res
}


coeffs_stocks_mod <- function(x) {
  tmp <- lm(data = x[timePointYears <= coeff_stock_year], supply_inc ~ supply_exc + trend)
  
  as.list(tmp$coefficients)
}

# This function will recalculate opening stocks from the first
# observation. Always.
update_opening_stocks <- function(x) {
  x <- x[order(geographicAreaM49, measuredItemSuaFbs, timePointYears)]
  
  groups <- unique(x[, c("geographicAreaM49", "measuredItemSuaFbs"), with = FALSE])
  
  res <- list()
  
  for (i in seq_len(nrow(groups))) {
    z <- x[groups[i], on = c("geographicAreaM49", "measuredItemSuaFbs")]
    if (nrow(z) > 1) {
      for (j in seq_len(nrow(z))[-1]) {
        # negative delta cannot be more than opening
        if (z$delta[j-1] < 0 & abs(z$delta[j-1]) > z$new_opening[j-1]) {
          z$delta[j-1] <- - z$new_opening[j-1]
        }
        z$new_opening[j] <- z$new_opening[j-1] + z$delta[j-1]
      }
      # negative delta cannot be more than opening
      if (z$delta[j] < 0 & abs(z$delta[j]) > z$new_opening[j]) {
        z$delta[j] <- - z$new_opening[j]
      }
    }
    res[[i]] <- z
  }
  rbindlist(res)
}

#send email function


send_mail <- function(from = NA, to = NA, subject = NA,
                      body = NA, remove = FALSE) {
  
  if (missing(from)) from <- 'no-reply@fao.org'
  
  if (missing(to)) {
    if (exists('swsContext.userEmail')) {
      to <- swsContext.userEmail
    }
  }
  
  if (is.null(to)) {
    stop('No valid email in `to` parameter.')
  }
  
  if (missing(subject)) stop('Missing `subject`.')
  
  if (missing(body)) stop('Missing `body`.')
  
  if (length(body) > 1) {
    body <-
      sapply(
        body,
        function(x) {
          if (file.exists(x)) {
            # https://en.wikipedia.org/wiki/Media_type 
            file_type <-
              switch(
                tolower(sub('.*\\.([^.]+)$', '\\1', basename(x))),
                txt  = 'text/plain',
                csv  = 'text/csv',
                png  = 'image/png',
                jpeg = 'image/jpeg',
                jpg  = 'image/jpeg',
                gif  = 'image/gif',
                xls  = 'application/vnd.ms-excel',
                xlsx = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                doc  = 'application/msword',
                docx = 'application/vnd.openxmlformats-officedocument.wordprocessingml.document',
                pdf  = 'application/pdf',
                zip  = 'application/zip',
                # https://stackoverflow.com/questions/24725593/mime-type-for-serialized-r-objects
                rds  = 'application/octet-stream'
              )
            
            if (is.null(file_type)) {
              stop(paste(tolower(sub('.*\\.([^.]+)$', '\\1', basename(x))),
                         'is not a supported file type.'))
            } else {
              res <- sendmailR:::.file_attachment(x, basename(x), type = file_type)
              
              if (remove == TRUE) {
                unlink(x)
              }
              
              return(res)
            }
          } else {
            return(x)
          }
        }
      )
  } else if (!is.character(body)) {
    stop('`body` should be either a string or a list.')
  }
  
  sendmailR::sendmail(from, to, subject, as.list(body))
}
# Fill NAs by LOCF/FOCB/interpolation if more than two
# non-missing observations are available, otherwhise just
# replicate the only non-missing observation
na.fill_ <- function(x) {
  if(sum(!is.na(x)) > 1) {
    zoo::na.fill(x, "extend")
  } else {
    rep(x[!is.na(x)], length(x))
  }
}



`%!in%` <- Negate(`%in%`)


# RemainingToProcessedParent() and RemainingProdChildToAssign() will
# be used in the derivation of shareDownUp

RemainingToProcessedParent <- function(data) {
  data[, 
       parent_already_processed :=
         ifelse(
           is.na(parent_qty_processed),
           parent_qty_processed,
           sum(processed_to_child / extractionRate, na.rm = TRUE)
         ),
       by = c("geographicAreaM49", "measuredItemParentCPC", "timePointYears")
       ]
  
  data[, remaining_processed_parent := round(parent_qty_processed - parent_already_processed)]
  
  data[remaining_processed_parent < 0, remaining_processed_parent := 0]
  
  data[,
       only_child_left :=
         sum(is.na(processed_to_child)) == 1 &
         is.na(processed_to_child) &
         !is.na(production_of_child) &
         !is.na(parent_qty_processed) &
         production_of_child > 0,
       by = c("geographicAreaM49", "measuredItemParentCPC", "timePointYears")
       ]
  
  data[
    only_child_left == TRUE,
    processed_to_child := remaining_processed_parent * extractionRate
    ]
  
  data[,
       parent_already_processed :=
         ifelse(
           is.na(parent_qty_processed),
           parent_qty_processed,
           sum(processed_to_child / extractionRate, na.rm = TRUE)
         ),
       by = c("geographicAreaM49", "measuredItemParentCPC", "timePointYears")
       ]
  
  data[, remaining_processed_parent := round(parent_qty_processed - parent_already_processed)]
  
  data[remaining_processed_parent < 0, remaining_processed_parent := 0]
  
  return(data)
}


RemainingProdChildToAssign <- function(data) {
  
  data[,
       available_processed_child := sum(processed_to_child, na.rm = TRUE),
       by = c("geographicAreaM49", "measuredItemChildCPC", "timePointYears")
       ]
  
  data[, remaining_to_process_child := round(production_of_child - available_processed_child)]
  
  data[remaining_to_process_child < 0, remaining_to_process_child := 0]
  
  data[,
       only_parent_left :=
         sum(is.na(processed_to_child)) == 1 &
         is.na(processed_to_child) &
         !is.na(parent_qty_processed) &
         parent_qty_processed >= 0
       ]
  
  data[only_parent_left==TRUE,processed_to_child:=ifelse(only_parent_left==TRUE,remaining_to_process_child,processed_to_child)]
  
  data[,
       available_processed_child := sum(processed_to_child, na.rm = TRUE),
       by = c("geographicAreaM49", "measuredItemChildCPC", "timePointYears")
       ]
  
  data[, remaining_to_process_child := round(production_of_child - available_processed_child)]
  
  data[remaining_to_process_child < 0, remaining_to_process_child := 0]
  
  return(data)
}


# The fmax function is used when fixing the processingShare of coproducts.
# If TRUE it means that "+" or "or" cases are involved.
fmax <- function(child, main, share, plusor = FALSE) {
  main <- unique(main)
  
  if (plusor) {
    found <- sum(sapply(child, function(x) grepl(x, main), USE.NAMES = FALSE))
    
    if (found == 0) {
      return(max(share, na.rm = TRUE))
    } else if (found == 1) {
      return(
        share[(1:length(child))[sapply(child, function(x) grepl(x, main), USE.NAMES = FALSE)]]
      )
    } else { # should be 2
      return(max(share[(1:length(child))[sapply(child, function(x) grepl(x, main), USE.NAMES = FALSE)]], na.rm = TRUE))
    }
  } else {
    if (sum(grepl(main, child)) > 0) {
      share[child == main]
    } else {
      max(share, na.rm = TRUE)
    }
  }
}

# Function that calculates imbalance as supply - utilizations (both calculated
# inside the function, dropped if keep_(supply|utilizations) set to FALSE).




# rollavg() is a rolling average function that uses computed averages
# to generate new values if there are missing values (and FOCB/LOCF).
# I.e.:
# vec <- c(NA, 2, 3, 2.5, 4, 3, NA, NA, NA)
#
#> RcppRoll::roll_mean(myvec, 3, fill = 'extend', align = 'right')
#[1]       NA       NA       NA 2.500000 3.166667 3.166667       NA       NA       NA 
#
#> rollavg(myvec)
#[1] 2.000000 2.000000 3.000000 2.500000 4.000000 3.000000 3.166667 3.388889 3.185185

rollavg <- function(x, order = 3) {
  # order should be > 2
  stopifnot(order >= 3)
  
  non_missing <- sum(!is.na(x))
  
  # For cases that have just two non-missing observations
  order <- ifelse(order > 2 & non_missing == 2, 2, order)
  
  if (non_missing == 1) {
    x[is.na(x)] <- na.omit(x)[1]
  } else if (non_missing >= order) {
    n <- 1
    while(any(is.na(x)) & n <= 10) { # 10 is max tries
      movav <- suppressWarnings(RcppRoll::roll_mean(x, order, fill = 'extend', align = 'right'))
      movav <- data.table::shift(movav)
      x[is.na(x)] <- movav[is.na(x)]
      n <- n + 1
    }
    
    x <- zoo::na.fill(x, 'extend')
  }
  
  return(x)
}



############################## / FUNCTIONS ##################################


dbg_print("end functions")



#####################################  TREE #################################

dbg_print("download tree")

#tree <- getCommodityTreeNewMethod(COUNTRY, YEARS)
#years range took with suerior margin with eccess (to not define years in back compilation)
tree <- getCommodityTreeNewMethod(COUNTRY,as.character(2000:2030))

stopifnot(nrow(tree) > 0)

tree <- tree[geographicAreaM49 %chin% COUNTRY]


# The `tree_exceptions` will npo be checked by validateTree()

# Exception: high share conmfirmed by official data
tree_exceptions <- tree[geographicAreaM49 == "392" & measuredItemParentCPC == "0141" & measuredItemChildCPC == "23995.01"]

if (nrow(tree_exceptions) > 0) {
  tree <- tree[!(geographicAreaM49 == "392" & measuredItemParentCPC == "0141" & measuredItemChildCPC == "23995.01")]
}

validateTree(tree)

if (nrow(tree_exceptions) > 0) {
  tree <- rbind(tree, tree_exceptions)
  rm(tree_exceptions)
}

## NA ExtractionRates are recorded in the sws dataset as 0
## for the standardization, we nee them to be treated as NA
## therefore here we are re-changing it

# TODO: Keep all protected 0 ERs
tree[Value == 0 & !(flagObservationStatus == "E" & flagMethod == "f"), Value := NA]

#proc_level_exceptions <- ReadDatatable("processing_level_exceptions")
#
#if (nrow(proc_level_exceptions) > 0) {
#  setnames(proc_level_exceptions, c("m49_code", "parent", "child"),
#           c("geographicAreaM49", "measuredItemParentCPC", "measuredItemChildCPC"))
#
#  tree <-
#    tree[!proc_level_exceptions[is.na(level)],
#         on = c("geographicAreaM49", "measuredItemParentCPC", "measuredItemChildCPC")]
#
#  proc_level_exceptions <- proc_level_exceptions[!is.na(level)]
#}

tree_to_send <- tree[is.na(Value) & measuredElementSuaFbs=="extractionRate"]

FILL_EXTRACTION_RATES = TRUE

if (FILL_EXTRACTION_RATES == TRUE) {
  
  expanded_tree <-
    merge(
      data.table(
        geographicAreaM49 = unique(tree$geographicAreaM49),
        timePointYears = as.character(sort(2000:(max(tree[, timePointYears]))))  
      ),
      unique(
        tree[,
             c("geographicAreaM49", "measuredElementSuaFbs",
               "measuredItemParentCPC", "measuredItemChildCPC"),
             with = FALSE
             ]
      ),
      by = "geographicAreaM49",
      all = TRUE,
      allow.cartesian = TRUE
    )
  
  tree <- tree[expanded_tree, on = colnames(expanded_tree)]
  
  # flags for carry forward/backward
  tree[is.na(Value), c("flagObservationStatus", "flagMethod") := list("E", "t")]
  
  tree <-
    tree[!is.na(Value)][
      tree,
      on = c("geographicAreaM49", "measuredElementSuaFbs",
             "measuredItemParentCPC", "measuredItemChildCPC",
             "timePointYears"),
      roll = -Inf
      ]
  
  tree <-
    tree[!is.na(Value)][
      tree,
      on = c("geographicAreaM49", "measuredElementSuaFbs",
             "measuredItemParentCPC", "measuredItemChildCPC",
             "timePointYears"),
      roll = Inf
      ]
  
  # keep orig flags
  tree[, flagObservationStatus := i.i.flagObservationStatus]
  tree[, flagMethod := i.i.flagMethod]
  
  tree[, names(tree)[grep("^i\\.", names(tree))] := NULL]
}


# XXX: connections removed here that should not exist in
# the commodity tree (so, they should be fixed there)
tree[
  #timePointYears >= 2014 &
  ((measuredItemParentCPC == "02211" & measuredItemChildCPC == "22212") |
     #cheese from whole cow milk cannot come from skim mulk of cow
     (measuredItemParentCPC == "22110.02" & measuredItemChildCPC == "22251.01")),
  `:=`(
    Value = NA,
    flagObservationStatus = "M",
    flagMethod = "n"
  )
  ]

#correction of milk tree for Czechia TO DO: generalize for the next round

tree[
  #timePointYears >= 2014 & 
  geographicAreaM49=="203" &
    
    #“whole milk powder”  from whole cow milk cannot come from skim mulk of cow
    ((measuredItemParentCPC == "22110.02" & measuredItemChildCPC == "22211") |
       #“whole milk condensed” from whole cow milk cannot come from skim mulk of cow
       (measuredItemParentCPC == "22110.02" & measuredItemChildCPC == "22222.01")),
  `:=`(
    Value = NA,
    flagObservationStatus = "M",
    flagMethod = "n"
  )
  ]


# saveRDS(
#   tree[
#     !is.na(Value) & measuredElementSuaFbs == "extractionRate",
#     -grepl("measuredElementSuaFbs", names(tree)),
#     with = FALSE
#     ],
#   file.path(R_SWS_SHARE_PATH, "FBSvalidation", COUNTRY, "tree.rds")
# )

tree_to_send <-
  tree_to_send %>% 
  dplyr::anti_join(
    tree[is.na(Value) & measuredElementSuaFbs == "extractionRate"],
    by = c("geographicAreaM49", "measuredElementSuaFbs",
           "measuredItemParentCPC", "measuredItemChildCPC", "timePointYears",
           "Value", "flagObservationStatus", "flagMethod")
  ) %>%
  dplyr::select(-Value) %>%
  dplyr::left_join(
    tree,
    by = c("geographicAreaM49", "measuredElementSuaFbs",
           "measuredItemParentCPC", "measuredItemChildCPC",
           "timePointYears", "flagObservationStatus", "flagMethod")
  ) %>%
  setDT()

tree_to_send <-
  tree_to_send[,
               c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemParentCPC",
                 "measuredItemChildCPC", "timePointYears", "Value",
                 "flagObservationStatus", "flagMethod"),
               with = FALSE
               ]

setnames(
  tree_to_send,
  c("measuredItemParentCPC", "measuredItemChildCPC"),
  c("measuredItemParentCPC_tree", "measuredItemChildCPC_tree")
)

tree_to_send <-
  nameData("suafbs", "ess_fbs_commodity_tree2", tree_to_send,
           except = c('measuredElementSuaFbs', 'timePointYears'))

tree_to_send[,
             `:=`(
               measuredItemParentCPC_tree = paste0("'", measuredItemParentCPC_tree),
               measuredItemChildCPC_tree = paste0("'", measuredItemChildCPC_tree))
             ]

#tmp_file_extr <- file.path(TMP_DIR, paste0("FILLED_ER_", COUNTRY, ".csv"))

#write.csv(tree_to_send, tmp_file_extr)

# XXX remove NAs
tree <- tree[!is.na(Value)]

uniqueLevels <- unique(tree[, c("geographicAreaM49", "timePointYears"), with = FALSE])

levels <- list()

treeLevels <- list()

for (i in seq_len(nrow(uniqueLevels))) {
  filter <- uniqueLevels[i, ]
  
  treeCurrent <- tree[filter, on = c("geographicAreaM49", "timePointYears")]
  
  levels <- findProcessingLevel(treeCurrent, "measuredItemParentCPC", "measuredItemChildCPC")
  
  setnames(levels, "temp", "measuredItemParentCPC")
  
  treeLevels[[i]] <- dt_left_join(treeCurrent, levels, by = "measuredItemParentCPC")
}

tree <- rbindlist(treeLevels)

tree[,
     processingLevel := max(processingLevel, na.rm = TRUE),
     by = c("geographicAreaM49", "timePointYears",
            "measuredElementSuaFbs", "measuredItemChildCPC")
     ]

# XXX Check if this one is still good or it can be obtained within the dataset
processed_item_datatable <- ReadDatatable("processed_item")
processedCPC <- processed_item_datatable[, measured_item_cpc]


# XXX what is this for?
itemMap <- GetCodeList(domain = "agriculture", dataset = "aproduction", "measuredItemCPC")
itemMap <- itemMap[, .(measuredItemSuaFbs = code, type)]

##################################### / TREE ################################


############################ POPULATION #####################################



############################ / POPULATION ##################################

############################################################### DATA #######


# 5510 Production[t]
# 5610 Import Quantity [t]
# 5071 Stock Variation [t]
# 5023 Export Quantity [t]
# 5910 Loss [t]
# 5016 Industrial uses [t]
# 5165 Feed [t]
# 5520 Seed [t]
# 5525 Tourist Consumption [t]
# 5164 Residual other uses [t]
# 5141 Food [t]
# 664 Food Supply (/capita/day) [Kcal]

elemKeys <- c("5510", "5610", "5071", "5113", "5023", "5910", "5016",
              "5165", "5520", "5525", "5164", "5166", "5141")

itemKeys <- GetCodeList(domain = "suafbs", dataset = "sua_unbalanced", "measuredItemFbsSua")
itemKeys <- itemKeys$code


# NOTE: the definition of "food_resid" changed (see inside newBalancing)
# (so, the tables below are not used anymore)

#food_classification_country_specific <-
#  ReadDatatable("food_classification_country_specific",
#                where = paste0("geographic_area_m49 IN ('", COUNTRY, "')"))
#
#food_classification_country_specific <-
#  food_classification_country_specific[geographic_area_m49 == COUNTRY]
#
#food_only_items <- food_classification_country_specific[food_classification == 'Food Residual', measured_item_cpc]


Utilization_Table <- ReadDatatable("utilization_table_2018")

stockable_items <- Utilization_Table[stock == 'X', cpc_code]

zeroWeight <- ReadDatatable("zero_weight")[, item_code]

flagValidTable <- ReadDatatable("valid_flags")


# We decided to unprotect E,e (replaced outliers). Done after they are
# replaced (i.e., if initially an Ee exists, it should remain; they need
# to be unprotected for balancing.
flagValidTable[flagObservationStatus == 'E' & flagMethod == 'e', Protected := FALSE]


# XXX: we need to unprotect I,c because it was being assigned
# to imputation of production of derived which is NOT protected.
# This will need to change in the next exercise.
flagValidTable[flagObservationStatus == 'I' & flagMethod == 'c', Protected := FALSE]


# Nutrients are:
# 1001 Calories
# 1003 Proteins
# 1005 Fats

if (CheckDebug()) {
  key <-
    DatasetKey(
      domain = "suafbs",
      dataset = "sua_unbalanced",
      dimensions =
        list(
          geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = COUNTRY),
          measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", keys = elemKeys),
          measuredItemFbsSua = Dimension(name = "measuredItemFbsSua", keys = itemKeys),
          timePointYears = Dimension(name = "timePointYears", keys = YEARS)
        )
    )
} else {
  key <- swsContext.datasets[[2]]
  
  key@dimensions$timePointYears@keys <- YEARS
  key@dimensions$measuredItemFbsSua@keys <- itemKeys
  key@dimensions$measuredElementSuaFbs@keys <- elemKeys
  key@dimensions$geographicAreaM49@keys <- COUNTRY
}

dbg_print("download data")


# LOAD
data <- GetData(key)

#if (unique(data[, geographicAreaM49]) != COUNTRY) { stop("Invalid Unbalance dataset")}
# Remove item that is not par of agriculture domain
data <- data[measuredItemFbsSua != "F1223"]

if (Item_Parent %!in% unique(data[, measuredItemFbsSua]) & Item_Parent %!in% unique(tree[, measuredItemParentCPC])) {
  #print(paste0("Not valid Parent Item code"))
  send_mail(
    from = "do-not-reply@fao.org",
    to = swsContext.userEmail,
    subject = "Not valid Parent Item code",
    body = c(paste("The parent item inserted is not correct"))
  )
  stop("Not valid Parent Item code")
}

dbg_print("convert sugar")

##############################################################
######### SUGAR RAW CODES TO BE CONVERTED IN 2351F ###########
##############################################################
data <- convertSugarCodes_new(data)

#################### FODDER CROPS ##########################################

# TODO: create SWS datatable for this.
# Some of these items may be missing in reference files,
# thus we carry forward the last observation.
fodder_crops_items <-
  tibble::tribble(
    ~description, ~code,
    "Maize for forage", "01911",
    "Alfalfa for forage", "01912",
    "Sorghum for forage", "01919.01",
    "Rye grass for forage", "01919.02",
    "Other grasses for forage", "01919.91",
    "Clover for forage", "01919.03",
    "Other oilseeds for forage", "01919.94",
    "Other legumes for  forage", "01919.92",
    "Cabbage for fodder", "01919.04",
    "Mixed grass and legumes for forage", "01919.93",
    "Turnips for fodder", "01919.05",
    "Beets for fodder", "01919.06",
    "Carrots for fodder", "01919.07",
    "Swedes for fodder", "01919.08",
    "Other forage products, nes", "01919.96",
    "Other forage crops, nes", "01919.95",
    "Hay for forage, from legumes", "01919.10",
    "Hay for forage, from grasses", "01919.09",
    "Hay for forage, from other crops nes", "01919.11"
  )


fodder_crops_availab <-
  data[
    measuredItemFbsSua %chin% fodder_crops_items$code &
      measuredElementSuaFbs == "5510"
    ]

if (nrow(fodder_crops_availab) > 0) {
  
  fodder_crops_complete <-
    CJ(
      geographicAreaM49 = unique(fodder_crops_availab$geographicAreaM49),
      measuredElementSuaFbs = "5510",
      timePointYears = unique(data$timePointYears),
      measuredItemFbsSua = unique(fodder_crops_availab$measuredItemFbsSua)
    )
  
  fodder_crops_complete <-
    fodder_crops_complete[order(geographicAreaM49, measuredItemFbsSua, timePointYears)]
  
  fodder_crops <-
    dt_left_join(
      fodder_crops_complete,
      fodder_crops_availab[, .(geographicAreaM49, measuredElementSuaFbs,
                               measuredItemFbsSua, timePointYears, Value)],
      by = c("geographicAreaM49", "measuredElementSuaFbs",
             "measuredItemFbsSua", "timePointYears")
    )
  
  fodder_crops[,
               Value := zoo::na.locf(Value),
               by = c("geographicAreaM49", "measuredItemFbsSua")
               ]
  
  fodder_crops_new <-
    fodder_crops[
      !fodder_crops_availab,
      on = c("geographicAreaM49", "measuredElementSuaFbs",
             "measuredItemFbsSua", "timePointYears")
      ]
  
  
  if (nrow(fodder_crops_new) > 0) {
    
    fodder_crops_new[, `:=`(flagObservationStatus = "E", flagMethod = "t")]
    
    data <- rbind(data, fodder_crops_new)
    
  }
}

#################### / FODDER CROPS ########################################


opening_stocks_2014 <-
  ReadDatatable(
    "opening_stocks_2014",
    where = paste0("m49_code IN (",
                   paste(shQuote(COUNTRY, type = "sh"), collapse = ", "), ")")
  )

# The procedure works even if no previous opening stocks  exists, but in that
# case there is no way to know if data does not exist because there was an
# issue downloading data or because it actually does not exist. Luckily there
# are many calls to SWS, which should fail if there are issues: it is unlikely
# that only this one fails.
#stopifnot(nrow(opening_stocks_2014) > 0)

non_null_prev_deltas <-
  unique(
    data[
      measuredElementSuaFbs == "5071" & timePointYears %in% reference_years
      ][,
        .SD[sum(!dplyr::near(Value, 0)) > 0],
        by = c("geographicAreaM49", "measuredItemFbsSua")
        ][,
          .(geographicAreaM49, measuredItemFbsSua)
          ]
  )

opening_stocks_2014[opening_stocks < 0, opening_stocks := 0]

opening_stocks_2014 <-
  opening_stocks_2014[
    m49_code %in% COUNTRY,
    .(
      geographicAreaM49 = m49_code,
      measuredItemFbsSua = cpc_code,
      timePointYears = "2014",
      Value_cumulated = opening_stocks
    )
    ]

# Keep only those for which recent variations are available
opening_stocks_2014 <-
  opening_stocks_2014[
    non_null_prev_deltas,
    on = c("geographicAreaM49", "measuredItemFbsSua"),
    nomatch = 0
    ]

original_opening_stocks <- data[measuredElementSuaFbs == "5113"]

original_opening_stocks <-
  flagValidTable[
    original_opening_stocks,
    on = c("flagObservationStatus", "flagMethod")
    ][,
      Valid := NULL
      ]


# Remove protected in 2014 from cumulated
opening_stocks_2014 <-
  opening_stocks_2014[
    !original_opening_stocks[
      timePointYears == "2014" & Protected == TRUE,
      .(geographicAreaM49, measuredItemFbsSua)
      ],
    on = c("geographicAreaM49", "measuredItemFbsSua")
    ]


all_opening_stocks <-
  merge(
    original_opening_stocks,
    opening_stocks_2014,
    by = c("geographicAreaM49", "measuredItemFbsSua", "timePointYears"),
    all = TRUE
  )


all_opening_stocks[
  !Protected %in% TRUE & is.na(Value) & !is.na(Value_cumulated),
  `:=`(
    Value = Value_cumulated,
    flagObservationStatus = "I",
    flagMethod = "-",
    # We protect these, in any case, because they should not
    # be overwritten, even if not (semi) official or expert
    Protected = TRUE,
    measuredElementSuaFbs = "5113",
    timePointYears = "2014"
  )
  ][,
    Value_cumulated := NULL
    ]


# Now, for all remaining stockable items, we create opening
# stocks in 2014 as 20% of supply in 2013

remaining_opening_stocks <-
  data[
    timePointYears == "2013" &
      measuredItemFbsSua %chin%
      setdiff(
        stockable_items,
        all_opening_stocks$measuredItemFbsSua
      )
    ][,
      .(
        opening_20 =
          sum(
            Value[measuredElementSuaFbs %chin% c("5510", "5610")],
            - Value[measuredElementSuaFbs == "5910"],
            na.rm = TRUE
          ) * 0.2,
        timePointYears = "2014"
      ),
      by = c("geographicAreaM49", "measuredItemFbsSua")
      ]

remaining_opening_stocks[opening_20 < 0, opening_20 := 0]

all_opening_stocks <-
  merge(
    all_opening_stocks,
    remaining_opening_stocks,
    by = c("geographicAreaM49", "measuredItemFbsSua", "timePointYears"),
    all = TRUE
  )

all_opening_stocks <- all_opening_stocks[!is.na(timePointYears)]

all_opening_stocks[
  !Protected %in% TRUE & is.na(Value) & !is.na(opening_20),
  `:=`(
    Value = opening_20,
    flagObservationStatus = "I",
    flagMethod = "i",
    # We protect these, in any case, because they should not
    # be overwritten, even if not (semi) official or expert
    Protected = TRUE
  )
  ][,
    opening_20 := NULL
    ]


complete_all_opening <-
  CJ(
    geographicAreaM49 = unique(all_opening_stocks$geographicAreaM49),
    timePointYears = as.character(min(all_opening_stocks$timePointYears):as.numeric(tail(YEARS,1))),
    measuredItemFbsSua = unique(all_opening_stocks$measuredItemFbsSua)
  )


all_opening_stocks <-
  merge(
    complete_all_opening,
    all_opening_stocks,
    by = c("geographicAreaM49", "measuredItemFbsSua", "timePointYears"),
    all = TRUE
  )

all_opening_stocks[, orig_val := Value]

all_opening_stocks <-
  all_opening_stocks[!is.na(Value)][
    all_opening_stocks,
    on = c("geographicAreaM49", "measuredItemFbsSua", "timePointYears"),
    roll = Inf]

all_opening_stocks[
  is.na(orig_val) & !is.na(Value),
  `:=`(
    Protected = FALSE,
    flagObservationStatus = "E",
    flagMethod = "t"
  )
  ]

all_opening_stocks[, measuredElementSuaFbs := "5113"]

all_opening_stocks[, orig_val := NULL]

all_opening_stocks[, names(all_opening_stocks)[grep("^i\\.", names(all_opening_stocks))] := NULL]

all_opening_stocks <- all_opening_stocks[!is.na(timePointYears)]



# Generate stocks variations for items for which opening
# exists for ALL years and variations don't exist

opening_avail_all_years <-
  all_opening_stocks[
    !is.na(Value) & timePointYears >= focus_interval[1],
    .SD[.N == (tail(focus_interval,1) - focus_interval[1] + 1)],
    measuredItemFbsSua
    ]

delta_avail_for_open_all_years <-
  data[
    measuredElementSuaFbs == "5071" &
      timePointYears >= focus_interval[1] &
      measuredItemFbsSua %chin% opening_avail_all_years$measuredItemFbsSua,
    .SD[.N == (tail(focus_interval,1) - focus_interval[1] + 1)],
    measuredItemFbsSua
    ]

to_generate_by_ident <-
  setdiff(
    opening_avail_all_years$measuredItemFbsSua,
    delta_avail_for_open_all_years$measuredItemFbsSua
  )

data_rm_ident <- data[measuredElementSuaFbs == "5071" & measuredItemFbsSua %chin% to_generate_by_ident]

if (length(to_generate_by_ident) > 0) {
  
  opening_avail_all_years <-
    opening_avail_all_years[measuredItemFbsSua %chin% to_generate_by_ident]
  
  opening_avail_all_years[, Protected := NULL]
  
  next_opening_key <-
    DatasetKey(
      domain = "Stock",
      dataset = "stocksdata",
      dimensions =
        list(
          geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = COUNTRY),
          measuredElementSuaFbs = Dimension(name = "measuredElement", keys = "5113"),
          measuredItemFbsSua = Dimension(name = "measuredItemCPC", keys = unique(opening_avail_all_years$measuredItemFbsSua)),
          timePointYears = Dimension(name = "timePointYears", keys = as.character(as.numeric(max(opening_avail_all_years$timePointYears)) + 1))
        )
    )
  
  next_opening <- GetData(next_opening_key)
  
  setnames(
    next_opening,
    c("measuredItemCPC", "measuredElement"),
    c("measuredItemFbsSua", "measuredElementSuaFbs")
  )
  
  opening_avail_all_years <-
    rbind(opening_avail_all_years, next_opening)
  
  opening_avail_all_years <-
    opening_avail_all_years[
      order(geographicAreaM49, measuredItemFbsSua, timePointYears)
      ]
  
  opening_avail_all_years[,
                          delta := shift(Value, type = "lead") - Value,
                          by = c("geographicAreaM49", "measuredItemFbsSua")
                          ]
  
  opening_avail_all_years[,
                          delta :=
                            ifelse(
                              timePointYears == max(timePointYears) & is.na(delta),
                              0,
                              delta
                            ),
                          by = c("geographicAreaM49", "measuredItemFbsSua")
                          ]
  
  opening_avail_all_years <-
    dt_left_join(
      opening_avail_all_years,
      flagValidTable,
      by = c("flagObservationStatus", "flagMethod")
    )
  
  opening_avail_all_years[shift(Protected, type = "lead") == TRUE & Protected == TRUE, `:=`(delta_flag_obs = "T", delta_flag_method = "p")]
  opening_avail_all_years[!(shift(Protected, type = "lead") == TRUE & Protected == TRUE), `:=`(delta_flag_obs = "I", delta_flag_method = "i")]
  
  delta_identity <-
    opening_avail_all_years[
      timePointYears %in% focus_interval[1]:tail(focus_interval,1),
      .(
        geographicAreaM49,
        measuredItemFbsSua,
        timePointYears,
        measuredElementSuaFbs = "5071",
        Value = delta,
        flagObservationStatus = delta_flag_obs,
        flagMethod = delta_flag_method
      )
      ]
  
  delta_identity <-
    delta_identity[!data_rm_ident,
                   on = c("geographicAreaM49", "measuredItemFbsSua", "timePointYears")]
  
  data <- rbind(data, delta_identity)
  
  data <-
    data[order(geographicAreaM49, measuredItemFbsSua,
               timePointYears, measuredElementSuaFbs)]
  
}

# / Generate stocks variations for items for which opening
# / exists for ALL years and variations don't exist



# Recalculate opening stocks

data_for_opening <-
  dt_left_join(
    all_opening_stocks[,
                       .(geographicAreaM49, measuredItemSuaFbs = measuredItemFbsSua,
                         timePointYears, new_opening = Value)
                       ],
    data[
      measuredElementSuaFbs == '5071' &
        timePointYears %in% focus_interval[1]:tail(focus_interval,1) &
        !is.na(Value),
      .(geographicAreaM49, measuredItemSuaFbs = measuredItemFbsSua,
        timePointYears, delta = Value)
      ],
    by = c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears")
  )

data_for_opening[is.na(delta), delta := 0]

data_for_opening <- data_for_opening[timePointYears >= focus_interval[1]]

data_for_opening <- update_opening_stocks(data_for_opening)

all_opening_stocks <-
  dt_left_join(
    all_opening_stocks,
    data_for_opening[,
                     .(
                       geographicAreaM49,
                       measuredItemFbsSua = measuredItemSuaFbs,
                       timePointYears,
                       new_opening
                     )
                     ],
    by = c("geographicAreaM49", "measuredItemFbsSua", "timePointYears")
  )

all_opening_stocks[
  !is.na(new_opening) & (round(new_opening) != round(Value) | is.na(Value)),
  `:=`(
    Value = new_opening,
    flagObservationStatus = "E",
    flagMethod = "u",
    Protected = FALSE
  )
  ]

all_opening_stocks[, new_opening := NULL]

# / Recalculate opening stocks



data <-
  dt_left_join(data, flagValidTable,
               by = c("flagObservationStatus", "flagMethod"))

data[flagObservationStatus %chin% c("", "T"), `:=`(Official = TRUE, Protected = TRUE)]
data[is.na(Official), Official := FALSE]
data[is.na(Protected), Protected := FALSE]


# We remove "5113" (opening stocks) as will be stored in separate table.
data <- data[measuredElementSuaFbs != "5113"]

dbg_print("elementToCodeNames")

# XXX FIXME: the elementCodesToNames below is
# not working proberply, see issue #38
codes <- as.data.table(tibble::tribble(
  ~measuredElementSuaFbs,  ~name,
  "5910", "exports",
  "5520", "feed",
  "5141", "food",
  "5023", "foodManufacturing",
  "5610", "imports",
  "5165", "industrial",
  "5016", "loss",
  "5510", "production",
  "5525", "seed",
  "5164", "tourist",
  "5071", "stockChange"
))

data <- dt_left_join(data, codes, by = "measuredElementSuaFbs")

data[, measuredElementSuaFbs := name]

data[, name := NULL]

# NOTE: if this should be used again (#38), camelCase the element names (#45)
#data <-
#  elementCodesToNames(
#    data,
#    itemCol = "measuredItemFbsSua",
#    elementCol = "measuredElementSuaFbs"
#  )


# XXX: there are some NAs here, but probably there shouldn't
data <- data[!is.na(measuredElementSuaFbs)]

setnames(data, "measuredItemFbsSua", "measuredItemSuaFbs")


########## Remove feed if new element and negative imbalance is huge

# Feed requires country-specific reference files, which are being updated.
# Until these get reviewed, the feed module will generate feed items in
# some countries, where it is not required/needed/appropriate. Below, we
# remove the NEW feed item if the NEGATIVE imbalance obtained by including
# it is more than 50% of supply (e.g., -72%).
##### calcuate Imbalance function #####
calculateImbalance <- function(data,
                               supply_add = c("production", "imports"),
                               supply_subtract = c("exports", "stockChange"),
                               supply_all = union(supply_add, supply_subtract),
                               item_name = "measuredItemSuaFbs",
                               bygroup = c("geographicAreaM49", "timePointYears", item_name),
                               keep_supply = TRUE,
                               keep_utilizations = TRUE) {
  
  stopifnot(is.data.table(data))
  
  data[,
       `:=`(
         supply =
           sum(Value[measuredElementSuaFbs %chin% supply_add],
               - Value[measuredElementSuaFbs %chin% supply_subtract],
               na.rm = TRUE),
         # All elements that are NOT supply elements
         utilizations =
           sum(Value[!(measuredElementSuaFbs %chin% supply_all)],
               na.rm = TRUE)
       ),
       by = bygroup
       ][,
         imbalance := supply - utilizations
         ]
  
  if (keep_supply == FALSE) {
    data[, supply := NULL]
  }
  
  if (keep_utilizations == FALSE) {
    data[, utilizations := NULL]
  }
  
}

outside <- function(x, lower = NA, upper = NA) {
  x < lower | x > upper
}


#######
new_feed <-
  data[
    measuredElementSuaFbs == "feed",
    .(
      pre = sum(!is.na(Value[timePointYears <= focus_interval[1]-1])),
      post = sum(!is.na(Value[timePointYears >= focus_interval[1]]))
    ),
    by = c("geographicAreaM49", "measuredItemSuaFbs")
    ][
      pre == 0 & post > 0
      ][,
        c("pre", "post") := NULL
        ]

msg_new_feed_remove <- "No NEW feed removed"
msg_new_feed_dubious <- "No other NEW items should be removed"

if (nrow(new_feed) > 0) {
  
  prev_data_avg <-
    data[
      new_feed, on = c("geographicAreaM49", "measuredItemSuaFbs")
      ][
        timePointYears >= focus_interval[1]-4 & timePointYears <= focus_interval[1]-1
        ][
          order(geographicAreaM49, measuredItemSuaFbs, measuredElementSuaFbs, timePointYears),
          Value := na.fill_(Value),
          by = c("geographicAreaM49", "measuredItemSuaFbs", "measuredElementSuaFbs")
          ][,
            .(Value = sum(Value) / sum(!is.na(Value)), timePointYears = 0),
            by = c("geographicAreaM49", "measuredItemSuaFbs", "measuredElementSuaFbs")
            ]
  
  calculateImbalance(prev_data_avg, supply_subtract = c("exports", "stockChange"))
  
  prev_processed_avg <-
    prev_data_avg[
      supply > 0 &
        utilizations > 0 &
        measuredElementSuaFbs == "foodManufacturing" &
        !is.na(Value),
      .(geographicAreaM49, measuredItemSuaFbs, proc_ratio = Value / supply)
      ]
  
  
  new_data_avg <-
    data[
      new_feed,
      on = c("geographicAreaM49", "measuredItemSuaFbs")
      ][
        measuredElementSuaFbs != "foodManufacturing" & timePointYears >= focus_interval[1]
        ][
          order(geographicAreaM49, measuredItemSuaFbs, measuredElementSuaFbs, timePointYears),
          Value := na.fill_(Value),
          by = c("geographicAreaM49", "measuredItemSuaFbs", "measuredElementSuaFbs")
          ][,
            .(Value = sum(Value) / sum(!is.na(Value) & !dplyr::near(Value, 0)), timePointYears = 1),
            by = c("geographicAreaM49", "measuredItemSuaFbs", "measuredElementSuaFbs")
            ][
              !is.nan(Value)
              ]
  
  calculateImbalance(
    new_data_avg,
    keep_utilizations = FALSE,
    supply_subtract = c("exports", "stockChange")
  )
  
  new_data_supply <-
    unique(
      new_data_avg[,
                   c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears", "supply"),
                   with = FALSE
                   ]
    )
  
  new_data_proc <-
    dt_left_join(
      new_data_supply,
      prev_processed_avg,
      by = c("geographicAreaM49", "measuredItemSuaFbs"),
      nomatch = 0
    )
  
  new_data_proc <-
    new_data_proc[
      supply > 0,
      .(geographicAreaM49, measuredItemSuaFbs,
        measuredElementSuaFbs = "foodManufacturing",
        Value = supply * proc_ratio, timePointYears = 1)
      ]
  
  new_data_avg[, c("supply", "imbalance") := NULL]
  
  new_data_avg <-
    rbind(
      new_data_avg,
      new_data_proc
    )
  
  calculateImbalance(new_data_avg, supply_subtract = c("exports", "stockChange"))
  
  
  new_feed_to_remove <-
    unique(
      new_data_avg[
        imbalance / supply <= -0.3,
        c("geographicAreaM49", "measuredItemSuaFbs"),
        with = FALSE
        ]
    )
  
  new_feed_to_remove[, measuredElementSuaFbs := "feed"]
  new_feed_to_remove[, remove_feed := TRUE]
  
  new_feed_dubious <-
    unique(
      new_data_avg[
        imbalance / supply > -0.3 & imbalance / supply <= -0.05,
        .(geographicAreaM49, measuredItemSuaFbs, x = imbalance / supply)
        ][order(x)][, x := NULL]
    )
  
  data <-
    merge(
      data,
      new_feed_to_remove,
      by = c("geographicAreaM49", "measuredItemSuaFbs", "measuredElementSuaFbs"),
      all.x = TRUE
    )
  
  feed_to_remove <-
    data[
      remove_feed == TRUE & Protected == FALSE
      ][,
        .(geographicAreaM49, measuredItemSuaFbs, measuredElementSuaFbs, timePointYears)
        ]
  
  data[, remove_feed := NULL]
  
  data <- data[!feed_to_remove, on = names(feed_to_remove)]
  
  if (nrow(feed_to_remove) > 0) {
    msg_new_feed_remove <- paste(new_feed_to_remove$measuredItemSuaFbs, collapse = ", ")
  }
  
  if (nrow(new_feed_dubious) > 0) {
    msg_new_feed_dubious <- paste(new_feed_dubious$measuredItemSuaFbs, collapse = ", ")
  }
}


########## / Remove feed if new element and negative imbalance is huge


treeRestricted <-
  tree[,
       c("measuredItemParentCPC", "measuredItemChildCPC", "processingLevel"),
       with = FALSE
       ]

treeRestricted <- unique(treeRestricted[order(measuredItemChildCPC)])

primaryInvolved <- faoswsProduction::getPrimary(processedCPC, treeRestricted, p)

dbg_print("primary involved descendents")

# XXX: check here, 0111 is in results
primaryInvolvedDescendents <-
  getChildren(
    commodityTree = treeRestricted,
    parentColname = "measuredItemParentCPC",
    childColname = "measuredItemChildCPC",
    topNodes = primaryInvolved
  )


# stocks need to be generated for those items for
# which "opening stocks" are available

items_to_generate_stocks <-
  unique(all_opening_stocks$measuredItemFbsSua)

stock <-
  CJ(
    measuredItemSuaFbs    = items_to_generate_stocks,
    measuredElementSuaFbs = 'stockChange',
    geographicAreaM49     = unique(data$geographicAreaM49),
    timePointYears        = unique(data$timePointYears)
  )

# rbind with anti_join
data <-
  rbind(
    data,
    stock[!data, on = c('measuredItemSuaFbs', 'measuredElementSuaFbs',
                        'geographicAreaM49', 'timePointYears')],
    fill = TRUE
  )

# XXX what is primaryInvolvedDescendents ?????????
#deriv <- CJ(measuredItemSuaFbs = primaryInvolvedDescendents, measuredElementSuaFbs = 'production', geographicAreaM49 = unique(data$geographicAreaM49), timePointYears = unique(data$timePointYears))
deriv <-
  CJ(
    measuredItemSuaFbs    = unique(tree$measuredItemChildCPC),
    measuredElementSuaFbs = 'production',
    geographicAreaM49     = unique(data$geographicAreaM49),
    timePointYears        = unique(data$timePointYears)
  )

# rbind with anti_join
data <-
  rbind(
    data,
    deriv[!data, on = c('measuredItemSuaFbs', 'measuredElementSuaFbs',
                        'geographicAreaM49', 'timePointYears')],
    fill = TRUE
  )



data[is.na(Official), Official := FALSE]
data[is.na(Protected), Protected := FALSE]



data[, stockable := measuredItemSuaFbs %chin% stockable_items]


# XXX: Remove items for which no production, imports or exports exist after 2013.
non_existing_for_imputation <-
  setdiff(
    data[
      measuredElementSuaFbs %chin% c('production', 'imports', 'exports') &
        timePointYears <= focus_interval[1]-1,
      unique(measuredItemSuaFbs)
      ],
    data[
      measuredElementSuaFbs %chin% c('production', 'imports', 'exports') &
        timePointYears >= focus_interval[1],
      unique(measuredItemSuaFbs)
      ]
  )

data <- data[!(measuredItemSuaFbs %chin% non_existing_for_imputation)]


############################################### Create derived production ###################################

dbg_print("derivation of shareDownUp")

############# ShareDownUp ----------------------------------------

#this table will be used to assign to zeroweight comodities 
#the processed quantities of their coproduct
coproduct_table <- ReadDatatable('zeroweight_coproducts')

stopifnot(nrow(coproduct_table) > 0)

# Can't do anything if this information if missing, so remove these cases
coproduct_table <- coproduct_table[!is.na(branch)]

### XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
### XXX removing these cases as '22242.01' appears as zero-weight XXXX
### XXX for main product = '22110.04'                             XXXX
### XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
coproduct_table <- coproduct_table[branch != '22242.01 + 22110.04']

coproduct_table <- coproduct_table[, .(measured_item_child_cpc, branch)]

coproduct_for_sharedownup <- copy(coproduct_table)

coproduct_for_sharedownup_easy <- coproduct_for_sharedownup[!grepl('\\+|or', branch)]

coproduct_for_sharedownup_plus <- coproduct_for_sharedownup[grepl('\\+', branch)]

coproduct_for_sharedownup_plus <-
  rbind(
    tidyr::separate(
      coproduct_for_sharedownup_plus,
      branch,
      into = c('main1', 'main2'),
      remove = FALSE,
      sep = ' *\\+ *')[, .(measured_item_child_cpc, branch= main1)],
    tidyr::separate(
      coproduct_for_sharedownup_plus,
      branch,
      into = c('main1', 'main2'),
      remove = FALSE,
      sep = ' *\\+ *')[, .(measured_item_child_cpc,branch = main2)]
  )

coproduct_for_sharedownup_plus <- unique(coproduct_for_sharedownup_plus)


coproduct_for_sharedownup_or <- coproduct_for_sharedownup[grepl('or', branch)]

coproduct_for_sharedownup_or <-
  rbind(
    #coproduct_table_or,
    tidyr::separate(
      coproduct_for_sharedownup_or,
      branch,
      into = c('main1', 'main2'),
      remove = FALSE,
      sep = ' *or *')[, .(measured_item_child_cpc, branch= main1)],
    tidyr::separate(
      coproduct_for_sharedownup_or,
      branch,
      into = c('main1', 'main2'),
      remove = FALSE,
      sep = ' *or *')[, .(measured_item_child_cpc,branch = main2)]
  )

coproduct_for_sharedownup_or <- unique(coproduct_for_sharedownup_or)

coproduct_for_sharedownup <-
  rbind(
    coproduct_for_sharedownup_easy,
    coproduct_for_sharedownup_plus,
    coproduct_for_sharedownup_or
  )


#Using the whole tree not by level
ExtrRate <-
  tree[
    !is.na(Value) &
      measuredElementSuaFbs == 'extractionRate'
    ][,
      .(
        measuredItemParentCPC,
        geographicAreaM49,
        measuredItemChildCPC,
        timePointYears,
        extractionRate = Value,
        processingLevel
      )
      ]

# We include utilizations to identify if proceseed if the only utilization
data_tree <-
  data[
    measuredElementSuaFbs %chin%
      c('production', 'imports', 'exports',
        'stockChange', 'foodManufacturing', 'loss',
        'food', 'industrial', 'feed', 'seed')
    ]


#subset the tree accordingly to parents and child present in the SUA data

ExtrRate <-
  ExtrRate[
    measuredItemChildCPC %chin% data_tree$measuredItemSuaFbs &
      measuredItemParentCPC %chin% data_tree$measuredItemSuaFbs
    ]


setnames(data_tree, "measuredItemSuaFbs", "measuredItemParentCPC")

dataProcessingShare<-copy(data_tree)

data_tree <-
  merge(
    data_tree,
    ExtrRate,
    by = c(p$parentVar, p$geoVar, p$yearVar),
    allow.cartesian = TRUE,
    all.y = TRUE
  )

data_tree <- as.data.table(data_tree)

# the availability for parent that have one child and only processed as utilization will 
# be entirely assigned to processed for that its unique child even for 2014 onwards
data_tree[,
          availability :=
            sum(
              Value[get(p$elementVar) %in% c(p$productionCode, p$importCode)],
              - Value[get(p$elementVar) %in% c(p$exportCode, "stockChange")],
              na.rm = TRUE
            ),
          by = c(p$geoVar, p$yearVar, p$parentVar, p$childVar)
          ]

# used to chack if a parent has processed as utilization
data_tree[,
          proc_Median :=
            median(
              Value[measuredElementSuaFbs == "foodManufacturing" & timePointYears %in% as.numeric(YEARS)],
              na.rm=TRUE
            ),
          by = c(p$parentVar, p$geoVar)
          ]

# boolean variable taking TRUE if the parent has only processed as utilization 
data_tree[,
          unique_proc :=
            proc_Median > 0 &
            !is.na(proc_Median) &
            # ... is the only utilization
            all(is.na(Value[!(measuredElementSuaFbs %chin%
                                c('production', 'imports', 'exports',
                                  'stockChange','foodManufacturing'))])),
          by = c(p$parentVar, p$geoVar, p$yearVar)
          ]

sel_vars <-
  c("measuredItemParentCPC", "geographicAreaM49", "timePointYears",
    "measuredElementSuaFbs", "flagObservationStatus", "flagMethod",
    "Value", "Official", "measuredItemChildCPC", "extractionRate",
    "processingLevel")

data_tree<-
  unique(
    data_tree[, c(sel_vars, "availability", "unique_proc"), with = FALSE],
    by = sel_vars
  )


# dataset to calculate the number of parent of each child and the number of children of each parent
# including zeroweight commodities
sel_vars <- c("geographicAreaM49", "measuredItemParentCPC",
              "measuredItemChildCPC","timePointYears")
data_count <- unique(data_tree[, sel_vars, with = FALSE], by = sel_vars)

#Caculate the number of parent of each child
data_count[,
           number_of_parent := .N,
           by = c("geographicAreaM49", "measuredItemChildCPC", "timePointYears")
           ]

# calculate the number of children of each parent
# we exclude zeroweight to avoid doublecounting of children (processing)
data_count[measuredItemChildCPC %!in% zeroWeight,
           number_of_children := uniqueN(measuredItemChildCPC),
           by = c("geographicAreaM49", "measuredItemParentCPC", "timePointYears")
           ]

data_tree <-
  dt_left_join(
    data_tree,
    data_count,
    by = c(p$parentVar, p$childVar, p$geoVar, p$yearVar),
    allow.cartesian = TRUE
  )

# dataset containing the processed quantity of parents
food_proc <-
  unique(
    data_tree[
      measuredElementSuaFbs == "foodManufacturing",
      c("geographicAreaM49", "measuredItemParentCPC", "timePointYears", "Value"),
      with = FALSE
      ],
    by = c("geographicAreaM49", "measuredItemParentCPC", "timePointYears", "Value")
  )

setnames(food_proc, "Value", "parent_qty_processed")

# avoid recaculation of shareDownUp from 2014 onwards
food_proc[timePointYears > focus_interval[1]-1, parent_qty_processed := NA_real_]

data_tree <-
  dt_left_join(
    data_tree,
    food_proc,
    by = c(p$parentVar, p$geoVar, p$yearVar),
    allow.cartesian = TRUE
  )

sel_vars <- 
  c("geographicAreaM49", "measuredItemParentCPC", "measuredItemChildCPC",
    "timePointYears", "extractionRate", "parent_qty_processed",
    "processingLevel", "number_of_parent", "number_of_children")

data_tree <-
  unique(
    data_tree[, c(sel_vars, "availability", "unique_proc"), with = FALSE],
    by = sel_vars
  )


# dataset containing the production of child commodities
dataprodchild <- data[measuredElementSuaFbs %chin% c('production')]

setnames(dataprodchild, "measuredItemSuaFbs", "measuredItemChildCPC")

dataprodchild <-
  unique(
    dataprodchild[,
                  c("geographicAreaM49", "measuredItemChildCPC",
                    "timePointYears", "Value", "flagObservationStatus",
                    "flagMethod"),
                  with = FALSE
                  ]
  )

setnames(dataprodchild, "Value", "production_of_child")


# to avoid resestimation based on estimated data (processed and production of child) from 2014 onwards
dataprodchild[timePointYears > focus_interval[1]-1, production_of_child := NA_real_]

data_tree <-
  dt_left_join(
    data_tree,
    dataprodchild,
    by = c(p$geoVar, p$childVar, p$yearVar)
  )

# ShareDownups for zeroweights are calculated  separately
# to avoid double counting when agregating processed quantities of parent

# dataset containing informations of zeroweight commodities
data_zeroweight <- data_tree[measuredItemChildCPC %chin% zeroWeight]

# import data for coproduct relation
zw_coproduct <-
  coproduct_for_sharedownup[,
                            .(zeroweight = measured_item_child_cpc, measuredItemChildCPC = branch)
                            ]

zw_coproduct <- unique(zw_coproduct, by = c("measuredItemChildCPC", "zeroweight"))

# We subset the zeroweight coproduct reference table by taking only zeroweights and their coproduct
# that are childcommodities in the tree of the country
zw_coproduct <-
  zw_coproduct[
    measuredItemChildCPC %chin% data_tree$measuredItemChildCPC &
      zeroweight %chin% data_tree$measuredItemChildCPC
    ]


# Computing information for non zeroweight commodities
data_tree <- data_tree[measuredItemChildCPC %!in% zeroWeight]

# this dataset will be used when creating processing share and shareUpdown
dataComplete <- copy(data_tree)

# Quantity of parent destined to the production of the given child (only for child with one parent for the moment)
data_tree[, processed_to_child := ifelse(number_of_parent == 1, production_of_child, NA_real_)]

# if a parent has one child, all the production of the child comes from that parent
data_tree[
  number_of_children == 1,
  processed_to_child := parent_qty_processed * extractionRate,
  processed_to_child
  ]

data_tree[production_of_child == 0, processed_to_child := 0]

# assigning the entired availability to processed for parent having only processed as utilization
data_tree[
  number_of_children == 1 & unique_proc == TRUE,
  processed_to_child := availability * extractionRate
  ]


# mirror assignment for imputing processed quantity for multple parent children
# 5 loop is sufficient to deal with all the cases

for (k in 1:5) {
  data_tree <- RemainingToProcessedParent(data_tree)
  data_tree <- RemainingProdChildToAssign(data_tree)
}

data_tree <- RemainingToProcessedParent(data_tree)

# proportional allocation of the remaing production of multiple parent children
data_tree[,
          processed_to_child :=
            ifelse(
              number_of_parent > 1 & is.na(processed_to_child),
              (remaining_to_process_child * is.na(processed_to_child) * remaining_processed_parent) / sum((remaining_processed_parent * is.na(processed_to_child)), na.rm = TRUE),
              processed_to_child),
          by = c("geographicAreaM49", "measuredItemChildCPC", "timePointYears")
          ]

# Update of remaining production to assing ( should be zero for 2000:2013)
data_tree[,
          parent_already_processed :=
            ifelse(
              is.na(parent_qty_processed),
              parent_qty_processed,
              sum(processed_to_child / extractionRate,na.rm = TRUE)
            ),
          by = c("geographicAreaM49", "measuredItemParentCPC", "timePointYears")
          ]

data_tree[, remaining_processed_parent := round(parent_qty_processed - parent_already_processed)]

data_tree[
  remaining_processed_parent < 0,
  remaining_processed_parent := 0
  ]


# Impute processed quantity for 2014 onwards using 3 years average
# (this only to imput shareDownUp)
data_tree <-
  data_tree[
    order(geographicAreaM49, measuredItemParentCPC, measuredItemChildCPC, timePointYears),
    processed_to_child_avg := rollavg(processed_to_child, order = 3),
    by = c("geographicAreaM49", "measuredItemParentCPC", "measuredItemChildCPC")
    ]

setkey(data_tree, NULL)

data_tree[timePointYears > focus_interval[1]-1 & is.na(processed_to_child), processed_to_child := processed_to_child_avg]


# Back to zeroweight cases(we assign to zeroweights the processed quantity of their coproduct(already calculated))

zw_coproduct_bis <-
  merge(
    data_tree,
    zw_coproduct,
    by = "measuredItemChildCPC",
    allow.cartesian = TRUE,
    all.y = TRUE
  )

zw_coproduct_bis[,
                 `:=`(
                   measuredItemChildCPC = zeroweight,
                   processed_to_child = processed_to_child / extractionRate
                 )
                 ]

sel_vars <- 
  c("geographicAreaM49", "measuredItemParentCPC",
    "measuredItemChildCPC", "timePointYears")

zw_coproduct_bis <-
  zw_coproduct_bis[, c(sel_vars, "processed_to_child"), with = FALSE]

# Correction to milk tree issue ( zeroweight can be associated with 2 main products from the same parent)
# example: butter of cow milk
zw_coproduct_bis[,
                 processed_to_child := sum(processed_to_child, na.rm = TRUE),
                 by = sel_vars 
                 ]

zw_coproduct_bis <-
  unique(zw_coproduct_bis, by = colnames(zw_coproduct_bis))

data_zeroweight <-
  dt_left_join(data_zeroweight, zw_coproduct_bis, by = sel_vars)

data_zeroweight[,
                processed_to_child := processed_to_child * extractionRate
                ]

sel_vars <-
  c("geographicAreaM49", "measuredItemParentCPC", "measuredItemChildCPC",
    "timePointYears", "number_of_parent", "parent_qty_processed",
    "production_of_child", "processed_to_child")

data_zeroweight <- data_zeroweight[, sel_vars, with = FALSE]

data_tree <- data_tree[, sel_vars, with = FALSE]

#combining zeroweight and non zero weight commodities
data_tree <- rbind(data_tree, data_zeroweight)

#calculate ShareDownUp
data_tree[,
          shareDownUp := processed_to_child / sum(processed_to_child, na.rm = TRUE),
          by = c("geographicAreaM49", "measuredItemChildCPC", "timePointYears")
          ]

#some corrections...

data_tree[is.na(shareDownUp) & number_of_parent == 1, shareDownUp := 1]

data_tree[
  (production_of_child == 0 | is.na(production_of_child)) &
    measuredItemChildCPC %!in% zeroWeight &
    timePointYears < focus_interval[1],
  shareDownUp := 0
  ]

data_tree[
  (parent_qty_processed == 0 | is.na(parent_qty_processed)) &
    timePointYears < focus_interval[1],
  shareDownUp :=0
  ]

data_tree[is.na(shareDownUp), shareDownUp := 0]

data_tree <-
  unique(
    data_tree[,
              c("geographicAreaM49", "measuredItemParentCPC",
                "measuredItemChildCPC", "timePointYears", "shareDownUp"),
              with = FALSE
              ]
  )

########################GENERARING ProcessedShare for parent----------------------------------------
######################## ANd ShareUpDown for children ##############################################
####################################################################################################

dataProcessingShare[,
                    availability :=
                      sum(
                        Value[get(p$elementVar) %in% c(p$productionCode, p$importCode)],
                        - Value[get(p$elementVar) %in% c(p$exportCode, "stockChange")],
                        na.rm = TRUE
                      ),
                    #by = c(p$geoVar, p$yearVar, p$parentVar, p$childVar)
                    by = c(p$geoVar, p$yearVar, p$parentVar)
                    ]

dataProcessingShare[,SumLoss:=sum(
  Value[measuredElementSuaFbs=="loss" & timePointYears %in% focus_interval],na.rm = TRUE
),
by=c("geographicAreaM49","measuredItemParentCPC")
]

dataProcessingShare[,SumProd:=sum(
  Value[measuredElementSuaFbs=="production" & timePointYears %in% focus_interval],na.rm = TRUE
),
by=c("geographicAreaM49","measuredItemParentCPC")
]


dataProcessingShare[,parent_qty_processed:=Value[measuredElementSuaFbs=="foodManufacturing"],
                    by=c("geographicAreaM49","measuredItemParentCPC","timePointYears")
                    ]

dataProcessingShare[,Prod:=Value[measuredElementSuaFbs=="production"],
                    by=c("geographicAreaM49","measuredItemParentCPC","timePointYears")
                    ]

dataProcessingShare[,RatioLoss:=SumLoss/SumProd]

dataProcessingShare[,
                    loss_Median_before :=
                      median(
                        Value[measuredElementSuaFbs == "loss" & timePointYears %in% 2000:focus_interval[1]-1],
                        na.rm=TRUE
                      ),
                    by = c(p$parentVar, p$geoVar)
                    ]

dataProcessingShare[,NewLoss:=ifelse(is.na(loss_Median_before),TRUE,FALSE)]

# dataset containing the processed quantity of parents
dataProcessingShare <-
  unique(
    dataProcessingShare[,
                        c("geographicAreaM49", "measuredItemParentCPC", "timePointYears", "Prod",
                          "availability","RatioLoss","NewLoss","parent_qty_processed"),
                        with = FALSE
                        ],
    by = c("geographicAreaM49", "measuredItemParentCPC", "timePointYears", "Prod",
           "availability","RatioLoss","NewLoss","parent_qty_processed")
  )
#setnames(dataProcessingShare, "Value", "Prod")

# Processing share and ShareUpDown
dataProcessingShare <-
  unique(
    dataProcessingShare[,
                        c("geographicAreaM49", "timePointYears", "availability",
                          "measuredItemParentCPC", "parent_qty_processed",
                          "RatioLoss","NewLoss","Prod"),
                        with = FALSE
                        ],
    by = c("geographicAreaM49", "timePointYears",
           "measuredItemParentCPC", "parent_qty_processed")
  )

dataProcessingShare[timePointYears %in% focus_interval, parent_qty_processed := NA_real_]

#correction of Pshare to adjust prcessed in case of new Loss

dataProcessingShare[is.na(RatioLoss),RatioLoss:=0]
dataProcessingShare[is.na(Prod),Prod:=0]


dataProcessingShare[,
                    Pshare := ifelse(NewLoss==TRUE,(parent_qty_processed-RatioLoss*Prod) / availability,
                                     parent_qty_processed / availability),
                    by = c("geographicAreaM49", "timePointYears", "measuredItemParentCPC")
                    ]

dataProcessingShare[Pshare > 1, Pshare := 1]
dataProcessingShare[Pshare < 0,Pshare := 0]
dataProcessingShare[is.nan(Pshare), Pshare := NA_real_]

dataProcessingShare <-
  dataProcessingShare[
    order(geographicAreaM49, measuredItemParentCPC, timePointYears),
    Pshare_avr := rollavg(Pshare, order = 3),
    by = c("geographicAreaM49", "measuredItemParentCPC")
    ]

setkey(dataProcessingShare, NULL)

# we estimate Pshare for 2014 onwards, even if processed are modified manually because a this stage
# availability of 2014 onwards are known (stocks are not generated yet)
dataProcessingShare[timePointYears > focus_interval[1]-1, Pshare := Pshare_avr]
dataProcessingShare[, Pshare_avr := NULL]

data_Pshare <-
  dataProcessingShare[,
                      c("geographicAreaM49", "timePointYears", "measuredItemParentCPC",
                        "parent_qty_processed", "Pshare"),
                      with = FALSE
                      ]


# calculating shareUpdown for each child
data_ShareUpDoawn <-
  dataComplete[,
               c("geographicAreaM49", "timePointYears", "measuredItemParentCPC",
                 "availability", "parent_qty_processed", "measuredItemChildCPC",
                 "extractionRate", "production_of_child", "number_of_children"),
               with = FALSE
               ]

data_ShareUpDoawn <-
  unique(
    data_ShareUpDoawn,
    by = colnames(data_ShareUpDoawn)
  )

data_ShareUpDoawn <- merge(
  data_ShareUpDoawn,
  data_tree,
  by = c("geographicAreaM49", "measuredItemParentCPC",
         "measuredItemChildCPC", "timePointYears"),
  all = TRUE
)

data_ShareUpDoawn <- data_ShareUpDoawn[measuredItemChildCPC %!in% zeroWeight]
data_ShareUpDoawn[, shareUpDown := NA_real_]

# Giulia: qui inserire una condizione, se parent qty processed =na la shares Up Down ==0 . cosi risolvi cherries e oil of linseed
data_ShareUpDoawn[
  extractionRate>0,
  shareUpDown := ifelse( !is.na(parent_qty_processed),(production_of_child / extractionRate) * shareDownUp / sum(production_of_child / extractionRate * shareDownUp, na.rm = TRUE),0),
  by = c("geographicAreaM49", "timePointYears", "measuredItemParentCPC")
  ]

data_ShareUpDoawn[is.nan(shareUpDown), shareUpDown := NA_real_]

data_ShareUpDoawn <-
  data_ShareUpDoawn[
    order(geographicAreaM49, measuredItemParentCPC, measuredItemChildCPC, timePointYears),
    shareUpDown_avg := rollavg(shareUpDown, order = 3),
    by = c("geographicAreaM49", "measuredItemParentCPC", "measuredItemChildCPC")
    ]

setkey(data_ShareUpDoawn, NULL)

data_ShareUpDoawn[timePointYears > focus_interval[1]-1, shareUpDown := shareUpDown_avg]

data_ShareUpDoawn[, shareUpDown_avg := NULL]

data_ShareUpDoawn[
  timePointYears > focus_interval[1]-1,
  shareUpDown := shareUpDown / sum(shareUpDown, na.rm = TRUE),
  by = c("geographicAreaM49", "timePointYears", "measuredItemParentCPC")
  ]

sel_vars <-
  c("geographicAreaM49", "measuredItemParentCPC",
    "measuredItemChildCPC", "timePointYears", "shareUpDown")

data_ShareUpDoawn_final <- data_ShareUpDoawn[, sel_vars, with = FALSE]

shareUpDown_zeroweight <-
  merge(
    data_ShareUpDoawn_final,
    zw_coproduct,
    by = "measuredItemChildCPC",
    allow.cartesian = TRUE,
    all.y = TRUE
  )

shareUpDown_zeroweight[, measuredItemChildCPC := zeroweight]

shareUpDown_zeroweight <- shareUpDown_zeroweight[, sel_vars, with = FALSE]

# Correction to milk tree issue ( zeroweight can be associated with 2 main products from the same parent)
# example: butter of cow milk
shareUpDown_zeroweight<-
  shareUpDown_zeroweight[,
                         shareUpDown := sum(shareUpDown, na.rm = TRUE),
                         by = c("geographicAreaM49", "measuredItemParentCPC", "measuredItemChildCPC", "timePointYears")
                         ]

shareUpDown_zeroweight <-
  unique(
    shareUpDown_zeroweight,
    by = colnames(shareUpDown_zeroweight)
  )

# /correction


data_ShareUpDoawn_final <- rbind(data_ShareUpDoawn_final, shareUpDown_zeroweight)

# some correction
data_ShareUpDoawn_final[is.na(shareUpDown), shareUpDown := 0]
data_ShareUpDoawn_final[!is.na(shareUpDown), flagObservationStatus := "I"]
data_ShareUpDoawn_final[!is.na(shareUpDown), flagMethod := "c"]


# sAVE DATA TO SWS

shareUpDown_to_save <- copy(data_ShareUpDoawn_final)

shareUpDown_to_save[, measuredElementSuaFbs := "5431"]

setnames(
  shareUpDown_to_save,
  c("measuredItemParentCPC", "measuredItemChildCPC"),
  c("measuredItemParentCPC_tree", "measuredItemChildCPC_tree")
)

setnames(shareUpDown_to_save, "shareUpDown", "Value")

sessionKey_shareUpDown <- swsContext.datasets[[1]]

CONFIG <- GetDatasetConfig(sessionKey_shareUpDown@domain, sessionKey_shareUpDown@dataset)
# taking updown shares from the session
data_shareUpDown_sws <- GetData(sessionKey_shareUpDown)

shareUpDown_to_save <- shareUpDown_to_save[timePointYears %in% startYear:endYear,]

faosws::SaveData(
  domain = "suafbs",
  dataset = "up_down_share",
  data = shareUpDown_to_save[measuredItemParentCPC_tree %in% Item_Parent, ],
  waitTimeout = 20000
)

print("Tree corrected succesfully")

