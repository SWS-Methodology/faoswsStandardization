library(faosws)
library(faoswsUtil)

areaKeys = GetCodeList(domain = "agriculture", dataset = "aproduction", "geographicAreaM49")
areaKeys = areaKeys[type == "country", code]
elemKeys = GetCodeTree(domain = "agriculture", dataset = "aproduction", "measuredElement")
elemKeys = elemKeys[parent %in% c("31", "41", "51", "61", "71", "91", "101", "111", "121", "131"),
                    paste0(children, collapse = ", ")]
elemKeys = strsplit(elemKeys, ", ")[[1]]
itemKeys = GetCodeList(domain = "agriculture", dataset = "aproduction", "measuredItemCPC")
itemKeys = itemKeys[, code]
key = DatasetKey(domain = "agriculture", dataset = "aproduction", dimensions = list(
    geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = areaKeys),
    measuredElementSuaFbs = Dimension(name = "measuredElement", keys = elemKeys),
    measuredItemSuaFbs = Dimension(name = "measuredItemCPC", keys = itemKeys),
    timePointYears = Dimension(name = "timePointYears", keys = as.character(2000:2011))
))
data = GetData(key)

codeMap = GetTableData(schemaName = "ess", tableName = "item_type_yield_elements")
itemKeys = GetCodeList(domain = "agriculture", dataset = "aproduction", "measuredItemCPC")
setnames(itemKeys, "code", "measuredItemCPC")
itemKeys = itemKeys[, c("measuredItemCPC", "type"), with = FALSE]
data = merge(data, itemKeys, by = "measuredItemCPC", all.x = TRUE)
elemKeys = GetCodeTree(domain = "agriculture", dataset = "aproduction", "measuredElement")
elementMap = adjacent2edge(elemKeys)
setnames(elementMap, "children", "measuredElement")
data = merge(data, elementMap, by = "measuredElement", all.x = TRUE)

dcast(data, formula = type ~ parent, value.var = "measuredElement",
      fun.aggregate = function(x) length(unique(x)))

data[, .N, c("measuredElement", "type", "parent")]
data[type == "CRPR", .N, c("measuredElement", "type", "parent")][order(as.numeric(parent)), ]
write.csv(data[, .N, c("measuredElement", "type", "parent")], file = "~/Desktop/temp.csv", row.names = FALSE)
