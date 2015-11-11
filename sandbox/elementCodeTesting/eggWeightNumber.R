library(faosws)
library(data.table)
library(ggplot2)

SetClientFiles(dir = "~/R certificate files/QA")
GetTestEnvironment(
    ## baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
    ## token = "7b588793-8c9a-4732-b967-b941b396ce4d"
    baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
    token = "3242f11d-28b2-4429-86b0-6fab97cb50bb"
)

itemCodes = GetCodeList("agriculture", "agriculture", "measuredItemCPC")
eggCodes = itemCodes[type == "EGGW", code]

key = DatasetKey(domain = "agriculture", dataset = "agriculture", dimensions = list(
    geographicAreaM49 = Dimension(name = "geographicAreaM49",
                                  keys = GetCodeList("agriculture", "agriculture", "geographicAreaM49")[, code]),
    measuredElement = Dimension(name = "measuredElement", keys = c("5510", "5513")), # egg weight and number
    measuredItemCPC = Dimension(name = "measuredItemCPC", keys = eggCodes),
    timePointYears = Dimension(name = "timePointYears", keys = as.character(1950:2015))))
data = GetData(key)
fun.agg = function(...){
    stopifnot(length(...) <= 1)
    mean(...)
}
newData = dcast.data.table(data, formula = geographicAreaM49 + measuredItemCPC + timePointYears ~ measuredElement,
                           fun.aggregate = fun.agg, value.var = "Value")
setnames(newData, c("5510", "5513"), c("weight", "count"))

## We certainly have some outlying values:
newData[, eggWeight := weight / count]
qplot(newData[eggWeight < Inf, eggWeight])
newData[eggWeight > 1000 & eggWeight < Inf, ]
qplot(newData[eggWeight < 1000, eggWeight])
newData[eggWeight > 10 & eggWeight < 100, ]
qplot(newData[eggWeight < 10, eggWeight])
newData[eggWeight > .5 & eggWeight < 10, ]
qplot(newData[eggWeight < 1, eggWeight])
qplot(newData[eggWeight < 0.5, eggWeight])

detectOutlier = function(x){
    params = MASS::huber(x)
    mu = params[[1]]
    sd = params[[2]]
    return(abs(x-mu) >= 4*sd)
}
newData[, outlier := detectOutlier(eggWeight), by = measuredItemCPC]
ggplot(newData[!(outlier), ]) + facet_wrap( ~ measuredItemCPC, scale = "free") +
    geom_bar(aes(x = eggWeight))

fit1 = lm(eggWeight ~ 1, data = newData[!is.na(eggWeight) & eggWeight < Inf, ])
fitFull = lm(eggWeight ~ measuredItemCPC + timePointYears + geographicAreaM49,
             data = newData[!is.na(eggWeight) & eggWeight < Inf, ])
summary(fitFull)
# Maybe it's reasonable to use a global average egg weight:
step(fitFull)
