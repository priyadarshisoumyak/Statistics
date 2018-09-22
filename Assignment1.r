library(readxl)
library(qcc)
uspopulation_new <- read_excel("C:/Users/suhai/Downloads/uspopulation_new.xlsx")
uspopulation_new <- aggregate(uspopulation_new$IJ, by=list(state = uspopulation_new$GH), FUN = sum)
orderByPopulationData <- uspopulation_new[order(-uspopulation_new$x),]
getTop10States <- head(orderByPopulationData, 10)
partoData <- getTop10States$x
names(partoData) <- getTop10States$state
pareto.chart(partoData)


