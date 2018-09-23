library(readxl)
library(tidyr);
library(qcc)
library(data.table)
#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
# test if there is at least one argument: if not, return an error
if (length(args)==0) {
    stop("At least one argument must be supplied (input file).n", call.=FALSE)
} else if (length(args)==1) {
    # default output file
    print(args[1]);
    # Load excel data into uspopulation
    excelData <- read_excel(args[1])
    names(excelData) <- c("A", "B", "C", "P", "NDM", "DMR", "NIM", "IMD", "NC")
    # Split County into County and State
    uspopulation <- separate(excelData,"C" , c("C", "S"), sep="," )
    # Aggregate population by state
    uspopulation <- aggregate(uspopulation$P, by=list(state = uspopulation$S), FUN = sum)
    # Order by population desending
    orderByPopulationData <- uspopulation[order(-uspopulation$x),]
    # Get Top 10 state with highest population
    getTop10States <- head(orderByPopulationData, 10)
    # Set State population to partoData
    partoData <- getTop10States$x
    # Update names of partoData
    names(partoData) <- getTop10States$state
    # Save File as Question_1.pdf
    pdf(file="Question_1.pdf")
    # Build chart for Top 10 Population By State
    pareto.chart(partoData, main = "Top 10 Population By State")
    summary(subset(excelData, select = `DMR`))
}