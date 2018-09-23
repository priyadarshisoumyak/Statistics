library(readxl)
library(tidyr);
library(qcc)
library(data.table)
library(ggplot2)
library(plyr)
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
    uspopulation <- aggregate(list(population = uspopulation$P), by=list(state = uspopulation$S), FUN = sum)
    # Order by population desending
    orderByPopulationData <- uspopulation[order(-uspopulation$population),]
    # Get Top 10 state with highest population
    getTop10States <- head(orderByPopulationData, 10)
    # Set State population to partoData
    partoData <- getTop10States$population
    # Update names of partoData
    names(partoData) <- getTop10States$state
    # Save File as Question_1.pdf
    pdf(file="Question_1.pdf")
    # Build chart for Top 10 Population By State
    pareto.chart(partoData, main = "Top 10 Population By State")
    summary(subset(excelData, select = `DMR`))

    #Question Number 2 
    #attaching Excel Data
    attach(excelData)
    #Binding the Domestic Migration Rate to the Matrix
    matrix <- cbind(prop.table(table(excelData$DMR)))
    # Reanaming the Column Name 
    colnames(matrix)<- c("Relative Frequency")
    # Saving File in Question_2.pdf
    pdf(file="Question_2.pdf")
    #Building the Histogram
    hist(matrix,xlab="Domestic Migration Rate",main = "Relative Frequency Diagram")

    #Question Number 3






}