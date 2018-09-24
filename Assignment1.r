library(readxl)
library(tidyr);
library(qcc)
library(data.table)
library(ggplot2)
library(plyr)
library(lattice)
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
    #attaching Domestic Migration Data into histo variable   
    histoDomesticMigrationRaate<- hist(excelData$DMR)
    # Saving File in Question_2.pdf 
    pdf(file="Question_2.pdf")
    #making the barplot with relative freqquency 
    barplot(histoDomesticMigrationRaate$counts/nrow(excelData),col="white",space=0)->bp
    #renaming the axis
    axis(1,at=c(bp),labels=histoDomesticMigrationRaate$mids)
    #renaming the title
    title(ylab="Relative Frequency",xlab="Domestic Migration Rate")

    #Question Number 3
    # Set Domestic intervals
    domesticIntervals = seq(-100, 100, by = 1)
    # Set cut to Domestic Rate
    domesticRate.cut = cut(excelData$DMR, domesticIntervals, right = FALSE)
    # Set frequency to Domestic Rate
    domesticRate.freq = table(domesticRate.cut)
    # Create cummulative frequency
    cumFreq = c(0, cumsum(domesticRate.freq))
    # Create pdf
    pdf(file="Question_3.pdf")
    # Create Plot intervals with cummulative Frequency
    plot(domesticIntervals, cumFreq, main = "Cumulative Frequency for Domestic Migration", xlab = "Domestic Migration Rate", ylab= "Cumulative Frequency" )
    # Create lines with cummulative Frequency
    lines(domesticIntervals, cumFreq) 

    #Question 4
    #using summary function to generate Numerical Statistics for Domestic Migration Rate    
    print(summary(excelData$DMR))

    #Question 5
    #saving Data in file Question_5
    lowestValue <- excelData$DMR[excelData$DMR>-22]
    highestValue <- lowestValue[lowestValue<22]
    pdf(file="Question_5.pdf")
    boxplot(highestValue, horizontal = T,main = "Outliers",xlab="Domestic Migration Rate")

    #Question 6.a
    #attaching Domestic Migration Data into histo variable   
    histoInternationalMigrationRaate<- hist(excelData$DMR)
    # Saving File in Question_2.pdf 
    pdf(file="Question_2.pdf")
    #making the barplot with relative freqquency 
    barplot(histoInternationalMigrationRaate$counts/nrow(excelData),col="white",space=0)->bp
    #renaming the axis
    axis(1,at=c(bp),labels=histoInternationalMigrationRaate$mids)
    #renaming the title
    title(ylab="Relative Frequency",xlab="International Migration Rate")

    #Question 6.b
    # Set International intervals
    internationalIntervals = seq(-100, 100, by = 1)
    # Set cut to International Rate
    internationalRate.cut = cut(excelData$IMD, internationalIntervals, right = FALSE)
    # Set frwquency to International Rate
    internationalRate.freq = table(internationalRate.cut)
    # Create cummulative frequency
    cumFreq = c(0, cumsum(internationalRate.freq))
    # Create pdf
    pdf(file="Question_6_b.pdf")
    # Create Plot intervals with cummulative Frequency
    plot(internationalIntervals, cumFreq, main = "Cumulative Frequency for International Migration", xlab = "International Migration Rate", ylab= "Cumulative Frequency" )
    # Create lines with cummulative Frequency
    lines(internationalIntervals, cumFreq) 

    #Question 6.c
    #using summary function to generate Numerical Statistics for International Migration Rate    
    print(summary(excelData$IMD))

    #Question 6.d
    lowestValueIMD <- excelData$IMD[excelData$IMD >-1]
    highestValueIMD <- lowestValueIMD[lowestValueIMD<2]
    pdf(file="Question_6_d.pdf")
    boxplot(highestValueIMD, horizontal = T,main = "Outliers",xlab="International Migration Rate")

    #Repeat from Question 2-5
    #Question 7
     # Saving File in Question_7.pdf
    pdf(file="Question_7.pdf")
    plot(excelData$DMR,excelData$IMD,main = "ScatterPlot",xlab = "DomesticMigrationRate",ylab = "InternationalMigrationRate",las =1,xlim = c(0,25),col = 2)
    abline(lm(excelData$DMR ~ excelData$IMD),col = 5)
    lines(smooth.spline(excelData$DMR,excelData$IMD),col=4)
}