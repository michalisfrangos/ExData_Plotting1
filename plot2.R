# Data Science Course,  Exploratory data analysis project 1

# Michalis Frangos
# frangos@frangos.eu

# set working directory
script.dir <- 'D:/FRANGOS_FOLDER/CoursesCertificates/Coursera_Spec_DataAnalysis_2016/ExploratoryDataAnalysis/ExData_Plotting1'

# Set working directory
#script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)

# clear workspace
rm(list = ls()) 

library(R.utils)
library(httr)
library(plyr)
library(dplyr)


## DOWNLOADING and UNZIPING DATA Function
downloadDataFile <- function(fileUrl,zipFileName,fileName){
        

        if (!file.exists("downloads") & !file.exists("exdata_data_household_power_consumption")) {
                message("- downloading data")
                dir.create("downloads")
                download.file(fileUrl,destfile = "./downloads/project_data.zip",method = "auto") 
                dateDownloaded <- date()
                message("- data downloaded")
                #file.remove("./downloads/project_data.zip")
        } else {
                message("- data already downloaded")  
        }
        
        file.name <- "household_power_consumption.txt"
        if  (!file.exists(fileName)){
                message("- unzipping data")
                unzip("./downloads/project_data.zip")
                message("- data unzipped")
        } else {
                message("- data file exists")      
        }
        
}

## Calculate a rough estimate of how much memory the dataset will require
testDataFileSize <- function(fileName,maxFileSizeInGBytes=1){
        message("- testing data size")
        # The dataset has 2,075,259 rows and 9 columns. 
        data <- read.csv(fileName,skip=1,nrow=10,sep=";",stringsAsFactors = FALSE)
        ncolumns <- ncol(data)
        nrows <- countLines(fileName)
        
        # Dont read files more than a GB
        bytes <- (ncolumns*nrows)*8
        GB <-  (bytes/2^20)/1000 
        if (GB<maxFileSizeInGBytes){ 
                message("- data size < 1 GB; will explore this data")
        } else{ 
                stop("- data size > 1 GB; will not explore this data")
        }
        
}

## LOADING DATA; returns a data frame
loadData<- function(fileName){
        message("- loading data")
        
        # Find from text file which lines correspond to dates 2007-02-01 and 2007-02-02.
        x <- grep("^(1/2/2007|2/2/2007)",readLines(fileName),value=FALSE)
        numberOflinesToRead <- length(x);
        numberOfLinesToSkip <- x[1]-1
        
        # Reading data from the dates 2007-02-01 and 2007-02-02
        # Note that in this dataset missing values are coded as ?.
        data.colnames <- colnames(read.csv(fileName, nrow = 1, sep=";", header = TRUE))
        data <- read.csv(fileName, sep = ";", skip = numberOfLinesToSkip, 
                         nrow = numberOflinesToRead, stringsAsFactors = FALSE,
                         na.strings= "?", col.names = data.colnames,header = FALSE)
        
        # Convert the Date variable to date class in R
        data$Date <- as.Date(data$Date,"%d/%m/%Y") # %>% as.Date("%d/%m/%Y")
        
        # Convert the Time variable to time Class in R
        dates <- data$Date
        times <- data$Time
        x <- paste(dates, times)
        data$Time <- strptime(x, "%Y-%m-%d %H:%M:%S")
        
        message("- data loaded (takes some time)")
        return(data)
}


makePlot2 <- function(data){
        # define options 
        xlableString <- ""
        ylableString <- "Global Active Power (kilowatts)"
        
        # plot
        with(data,plot(Time,Global_active_power, 
                       xlab = xlableString,
                       ylab = ylableString, type = "l"))
}



## MAKING PLOTS

fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
zipFileName <- "exdata_data_household_power_consumption"
fileName <- "household_power_consumption.txt"

maxFileSizeInGBytes = 1 # max data for file to play with in Gbytes 


downloadDataFile(fileUrl,zipFileName,fileName)
testDataFileSize(fileName)
data <- loadData(fileName)

graphics.off() 

png(filename ="plot2.png", width = 480, height = 480)
makePlot2(data)
dev.off()

