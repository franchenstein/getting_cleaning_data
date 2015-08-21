#Name: run_analysis.R
#Author: Daniel K. Franch
#Description: Script used for the final project of Coursera's Getting and Cleaning Data taught by
#Dr. Jeff Leek from Johns Hopkins Bloomberg School of Public Health. This script will download
#data from UC Irvine's Machine Learning Repository. The data is filtered information from 
#Samsung Galaxy SII's accelerometer and gyroscope obtained while people (test subjects) were
#doing activities (walking, sitting, etc.) with the phone This script will get this data, 
#merge the test and training data sets and extract only the informations that are the
#averages and standard deviations of measured values. Then, it will give descriptive names
#to the variables.Finally, based on this data set a new one will be  generated and saved with
#the averages per activity and per test.

library(dplyr)
library(tidyr)

#The URL from where the data will be obtained:
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

#Creates the directory to store the data if it does not exist yet.
if (!file.exists("data")){
    dir.create("data")
}

#Downloads and unzips the zip file containing the data if it has not been done before.
#As the file is quite large and takes a while to be downloaded, it is better to only
#do it if the files are not there yet.
if (!file.exists("./data/projectdata.zip")){
    download.file(fileURL, destfile = "./data/projectdata.zip", method = "curl")
    dateDownloaded <- date()
    unzip('./data/projectdata.zip', exdir = './data')
}

#Data sets related to the test data:
testData <- read.table('./data/UCI HAR Dataset/test/X_test.txt')
testActivities <- read.table('./data/UCI HAR Dataset/test/y_test.txt')
testActivities <- select(testActivities, Activity = V1)
testSubjects <- read.table('./data/UCI HAR Dataset/test/subject_test.txt')
testSubjects <- select(testSubjects, Subject = V1)
test <- cbind(testActivities, testSubjects, testData)

#Data sets related to the train data:
trainData <- read.table('./data/UCI HAR Dataset/train/X_train.txt')
trainActivities <- read.table('./data/UCI HAR Dataset/train/y_train.txt')
trainActivities <- select(trainActivities, Activity = V1)
trainSubjects <- read.table('./data/UCI HAR Dataset/train/subject_train.txt')
trainSubjects <- select(trainSubjects, Subject = V1)
train <- cbind(trainActivities, trainSubjects, trainData)

#Merging the test and train sets:
data <- bind_rows(test, train)

#Reads the file containing the meaning of each variable in the columns:
names <- read.delim("./data/UCI HAR Dataset/features.txt", header = FALSE, sep = " ")
#Converts the variable's names to strings
names <- lapply(names[,2],as.character)
#Renames the variables to the desired names
colnames(data)[3:length(data)] <- names

#Uses grep to find the columns containing means and standard deviations:
means <- data[grepl("mean", names(data))]
stds <- data[grepl("[Ss]td", names(data))]
#Creates the final dataframe using the Activity, Subject, Means and Std columns
fData <- cbind(data[,1:2], means, stds)
#Ordering the data by activity:
fData <- fData[order(fData$Activity),]

#Reads the activity labels file, that will associate activities numbers' to 
#their names:
actNames <- read.delim("./data/UCI HAR Dataset/activity_labels.txt", header = FALSE, sep = " ")
actNames <- as.character(actNames[,2])

#Loops through the finalData substituting the activity number by its name:
finalData <- fData
for(i in 1:length(fData$Activity)){
    aux <- fData$Activity[i]
    finalData$Activity[i] <- actNames[aux]
}

avgByActBySubj <- NULL
for (i in 1:length(actNames)){
    #Separates the data by activity:
    dataByActivity <- finalData[finalData$Activity == actNames[i],]
    #Groups the separated data by subject
    dataByActivityByGroup <- group_by(dataByActivity, Subject)
    #Calculates the average of each variable by subject
    aux <- summarise_each(dataByActivityByGroup[,2:ncol(dataByActivityByGroup)], funs(mean))
    #Creates a data frame with the current activity to be binded:
    acts <- data.frame(Activity = rep(actNames[i], each = nrow(aux)))
    #Adds the new data do the data frame:
    avgByActBySubj <- bind_rows(avgByActBySubj, bind_cols(acts,aux))
}

#Saves the tidy data set generated in a text file:
write.table(avgByActBySubj, file = "tidyData.txt", row.names = FALSE)