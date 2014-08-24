# Getting and Cleaning Data Course Project
# Vinay Vasudev

# This R script is used to read and clean a data set so that the data
# can be used for a later analysis This script does the following:
#			1. read the training and test data sets
#			2. read the activity labels and descriptions of the columns
#				in the data sets
#			3. cleans the descriptive column variable names and improve the
#				readability and appearance in line with tidy data guidelines
#			4. combines all the read and cleaned data in a merged data set
#			5. extracts the only measurements from the merged data set which are 
#				mean and standard deviation of various measurements  
#			6. creates a new independent tidy data set with the average of each
#				 variable for each activity and subject
#			7. exports the last tidy data as a text file 

# Load the required packages

library(plyr)
library(reshape2)

# Load all the data files

setwd("C:/Users/vasudev/Desktop/Coursera Project")
xTrainData <- read.table("X_Train.txt")
xTestData <- read.table("X_Test.txt")
yTrainData <- read.table("y_Train.txt")
yTestData <- read.table("y_Test.txt")
subjectTestData <- read.table("subject_test.txt")
subjectTrainData <- read.table("subject_train.txt")
columnNames <- read.table("features.txt")

# Replace activity numbers by activity labels such as "Walking", etc.

activityLabels <- read.table("activity_labels.txt")
factors <- factor(yTestData$V1)
levels(factors) <- c(as.character(activityLabels$V2))
yTestData$V1 <- factors
factors <- factor(yTrainData$V1)
levels(factors) <- c(as.character(activityLabels$V2))
yTrainData$V1 <- factors

# Merge the training data into one data frame which includes training data set, subjects and activity information

trainData <- cbind(subjectTrainData, yTrainData, xTrainData)

# Merge the test data into one data frame which includes test data set, subjects and activity information

testData <- cbind(subjectTestData, yTestData, xTestData)

# Merge the training and test data sets into one data frame 

mergedData <- rbind(trainData, testData)

# Prepare the descriptive variable names by cleaning the names loaded from the file "features.txt"
# Basic approach to cleaning the variable names is that variables are transformed into Camel Case
# notation because the names are very long and has several words connected to each other which
# cannot be read clearly. Variable names are made more readable by making the first letter of the word
# capital. Special characters such as "(), -" were removed. While several ideas from tidy data approach
# has been used here in cleaning up the variable names, some are ignored to improve the
# readability for meaning. Variable names which show data for different axes are shown with ".Xaxis",
# ".Yaxis", etc. at the end of the variable name. While R is not very good in handling special
# characters in variable names, period are OK. Abbreviated names have been expanded such as "Acc" to
# "Acceleration", "std" to "StDev", etc. Several variable names start with Time or FFT representing
# either the Time series data or Fast Fourier Transform data.

columnNames$V2 <- gsub("-|\\()", "", columnNames$V2)
columnNames$V2 <- gsub("Acc", "Acceleration", columnNames$V2)
columnNames$V2 <- gsub("mean", "Mean", columnNames$V2)
columnNames$V2 <- gsub("X", ".Xaxis", columnNames$V2)
columnNames$V2 <- gsub("Y", ".Yaxis", columnNames$V2)
columnNames$V2 <- gsub("Z", ".Zaxis", columnNames$V2)
columnNames$V2 <- gsub("std", "StDev", columnNames$V2)
columnNames$V2 <- gsub("min", "Minimum", columnNames$V2)
columnNames$V2 <- gsub("max", "Maximum", columnNames$V2)
columnNames$V2 <- gsub("Inds", "Index", columnNames$V2)
columnNames$V2 <- gsub("Mag", "Magnitude", columnNames$V2)
columnNames$V2 <- gsub("arCoeff", "AutoRegCoefficient", columnNames$V2)
columnNames$V2 <- gsub("entropy", "Entropy", columnNames$V2)
columnNames$V2 <- gsub("energy", "Energy", columnNames$V2)
columnNames$V2 <- gsub("band", "Band", columnNames$V2)
columnNames$V2 <- gsub("sma", "SignalMagnitudeArea", columnNames$V2)
columnNames$V2 <- gsub("mad", "MedianAbsDeviation", columnNames$V2)
columnNames$V2 <- gsub("iqr", "IQR", columnNames$V2)
columnNames$V2 <- gsub("Gyro", "Gyroscope", columnNames$V2)
columnNames$V2 <- gsub("skewness", "Skewness", columnNames$V2)
columnNames$V2 <- gsub("kurtosis", "Kurtosis", columnNames$V2)
columnNames$V2 <- gsub("gravity", "Gravity", columnNames$V2)
columnNames$V2 <- gsub("angle\\(", "AngleBetween", columnNames$V2)
columnNames$V2 <- gsub("BodyBody", "Body", columnNames$V2)
columnNames$V2 <- gsub("^[t]", "Time", columnNames$V2)
columnNames$V2 <- gsub("^[f]", "FFT", columnNames$V2)

# Add Column Headers for Subject and Activity Labels

vec1 <- c(1, "Subject")
vec2 <- c(2, "Activity")
columnNames1 <- rbind(vec1, vec2, columnNames)

# Replace column names with descriptive variable names in the merged data

colnames(mergedData) <- c(columnNames1[ , 2])

# Separate Subjects and Activity Labels from "mergedData" before extracting the columns for mean
# and standard deviation

subjectsActivityData <- mergedData[, 1:2]

# Extract the measurement data on the mean and standard deviation for each measurement from the merged data

columnsMeanStd1 <- grep("[Mm]ean|[Ss][Tt][Dd]", names(mergedData))
meanStdData1 <- mergedData[, columnsMeanStd1]

# Exclude the measurement data which include angle between two measurements

columnsMeanStd2 <- grep("^[Angle]", names(meanStdData1))
meanStdData <- meanStdData1[, -columnsMeanStd2]

# Combine the Subjects and Activity Labels info back with extracted data "meanStdData"

tidyData <- cbind(subjectsActivityData, meanStdData)
  
# Aggregate data frame meanStdData by Subjects and Activity Labels, returning average of each variable
# for each activity and each subject. Save the data into into an independent tidy data set named
# "tidyData" 

meltedData <- melt(tidyData, id.vars = c("Subject", "Activity"))
tidyData <- ddply(meltedData, c("Subject", "Activity", "variable"), summarise, Mean = mean(value))

# Export the tidyData set as a text file

write.table(tidyData, "tidyData.txt", sep="\t", row.name=FALSE)
