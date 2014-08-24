Coursera-Data-Cleanup-Project
=============================

## This R script is used to read and clean a data set so that the data
## can be used for a later analysis This script does the following:
			1. read the training and test data sets
			2. read the activity labels and descriptions of the columns
				in the data sets
			3. cleans the descriptive column variable names and improve the
				readability and appearance in line with tidy data guidelines
			4. combines all the read and cleaned data in a merged data set
			5. extracts the only measurements from the merged data set which are 
				mean and standard deviation of various measurements  
			6. creates a new independent tidy data set with the average of each
				 variable for each activity and subject
			7. exports the last tidy data as a text file 

* Following required packages are needed during running of script for aggregating the tidy data

	library(plyr)
	library(reshape2)

* Following segment of the script reads all the input data sets into R assuming that they exist in the 	  currently set working directory

	setwd("C:/Users/vasudev/Desktop/Coursera Project")
	xTrainData <- read.table("X_Train.txt")
	xTestData <- read.table("X_Test.txt")
	yTrainData <- read.table("y_Train.txt")
	yTestData <- read.table("y_Test.txt")
	subjectTestData <- read.table("subject_test.txt")
	subjectTrainData <- read.table("subject_train.txt")
	columnNames <- read.table("features.txt")			

* Following segment of the script replaces the activity numbers in the data with activity labels such as "Walking", etc.

	activityLabels <- read.table("activity_labels.txt")
	factors <- factor(yTestData$V1)
	levels(factors) <- c(as.character(activityLabels$V2))
	yTestData$V1 <- factors
	factors <- factor(yTrainData$V1)
	levels(factors) <- c(as.character(activityLabels$V2))
	yTrainData$V1 <- factors

Then the training data is created by by merging training data set, subjects and activity information into a single data frame, named "trainData". Similar process is followed for test data set.

	trainData <- cbind(subjectTrainData, yTrainData, xTrainData)

	testData <- cbind(subjectTestData, yTestData, xTestData)

Then both above data sets are merged to create a merged data set with all the feature vectors and the activity and subject information. 

	mergedData <- rbind(trainData, testData)
	
The following prepares the descriptive variable names by cleaning the names loaded from the file "features.txt". Basic approach to cleaning the variable names is that variables are transformed into Camel Case notation because the names are very long and has several words connected to each other which cannot be read clearly. Variable names are made more readable by making the first letter of the each word
capital. Special characters such as "(), -" were removed. While several ideas from tidy data approach has been used here in cleaning	up the variable names, some are ignored to improve the readability for meaning. Variable names which show data for different axes are shown with ".Xaxis", ".Yaxis", etc. at the end of the variable name. While R is not very good in handling special characters in variable names, period are OK. Abbreviated names have been expanded such as "Acc" to "Acceleration", "std" to "StDev", etc. Several variable names start with Time or FFT representing either the Time series data or Fast Fourier Transform data.

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

The following adds Column Headers for Subject and Activity Labels and they are placed as the descriptive variable names in the right place in the merged data.

	vec1 <- c(1, "Subject")
	vec2 <- c(2, "Activity")
	columnNames1 <- rbind(vec1, vec2, columnNames)
	colnames(mergedData) <- c(columnNames1[ , 2])

The following steps are executed to correctly extract the data columns with means and standard deviations from the merged data. It includes separating Subjects and Activity Labels from "mergedData" before extracting the columns. Also, all the columns with descriptive variable names with mean and standard deviation are extracted. THis includes data with MeanFreq since it is understood from the authors' description that these values represent Weighted Average of frequency components of various frequency domain signal measurements. On the other hand, several measurements with "mean" in the descriptive names are not included because they are actually angular measurements between two vectors and in the data set they are denoted by "Angle Between....". 

	subjectsActivityData <- mergedData[, 1:2]
	columnsMeanStd1 <- grep("[Mm]ean|[Ss][Tt][Dd]", names(mergedData))
	meanStdData1 <- mergedData[, columnsMeanStd1]

Exclude the measurement data which include angle between two measurements

	columnsMeanStd2 <- grep("^[Angle]", names(meanStdData1))
	meanStdData <- meanStdData1[, -columnsMeanStd2]

The following combines the Subjects and Activity Labels info back with extracted data set, named "meanStdData"

	tidyData <- cbind(subjectsActivityData, meanStdData)
  
The following aggregates data frame "meanStdData" by Subjects and Activity Labels, returning average of each variable for each activity and each subject. This gives the final Tidy Data set, named "tidyData" which has a format of 14220 rows (30 subjects X 6 activities X 79 measurements with mean & std. deviation) and 4 columns. Each row in the data set is vector of subject, activity, the measurement and its mean value for that subject and activity. Results are exported to a text file, named "tidyData.txt".   

	meltedData <- melt(tidyData, id.vars = c("Subject", "Activity"))
	tidyData <- ddply(meltedData, c("Subject", "Activity", "variable"), summarise, Mean = mean(value))

	write.table(tidyData, "tidyData.txt", sep="\t", row.name=FALSE)