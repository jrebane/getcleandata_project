## Load required libraries
require(dplyr)
require(lubridate)
require(ggplot2)
require(ggthemes)
require(timeDate)
library(reshape2)

## Loading and preprocessing the data

fURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFileName <- "./activity.zip"

## If csv file not in working directory, download from repository and extract to working directory
if (!file.exists(zipFileName)){
        message("Downloading and unpacking zip file")
        zipFile <- download.file(url = fURL,destfile = zipFileName, method='auto')
        unzip(zipFileName)
} else {message("Data file found in working directory")}


xtrain <- read.table(file = "./UCI HAR Dataset/train/X_train.txt")
ytrain <- read.table(file = "./UCI HAR Dataset/train/y_train.txt")
subjecttrain <- read.table(file = "./UCI HAR Dataset/train/subject_train.txt")

xtest <- read.table(file = "./UCI HAR Dataset/test/X_test.txt")
ytest <- read.table(file = "./UCI HAR Dataset/test/y_test.txt")
subjecttest <- read.table(file = "./UCI HAR Dataset/test/subject_test.txt")

ylabels <- read.table(file = "./UCI HAR Dataset/features.txt")
activitylabels <- read.table(file = "./UCI HAR Dataset/activity_labels.txt")

names(xtrain) <- ylabels$V2
xtrain$activity <- ytrain$V1
xtrain$subject <- subjecttrain$V1

names(xtest) <- ylabels$V2
xtest$activity <- ytest$V1
xtest$subject <- subjecttest$V1

combineddata <- rbind(xtrain, xtest)

pareddata <- combineddata[,grep("(mean\\(\\)|std\\(\\))|subject|activity", colnames(combineddata))]

activitylabels$V2 <- c("walk", "walkup", "walkdown", "sitting", "standing", "laying")

pareddata$activity <- activitylabels$V2[match(pareddata$activity,activitylabels$V1)]
pareddata <- pareddata[,c(68, 67, 1:66)]

pareddata$activity <- as.factor(pareddata$activity)
pareddata$subject <- as.factor(pareddata$subject)

colnames(pareddata)[3:5] <- c(
        "meanAccelerationBodyPhoneXAxis",
        "meanAccelerationBodyPhoneYAxis",
        "meanAccelerationBodyPhoneZAxis")
colnames(pareddata)[6:8] <- c(
        "stddevAccelerationBodyPhoneXAxis",
        "stddevAccelerationBodyPhoneYAxis",
        "stddevAccelerationBodyPhoneZAxis")
colnames(pareddata)[9:11] <- c(
        "meanAccelerationGravityPhoneXAxis",
        "meanAccelerationGravityPhoneYAxis",
        "meanAccelerationGravityPhoneZAxis")
colnames(pareddata)[12:14] <- c(
        "stdevAccelerationGravityPhoneXAxis",
        "stdevAccelerationGravityPhoneYAxis",
        "stdevAccelerationGravityPhoneZAxis")
colnames(pareddata)[15:17] <- c(
        "meanAccelerationJerkSignalsPhoneXAxis",
        "meanAccelerationJerkSignalsPhoneYAxis",
        "meanAccelerationJerkSignalsPhoneZAxis")
colnames(pareddata)[18:20] <- c(
        "stddevAccelerationJerkSignalsPhoneXAxis",
        "stddevAccelerationJerkSignalsPhoneYAxis",
        "stddevAccelerationJerkSignalsPhoneZAxis")
colnames(pareddata)[21:23] <- c(
        "meanAngularVelocityBodyPhoneXAxis",
        "meanAngularVelocityBodyPhoneYAxis",
        "meanAngularVelocityBodyPhoneZAxis")
colnames(pareddata)[24:26] <- c(
        "stddevAngularVelocityPhoneXAxis",
        "stddevAngularVelocityPhoneYAxis",
        "stddevAngularVelocityPhoneZAxis")
colnames(pareddata)[27:29] <- c(
        "meanAngularVelocityJerkSignalsPhoneXAxis",
        "meanAngularVelocityJerkSignalsPhoneYAxis",
        "meanAngularVelocityJerkSignalsPhoneZAxis")
colnames(pareddata)[30:32] <- c(
        "stddevAngularVelocityJerkSignalsPhoneXAxisstd",
        "stddevAngularVelocityJerkSignalsPhoneYAxisstd",
        "stddevAngularVelocityJerkSignalsPhoneZAxisstd")
colnames(pareddata)[33:34] <- c(
        "meanMagnitudeAccelerationBody",
        "stddevMagnitudeAccelerationBody")
colnames(pareddata)[35:36] <- c(
        "meanMagnitudeAccelerationGravity",
        "stddevMagnitudeAccelerationGravity")
colnames(pareddata)[37:38] <- c(
        "meanMagnitudeAccelerationBodyJerkSignals",
        "stddevMagnitudeAccelerationBodyJerkSignals")
colnames(pareddata)[39:40] <- c(
        "meanMagnitudeAngularVelocityBody",
        "stddevMagnitudeAngularVelocityBody")
colnames(pareddata)[41:42] <- c(
        "meanMagnitudeAngularVelocityBodyJerkSignals",
        "stddevMagnitudeAngularVelocityBodyJerkSignals")
colnames(pareddata)[43:45] <- c(
        "meanFrequencyDomainSignalsAccelerationBodyPhoneXAxis",
        "meanFrequencyDomainSignalsAccelerationBodyPhoneYAxis",
        "meanFrequencyDomainSignalsAccelerationBodyPhoneZAxis")
colnames(pareddata)[46:48] <- c(
        "stddevFrequencyDomainSignalsAccelerationBodyPhoneXAxis",
        "stddevFrequencyDomainSignalsAccelerationBodyPhoneYAxis",
        "stddevFrequencyDomainSignalsAccelerationBodyPhoneZAxis")
colnames(pareddata)[49:51] <- c(
        "meanFrequencyDomainSignalsAccelerationJerkSignalsPhoneXAxis",
        "meanFrequencyDomainSignalsAccelerationJerkSignalsPhoneYAxis",
        "meanFrequencyDomainSignalsAccelerationJerkSignalsPhoneZAxis")
colnames(pareddata)[52:54] <- c(
        "stddevFrequencyDomainSignalsAccelerationJerkSignalsPhoneXAxis",
        "stddevFrequencyDomainSignalsAccelerationJerkSignalsPhoneYAxis",
        "stddevFrequencyDomainSignalsAccelerationJerkSignalsPhoneZAxis")
colnames(pareddata)[55:57] <- c(
        "meanFrequencyDomainSignalAngularVelocityBodyPhoneXAxis",
        "meanFrequencyDomainSignalAngularVelocityBodyPhoneYAxis",
        "meanFrequencyDomainSignalAngularVelocityBodyPhoneZAxis")
colnames(pareddata)[58:60] <- c(
        "stddevFrequencyDomainSignalAngularVelocityPhoneXAxis",
        "stddevFrequencyDomainSignalAngularVelocityPhoneYAxis",
        "stddevFrequencyDomainSignalAngularVelocityPhoneZAxis")
colnames(pareddata)[61:62] <- c(
        "meanMagnitudeFrequencyDomainSignalAccelerationBody",
        "stddevMagnitudeFrequencyDomainSignalAccelerationBody")
colnames(pareddata)[63:64] <- c(
        "meanMagnitudeFrequencyDomainSignalAccelerationBodyJerkSignals",
        "stddevMagnitudeFrequencyDomainSignalAccelerationBodyJerkSignals")
colnames(pareddata)[65:66] <- c(
        "meanMagnitudeFrequencyDomainSignalAngularVelocityBody",
        "stddevMagnitudeFrequencyDomainSignalAngularVelocityBody")
colnames(pareddata)[67:68] <- c(
        "meanMagnitudeFrequencyDomainSignalAngularVelocityBodyJerkSignals",
        "stddevMagnitudeFrequencyDomainSignalAngularVelocityBodyJerkSignals")

melted_data <- melt(pareddata, id.vars=c("subject", "activity"))
tidy_mean_data <- dcast(melted_data, subject + activity ~ variable, mean)

write.table(tidy_mean_data, "tidy_mean_data.txt", row.name=FALSE)