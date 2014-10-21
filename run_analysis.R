# Load required libraries
require(dplyr)
library(reshape2)

# The data set is unzipped into the working directory - the associated files 
# can now be found the in the `/UCI HAR Dataset/` directory
fURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFileName <- "./activity.zip"

# If 'activity.zip' file not in working directory, download from link and 
# extract to working directory
if (!file.exists(zipFileName)){
        message("Downloading and unpacking zip file")
        zipFile <- download.file(url = fURL,destfile = zipFileName, 
                                 method='auto')
        unzip(zipFileName)
} else {message("Data file found in working directory")}

# First, the relevant files from the data directory are loaded in using the 
# `read.table` function. 

## Training data
xtrain <- read.table(file = "./UCI HAR Dataset/train/X_train.txt")
ytrain <- read.table(file = "./UCI HAR Dataset/train/y_train.txt")
subjecttrain <- read.table(file = "./UCI HAR Dataset/train/subject_train.txt")

## Test data
xtest <- read.table(file = "./UCI HAR Dataset/test/X_test.txt")
ytest <- read.table(file = "./UCI HAR Dataset/test/y_test.txt")
subjecttest <- read.table(file = "./UCI HAR Dataset/test/subject_test.txt")

## List of measurement variable names
ylabels <- read.table(file = "./UCI HAR Dataset/features.txt")

## List of values for activities performed by test subjects
activitylabels <- read.table(file = "./UCI HAR Dataset/activity_labels.txt")

# The variable labels are applied as the column names for both the test and 
# training data sets.
names(xtrain) <- ylabels$V2
names(xtest) <- ylabels$V2

# Activity and subject data are appended to the test and training data, 
# creating new columns for each
xtrain$activity <- ytrain$V1
xtrain$subject <- subjecttrain$V1
xtest$activity <- ytest$V1
xtest$subject <- subjecttest$V1

# Test and training data are combined using the `rbind()` function
combineddata <- rbind(xtrain, xtest)

# The combined data set is subsetted to include only the mean and standard 
# deviation variables along with identifier data (as outlined by the assignment 
# guidelines) using regular expressions. This new data set has 68 variables.
pareddata <- combineddata[,grep("(mean\\(\\)|std\\(\\))|subject|activity", 
                                colnames(combineddata))]

# Activity labels from `activity_labels.txt` are renamed
activitylabels$V2 <- c("walk", "walkup", "walkdown", "sitting", "standing", 
                       "laying")

# Activity labels are applied to the pared data by activity ID
pareddata$activity <- activitylabels$V2[match(pareddata$activity,
                                              activitylabels$V1)]

# The `activity` and `subject` variables are brought to the front of the table
pareddata <- pareddata[,c(68, 67, 1:66)]

# As per assignment instructions, the `activity` and `subject` variables are 
# converted to factor variables using the `as.factor()` function
pareddata$activity <- as.factor(pareddata$activity)
pareddata$subject <- as.factor(pareddata$subject)

# Updated, clear, and tidy variable names are applied to the data frame
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
        "stddevAngularVelocityJerkSignalsPhoneXAxis",
        "stddevAngularVelocityJerkSignalsPhoneYAxis",
        "stddevAngularVelocityJerkSignalsPhoneZAxis")
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

# Using the `melt` and `dcast` functions in combination with the `mean` function, the dataset is tidied to embody the following characteristics:
#   - One variable per column
#   - A single observation per row (where each observation is the unique observation of a single subject and a single activity and the summary of means of all measures collected for that subject and activity) 
#   - A table holding only elements of one kind
melted_data <- melt(pareddata, id.vars=c("subject", "activity"))
tidy_mean_data <- dcast(melted_data, subject + activity ~ variable, mean)

# The tidy data set is written to a text file, `tidy_mean_data.txt`, using the `write.table()` function.
write.table(tidy_mean_data, "tidy_mean_data.txt", row.name=FALSE)