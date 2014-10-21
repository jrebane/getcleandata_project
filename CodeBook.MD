#Code Book

 This document describes the variables, the data, and any transformations or work performed to clean up the source data.

# Description of Source Data

The raw data comes from the University of California Irvine Machine Learning Repository (UCI-MLR). It was donated by researchers from Universita degli Studi di Genova. It is intended for use by machine learning researchers to develop algorithms for detecting the type of activity a person wearing a smartphone is undertaking based upon readings from the accelerometer and gyroscope embedded in the smartphone.

These data can be retrieved from the following links:

Data file: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
Codebook: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

## Description Excerpt from Original dataset

All of the following excerpts are sourced from files by the original authors:

Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

### Data Set Information

> The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 
> 
> The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details.

Source: `README.txt`

### Attribute Information

> For each record in the dataset it is provided:
> 
> - Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
> - Triaxial Angular velocity from the gyroscope. 
> - A 561-feature vector with time and frequency domain variables. 
> - Its activity label. 
> - An identifier of the subject who carried out the experiment.

Source: `README.txt`

### Features Information

> The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated     into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 
> 
> Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 
> 
> Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

Source: `features_info.txt`

# Data Acquisition

The data is downloaded automatically by the script unless the zip file is found in the directory.

When the `run_analysis.R` script is run, the following steps take place to acquire and set up the data for processing:

1. The script tests to see if the file `activity.zip` exists in the working directory
2. If `activity.zip` does not exist in the working directory, the data set is downloaded from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip into the working directory and named `activity.zip` 
3. The data set is unzipped into the working directory - the associated files can now be found the in the `/UCI HAR Dataset/` directory

# Data Transformation

The following steps are taken to process and transform the data into a "tidy" data set.

1. The relevant files from the data directory are loaded in using the `read.table` function. 
    - Training data:
        + `/train/x_train.txt`
        + `/train/y_train.txt`
        + `/train/subject_train.txt`
    - Test data
        + `/test/x_test.txt`
        + `/test/y_test.txt`
        + `/test/subject_test.txt`
    - Measurement variable names
        + `/features.txt`
    - Values for activities performed by test subjects
        + `activity_labels.txt`
2. The variable labels from the `features.txt` file are applied as the column names for both the test and training data sets.
3. Activity and subject data are appended to the test and training data, creating new columns for each (`activity` and `subject`)
4. Test and training data are combined using the `rbind()` function
5. The combined data set is subsetted to include only the mean and standard deviation variables along with identifier data (as outlined by the assignment guidelines) using the following regular expression: `"(mean\\(\\)|std\\(\\))|subject|activity"`. This new data set has 68 variables.
6. Activity labels from `activity_labels.txt` are renamed to "walk", "walkup", "walkdown", "sitting", "standing", and "laying" to be lowercase and to remove underscores
7. Activity labels are applied to the pared data by activity ID
8. The `activity` and `subject` variables are brought to the front of the table
9. As per assignment instructions, the `activity` and `subject` variables are converted to factor variables using the `as.factor()` function
10. Updated easy-to-understand, informative, and tidy variable names are applied to the data frame
11. Using the `melt` and `dcast` functions in combination with the `mean` function, the dataset is tidied to embody the following characteristics:
    - One variable per column
    - A single observation per row (where each observation is the unique observation of a single subject and a single activity and the summary of means of all measures collected for that subject and activity) 
    - A table holding only elements of one kind
12. The tidy data set is written to a text file, `tidy_mean_data.txt`, using the `write.table()` function.

# Variables

The following is a description of the variables of the tidy data set resulting from the aforementioned transformations. 

The resulting dataset is a smaller extract of the original with 180 observations on 68 variables. These observations represent the average values of 66 numeric measures for six different activities performed by 30 different test participants.

## Identifiers

The identifiers for the observations are found in the first two columns of the data set and are described below:

| Column | Variable Name | Values    | Description                                                                                                                               |
|:------:|---------------|-----------|-------------------------------------------------------------------------------------------------------------------------------------------|
| [,1]   | subject       | integer   | id assigned to one of 30 test participants in original experiment                                                                         |
| [,2]   | activity      | character | name of the activity performed by the test participant. Value is one of the set ```{walk, walkup, walkdown, sitting, standing, laying}``` |


##  Measures

The following are the measures collected in the data set for each observation.

| Column | Variable Name                                                      | Original Name             | Values  |
|--------|--------------------------------------------------------------------|---------------------------|---------|
| [,3]   | meanAccelerationBodyPhoneXAxis                                     | tBodyAcc-mean(x)          | numeric |
| [,4]   | meanAccelerationBodyPhoneYAxis                                     | tBodyAcc-mean(y)          | numeric |
| [,5]   | meanAccelerationBodyPhoneZAxis                                     | tBodyAcc-mean(z)          | numeric |
| [,6]   | stddevAccelerationBodyPhoneXAxis                                   | tBodyAcc-std(x)           | numeric |
| [,7]   | stddevAccelerationBodyPhoneYAxis                                   | tBodyAcc-std(y)           | numeric |
| [,8]   | stddevAccelerationBodyPhoneZAxis                                   | tBodyAcc-std(z)           | numeric |
| [,9]   | meanAccelerationGravityPhoneXAxis                                  | tGravityAcc-mean(x)       | numeric |
| [,10]  | meanAccelerationGravityPhoneYAxis                                  | tGravityAcc-mean(y)       | numeric |
| [,11]  | meanAccelerationGravityPhoneZAxis                                  | tGravityAcc-mean(z)       | numeric |
| [,12]  | stdevAccelerationGravityPhoneXAxis                                 | tGravityAcc-std(x)        | numeric |
| [,13]  | stdevAccelerationGravityPhoneYAxis                                 | tGravityAcc-std(y)        | numeric |
| [,14]  | stdevAccelerationGravityPhoneZAxis                                 | tGravityAcc-std(z)        | numeric |
| [,15]  | meanAccelerationJerkSignalsPhoneXAxis                              | tBodyAccJerk-mean(x)      | numeric |
| [,16]  | meanAccelerationJerkSignalsPhoneYAxis                              | tBodyAccJerk-mean(y)      | numeric |
| [,17]  | meanAccelerationJerkSignalsPhoneZAxis                              | tBodyAccJerk-mean(z)      | numeric |
| [,18]  | stddevAccelerationJerkSignalsPhoneXAxis                            | tBodyAccJerk-std(x)       | numeric |
| [,19]  | stddevAccelerationJerkSignalsPhoneYAxis                            | tBodyAccJerk-std(y)       | numeric |
| [,20]  | stddevAccelerationJerkSignalsPhoneZAxis                            | tBodyAccJerk-std(z)       | numeric |
| [,21]  | meanAngularVelocityBodyPhoneXAxis                                  | tBodyGyro-mean(x)         | numeric |
| [,22]  | meanAngularVelocityBodyPhoneYAxis                                  | tBodyGyro-mean(y)         | numeric |
| [,23]  | meanAngularVelocityBodyPhoneZAxis                                  | tBodyGyro-mean(z)         | numeric |
| [,24]  | stddevAngularVelocityPhoneXAxis                                    | tBodyGyro-std(x)          | numeric |
| [,25]  | stddevAngularVelocityPhoneYAxis                                    | tBodyGyro-std(y)          | numeric |
| [,26]  | stddevAngularVelocityPhoneZAxis                                    | tBodyGyro-std(z)          | numeric |
| [,27]  | meanAngularVelocityJerkSignalsPhoneXAxis                           | tBodyGyroJerk-mean(x)     | numeric |
| [,28]  | meanAngularVelocityJerkSignalsPhoneYAxis                           | tBodyGyroJerk-mean(y)     | numeric |
| [,29]  | meanAngularVelocityJerkSignalsPhoneZAxis                           | tBodyGyroJerk-mean(z)     | numeric |
| [,30]  | stddevAngularVelocityJerkSignalsPhoneXAxis                         | tBodyGyroJerk-std(x)      | numeric |
| [,31]  | stddevAngularVelocityJerkSignalsPhoneYAxis                         | tBodyGyroJerk-std(y)      | numeric |
| [,32]  | stddevAngularVelocityJerkSignalsPhoneZAxis                         | tBodyGyroJerk-std(z)      | numeric |
| [,33]  | meanMagnitudeAccelerationBody                                      | tBodyAccMag-mean          | numeric |
| [,34]  | stddevMagnitudeAccelerationBody                                    | tBodyAccMag-std           | numeric |
| [,35]  | meanMagnitudeAccelerationGravity                                   | tGravityAccMag-mean       | numeric |
| [,36]  | stddevMagnitudeAccelerationGravity                                 | tGravityAccMag-std        | numeric |
| [,37]  | meanMagnitudeAccelerationBodyJerkSignals                           | tBodyAccJerkMag-mean      | numeric |
| [,38]  | stddevMagnitudeAccelerationBodyJerkSignals                         | tBodyAccJerkMag-std       | numeric |
| [,39]  | meanMagnitudeAngularVelocityBody                                   | tBodyGyroMag-mean         | numeric |
| [,40]  | stddevMagnitudeAngularVelocityBody                                 | tBodyGyroMag-std          | numeric |
| [,41]  | meanMagnitudeAngularVelocityBodyJerkSignals                        | tBodyGyroJerkMag-mean     | numeric |
| [,42]  | stddevMagnitudeAngularVelocityBodyJerkSignals                      | tBodyGyroJerkMag-std      | numeric |
| [,43]  | meanFrequencyDomainSignalsAccelerationBodyPhoneXAxis               | fBodyAcc-mean(x)          | numeric |
| [,44]  | meanFrequencyDomainSignalsAccelerationBodyPhoneYAxis               | fBodyAcc-mean(y)          | numeric |
| [,45]  | meanFrequencyDomainSignalsAccelerationBodyPhoneZAxis               | fBodyAcc-mean(z)          | numeric |
| [,46]  | stddevFrequencyDomainSignalsAccelerationBodyPhoneXAxis             | fBodyAcc-std(x)           | numeric |
| [,47]  | stddevFrequencyDomainSignalsAccelerationBodyPhoneYAxis             | fBodyAcc-std(y)           | numeric |
| [,48]  | stddevFrequencyDomainSignalsAccelerationBodyPhoneZAxis             | fBodyAcc-std(z)           | numeric |
| [,49]  | meanFrequencyDomainSignalsAccelerationJerkSignalsPhoneXAxis        | fBodyAccJerk-mean(x)      | numeric |
| [,50]  | meanFrequencyDomainSignalsAccelerationJerkSignalsPhoneYAxis        | fBodyAccJerk-mean(y)      | numeric |
| [,51]  | meanFrequencyDomainSignalsAccelerationJerkSignalsPhoneZAxis        | fBodyAccJerk-mean(z)      | numeric |
| [,52]  | stddevFrequencyDomainSignalsAccelerationJerkSignalsPhoneXAxis      | fBodyAccJerk-std(x)       | numeric |
| [,53]  | stddevFrequencyDomainSignalsAccelerationJerkSignalsPhoneYAxis      | fBodyAccJerk-std(y)       | numeric |
| [,54]  | stddevFrequencyDomainSignalsAccelerationJerkSignalsPhoneZAxis      | fBodyAccJerk-std(z)       | numeric |
| [,55]  | meanFrequencyDomainSignalAngularVelocityBodyPhoneXAxis             | fBodyGyro-mean(x)         | numeric |
| [,56]  | meanFrequencyDomainSignalAngularVelocityBodyPhoneYAxis             | fBodyGyro-mean(y)         | numeric |
| [,57]  | meanFrequencyDomainSignalAngularVelocityBodyPhoneZAxis             | fBodyGyro-mean(z)         | numeric |
| [,58]  | stddevFrequencyDomainSignalAngularVelocityPhoneXAxis               | fBodyGyro-std(x)          | numeric |
| [,59]  | stddevFrequencyDomainSignalAngularVelocityPhoneYAxis               | fBodyGyro-std(y)          | numeric |
| [,60]  | stddevFrequencyDomainSignalAngularVelocityPhoneZAxis               | fBodyGyro-std(z)          | numeric |
| [,61]  | meanFrequencyDomainSignalMagnitudeAccelerationBody                 | fBodyAccMag-mean          | numeric |
| [,62]  | stddevFrequencyDomainSignalMagnitudeAccelerationBody               | fBodyAccMag-std           | numeric |
| [,63]  | meanFrequencyDomainSignalMagnitudeAccelerationBodyJerkSignals      | fBodyBodyAccJerkMag-mean  | numeric |
| [,64]  | stddevFrequencyDomainSignalMagnitudeAccelerationBodyJerkSignals    | fBodyBodyAccJerkMag-std   | numeric |
| [,65]  | meanFrequencyDomainSignalMagnitudeAngularVelocityBody              | fBodyBodyGyroMag-mean     | numeric |
| [,66]  | stddevFrequencyDomainSignalMagnitudeAngularVelocityBody            | fBodyBodyGyroMag-std      | numeric |
| [,67]  | meanFrequencyDomainSignalMagnitudeAngularVelocityBodyJerkSignals   | fBodyBodyGyroJerkMag-mean | numeric |
| [,68]  | stddevFrequencyDomainSignalMagnitudeAngularVelocityBodyJerkSignals | fBodyBodyGyroJerkMag-std  | numeric |


## Variable naming

The variables have been named with the following components, with mean or std leading the variable name:

| Component             | Definition                                         |
|-----------------------|----------------------------------------------------|
| mean                  | Average Value                                      |
| std                   | Standard Deviation                                 |
| FrequencyDomainSignal | Frequency                                          |
| Magnitude             | Euclidean Magnitude                                |
| Acceleration          | Accelerometer Measure                              |
| Angular Velocity      | Gyroscope measure                                  |
| X, Y, Z Axis          | Corresponding Axis                                 |
| Body                  | Body of Phone as Reference Frame                   |
| Gravity               | Gravity as Reference Frame                         |
| Jerk                  | Jerk Signal (vs. Smooth Signal for other measures) |
