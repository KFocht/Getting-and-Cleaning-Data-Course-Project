#Read in the tables:
subjectTest <- read.table("subject_test.txt")
xTest <- read.table("X_test.txt")
yTest <- read.table("y_test.txt")
subjectTrain <- read.table("subject_train.txt")
xTrain <- read.table("X_train.txt")
yTrain <- read.table("y_train.txt")
featuresLabels <- read.table("features.txt", stringsAsFactors = FALSE)

##Bind the tables together:
subjects <- rbind(subjectTest, subjectTrain)
activities <- rbind(yTest, yTrain)
features <- rbind(xTest, xTrain)
data <- cbind(subjects, activities, features)

##Add column names:
colnames(data) <- c("Subject", "Activity", featuresLabels[,2])

##Extract mean and std columns:
dataExtract <- data[,c("Subject", "Activity", colnames(data)[grep("mean",colnames(data))],
                       colnames(data)[grep("std", colnames(data))])]

##Add descriptive names to Activity column:
dataExtract$Activity[dataExtract$Activity == 1] = "Walking"
dataExtract$Activity[dataExtract$Activity == 2] = "Walking Upstairs"
dataExtract$Activity[dataExtract$Activity == 3] = "Walking Downstairs"
dataExtract$Activity[dataExtract$Activity == 4] = "Sitting"
dataExtract$Activity[dataExtract$Activity == 5] = "Standing"
dataExtract$Activity[dataExtract$Activity == 6] = "Laying"

##Tidy up the labels:
colnames(dataExtract) <- c("Subject", "Activity", "tBodyAccXMean", "tBodyAccYMean",
                           "tBodyAccZMean", "tGravityAccXMean", "tGravityAccYMean",
                           "tGravityAccZMean", "tBodyAccJerkXMean", "tBodyAccJerkYMean",
                           "tBodyAccJerkZMean", "tBodyGyroXMean", "tBodyGyroYMean",
                           "tBodyGyroZMean", "tBodyGyroJerkXMean", "tBodyGyroJerkYMean",
                           "tBodyGyroJerkZMean", "tBodyAccMagMean", "tGravityAccMagMean",
                           "tBodyAccJerkMagMean", "tBodyGyroMagMean", "tBodyGyroJerkMagMean",
                           "fBodyAccXMean", "fBodyAccYMean", "fBodyAccZMean",
                           "fBodyAccXMeanFrequency", "fBodyAccYMeanFrequency",
                           "fBodyAccZMeanFrequency", "fBodyAccJerkXMean",
                           "fBodyAccJerkYMean", "fBodyAccJerkZMean",
                           "fBodyAccJerkXMeanFrequency", "fBodyAccJerkYMeanFrequency",
                           "fBodyAccJerkZMeanFrequency", "fBodyGyroXMean", "fBodyGyroYMean",
                           "fBodyZMean", "fBodyGyroXMeanFrequency",
                           "fBodyGyroYMeanFrequency", "fBodyGyroZMeanFrequency",
                           "fBodyAccMagMean", "fBodyAccMagMeanFrequency",
                           "fBodyAccJerkMagMean", "fBodyAccJerkMagMeanFrequency",
                           "fBodyGyroMagMean", "fBodyGyroMagMeanFrequency",
                           "fBodyGyroJerkMagMean", "fBodyGyroJerkMagMeanFrequency",
                           "tBodyAccXStandardDeviation", "tBodyAccYStandardDeviation",
                           "tBodyAccZStandardDeviation", "tGravityAccXStandardDeviation",
                           "tGravityAccYStandardDeviation", "tGravityAccZStandardDeviation",
                           "tBodyAccJerkXStandardDeviation", "tBodyAccJerkYStandardDeviation",
                           "tBodyAccJerkZStandardDeviation", "tBodyGyroXStandardDeviation",
                           "tBodyGyroYStandardDeviation", "tBodyGyroZStandardDeviation",
                           "tBodyGyroJerkXStandardDeviation", "tBodyGyroJerkYStandardDeviation",
                           "tBodyGyroJerkZStandardDeviation", "tBodyAccMagStandardDeviation",
                           "tGravityAccMagStandardDeviation", "tBodyAccJerkMagStandardDeviation",
                           "tBodyGyroMagStandardDeviation", "tBodyGyroJerkMagStandardDeviation",
                           "fBodyAccXStandardDeviation", "fBodyAccYStandardDeviation",
                           "fBodyAccZStandardDeviation", "fBodyAccJerkXStandardDeviation",
                           "fBodyAccJerkYStandardDeviation", "fBodyAccJerkZStandardDeviation",
                           "fBodyGyroXStandardDeviation", "fBodyGyroYStandardDeviation",
                           "fBodyGyroZStandardDeviation", "fBodyAccMagStandardDeviation",
                           "fBodyAccJerkMagStandardDeviation", "fBodyGyroMagStandardDeviation",
                           "fBodyGyroJerkMagStandardDeviation")

##Split data by Activity, then aggregate by Subject and find means:
library(dplyr)
walkingData <- filter(dataExtract, Activity == "Walking")
walkingMeans <- aggregate(walkingData[,3:81], walkingData["Subject"],
                            function(x) mean(x))
Activity <- rep("Walking", 30)
walkingMeans <- cbind(Activity, walkingMeans)

walkingUpstairsData <- filter(dataExtract, Activity == "Walking Upstairs")
walkingUpstairsMeans <- aggregate(walkingUpstairsData[,3:81], walkingUpstairsData["Subject"],
                                    function(x) mean(x))
Activity <- rep("Walking Upstairs", 30)
walkingUpstairsMeans <- cbind(Activity, walkingUpstairsMeans)

walkingDownstairsData <- filter(dataExtract, Activity == "Walking Downstairs")
walkingDownstairsMeans <- aggregate(walkingDownstairsData[,3:81], walkingDownstairsData["Subject"],
                                    function(x) mean(x))
Activity <- rep("Walking Downstairs", 30)
walkingDownstairsMeans <- cbind(Activity, walkingDownstairsMeans)

sittingData <- filter(dataExtract, Activity == "Sitting")
sittingMeans <- aggregate(sittingData[,3:81], sittingData["Subject"],
                            function(x) mean(x))
Activity <- rep("Sitting", 30)
sittingMeans <- cbind(Activity, sittingMeans)

standingData <- filter(dataExtract, Activity == "Standing")
standingMeans <- aggregate(standingData[,3:81], standingData["Subject"],
                            function(x) mean(x))
Activity <- rep("Standing", 30)
standingMeans <- cbind(Activity, standingMeans)

layingData <- filter(dataExtract, Activity == "Laying")
layingMeans <- aggregate(layingData[,3:81], layingData["Subject"],
                            function(x) mean(x))
Activity <- rep("Laying", 30)
layingMeans <- cbind(Activity, layingMeans)


##Combine all Activity Means datasets into one tidy datset:
tidyData <- rbind(walkingMeans, walkingDownstairsMeans, walkingUpstairsMeans, sittingMeans,
      standingMeans, layingMeans)
