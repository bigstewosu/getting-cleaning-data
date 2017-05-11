# This script will perform the following steps on the UCI HAR Dataset downloaded from
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merge the training and the test sets to create one data set
# 2. Extract only the measurements on the mean and standard deviation for each measurement
# 3. Use descriptive activity names to name the activities in the dat set
# 4. Appropriately label the data set with descriptive activity names
# 5. Create a second, independent, tidy data set with the average of each variable for each activity and each subject

#clean up workspace
rm(list=ls())
   
#merge the training and test sets into one   

setwd('/Users/dale.stewart/datasciencecoursera/GetCleanData/UCI HAR Dataset/')

# read in training data
features <- read.table('./features.txt', header = FALSE)
activityType <- read.table('./activity_labels.txt', header = FALSE)
TrainSubject <- read.table('./train/subject_train.txt', header = FALSE)
x_Train <- read.table('./train/x_train.txt', header = FALSE)
y_Train <- read.table('./train/y_train.txt', header = FALSE)

# set column names
colnames(activityType) <- c('activityId','activityType')
colnames(TrainSubject) <- "subjectId"
colnames(x_Train) <- features[,2]
colnames(y_Train) <- "activityId"

#bind all training data together
trainData <- cbind(y_Train, TrainSubject, x_Train)

#read in test data
TestSubject <- read.table('./test/subject_test.txt', header = FALSE)
x_Test <- read.table('./test/x_test.txt', header = FALSE)
y_Test <- read.table('./test/y_test.txt', header = FALSE)

# set column names
colnames(TestSubject) <- "subjectId"
colnames(x_Test) <- features[,2]
colnames(y_Test) <- "activityId"

# bind all test data together
testData <- cbind(y_Test, TestSubject, x_Test)

# bind the test and the training data together
finalData <- rbind(trainData, testData)

# create a vector for the column names to test on later
finalCols <- colnames(finalData)

# create a logical vector that brings back TRUE for ID, mean, and stdev columns

logVector <- (grepl("activity..", finalCols) | grepl("subject..",finalCols) | grepl("-mean..",finalCols) & !grepl("-meanFreq..",finalCols) & !grepl("mean..-",finalCols) | grepl("-std..",finalCols) & !grepl("-std()..-",finalCols))

# subset finalData table on the TRUE columns from the logical vector
finalData <- finalData[logVector==TRUE]

####Use descriptive activity names to name the activities in the data set

finalData <- merge(finalData, activityType, by='activityId', all.x = TRUE)

finalCols <- colnames(finalData)

for (i in 1:length(finalCols)) {
        finalCols[i] = gsub("\\()","", finalCols[i])
        finalCols[i] = gsub("-std$","StdDev", finalCols[i])
        finalCols[i] = gsub("-mean","Mean", finalCols[i])
        finalCols[i] = gsub("^(t)","time", finalCols[i])
        finalCols[i] = gsub("^(f)","freq", finalCols[i])
        finalCols[i] = gsub("[Gg]ravity","Gravity", finalCols[i])
        finalCols[i] = gsub("[Bb]ody[Bb]ody[Bb]ody","Body", finalCols[i])
        finalCols[i] = gsub("[Gg]yro","Gyro", finalCols[i])
        finalCols[i] = gsub("AccMag","AccMagnitude", finalCols[i])
        finalCols[i] = gsub("[Bb]odyaccjerkmag","BodyAccJerkMagnitude", finalCols[i])
        finalCols[i] = gsub("JerkMag","JerkMagnitude", finalCols[i])
        finalCols[i] = gsub("GyroMag","GyroMagnitude", finalCols[i])
}

colnames(finalData) <- finalCols

####Create a second independent tidy data set

finalDataNoActivityType <- finalData[, names(finalData) != 'activityType']

tidyData <- aggregate(finalDataNoActivityType[, names(finalDataNoActivityType) != c('activityId', 'subjectId')], by = list(activityId=finalDataNoActivityType$activityId, subjectId=finalDataNoActivityType$subjectId), mean)

tidyData <- merge(tidyData, activityType, by = 'activityId', all.x = TRUE)

write.table(tidyData, './tidyData.txt', row.names = FALSE, sep = '\t')





