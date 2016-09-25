########
## Part 1- Merge the training and the test sets to create one data set.

# setting the working directory on my computer
setwd('/Users/Sheila/Documents/HAR Dataset/');

# (previously read the necessary data)
# Creating the data set
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

# Name the columns
colnames(features) <- t(featureNames[2])

# Merge the data
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)

## Part 2- Extracts only the measurements on the mean and standard deviation for each measurement.

columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

# Looking at the dimensions
requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)
# and again
extractedData <- completeData[,requiredColumns]
dim(extractedData)

## Part 3- Uses descriptive activity names to name the activities in the data set
# changing the type here
extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])

# Adding a factor
extractedData$Activity <- as.factor(extractedData$Activity)

## Part 4- Appropriately labels the data set with descriptive variable names.
names(extractedData)

# Replacing names
names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

## Part 5 - From the data set in step 4, creates a second, independent tidy data set with the 
average of each variable for each activity and each subject

# Setting a subject 
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

# Creating tidy data
tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)
}
