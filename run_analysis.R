library(data.table)
###Read data
features <- read.table("UCI HAR Dataset/features.txt")
activity_label <- read.table("UCI HAR Dataset/activity_labels.txt", header=FALSE)
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt",header=FALSE)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt",header=FALSE)
x_train <- read.table("UCI HAR Dataset/train/X_train.txt",header=FALSE)
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt",header=FALSE)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt",header=FALSE)
x_test <- read.table("UCI HAR Dataset/test/X_test.txt",header=FALSE)
###Merge training and test set
subjectMerged <- rbind(subject_train, subject_test)
activityMerged <- rbind(y_train, y_test)
featuresMerged <- rbind(x_train, x_test)
colnames(featuresMerged) <- t(features[2])
colnames(activityMerged) <- "Activity"
colnames(subjectMerged) <- "Subject"
data <- cbind(featuresMerged, activityMerged, subjectMerged)
###Extracts the measurements on mean and sd
meanSTDColumns <- grep(".*Mean.*|.*Std.*",names(data), ignore.case = TRUE)
requiredColumns <- c(meanSTDColumns, 562, 563)
extractData <- data[,requiredColumns]
###Name the activities
extractData$Activity <- as.character(extractData$Activity)
for (i in 1:6) {
  extractData$Activity[extractData$Activity == i] <- as.character(activity_label[i,2])
}
extractData$Activity <- as.factor(extractData$Activity)
###Label the data set with descriptive name
names(extractData)<-gsub("Acc", "Accelerometer", names(extractData))
names(extractData)<-gsub("Gyro", "Gyroscope", names(extractData))
names(extractData)<-gsub("BodyBody", "Body", names(extractData))
names(extractData)<-gsub("Mag", "Magnitude", names(extractData))
names(extractData)<-gsub("^t", "Time", names(extractData))
names(extractData)<-gsub("^f", "Frequency", names(extractData))
names(extractData)<-gsub("tBody", "TimeBody", names(extractData))
names(extractData)<-gsub("-mean()", "Mean", names(extractData), ignore.case = TRUE)
names(extractData)<-gsub("-std()", "STD", names(extractData), ignore.case = TRUE)
names(extractData)<-gsub("-freq()", "Frequency", names(extractData), ignore.case = TRUE)
names(extractData)<-gsub("angle", "Angle", names(extractData))
names(extractData)<-gsub("gravity", "Gravity", names(extractData))
###Export tidy data set
extractData$Subject <- as.factor(extractData$Subject)
extractData <- data.table(extractData)
tidyData <- aggregate(.~Subject+Activity,extractData, mean)
tidyData <- tidyData[order(tidyData$Subject, tidyData$Activity),]
write.table(tidyData, file="Tidy.txt", row.names = FALSE)