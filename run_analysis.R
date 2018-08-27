library (dplyr)

##Reading data

# Read training data
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
Y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt")
Sub_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

#Read test data
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
Y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt")
Sub_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

##Read features
Features <- read.table("./UCI HAR Dataset/features.txt")

##Read activity labels
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

## Merge the training and the test sets to create one data set
X_total <- rbind(X_train, X_test)
Y_total <- rbind(Y_train, Y_test)
Sub_total <- rbind(Sub_train, Sub_test)

## Extract only the measurements on the mean and standard deviation for each measurement

Mean_std <- Features[grep("mean\\(\\)|std\\(\\)",Features[,2]),]
X_total <- X_total[,Mean_std[,1]]

## Use descriptive activity names to name the activities in the data set
colnames(Y_total) <- "activity"
Y_total$activitylabel <- factor(Y_total$activity, labels = as.character(activity_labels[,2]))
activitylabel <- Y_total[,-1]

## Appropriately labels the data set with descriptive variable names
colnames(X_total) <- Features[Mean_std[,1],2]

## From the data set in step 4, create a second, independent tidy data set with the average
## of each variable for each activity and each subject
colnames(Sub_total) <- "subject"
total <- cbind(X_total, activitylabel, Sub_total)
total_mean <- total %>% group_by(activitylabel, subject) %>% summarize_each(funs(mean))
write.table(total_mean, file = "./UCI HAR Dataset/tidydata.txt", row.names = FALSE, col.names = TRUE)