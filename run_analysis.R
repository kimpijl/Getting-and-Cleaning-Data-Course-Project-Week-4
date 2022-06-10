library(dplyr)

file_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destfile <- "UCI HAR Dataset.zip"

download.file(file_url, destfile)

unzip(destfile)
datapath <- "UCI HAR Dataset"

xtrain <- read.table(file.path(datapath, "train", "X_train.txt"),header = FALSE)
ytrain <- read.table(file.path(datapath, "train", "y_train.txt"),header = FALSE)
subject_train <- read.table(file.path(datapath, "train", "subject_train.txt"),header = FALSE)

xtest <- read.table(file.path(datapath, "test", "X_test.txt"),header = FALSE)
ytest <- read.table(file.path(datapath, "test", "y_test.txt"),header = FALSE)
subject_test <- read.table(file.path(datapath, "test", "subject_test.txt"),header = FALSE)

features <- read.table(file.path(datapath, "features.txt"),header = FALSE)
activityLabels <- read.table(file.path(datapath, "activity_labels.txt"),header = FALSE)

#ascribing appropriate columnames to each dataset
colnames(xtrain) <- features[,2]
colnames(ytrain) <- "activityId"
colnames(subject_train) <- "subjectId"

colnames(xtest) <- features[,2]
colnames(ytest) <- "activityId"
colnames(subject_test) <- "subjectId"

colnames(activityLabels) <- c("activityId", "activityLabel")

#putting all sets together, first by columnbind, than by rowbind
aggregated_data <- rbind(
  cbind(subject_train, ytrain, xtrain),
  cbind(subject_test, ytest, xtest)
)


#columns that comply with the conditions (hence, the columnname should contain either of the following: "subject|activity|mean|std")

columns <- grepl("subject|activity|mean|std", colnames(aggregated_data))

#subsetting the data based on the columns that complied with the condition
subset_std_mean <- aggregated_data[, columns]

#adding descriptive labels to each activity number
subset_std_mean$activityId <- factor(subset_std_mean$activityId, 
                                 levels = activityLabels[, 1], labels = activityLabels[, 2])

column_names <- colnames(subset_std_mean)

#giving descriptive names to the columns

column_names <- gsub("^f", "frequency_domain", column_names)
column_names <- gsub("^t", "time_domain", column_names)
column_names <- gsub("Acc", "Accelerometer", column_names)
column_names <- gsub("Gyro", "Gyroscope", column_names)
column_names <- gsub("Mag", "Magnitude", column_names)
column_names <- gsub("Freq", "Frequency", column_names)
column_names <- gsub("std", "standard_deviation", column_names)

#assign newly created column names to earlier dataset
colnames(subset_std_mean) <- column_names

#average each column by subject and activity
tidydata <- subset_std_mean %>% group_by(subjectId, activityId) %>% summarise_each(funs(mean)) 

#create a .txt file containing the tidy data

write.table(tidydata, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)




