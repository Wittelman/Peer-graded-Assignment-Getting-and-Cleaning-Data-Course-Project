#Step 0.create destination directory,load, and unzip file in said directory
if(!file.exists("./data")){dir.create("./data")}

#Here are the data for the project:

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip")

# Unzip dataSet to /data directory
unzip(zipfile="./data/Dataset.zip",exdir="./data")



#Step 1.Merges the training and the test sets to create one data set.


# 1.1 Reading files

# 1.1.1  Reading trainings tables:

x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")

# 1.1.2 Reading testing tables:
x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

# 1.1.3 Reading feature vector:
features <- read.table('./data/UCI HAR Dataset/features.txt')

# 1.1.4 Reading activity labels:
activityLabels <- read.table("./data/UCI HAR Dataset/activity_labels.txt")

#1.2 Assigning column names
colnames(x_train)<-features[,2]
colnames(y_train)<-"activityId"
colnames(subject_train)<-"subjectId"

colnames(x_test)<-features[,2]
colnames(y_test)<-"activityId"
colnames(subject_test)<-"subjectId"

colnames(activityLabels)<-c("activityId","activityType")

#1.3 Merging data tables
train_mrg<-cbind(y_train,subject_train,x_train)
test_mrg <- cbind(y_test, subject_test, x_test)
final_mrg <- rbind(train_mrg, test_mrg)

#__________________________________________________________________
#Step 2.-Extracts only the measurements on the mean and standard deviation for each measurement.
#__________________________________________________________________

#2.1 Reading column names:

final_col <- colnames(final_mrg)

#2.2 Create vector for defining ID, mean and standard deviation:

mean_and_std <- (grepl("activityId" , final_col) | 
                   grepl("subjectId" , final_col) | 
                   grepl("mean.." , final_col) | 
                   grepl("std.." , final_col) 
)

#2.3 Making nessesary subset from setAllInOne:

mean_and_std_set <- final_mrg[ , mean_and_std == TRUE]

#__________________________________________________________________
#Step 3. Uses descriptive activity names to name the activities in the data set
#__________________________________________________________________

final_set<-merge(mean_and_std_set,activityLabels,by="activityId",all.x=TRUE)

#__________________________________________________________________
#Step 4. Appropriately labels the data set with descriptive variable names.
#__________________________________________________________________

#Done in previous steps, see 1.3,2.2 and 2.3

#__________________________________________________________________
#Step 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#__________________________________________________________________

#5.1 Making a second tidy data set

library(dplyr)
Tidydata<-final_set %>%
  group_by(subjectId,activityId) %>%
  summarise_each(funs(mean))

Tidydata <- Tidydata[order(Tidydata$subjectId, Tidydata$activityId),]

#5.2 Writing second tidy data set in txt file

write.table(Tidydata, "Tidydata.txt", row.name=FALSE)