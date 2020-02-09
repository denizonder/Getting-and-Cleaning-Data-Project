#Downloading the files
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip")

unzip(zipfile="./data/Dataset.zip",exdir="./data")

project_files <- file.path("./data" , "UCI HAR Dataset")
files<-list.files(project_files, recursive=TRUE)

## Reading the flat files into tables
features_test <- read.table(file.path(project_files, "Test", "X_test.txt"), header = F)
features_train <- read.table(file.path(project_files, "Train", "X_train.txt"), header = F)
subject_train <- read.table(file.path(project_files, "Train", "subject_train.txt"), header = F)
subject_test <- read.table(file.path(project_files, "Test", "subject_test.txt"), header = F)
activity_test <- read.table(file.path(project_files, "Test", "y_test.txt"), header = F)
activity_train <- read.table(file.path(project_files, "Train", "y_train.txt"), header = F)

## Read in the features.txt file which we will be using
## as header names later on
features_names <- read.table(file.path(path_rf, "features.txt"), header = F)

## Row binding the train and test datas into single tables
activity_merged <- rbind(activity_train, activity_test)
features_merged <- rbind(features_train, features_test)
subject_merged <- rbind(subject_train, subject_test)

## Assigning the column names
names(features_merged)<- features_names$V2
names(activity_merged)<- c("subject")
names(subject_merged)<-c("activity")

# Merge all the data by columns and create a unified table
subject_activity_merged <- cbind(subject_merged, activity_merged)
Complete_Data <- cbind(features_merged, subject_activity_merged)

# Subsetting the data
features_names_specified<-features_names$V2[grep("mean\\(\\)|std\\(\\)", features_names$V2)]
last_names <- c(as.character(features_names_specified), "activity", "subject")
Last_Data <- subset(Complete_Data, select=last_names)
str(Last_Data)

# Reading in the Activity Labels
activityLabels <- read.table(file.path(project_files, "activity_labels.txt"),header = FALSE)

# For some reason I could'nt understand, the activity and subject names were mixed up 
# so I'm correcting them
as_tibble(Last_Data)
Last_Data_Renamed <- rename(Last_Data, activity = subject, subject = activity) 
tail(Last_Data_Renamed)

#Factorizing the activity variable and replacing the names with the corresponding 
#names in activityLabels 
Last_Data_Renamed$activity <- as.factor(Last_Data_Renamed$activity)
levels(Last_Data_Renamed$activity) <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")
levels(Last_Data_Renamed$activity)

#Descriptive variable names
names(Last_Data_Renamed)<-gsub("Mag", "Magnitude", names(Last_Data_Renamed))
names(Last_Data_Renamed)<-gsub("BodyBody", "Body", names(Last_Data_Renamed))
names(Last_Data_Renamed)<-gsub("^t", "Time", names(Last_Data_Renamed))
names(Last_Data_Renamed)<-gsub("Acc", "Accelerometer", names(Last_Data_Renamed))
names(Last_Data_Renamed)<-gsub("Gyro", "Gyroscope", names(Last_Data_Renamed))
names(Last_Data_Renamed)<-gsub("^f", "Frequency", names(Last_Data_Renamed))

#Checking the table one last time
glimpse(Last_Data_Renamed)
View(Last_Data_Renamed)

#Creating the secon table
library(plyr);
Second_Table<-aggregate(. ~subject + activity, Last_Data_Renamed, mean)
Second_Table<-Second_Table[order(Second_Table$subject,Second_Table$activity),]
glimpse(Second_Table)

#Writing the Second_Table as a txt file
write.table(Second_Table, file = "Summary_Table.txt",row.name=FALSE)