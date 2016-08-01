#####################################################################
## 0.- Load path, files and packages
#####################################################################
rm(list = ls())

require(data.table)

# set your path
setwd("C:\\Users\\Diego\\Documents\\Coursera\\Getting and Cleaning Data")
fileDirectorty <- ".\\UCI HAR Dataset"

# read train data
subjectTrain <- data.table(read.table(file.path(fileDirectorty, "train", "subject_train.txt")))
labelTrain   <- data.table(read.table(file.path(fileDirectorty, "train", "Y_train.txt")))
setTrain     <- data.table(read.table(file.path(fileDirectorty, "train", "X_train.txt")))

# read test data
subjectTest  <- data.table(read.table(file.path(fileDirectorty, "test", "subject_test.txt")))
labelTest    <- data.table(read.table(file.path(fileDirectorty, "test", "Y_test.txt")))
setTest      <- data.table(read.table(file.path(fileDirectorty, "test", "X_test.txt")))

# tables in memory

tables()
#####################################################################                         
## 1.- Merges the training and the test sets to create one data set.
#####################################################################

# concatenate row  

subject <- rbind(subjectTrain,subjectTest)
setnames(subject, "V1", "subject")

label <- rbind(labelTrain,labelTest)
setnames(label, "V1", "label")

set <-rbind(setTrain,setTest)

# concatenate columns 

dataset <- cbind(subject,label, set)

###############################################################################################
## 2.- Extracts only the measurements on the mean and standard deviation for each measurement.
###############################################################################################
# read features
features <- read.table(file.path(fileDirectorty, "features.txt"))

#set names in dataset with features names
setnames(dataset,names(dataset[,3:ncol(dataset), with=FALSE]), as.character(features$V2))

# extracts the measurements mean and standard deviation
dataset2 <- dataset[,grepl("mean\\(\\)|std\\(\\)", names(dataset)),with=FALSE]

dataset2 <- cbind(dataset[,c(1,2),with=FALSE], dataset2)

###############################################################################################
## 3.- Uses descriptive activity names to name the activities in the data set
###############################################################################################

activitynames <- data.table(read.table(file.path(fileDirectorty, "activity_labels.txt")))
setnames(activitynames, names(activitynames), c("activity", "activityNames"))
setnames(dataset2, "label", "activity")

setkey(dataset2,activity)
setkey(activitynames,activity)

# join with activity
dataset3 <- merge(dataset2, activitynames, by ="activity", all.x=TRUE)

###############################################################################################
## 4.- Appropriately labels the data set with descriptive variable names.
###############################################################################################

variablenames <- names(dataset3)
variablenames <- gsub("\\(","",variablenames) 
variablenames <- gsub("\\)","",variablenames) 
variablenames <- gsub("tGravity","timeGravity",variablenames) 
variablenames <- gsub("tBody","time",variablenames) 
variablenames <- gsub("Acc","Acceleration",variablenames) 
variablenames <- gsub("Gyro","Gyroscope",variablenames) 
variablenames <- gsub("Mag","Magnitude",variablenames) 
variablenames <- gsub("fBodyBody","frequency",variablenames) 
variablenames <- gsub("fBody","frequency",variablenames) 
variablenames <- gsub("-","_",variablenames) 


setnames(dataset3, names(dataset3), variablenames)

names(dataset3)

###############################################################################################
## 5.- From the data set in step 4, creates a second, independent tidy data set with the average 
##     of each variable for each activity and each subject.
###############################################################################################

setkey(dataset3, activity,subject)

tidydata <- dataset3[, list(
  mean(timeAcceleration_mean_X),mean(timeAcceleration_mean_Y), mean(timeAcceleration_mean_Z),
  mean(timeAcceleration_std_X),mean(timeAcceleration_std_Y),mean(timeAcceleration_std_Z),
  mean(timeGravityAcceleration_mean_X),mean(timeGravityAcceleration_mean_Y),mean(timeGravityAcceleration_mean_Z),
  mean(timeGravityAcceleration_std_X),mean(timeGravityAcceleration_std_Y),mean(timeGravityAcceleration_std_Z),
  mean(timeAccelerationJerk_mean_X),mean(timeAccelerationJerk_mean_Y),mean(timeAccelerationJerk_mean_Z),
  mean(timeAccelerationJerk_std_X),mean(timeAccelerationJerk_std_Y),mean(timeAccelerationJerk_std_Z),
  mean(timeGyroscope_mean_X),mean(timeGyroscope_mean_Y),mean(timeGyroscope_mean_Z),
  mean(timeGyroscope_std_X),mean(timeGyroscope_std_Y),mean(timeGyroscope_std_Z),mean(timeGyroscopeJerk_mean_X),
  mean(timeGyroscopeJerk_mean_Y),mean(timeGyroscopeJerk_mean_Z),mean(timeGyroscopeJerk_std_X),
  mean(timeGyroscopeJerk_std_Y),mean(timeGyroscopeJerk_std_Z),mean(timeAccelerationMagnitude_mean),
  mean(timeAccelerationMagnitude_std),mean(timeGravityAccelerationMagnitude_mean),mean(timeGravityAccelerationMagnitude_std),
  mean(timeAccelerationJerkMagnitude_mean),mean(timeAccelerationJerkMagnitude_std),mean(timeGyroscopeMagnitude_mean),
  mean(timeGyroscopeMagnitude_std),mean(timeGyroscopeJerkMagnitude_mean),mean(timeGyroscopeJerkMagnitude_std),
  mean(frequencyAcceleration_mean_X),mean(frequencyAcceleration_mean_Y),mean(frequencyAcceleration_mean_Z),
  mean(frequencyAcceleration_std_X),mean(frequencyAcceleration_std_Y),mean(frequencyAcceleration_std_Z),
  mean(frequencyAccelerationJerk_mean_X),mean(frequencyAccelerationJerk_mean_Y),mean(frequencyAccelerationJerk_mean_Z),
  mean(frequencyAccelerationJerk_std_X),mean(frequencyAccelerationJerk_std_Y),mean(frequencyAccelerationJerk_std_Z),
  mean(frequencyGyroscope_mean_X),mean(frequencyGyroscope_mean_Y),mean(frequencyGyroscope_mean_Z),
  mean(frequencyGyroscope_std_X),mean(frequencyGyroscope_std_Y),mean(frequencyGyroscope_std_Z),
  mean(frequencyAccelerationMagnitude_mean),mean(frequencyAccelerationMagnitude_std),mean(frequencyAccelerationJerkMagnitude_mean),
  mean(frequencyAccelerationJerkMagnitude_std),mean(frequencyGyroscopeMagnitude_mean),mean(frequencyGyroscopeMagnitude_std),
  mean(frequencyGyroscopeJerkMagnitude_mean),mean(frequencyGyroscopeJerkMagnitude_std)) , by=key(dataset3)] 


setnames(tidydata, names(tidydata), names(dataset3[,1:(ncol(dataset3)-1),with=FALSE]))

# join with activity
setkey(tidydata,activity)

tidydata <- merge(tidydata, activitynames, by ="activity", all.x=TRUE)
