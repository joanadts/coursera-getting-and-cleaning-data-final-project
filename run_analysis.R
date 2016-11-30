###############################################################################
# Step 1: Merge the training and test sets to create one data set

# Read test data:
x_train <- read.table("./train/X_train.txt")
y_train <- read.table("./train/y_train.txt")
subject_train <- read.table("./train/subject_train.txt")

# Read train data:
x_test <- read.table("./test/X_test.txt")
y_test <- read.table("./test/y_test.txt")
subject_test <- read.table("./test/subject_test.txt")

# Read feature file:
features <- read.table('features.txt')

# Read activity labels:
activityLabels = read.table('activity_labels.txt')

#Assigning column names test data:
colnames(x_test) <- features[,2] 
colnames(y_test) <- "activityId"
colnames(subject_test) <- "subjectId"

#Assigning column names train data:
colnames(x_train) <- features[,2] 
colnames(y_train) <-"activityId"
colnames(subject_train) <- "subjectId"

#Assigning column names to activity data:
colnames(activityLabels) <- c('activityId','activityType')

# Merging all data into one set
mrg_train <- cbind(y_train, subject_train, x_train)  #concatenate by column
mrg_test <- cbind(y_test, subject_test, x_test)      #concatenate by column
OneSet <- rbind(mrg_train, mrg_test)                 #concatenate by row



###############################################################################
# Step 2: Extract only the measurements on the mean and standard deviation for each measurement
colNames <- colnames(OneSet)
meanstd <- grep("-(mean|std)\\(\\)", colNames) # we want -mean() and -std() only

subOneSet <- OneSet[,c(1,2, meanstd)]  #subset with only the mean and standard deviation for each measurement, keep activityID and subjectID columns



###############################################################################
# Step 3: Use descriptive activity names to name the activities in the data set
SetActivityNames <- merge(activityLabels, subOneSet, by='activityId', all.y=TRUE)



###############################################################################
# Step 4: Appropriately label the data set with descriptive variable names
# Label Features names using descriptive variable names:
# t replaced by time
# Acc replaced by Accelerometer
# Gyro replaced by Gyroscope
# f replaced by frequency
# Mag replaced by Magnitude
# BodyBody is replaced by Body
names(SetActivityNames)<-gsub("^t", "time", names(SetActivityNames))
names(SetActivityNames)<-gsub("^f", "frequency", names(SetActivityNames))
names(SetActivityNames)<-gsub("Acc", "Accelerometer", names(SetActivityNames))
names(SetActivityNames)<-gsub("Gyro", "Gyroscope", names(SetActivityNames))
names(SetActivityNames)<-gsub("Mag", "Magnitude", names(SetActivityNames))
names(SetActivityNames)<-gsub("BodyBody", "Body", names(SetActivityNames))



###############################################################################
# Step 5: create a second, independent tidy data set with the average of each variable
# for each activity and each subject

library(plyr)
TidySet <- ddply(SetActivityNames, .(subjectId, activityId), function(x) colMeans(x[,4:69]))


