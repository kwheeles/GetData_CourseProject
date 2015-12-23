# 1.Merges the training and the test sets to create one data set.
# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3.Uses descriptive activity names to name the activities in the data set
# 4.Appropriately labels the data set with descriptive variable names. 
# 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#
# - 'features.txt': List of all features.
# - 'activity_labels.txt': Links the class labels with their activity name.
# - 'train/X_train.txt': Training set.
# - 'train/y_train.txt': Training labels.
# - 'test/X_test.txt': Test set.
# - 'test/y_test.txt': Test labels.

library(dplyr)
library(data.table)

# features is for column names of dataset
features <- fread("features.txt")

# get activity labels
activity <- fread("activity_labels.txt")

# read in test data and labels for it
testdf <- fread("test/x_test.txt")
names(testdf) <- features$V2
testdf$Label <- fread("test/y_test.txt")
testdf$Subject <- fread("test/subject_test.txt")

# read in train data and labels for it
traindf <- fread("train/x_train.txt")
names(traindf) <- features$V2
traindf$Label <- fread("train/y_train.txt")
traindf$Subject <- fread("train/subject_train.txt")

# combine them
fulldf <- rbind(testdf,traindf)


fulldf$Activity <- activity$V2[match(fulldf$Label,activity$V1)]


extractdf <- fulldf %>% select(matches("mean"),matches("std"),Activity,Subject)

# now to clean up the column names

namecleanlst <- c("^f","Freq: ", "^t", "Time: ", "-mean\\(\\)",", Mean", 
                "-std\\(\\)", ", Std Dev ", "BodyAcc","Body Acceleration ", 
                "GravityAcc","Gravity Acceleration", "-X", " X Axis",
                "-Y", " Y Axis", "-Z", " Z Axis", "BodyBody", "Body/Body",
                "Mag", " Magnitude", "Jerk"," Jerk", "-meanFreq\\(\\)",
                ", Mean Freq", "^angle","Angle: ", "\\(t", "Time ", "\\(X",
                " X Axis", "\\(Y", " Y Axis", "\\(Z", " Z Axis", 
                "BodyGyro","Gyro ",",gravity",", Gravity ", "\\)", "",
                " , ", ", ","  ", " ")
namecleanmat <- as.matrix(namecleanlst)
dim(namecleanmat) <- c(2,length(namecleanlst)/2)
namecleanmat <- t(namecleanmat)


clean_name <- function(x) {
        for (i in 1:dim(namecleanmat)[1]) {
                x <- gsub(namecleanmat[i,1],namecleanmat[i,2],x)
        }
        x
}       

colnames(extractdf) <- as.character(lapply(colnames(extractdf),clean_name))

tidydf <- extractdf %>% group_by(Activity,Subject) %>%
        summarise_each(funs(mean))
write.table(tidydf, file = "tidy.txt",row.name=FALSE)