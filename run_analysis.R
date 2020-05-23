setwd("/home/adrian/Dropbox/R/course-data-science-specialization/course-03-data-cleaning/course-03-cleaning-data-week-04/")

# Main path to paste with the specific path to each archive
main_path <- "/home/adrian/Dropbox/R/course-data-science-specialization/course-03-data-cleaning/course-03-cleaning-data-week-04/"

# Load the archives in R using the main path plus specific path
# Feature are the variables names of the dataset that are valid for both, train and test
features_path <- paste(main_path, "archivo/UCI HAR Dataset/features.txt", sep = "")
features <- read.table(features_path)

# The name of the activities and their codes. I rename the columns of the dataset
activity_labels_path <- paste(main_path, "archivo/UCI HAR Dataset/activity_labels.txt", sep = "")
activity_labels <- read.table(activity_labels_path)
colnames(activity_labels) <- c("activity", "activity_names")

# Subject are the people who made the proof.
subject_path <- paste(main_path, "archivo/UCI HAR Dataset/test/subject_test.txt", sep = "")
subject_test <- read.table(subject_path)
colnames(subject_test) <- "subject"

subject_path_train <- paste(main_path, "archivo/UCI HAR Dataset/train/subject_train.txt", sep = "")
subject_train <- read.table(subject_path_train)
colnames(subject_train) <- "subject"

# Test and Train are the main datasets
x_test_path <- paste(main_path, "archivo/UCI HAR Dataset/test/X_test.txt", sep = "")
y_test_path <- paste(main_path, "archivo/UCI HAR Dataset/test/y_test.txt", sep = "")
x_test <- read.table(x_test_path)
y_test <- read.table(y_test_path)
colnames(y_test) <- "activity"

x_train_path <- paste(main_path, "archivo/UCI HAR Dataset/train/X_train.txt", sep = "")
y_train_path <- paste(main_path, "archivo/UCI HAR Dataset/train/y_train.txt", sep = "")
x_train <- read.table(x_train_path)
y_train <- read.table(y_train_path)
colnames(y_train) <- "activity"


# -------------------------------------------------------------------------------------------------
#           4 - Appropriately labels the data set with descriptive variable names.
#--------------------------------------------------------------------------------------------------      
# Add the variables names (features) to the main datasets
colnames(x_test) <- features[, 2] 
colnames(x_train) <- features[, 2] 

# Combine the datasets 
x_test <- cbind(x_test, y_test, subject_test)
x_train <- cbind(x_train, y_train, subject_train)


# -------------------------------------------------------------------------------------------------      
#      3 - Uses descriptive activity names to name the activities in the data set      
# -------------------------------------------------------------------------------------------------      
# I use the inner join of the merge function to give to each dataset descriptive activities names
x_test <- merge(x = x_test, y = activity_labels, by = "activity", all.x = TRUE)
x_train <- merge(x = x_train, y = activity_labels, by = "activity", all.x = TRUE)      


# -------------------------------------------------------------------------------------------------      
#      1 - Merges the training and the test sets to create one data set.
# -------------------------------------------------------------------------------------------------                      
x_test_train <- rbind(x_test, x_train)


# -------------------------------------------------------------------------------------------------      
#      2 - Extracts only the measurements on the mean and standard deviation for each measurement.
# -------------------------------------------------------------------------------------------------                            
# Additionally, I added the activities ids and subjects         
x_test_train_mean <- x_test_train[, grep("mean", colnames(x_test_train))]
x_test_train_std <- x_test_train[, grep("std", colnames(x_test_train))]
x_test_train_subject <- x_test_train[, grep("subject", colnames(x_test_train))]
x_test_train_activity <- x_test_train[, grep("activity", colnames(x_test_train))]

x_mean_std <- cbind(x_test_train_mean, x_test_train_std, x_test_train_activity, x_test_train_subject)  
colnames(x_mean_std)[ncol(x_mean_std)] <- "subject"


# -------------------------------------------------------------------------------------------------      
#      5 - From the data set in step 4, creates a second, independent tidy data set with the 
#           average of each variable for each activity and each subject.
# -------------------------------------------------------------------------------------------------                                        
mean_std <- x_mean_std %>%
    group_by(subject, activity, activity_names) %>%
    summarize_at(vars(1:79), mean)         


write.table(mean_std, "mean_std.txt", row.name=FALSE)
















