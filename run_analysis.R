
# Main path
main_path <- "/home/adrian/Dropbox/R/course-data-science-specialization/course-03-data-cleaning/course-03-cleaning-data-week-04/"

# Loading features
features_path <- paste(main_path, "archivo/UCI HAR Dataset/features.txt", sep = "")
features_table <- read.table(features_path)

# Loading activities and changing their columns names
activity_labels_path <- paste(main_path, "archivo/UCI HAR Dataset/activity_labels.txt", sep = "")
activity_labels <- read.table(activity_labels_path)
colnames(activity_labels) <- c("activity", "activity_names")

# Loading the subject test data to R
subject_path <- paste(main_path, "archivo/UCI HAR Dataset/test/subject_test.txt", sep = "")
subject_table <- read.table(subject_path)

# Loading the main test data archives into R
x_test_path <- paste(main_path, "archivo/UCI HAR Dataset/test/X_test.txt", sep = "")
y_test_path <- paste(main_path, "archivo/UCI HAR Dataset/test/y_test.txt", sep = "")
x_test_table <- read.table(x_test_path)
y_test_table <- read.table(y_test_path)


# Loading the subject train archives into R
subject_path_train <- paste(main_path, "archivo/UCI HAR Dataset/train/subject_train.txt", sep = "")
subject_table_train <- read.table(subject_path_train)

# Loading the main train data archives into R
x_train_path <- paste(main_path, "archivo/UCI HAR Dataset/train/X_train.txt", sep = "")
y_train_path <- paste(main_path, "archivo/UCI HAR Dataset/train/y_train.txt", sep = "")
x_train_table <- read.table(x_train_path)
y_train_table <- read.table(y_train_path)



#--------------------------------------------------------------------------------------------------
#                                   I build the working test dataset
#--------------------------------------------------------------------------------------------------
# I use the features to named the test data
colnames(x_test_table) <- features_table[, 2] 

# I add the activities vector to the main table, and rename the variable
x_test <- cbind(x_test_table, y_test_table)
colnames(x_test)[ncol(x_test)] <- "activity"

# I use merge left join to join the main data test to the activities
x_test <- merge(x = x_test, y = activity_labels, by = "activity", all.x = TRUE)

# I add the subjects
x_test <- cbind(x_test, subject_table)
colnames(x_test)[ncol(x_test)] <- "subject"

#--------------------------------------------------------------------------------------------------
#                                   I buil the working train dataset
#--------------------------------------------------------------------------------------------------
# I use the features to named the train data
colnames(x_train_table) <- features_table[, 2] 

# I add the activities vector to the main table, and rename the variable
x_train <- cbind(x_train_table, y_train_table)
colnames(x_train)[ncol(x_train)] <- "activity"

# I use merge left join to join the main data train to the activities
x_train <- merge(x = x_train, y = activity_labels, by = "activity", all.x = TRUE)      

# I add the subjects
x_train <- cbind(x_train, subject_table_train)
colnames(x_train)[ncol(x_train)] <- "subject"      


#--------------------------------------------------------------------------------------------------
#                                   Merging two tables
#--------------------------------------------------------------------------------------------------      
x_test_train <- rbind(x_test, x_train)


#--------------------------------------------------------------------------------------------------
#           Extraction of the variables that contains mean, std, subject, and activity
#--------------------------------------------------------------------------------------------------      
x_test_train_mean <- x_test_train[, grep("mean", colnames(x_test_train))]
x_test_train_std <- x_test_train[, grep("std", colnames(x_test_train))]
x_test_train_subject <- x_test_train[, grep("subject", colnames(x_test_train))]
x_test_train_activity <- x_test_train[, grep("activity", colnames(x_test_train))]

x_test_train_mean_std <- cbind(x_test_train_mean, x_test_train_std, x_test_train_activity, x_test_train_subject)
colnames(x_test_train_mean_std)[ncol(x_test_train_mean_std)] <- "subject"


#--------------------------------------------------------------------------------------------------
#                                           Final grouping
#--------------------------------------------------------------------------------------------------      
x <- x_test_train_mean_std %>%
    group_by(subject, activity, activity_names) %>%
    summarize_at(vars(1:79), mean)      




















