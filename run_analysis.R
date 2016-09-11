# Getting and Cleaning Coursera Final Project
# Jamifo
# 9/10/2016

library(dplyr)

# Bring in the activity and features labels

setwd("C:/Users/uszlj8j/Desktop/Learning R/Data Science CourseRA/R_Cleaning_Data/UCI HAR Dataset")

activity_labels <- read.table("activity_labels.txt")
features <- read.table("features.txt")

#Bring in Training Datasets

setwd("C:/Users/uszlj8j/Desktop/Learning R/Data Science CourseRA/R_Cleaning_Data/UCI HAR Dataset/train")

y_train <- read.table("y_train.txt")
X_train <- read.table("X_train.txt")
subject_train <- read.table("subject_train.txt") #These are the subjects (out of 30) in the test set

setwd("C:/Users/uszlj8j/Desktop/Learning R/Data Science CourseRA/R_Cleaning_Data/UCI HAR Dataset/train/Inertial Signals")

body_acc_x_train <- read.table("body_acc_x_train.txt")
body_acc_y_train <- read.table("body_acc_y_train.txt")
body_acc_z_train <- read.table("body_acc_z_train.txt")
body_gyro_x_train <- read.table("body_gyro_x_train.txt")
body_gyro_y_train <- read.table("body_gyro_y_train.txt")
body_gyro_z_train <- read.table("body_gyro_z_train.txt")
total_acc_x_train <- read.table("total_acc_x_train.txt")
total_acc_y_train <- read.table("total_acc_y_train.txt")
total_acc_z_train <- read.table("total_acc_z_train.txt")


#Bring in Test Datasets

setwd("C:/Users/uszlj8j/Desktop/Learning R/Data Science CourseRA/R_Cleaning_Data/UCI HAR Dataset/test")
y_test <- read.table("y_test.txt")
X_test <- read.table("X_test.txt")
subject_test <- read.table("subject_test.txt") #These are the subjects (out of 30) in the test set

setwd("C:/Users/uszlj8j/Desktop/Learning R/Data Science CourseRA/R_Cleaning_Data/UCI HAR Dataset/test/Inertial Signals")

body_acc_x_test <- read.table("body_acc_x_test.txt")
body_acc_y_test <- read.table("body_acc_y_test.txt")
body_acc_z_test <- read.table("body_acc_z_test.txt")
body_gyro_x_test <- read.table("body_gyro_x_test.txt")
body_gyro_y_test <- read.table("body_gyro_y_test.txt")
body_gyro_z_test <- read.table("body_gyro_z_test.txt")
total_acc_x_test <- read.table("total_acc_x_test.txt")
total_acc_y_test <- read.table("total_acc_y_test.txt")
total_acc_z_test <- read.table("total_acc_z_test.txt")


# Merge the Train and Test Sets

#Add the column names to the X_test/X_train datasets
Features_transpose <- t(features) 
Features_transpose2 <- Features_transpose[-1,]
Features_list <- as.character(Features_transpose2)

colnames(X_test) = Features_list
colnames(X_train) = Features_list

#Merge on the Activity Labels to Y
Y_test_acty <- left_join(y_test,activity_labels, by = c("V1")) 
Y_train_acty <- left_join(y_train,activity_labels, by = c("V1")) 

#Stack the y datasets
Y_train_test <- rbind(Y_train_acty,Y_test_acty)  
Y_train_test <- select(Y_train_test, -V1)
colnames(Y_train_test) = "Activity"

#Merge Subject to Test/Train X datasets
colnames(subject_train) = "Subject"
colnames(subject_test) = "Subject"
X_train_subject <- cbind(X_train, subject_train)
X_test_subject <- cbind(X_test, subject_test)


# Stack the X_train and X_test datasets
X_Train_Test <- rbind(X_train_subject,X_test_subject)
View(head(X_Train_Test[,550:562]))


#Merge the X and Y datasets
X_Y_train_test <- cbind(Y_train_test,X_Train_Test)


# Extract the Measurements of Mean and Standard Deviation for Each Measurement
X_Y_train_test_mean <-  X_Y_train_test[,grepl(("mean") , colnames(X_Y_train_test))]
X_Y_train_test_std  <-  X_Y_train_test[,grepl("std" , colnames(X_Y_train_test))]
Subject <-  X_Y_train_test[,grepl("Subject" , colnames(X_Y_train_test))]
Activity  <-  X_Y_train_test[,grepl("Activity" , colnames(X_Y_train_test))]

X_Y_train_test_mn_std <- cbind(X_Y_train_test_mean,X_Y_train_test_std, Subject, Activity)

# Create a second dataset with the average of each variable for each activity and each subject
# Appropriately label the data set with descriptive variable names
# Would expect a 6*30 = 180 row dataframe.  This is what is output so we know we should be good. 
Avg_each_var_acty_sub <- X_Y_train_test_mn_std %>% group_by(Activity,Subject) %>%
                         summarize_all(mean) %>% setNames(c(names(.)[1], paste0(names(.)[-1],"_mean")))

colnames(X_Y_train_test_mn_std)

#Write out the final dataframe
setwd("C:/Users/uszlj8j/Desktop/Learning R/Data Science CourseRA/R_Cleaning_Data")
write.table(Avg_each_var_acty_sub, "Mean_each_var_Acty_Sub.txt")
write.csv(Avg_each_var_acty_sub, "Mean_each_var_Acty_Sub.csv")

