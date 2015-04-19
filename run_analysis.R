##------------------------------------------------------------------
##                   OBJECTIVE OF THIS SCRIPT
##
## This script first imports the training and test datasets from the 
## "UCI HAR Dataset" directory, which is assumed to be located in the working
## directory.  The script then merges the datasets, and extracts 
## specified mean and standard deviation variables (see CodeBook).  
## A second dataset is created with the average of each selected variable
## for each activity and each subject.
## The result is written to a text file located in the working directory.

## This section imports the x_*.txt, y_*.txt and subjet_*.txt data from the
## "UCI HAR Dataset/train" and "UCI HAR Dataset/test" directories. 
print("Importing data...")
xtrain1 <- read.table("./UCI HAR Dataset/train/X_train.txt")
ytrain1 <- read.table("./UCI HAR Dataset/train/y_train.txt")
subjtrain1 <- read.table("./UCI HAR Dataset/train/subject_train.txt")
xtest1 <- read.table("./UCI HAR Dataset/test/X_test.txt")
ytest1 <- read.table("./UCI HAR Dataset/test/y_test.txt")
subjtest1 <- read.table("./UCI HAR Dataset/test/subject_test.txt")

## Importing the features.txt document which will provide data labels
features561 <- read.table("./UCI HAR Dataset/features.txt")
print("All required data has been succesfully imported in R")

## The next section adds column names to the training and test datasets
feat1 <- features561[,2]   ## this is a factor: the column containing labels
feat1 <- as.character(feat1)
feat1 <- as.vector(feat1)
colnames(xtrain1) <- feat1  ## assigns variable labels for each column
colnames(xtest1) <- feat1

print("Merging Training and Test datasets...")
## Adding the subjects as the first column in each file
xtrain2 <- cbind(subjtrain1, xtrain1)
xtest2 <- cbind(subjtest1, xtest1)
## rename the first column of each file: "subject"
names(xtrain2)[names(xtrain2)=="V1"] <- "subject"
names(xtest2)[names(xtest2)=="V1"] <- "subject"

## Add the activity code column to each file
xtrain3 <- cbind(xtrain2, ytrain1)
xtest3 <- cbind(xtest2, ytest1)
## Rename the new column (which is the last one): "Act"
names(xtrain3)[names(xtrain3)=="V1"] <- "Act"
names(xtest3)[names(xtest3)=="V1"] <- "Act"

## The next section makes a vector with activity codes and activity labels 
## and adds a column to the datasets with the correct activity description
label.code <- c(WALKING = 1, WALKING_UPSTAIRS=2, WALKING_DOWNSTAIRS=3, SITTING=4, STANDING=5, LAYING=6)
xtrain3$Activity <- names(label.code)[match(xtrain3$Act, label.code)]
xtest3$Activity <- names(label.code)[match(xtest3$Act, label.code)]

## Prior to merging, add a column in each file specifying TRAIN or TEST
## Call new column "type"
t1<- "TRAIN"
xtrain4<- cbind(xtrain3, t1)
names(xtrain4)[names(xtrain4)=="t1"] <- "type"    
t2 <- "TEST"
xtest4 <- cbind(xtest3, t2)
names(xtest4)[names(xtest4)=="t2"] <- "type"

## The next section merges the two datasets.  Since the columns names and
## content is exactly the same in both datasets, it seems reasonable
## to just bind (rbind) the two data sets.  The "type" column will help
## identify the TRAIN vs. TEST rows if needed.
all1 <- rbind(xtrain4, xtest4)   ## This dataframe has 10299 rows and 565 columns

## Prior to selecting specific columns, will make the 
## column names R-compatible.  The "-" in original column names causes what
## follows to be ignored (hence getting false duplicated columns errors).
valid_column_names <- make.names(names=names(all1), unique=TRUE, allow_ = TRUE)
names(all1) <- valid_column_names

## make a tbl file here to use dplyr
print("Loading dplyr library")
library(dplyr)
all2 <- tbl_df(all1)

## USed the documentation to select mean and standard deviation variables
## that are not X-, Y- or Z- specific.  Twelve variables were specified.
## See CodeBook for selection criteria and details.
all3 <- select(all2, subject, Activity, type, tBodyAccMag.mean..,tBodyAccMag.std..,tGravityAccMag.mean..,tGravityAccMag.std..,tBodyAccJerkMag.mean..,tBodyAccJerkMag.std.., tBodyGyroMag.mean.., tBodyGyroMag.std.., tBodyGyroJerkMag.mean..,tBodyGyroJerkMag.std..,fBodyAccMag.mean..,fBodyAccMag.std.. )
## renaming the columns to make them more explicit
all3_explicit<-rename(all3, Mean_tBodyAccMag=tBodyAccMag.mean..,StDev_tBodyAccMag=tBodyAccMag.std..,Mean_tGravityAccMag=tGravityAccMag.mean..,StDev_tGravityAccMag=tGravityAccMag.std..,Mean_tBodyAccJerkMag=tBodyAccJerkMag.mean..,StDev_tBodyAccJerkMag=tBodyAccJerkMag.std.., Mean_tBodyGyroMag=tBodyGyroMag.mean.., StDev_tBodyGyroMag=tBodyGyroMag.std.., Mean_tBodyGyroJerkMag=tBodyGyroJerkMag.mean..,StDev_tBodyGyroJerkMag=tBodyGyroJerkMag.std..,Mean_fBodyAccMag=fBodyAccMag.mean..,StDev_fBodyAccMag=fBodyAccMag.std.. )

## The following section does the following:
## 1) Group by Activity and subject
## 2) Use summarize_each to calculate all means by group but remove the
## "type" data since we are calculating means for TRAIN and TEST data
## all together
FinalData<-select(all3_explicit, -type) %>% group_by(Activity, subject) %>% summarise_each(funs(mean))

## Write the data to the working directory as a text file named "result.txt"
write.table(FinalData, file="./result.txt",row.name=FALSE, sep=" ")
print("The file result.txt has been written to the working directory")

