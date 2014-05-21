## Tidy the data
run_analysis <- function() {
    ## download the data
    download()
    ## 1. Merges the training and the test sets to create one data set.
    ## Get consolidated sets
    test <- combine("test")
    train <- combine("train")
    
    ## Merge the two sets
    m <- rbind(test, train)
    
    ## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
    features <- names(m)
    ## select all columns with means and std, subject and activity_id
    column_selection <- c(grep("std|mean",features),562,563)
    m <- m[,column_selection]
    
    ## 3. Uses descriptive activity names to name the activities in the data set
    m <- add_labels(m)
    ## 4. Appropriately labels the data set with descriptive activity names. 
    ## Data is already labeled
    ## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
    avg_by_activity_subject <- aggregate(m[,3:(ncol(m))], by=list(subject = m$subject, activity_name = m$activity_name), FUN=mean)
    ## Save the two tidy data frames
    write.table(m, file = "tidy.txt")
    write.table(avg_by_activity_subject, file = "average.txt")
    
}

download <- function() {
    if(!file.exists("data.zip")) {
        url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(url, "data.zip", method="curl")
        unzip("data.zip")
    }
    
}

## Uses descriptive activity names to name the activities in the data set
add_labels <- function(m) {
    ## get labels
    activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
    names(activity_labels) <- c("id", "activity_name")
    ## add labels to data frame
    m <- merge(m, activity_labels, by.x = "activity_id", by.y = "id")
    ## remove the first column (activity_id)
    m <- m[,2:ncol(m)]
    ## Move the two last column at the front of the table
    last <- m[,(ncol(m)-1):(ncol(m))]
    m <- m[,1:(ncol(m)-2)]
    m <- cbind(last, m)
}

## Consolidate a set
combine <- function(set) {
    ## file paths
    path_x <- paste("UCI HAR Dataset/", set, "/X_", set, ".txt", sep="")
    path_y <- paste("UCI HAR Dataset/", set, "/y_", set, ".txt", sep="")
    path_subject <- paste("UCI HAR Dataset/", set, "/subject_", set, ".txt", sep="")
    ## read the x_test file
    x <- read.table(path_x)
    ## set names for each variable
    features <- read.table("UCI HAR Dataset/features.txt")
    names(x) <- features[,2]
    
    ## Add activity id (label) for each row
    activity_id <- read.table(path_y)
    names(activity_id) <- "activity_id"
    x <- cbind(x, activity_id)
    
    ## Add subject for each row
    subject <- read.table(path_subject)
    names(subject) <- "subject"
    x <- cbind(x, subject)
    x
}
