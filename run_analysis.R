library(dplyr)
#Get a vector of subjects from training and test set
getSubject <- function() {
    trainingFile = "./data/subject_train.txt"
    testFile = "./data/subject_test.txt"
    
    con <- file(trainingFile)
    s1 <-  readLines(con)
    close(con)
    
    con <- file(testFile)
    s2 <-  readLines(con)
    close(con)
    
    c(s1, s2)
}

# Get a vector of activities from training and test set
getActivity <- function() {
    trainingFile = "./data/y_train.txt"
    testFile = "./data/y_test.txt"
    Activities = c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", 
                   "SITTING", "STANDING", "LAYING")
    
    con <- file(trainingFile)
    s1 <-  readLines(con)
    close(con)
    
    con <- file(testFile)
    s2 <-  readLines(con)
    close(con)
    
    s <- c(s1, s2)
    s <- as.numeric(s)
    
    Activities[s]
}

# Get Mean
getMean <- function(files) {
    # trainingFile = "./data/body_acc_x_train.txt"
    # testFile = "./data/body_acc_x_test.txt"
    
    trainingFile = paste("./data", files[1], sep = "/")
    testFile = paste("./data", files[2], sep = "/")
    
    
    con <- file(trainingFile)
    s1 <-  readLines(con)
    close(con)
    
    con <- file(testFile)
    s2 <-  readLines(con)
    close(con)
    
    s <- c(s1, s2)
    
    s <- sapply(s, function(x){
        x <- trimws(x)
        x <- strsplit(x, split = "(  )|( -)")
        x <- unlist(x)
        x <- as.numeric(x)
        mean(x)
    })
    names(s) <- NULL
    s
}

# Get Sd
getSd <- function(files) {
    # trainingFile = "./data/body_acc_x_train.txt"
    # testFile = "./data/body_acc_x_test.txt"
    
    trainingFile = paste("./data", files[1], sep = "/")
    testFile = paste("./data", files[2], sep = "/")
    
    
    con <- file(trainingFile)
    s1 <-  readLines(con)
    close(con)
    
    con <- file(testFile)
    s2 <-  readLines(con)
    close(con)
    
    s <- c(s1, s2)
    
    s <- sapply(s, function(x){
        x <- trimws(x)
        x <- strsplit(x, split = "(  )|( -)")
        x <- unlist(x)
        x <- as.numeric(x)
        sd(x)
    })
    names(s) <- NULL
    s
}

# Create Tidy data
createTidyData <- function() {
    subject <- getSubject()
    activity <- getActivity()
    
    tBodyAccMeanX <- c("body_acc_x_train.txt", "body_acc_x_test.txt")
    tBodyAccMeanY <- c("body_acc_y_train.txt", "body_acc_y_test.txt")
    tBodyAccMeanZ <- c("body_acc_z_train.txt", "body_acc_z_test.txt")
    
    tBodyAccStdX <- c("body_acc_x_train.txt", "body_acc_x_test.txt")
    tBodyAccStdY <- c("body_acc_y_train.txt", "body_acc_y_test.txt")
    tBodyAccStdZ <- c("body_acc_z_train.txt", "body_acc_z_test.txt")
    
    tBodyGyroMeanX <- c("body_gyro_x_train.txt", "body_gyro_x_test.txt")
    tBodyGyroMeanY <- c("body_gyro_y_train.txt", "body_gyro_y_test.txt")
    tBodyGyroMeanZ <- c("body_gyro_z_train.txt", "body_gyro_z_test.txt")
    
    tBodyGyroStdX <- c("body_gyro_x_train.txt", "body_gyro_x_test.txt")
    tBodyGyroStdY <- c("body_gyro_y_train.txt", "body_gyro_y_test.txt")
    tBodyGyroStdZ <- c("body_gyro_z_train.txt", "body_gyro_z_test.txt")
    
    tGravityAccMeanX <- c("total_acc_x_train.txt", "total_acc_x_test.txt")
    tGravityAccMeanY <- c("total_acc_y_train.txt", "total_acc_y_test.txt")
    tGravityAccMeanZ <- c("total_acc_z_train.txt", "total_acc_z_test.txt")
    
    tGravityAccStdX <- c("total_acc_x_train.txt", "total_acc_x_test.txt")
    tGravityAccStdY <- c("total_acc_y_train.txt", "total_acc_y_test.txt")
    tGravityAccStdZ <- c("total_acc_z_train.txt", "total_acc_z_test.txt")
    
    tBodyAccMeanX <- getMean(tBodyAccMeanX)
    tBodyAccMeanY <- getMean(tBodyAccMeanY)
    tBodyAccMeanZ <- getMean(tBodyAccMeanZ)
    
    tBodyAccStdX <- getSd(tBodyAccStdX)
    tBodyAccStdY <- getSd(tBodyAccStdY)
    tBodyAccStdZ <- getSd(tBodyAccStdZ)
    
    tBodyGyroMeanX <- getMean(tBodyGyroMeanX)
    tBodyGyroMeanY <- getMean(tBodyGyroMeanY)
    tBodyGyroMeanZ <- getMean(tBodyGyroMeanZ)

    tBodyGyroStdX <- getSd(tBodyGyroStdX)
    tBodyGyroStdY <- getSd(tBodyGyroStdY)
    tBodyGyroStdZ <- getSd(tBodyGyroStdZ)
    
    tGravityAccMeanX <- getMean(tGravityAccMeanX)
    tGravityAccMeanY <- getMean(tGravityAccMeanY)
    tGravityAccMeanZ <- getMean(tGravityAccMeanZ)

    tGravityAccStdX <- getSd(tGravityAccStdX)
    tGravityAccStdY <- getSd(tGravityAccStdY)
    tGravityAccStdZ <- getSd(tGravityAccStdZ)
 
    tidyData <- data.frame(Subject = subject, Activity = activity,
                           tBodyAccMeanX = tBodyAccMeanX,
                           tBodyAccMeanY = tBodyAccMeanY,
                           tBodyAccMeanZ = tBodyAccMeanZ,
                           tBodyAccStdX = tBodyAccStdX,
                           tBodyAccStdY = tBodyAccStdY,
                           tBodyAccStdZ = tBodyAccStdZ,
                           tBodyGyroMeanX = tBodyGyroMeanX,
                           tBodyGyroMeanY = tBodyGyroMeanY,
                           tBodyGyroMeanZ = tBodyGyroMeanZ,
                           tBodyGyroStdX = tBodyGyroStdX,
                           tBodyGyroStdY = tBodyGyroStdY,
                           tBodyGyroStdZ = tBodyGyroStdZ,
                           tGravityAccMeanX = tGravityAccMeanX,
                           tGravityAccMeanY = tGravityAccMeanY,
                           tGravityAccMeanZ = tGravityAccMeanZ,
                           tGravityAccStdX = tGravityAccStdX,
                           tGravityAccStdY = tGravityAccStdY,
                           tGravityAccStdZ = tGravityAccStdZ)
}
# Create Average Table
createAvgTable <- function() {
    df <- createTidyData()
    x <- df %>% group_by(Subject, Activity) %>% summarise(
                        tBodyAccAvgX=mean(tBodyAccMeanX),
                        tBodyAccAvgY=mean(tBodyAccMeanY),
                        tBodyAccAvgZ=mean(tBodyAccMeanZ),
                        tBodyAccStdAvgX=mean(tBodyAccStdX),
                        tBodyAccStdAvgY=mean(tBodyAccStdY),
                        tBodyAccStdAvgZ=mean(tBodyAccStdZ),
                        tBodyGyroAvgX=mean(tBodyGyroMeanX),
                        tBodyGyroAvgY=mean(tBodyGyroMeanY),
                        tBodyGyroAvgZ=mean(tBodyGyroMeanZ),
                        tBodyGyroStdAvgX=mean(tBodyGyroStdX),
                        tBodyGyroStdAvgY=mean(tBodyGyroStdY),
                        tBodyGyroStdAvgZ=mean(tBodyGyroStdZ),
                        tGravityAccAvgX=mean(tGravityAccMeanX),
                        tGravityAccAvgY=mean(tGravityAccMeanY),
                        tGravityAccAvgZ=mean(tGravityAccMeanZ),
                        tGravityAccStdAvgX=mean(tGravityAccStdX),
                        tGravityAccStdAvgY=mean(tGravityAccStdY),
                        tGravityAccStdAvgZ=mean(tGravityAccStdZ))
    write.table(x = x, file = "AvgTable.txt", row.names = FALSE)
   
}