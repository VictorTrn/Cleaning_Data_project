# Coursera Getting and Cleaning Data Course Project
# 21.06.15

# 1. Merge the training and the test sets to create one data set.
# Please note th single data set will be created at step 4
# 1.1 Import X data

Train <- read.table("train/X_train.txt")
Test <- read.table("test/X_test.txt")
X_tot <- rbind(Train, Test)

# 1.2 Imoprt Subject Data
Train <- read.table("train/subject_train.txt")
Test <- read.table("test/subject_test.txt")
S_tot <- rbind(Train, Test)

# 1.3 Import Y data
Train <- read.table("train/y_train.txt")
Test <- read.table("test/y_test.txt")
Y_tot <- rbind(Train, Test)
# 1.4 Removing temp. variables 
remove('Test','Train')

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

feat <- read.table("features.txt")
i <- grep("-mean\\(\\)|-std\\(\\)", feat[, 2])
X_tot <- X_tot[, i]
names(X_tot) <- feat[i, 2]
names(X_tot) <- gsub("\\(|\\)", "", names(X_tot))
names(X_tot) <- tolower(names(X_tot))

# 3. Uses descriptive activity names to name the activities in the data set.
act<- read.table("activity_labels.txt")
act[, 2] = gsub("_", "", tolower(as.character(act[, 2])))
Y_tot[,1] = act[Y_tot[,1], 2]
names(Y_tot) <- "activity"

# 4. Appropriately labels the data set with descriptive activity names.
names(S_tot) <- "subject"
cleaned <- cbind(Y_tot,S_tot,X_tot)

# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.
unSub = unique(S_tot)[,1]
nSub = length(unique(S_tot)[,1])
nAct = length(act[,1])
nCols = dim(cleaned)[2]
result = cleaned[1:(nSub*nAct), ]
row = 1
for (j in 1:nSub) {
  for (k in 1:nAct) {
    result[row, 1] = unSub[j]
    result[row, 2] = act[k, 2]
    tmp <- cleaned[cleaned$subject==j & cleaned$activity==act[k, 2], ]
    result[row, 3:nCols] <- colMeans(tmp[, 3:nCols])
    row = row+1
  }
}
write.table(result, "tidy_data.txt")