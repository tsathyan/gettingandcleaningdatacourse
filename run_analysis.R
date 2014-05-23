library(data.table) # required for %like%
library(plyr) # required for mapvalues
library(reshape2) # required for melt, dcast

# various filenames
trainDataFile = "train/X_train.txt"
trainActivityFile <- "train/y_train.txt"
trainSubjectFile <- "train/subject_train.txt"

testDataFile <- "test/X_test.txt"
testActivityFile <- "test/y_test.txt"
testSubjectFile <- "test/subject_test.txt"

featuresFile <- "features.txt"
activityLabelFile <- "activity_labels.txt"

# read test and training measurements, activities, subjects
testData <- read.table(testDataFile)
trainData <- read.table(trainDataFile)

testActivity <- read.table(testActivityFile)
trainActivity <- read.table(trainActivityFile)

testSubject <- read.table(testSubjectFile)
trainSubject <- read.table(trainSubjectFile)

# Q1: merged training and test data
mergedData <- rbind(cbind(trainData,trainActivity,trainSubject), 
                    cbind(testData,testActivity,testSubject))

# read features from file, extract those features that are related to mean and
# standard devication, and convert the feature names to strings
features <- read.table(featuresFile)
featuresToExtract <- features$V2 %like% "mean()" | features$V2 %like% "std()"
featureNames <- as.character(features$V2[featuresToExtract])

# append the locations of the features of interest with the last two columns
# which relate to activity and subject
featuresToExtract <- c(featuresToExtract,TRUE,TRUE)
# extend the feature names vector to include activity and subject
featureNames <- c(featureNames,"activity","subject")
# convert all feature names to lower case, remove braces, and hyphen
featureNames <- tolower(featureNames)
featureNames <- gsub("\\(\\)", "", featureNames)
featureNames <- gsub("-", "", featureNames)

# Q2: extract data that contain means and standard deviations
extractedData <- mergedData[,featuresToExtract]
# Q4: set names the names of the extractedData to meaningful names
names(extractedData) <- featureNames

# read activity labels from file, covert names to lower case values and remove underscore
activityLabels <- read.table(activityLabelFile)
activityLabels[,2] <- tolower(as.character(activityLabels[,2]))
activityLabels[,2] <- gsub("_", "", activityLabels[,2])

# Q3: use descriptive names for the activities in the extracted dataset
extractedData$activity <- mapvalues(extractedData$activity,activityLabels[,1],activityLabels[,2])

# Q5: independent tidy data set for each with the average of each variable for each activity and each subject
meltedData <- melt(extractedData,id.vars=c("subject", "activity"))
castData <- dcast(meltedData, subject + activity ~ variable, mean)

# write results to a file with tap separation
write.table(castData, "tidydata.txt", sep = "\t", col.names = TRUE, row.names = FALSE)
