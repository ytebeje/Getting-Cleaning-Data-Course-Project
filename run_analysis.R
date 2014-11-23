#++++++++++++++++++++++++++++++++++++++++++++++++++
library(plyr)
#STEP ZERO : READ THE DATA & ASSIGN COLUMN NAMES TO EACH 

# Read the features and Activity labels files 
features<-read.table("./features.txt",header=FALSE)
activityLabels<-read.table("./activity_labels.txt",header=FALSE)
#Assign Column names 
colnames(activityLabels)<-c("activityId","activityType")
head(activityLabels)

# Read the  train data's
subjectTrain<-read.table("./train/subject_train.txt",header=FALSE)
xTrain<-read.table("./train/x_train.txt",header=FALSE)
yTrain <-read.table("./train/y_train.txt",header=FALSE)

# Assign Column names for train data
colnames(subjectTrain)<-"subjectId"
colnames(xTrain)<-features[,2]
colnames(yTrain)<-"activityId"

# Read the test data 
subjectTest<-read.table("./test/subject_test.txt",header=FALSE)
xTest<-read.table("./test/x_test.txt",header=FALSE)
yTest<-read.table("./test/y_test.txt",header=FALSE)

# Assign Column names for test data 
colnames(subjectTest)<-"subjectId"
colnames(xTest)<-features[,2]
colnames(yTest)<-"activityId"
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# STEP ONE: Merge the training and the test sets to create one data set. 
MergedData<-rbind(cbind(yTrain,subjectTrain,xTrain), cbind(yTest,subjectTest,xTest))


# STEP TWO: Extracts only the measurements on the mean and standard deviation for each measurement. 
head(MergedData, n=1)
mean_std<-MergedData[,grepl("mean|std|subject|activityId", names(MergedData))]


#STEP THREE: Uses descriptive activity names to name the activities in the data set

MergedData<-merge(MergedData,activityType, by="activityId", all.x=TRUE)

# STEP FOUR: Appropriately labels the data set with descriptive variable names. 

#  Create a name list vector
Names<-colnames(MergedData)
#use a descriptive variable name
for ( i in 1: length(Names))
{
  Names[i] <-gsub("\\()","",Names[i])
  Names[i] <-gsub("-std$","StdDev",Names[i])
  Names[i] <- gsub("-mean","Mean",Names[i])
  Names[i] <- gsub("^(t)","time",Names[i])
  Names[i] <- gsub("^(f)","freq",Names[i])
  Names[i] <- gsub("([Gg]ravity)","Gravity",Names[i])
  Names[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",Names[i])
  Names[i] <- gsub("[Gg]yro","Gyro",Names[i])
  Names[i] <- gsub("AccMag","AccMagnitude",Names[i])
  Names[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",Names[i])
  Names[i] <- gsub("JerkMag","JerkMagnitude",Names[i])
  Names[i] <- gsub("GyroMag","GyroMagnitude",Names[i])
}

#Assign the new names to the merged data
colnames(MergedData)<-Names

# STEP FIVE: From the data set in step 4, creates a second, independent tidy data set 
            #with the average of each variable for each activity and each subject.

# Remove Activity Type
NewData  = MergedData[,names(MergedData) != 'activityType'];

# Recalculate the mean
tidyData    = aggregate(NewData[,names(NewData) != c('activityId','subjectId')],by=list(activityId=NewData$activityId,subjectId = NewData$subjectId),mean);

# Include descriptive acitvity names
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=FALSE,sep='\t');
