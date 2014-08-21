
# Bill Wolf
# Get+Clean Data Course Project 1
# 8/10/14

# Task: create one R script called run_analysis.R that does the following: 
#   1. Merge the training and the test sets to create one data set.
#   2. Extract the measurements on the mean and standard deviation for each measurement. 
#   3. Uses descriptive activity names to name the activities in the data set
#   4. Appropriately label the data set with descriptive variable names. 
#   5. Creates a second, independent tidy data set with the average of each 
#        variable by activity and subject. 




############################################
# Step 1: create combined data set
############################################

read.UCI.HAR.Data <- function(zipFile,fileToExtract,columnLabels) {
    ## utility function to normalize and read UCI HAR Data measurement files.
    ## The data files appear to be space and new line seperated. There
    ## is a negative sign in front of negative numbers but an "extra" space
    ## in front of positive values.  A different way of thinking of this is
    ## as fixed width column values.  read.fwf() maybe could have read
    ## the file but I wanted to use read.csv().
    
    # read content of input file as a text blob to start 
    rawConn<-unz(zipFile,fileToExtract)
    rawText<-readLines(rawConn)
    close(rawConn)
    
    # normalize the text by removing leading and training spaces and
    # collapse each consecutive white space substrings to a single space
    for (i in 1:length(rawText)) {
        rawText[i]<-gsub("^\\s+|\\s+$", "",rawText[i]) # leading, training
        rawText[i]<-gsub("\\s+"," ",rawText[i])        # collapse multiple
    }
    
    # read the normalized file into data frame 
    normalizedConn<-textConnection(rawText)
    df <-read.csv( normalizedConn, 
                    sep=" ",                    
                    stringsAsFactors=FALSE,
                    header=FALSE)
    close(normalizedConn)
    
    # apply specified column labels (choosing not to do it in the read.csv call to work
    # around the the punctuation and other characters embedded in some of the names)
    colnames(df)<-columnLabels
    
    # return the data in a data frame
    df
}

# load raw data
remoteZip<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
localRoot <-"UCI HAR Dataset"
localZip <- paste(localRoot,".zip",sep="")

# download the file to current working directory if it hasnt been downloaded already
if (!file.exists(localZip)) {
    download.file(remoteZip,localZip,method="auto")
}

# get  variable labels from the study's documentation
colLabels<-read.csv( unz(localZip,paste(localRoot,"features.txt",sep="/")),
                     sep=" ",
                     colClasses=c("numeric","character"), 
                     col.names=c("num","variable"),
                     header=FALSE)

# load measurement data using the study's variable labels as column names
x.train <- read.UCI.HAR.Data(localZip,
                             paste(localRoot,"train/X_train.txt",sep="/"),
                             colLabels$variable)
x.test <- read.UCI.HAR.Data(localZip,
                            paste(localRoot,"test/X_test.txt",sep="/"),
                            colLabels$variable)

# load subject data 
x.train.subject<- read.csv( unz(localZip,paste(localRoot,"train/subject_train.txt",sep="/")),
                            col.names="subject",
                            colClasses="numeric",   # make it a factor in step 3
                            header=FALSE)
x.test.subject<- read.csv( unz(localZip,paste(localRoot,"test/subject_test.txt",sep="/")),
                           col.names="subject",
                           colClasses="numeric",    # make it a factor in step 3
                           header=FALSE)
 
# load activity data 
y.train <- read.csv( unz(localZip,paste(localRoot,"train/y_train.txt",sep="/")),
                            col.names="activity",
                            colClasses="numeric",  # make it a factor later in step 3
                            header=FALSE)
y.test <- read.csv( unz(localZip,paste(localRoot,"test/y_test.txt",sep="/")),
                           col.names="activity",
                           colClasses="numeric",   # make it a factor later in step 3
                           header=FALSE)

# combine training measurement data, activity labels and subject labels
combined.train <- cbind(x.train, x.train.subject, y.train)

# combine test measurement data, activity labels and subject labels
combined.test <- cbind(x.test, x.test.subject, y.test)
 
# combine train, test data into one overall combined data frame
combined.all <- rbind(combined.train, combined.test)

############################################
# Step 1: done - we have a combined data set!!
############################################



############################################
# Step 2: Down select to mean, sd variables
############################################

# pare this down to "mean", "standard deviation", "subject" and "activity"
#   columns.  The study uses a variable naming convention that helps us here.
#   The include the strings "mean()" and "sd()" to variables with those data
#   values, so we will use that to select which to retain.  There are other 
#   variable names in the data set that also contain the string "Mean" in 
#   the name but my reading of the study documentation made me decide not 
#   to include them.

retainList<-grep("mean\\(\\)|std\\(\\)|activity|subject",colnames(combined.all),value=TRUE)

# here is the subset of fields as a new data frame
combined.subset<-combined.all[,retainList]

############################################
# Step 2: done - we have the subest!!
############################################



############################################
# Step 3: apply descriptive labels to activities
############################################

# apply descriptive labels to the activity measurements
#   use the labels taken from the study documentation
combined.subset$activity<-factor(combined.subset$activity,
                                 levels=c(1,2,3,4,5,6),
                                 ordered=TRUE,
                                 labels=c( "WALKING",
                                           "WALKING_UPSTAIRS",
                                           "WALKING_DOWNSTAIRS",
                                           "SITTING",
                                           "STANDING",
                                           "LAYING"))

############################################
# Step 3: done - activity labels are applied!!
############################################



############################################
# Step 4: Apply descriptive labels to variables
############################################

## Replace the studies variable names with more descriptive labels
##   make every name component lower case and dot seperated; get rid of embedded punctuation

improvedColumnNames<-colnames(combined.subset)

for (i in 1:length(improvedColumnNames)) {

  ## correct BodyBody to Body (fix what looks like a naming mistake in the data)
  improvedColumnNames[i]<-gsub(pattern="BodyBody",replacement="Body",improvedColumnNames[i])
    
  ## normalize t to time; f to freq
  improvedColumnNames[i]<-sub(pattern="^t",replacement="time.",improvedColumnNames[i])
  improvedColumnNames[i]<-sub(pattern="^f",replacement="freq.",improvedColumnNames[i])

  ## normalize to Body to body; Gravity to grav Acc to acc; Gyro to gyro; Jerk to jerk; Mag to mag 
  improvedColumnNames[i]<-sub(pattern="Body",replacement="body.",improvedColumnNames[i])
  improvedColumnNames[i]<-sub(pattern="Gravity",replacement="grav.",improvedColumnNames[i])
  improvedColumnNames[i]<-sub(pattern="Acc",replacement="acc.",improvedColumnNames[i])
  improvedColumnNames[i]<-sub(pattern="Gyro",replacement="gyro.",improvedColumnNames[i])
  improvedColumnNames[i]<-sub(pattern="Jerk",replacement="jerk.",improvedColumnNames[i])
  improvedColumnNames[i]<-sub(pattern="Mag",replacement="mag.",improvedColumnNames[i])
 
  ## normalize to Mean() to mean; std() to std 
  improvedColumnNames[i]<-sub(pattern="-mean\\(\\)",replacement="mean",improvedColumnNames[i])
  improvedColumnNames[i]<-sub(pattern="-std\\(\\)",replacement="std",improvedColumnNames[i])

  ## normalize to X to x; Y to y; Z to z 
  improvedColumnNames[i]<-sub(pattern="-X",replacement=".x",improvedColumnNames[i])
  improvedColumnNames[i]<-sub(pattern="-Y",replacement=".y",improvedColumnNames[i])
  improvedColumnNames[i]<-sub(pattern="-Z",replacement=".z",improvedColumnNames[i])
}

colnames(combined.subset)<-improvedColumnNames

############################################
# Step 4: done - variables have descriptive labels!!
############################################



############################################
# Step 5: create tidy data set of mean for each activity and subject
############################################

#  make the subject data a factor and specify that order counts so it will
#   sort intuitively (otherwise it will sort lexographically)
combined.subset$subject<-factor(combined.subset$subject,
                                levels=c(1:30),
                                ordered=TRUE)

# collect the average by activity in activity_mean() matrix
tmp.list<-list()
i<-1
for (variable in colnames(combined.subset)[1:(length(colnames(combined.subset))-2)]) {
    tmp.list[[i]]<-tapply(combined.subset[,variable],combined.subset$activity,mean)
    i<-i+1
}
activity_mean<-do.call(rbind,tmp.list)
rownames(activity_mean)<-colnames(combined.subset)[1:(length(colnames(combined.subset))-2)]


# collect the average by subject in subject_mean() matrix
tmp.list<-list()
i<-1
for (variable in colnames(combined.subset)[1:(length(colnames(combined.subset))-2)]) {
    tmp.list[[i]]<-tapply(combined.subset[,variable],combined.subset$subject,mean)
    i<-i+1
}
subject_mean<-do.call(rbind,tmp.list)
rownames(subject_mean)<-colnames(combined.subset)[1:(length(colnames(combined.subset))-2)]


# combine the activity and subject mean data for output
final.data<-rbind(t(activity_mean),t(subject_mean))

# output the data to current working directory
write.table(final.data,"UCI_HAR_Data_output_wjwolf.txt",row.name=FALSE)

############################################
# Step 5: all done - we have tidy data set of activity and subject mean values!!
############################################ 






