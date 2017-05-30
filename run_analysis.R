# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names.
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


XtestURL  <- "ENTER FILEPATH/FILENAME"
XtrainURL <- "ENTER FILEPATH/FILENAME"
YtestURL <-  "ENTER FILEPATH/FILENAME"
YtrainURL <- "ENTER FILEPATH/FILENAME"
SubTest <- "ENTER FILEPATH/FILENAME"
SubTrain <- "ENTER FILEPATH/FILENAME"
featuresURL <- "ENTER FILEPATH/FILENAME"
activityURL <- "ENTER FILEPATH/FILENAME"
tidyDataSetFile <- "ENTER FILEPATH/FILENAME"

#### files defined in variables above outside function  ###
## MISREAD- FUnction not required 
##run_analysis <- function()     
##{
  
  ### 1. merged data and training sets create one set ####
  Xtest <- read.table(XtestURL)
  Xtrain <- read.table(XtrainURL)
  Ytest <- read.table(YtestURL)
  Ytrain <- read.table(YtrainURL)
  features <- read.table(featuresURL)
  activityLabels <- read.table(activityURL)
  subTrain <- read.table(SubTrain)
  subTest <- read.table(SubTest)
  
  
  # bind rows. row count the same
  
  XSetsNames <- features
  names(Xtrain) <- XSetsNames[,2]
  names(Xtest) <- XSetsNames[,2]
  ## (a) added names to activities 
  Ytrain <- replace(Ytrain,Ytrain[1] == 1,"WALKING")
  Ytrain <- replace(Ytrain,Ytrain == 2,"WALKING_UPSTAIRS")
  Ytrain <- replace(Ytrain,Ytrain == 3,"WALKING_DOWNSTAIRS")
  Ytrain <- replace(Ytrain,Ytrain == 4,"SITTING")
  Ytrain <- replace(Ytrain,Ytrain == 5,"STANDING")
  Ytrain <- replace(Ytrain,Ytrain == 6,"LAYING")
  
  Ytest <- replace(Ytest,Ytest[1] == 1,"WALKING")
  Ytest <- replace(Ytest,Ytest == 2,"WALKING_UPSTAIRS")
  Ytest <- replace(Ytest,Ytest == 3,"WALKING_DOWNSTAIRS")
  Ytest <- replace(Ytest,Ytest == 4,"SITTING")
  Ytest <- replace(Ytest,Ytest == 5,"STANDING")
  Ytest <- replace(Ytest,Ytest == 6,"LAYING")
  
  
  
  names(Ytrain) <- "Activity"
  names(Ytest) <- "Activity"
  names(subTrain) <- "Subject"
  names(subTest) <- "Subject"
  
  
  XSets <- cbind(Xtrain,Ytrain,subTrain)
  YSets <- cbind(Xtest,Ytest,subTest)
  CompleteSets <- rbind(XSets, YSets)
  
  ### 2. Extracts only the measurements on the mean and standard deviation for each measurement.####
  #need to rename to get measurements 
  
  
  mean_std <- grep(("(mean|std)\\(\\)")
                   , names(CompleteSets)
  )
  
  
  mean_stdSet <- CompleteSets[, mean_std]
  mean_stdSet <-cbind(mean_stdSet,CompleteSets[562:563])
  
  ### 3. Uses descriptive activity names to name the activities in the data set ####
  #### !!! See note (a) line 42. Renamed in compilation of set  ####
  
  ### 4. Appropriately labels the data set with descriptive variable names.####
  #using gsub to replace occurances of string pattern 
  #See features_info.txt for variable names and descriptions
  newMeanName <- gsub("-mean\\(\\)", "Mean", names(mean_stdSet), ignore.case=FALSE
  )
  names(mean_stdSet) <- newMeanName
  newStdDevName <- gsub("-std\\(\\)", "StdDev", names(mean_stdSet), ignore.case=FALSE
  )
  names(mean_stdSet) <- newStdDevName
  timeNames <- gsub("^t", "Time", names(mean_stdSet), ignore.case=FALSE
  )
  names(mean_stdSet) <- timeNames
  newFreqName <- gsub("^f", "Frequency", names(mean_stdSet), ignore.case=FALSE
  )
  names(mean_stdSet)  <- newFreqName
  
  #### 5. From the data set in step 4, creates a second, independent tidy data set with ####
  # the average of each variable for each activity and each subject. 
  # Fixed where function numcolwise is needed to get mean on columns
  
  
  subAct_Means = plyr::ddply(mean_stdSet, c("Subject","Activity"),plyr::numcolwise(mean)
  )
  y= subAct_Means
  # commenting out write as it did not specify to write the set to a directory. Using print to show the results in console 
  write.csv(y,tidyDataSetFile)
  print(y)
##}



