##STEP 1 Merges the training and the test sets to create one data set.

## read test and train data then combine two data sets. 
test_data<-read.table("./test/X_test.txt",header=FALSE,row.names=NULL)
test_subject<-read.table("./test/subject_test.txt",header=FALSE,row.names=NULL)
test_data$subject<-test_subject$V1

train_data<-read.table("./train/X_train.txt",header=FALSE,row.names=NULL)
train_subject<-read.table("./train/subject_train.txt",header=FALSE,row.names=NULL)
train_data$subject<-train_subject$V1

all_data<-rbind(test_data,train_data)

## read features file and names all_data variables
feature<-read.table("features.txt",header=FALSE,row.names=NULL)
feature <- data.frame(lapply(feature, as.character), stringsAsFactors=FALSE)
names(all_data)<-c(feature[,2],"subject")


## STEP 2 Extracts only the measurements on the mean and standard deviation for each measurement. 
sub_data<-all_data[,1:6]
sub_data$subject<-all_data$subject


## STEP 3 Uses descriptive activity names to name the activities in the data set
## read activity labels of two data sets and rbine them
test_act<-read.table("./test/y_test.txt",header=FALSE,row.names=NULL)
train_act<-read.table("./train/y_train.txt",header=FALSE,row.names=NULL)
act_data<-rbind(test_act,train_act)

act_label<-read.table("activity_labels.txt",header=FALSE,row.names=NULL)
act_label<-data.frame(lapply(act_label, as.character), stringsAsFactors=FALSE)

label_data<-merge(act_data,act_label,by.X=act_data$V1,by.Y=act_label$V1)

## STEP 4 Appropriately labels the data set with descriptive variable names. 
new_data<-sub_data
new_data$act_label<-label_data$V2


## STEP 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
names(new_data)<-c("V1","V2","V3","V4","V5","V6","subject","act_label")
new_data$subject<-as.factor(new_data$subject)
new_data$act_label<-as.factor(new_data$act_label)

# group data and calculate the mean of each variable
tidy_data<-group_by(new_data,subject,act_label)
tidy_data<-summarize(tidy_data,mean(V1),mean(V2),mean(V3),mean(V4),mean(V5),mean(V6))
names(tidy_data)<-c("subject","act_label","tBodyAcc_mean()_X","tBodyAcc_mean()_Y","tBodyAcc_mean()_Z",
                    "tBodyAcc_std()_X","tBodyAcc-std()-Y","tBodyAcc-std()-Z")

# write results to a txt file
write.table(tidy_data,file="tidy_data.txt",row.name=FALSE)


