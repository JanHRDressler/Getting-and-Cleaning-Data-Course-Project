#Code starts with steps 1, 3 and 4
    #1. Merge the training and the test sets to create one data set
    #3. Use descriptive activity names to name the activities in the data set
    #4. Appropriately label the data set with descriptive variable name
    #If your working directory contains the unzipped folder UCI HAR Dataset the following code works
        #read train data
            X_train = read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)
            y_train = read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE)
            subject_train = read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)
        #read test data
            X_test = read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)
            y_test = read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE)
            subject_test = read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)
        #read labels
            activity_labels = read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE)
            features = read.table("./UCI HAR Dataset/features.txt", header = FALSE)
        #give labels colnames
            colnames(activity_labels) = c("activity_num","activity_char")
        #give train data colnames
            features = t(features)
            features = features[2,]
            colnames(X_train) = features
            colnames(y_train) = "activity_num"
            colnames(subject_train) = "subject"
        #give test data colnames
            colnames(X_test) = features
            colnames(y_test) = "activity_num"
            colnames(subject_test) = "subject"
        #merge the data to the first data set data1
            data1 = rbind(cbind(subject_train,y_train,X_train),cbind(subject_test,y_test,X_test))
            data1 = merge(activity_labels,data1,by.x="activity_num",by.y="activity_num")
        #reorder the data set data1 by subject, swap subject to first col
            data1 = data1[order(data1$subject),]
            data1 = data1[,c(c(3,1,2),4:564)]

#Code continuous with step 2
    #2. Extract only the measurements on the mean and standard deviation for each measurement 
        #load dplyr
            library(dplyr)
        #extract the first two cols (subject and activity_num)
            data1sup = data1[,1:3]
        #select all the cols with "mean" or "std" in the headline
            data1 = select(data1,contains(c("mean","Mean","std","Std")))
        #put those cols together with subject and activity_num again
            data1 = cbind(data1sup,data1)

#Code finishes with step 5
    #5. From the data set in step 4 (here 2.), create a second, independent tidy data set with 
    #the average of each variable for each activity and each subject
        #swap first two cols (subject and activity_num)
            data2sup = data1[,c(c(2,1),4:89)]
        #split data1 by activity_num, then by subject
            data2sup = split(data2sup,data2sup[,1:2])
        #loop colMeans through all 180 matrices in that list 
        #(1 matrix for each of the 6 activities for each of the 30 subjects)
            #create support matrix
                data2 = matrix(0,nrow=1,ncol=88)
            #loop
                for (i in 1:180){
                    data2sup2 = t(as.matrix(colMeans(as.data.frame(data2sup[i]))))
                    data2 = rbind(data2, data2sup2)
                }
        #cut off the first row (support matrix created for the loop)
            data2 = data2[2:181,c(c(2,1),3:88)]
        #use the same colnames as in data1 again
            colnames(data2) = names(data1[,c(c(1,2),4:89)])
        #merge with activity_char again
            data2 = merge(activity_labels,data2,by.x="activity_num",by.y="activity_num")
        #swap subject to first col again
            data2 = data2[,c(c(3,1,2),4:89)]
        #order by subject again
            data2 = data2[order(data2$subject),]
        #show head of data2  
            head(data2)
        #Export as .txt
            write.table(data2,"./run_analysis_tidy2.txt", row.names = FALSE)
            
#Read run_analysis_tidy2.txt
    data2read = read.table("./run_analysis_tidy2.txt", header = TRUE)
