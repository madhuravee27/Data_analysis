######
# Bisecting K-Means and KNN Classifier
# Rename this file before submitting 
#####

require(RColorBrewer)
require(class)
require(caret)

# set seed to ensure consistent results
set.seed(100)


################################################
#   TA defined functions
#   Code has already been provided for you - you don't need to modify this 
###############################################
# Plot clustering result
plot_clustering_result <- function(data.df, result, title, k){
  # Plot using two columns in the data
  plot(data.df[, c(2,4)], col = brewer.pal(k, "Set1")[result[[1]]], pch = '.',
       cex = 3, main = title)
}

################################################
# GRADED FUNCTIONS
# Write your code using the given function definitions
# Input and output types are defined for each function
###############################################

# Implement bisecting k means.
# Input:
# data.df: data frame loaded using load_data() function
# iter.max: Max. number of trials for kmeans, as per Algorithm 8.2 in textbook. 
# You are allowed to use the pre-defined kmeans() function.
# k: Number of clusters to find in bisecting k-means

# Output:
# Your output/function return value will be a list containing 2 elements
# first element of the list is a vector containing cluster assignments (i.e., values 1 to k assigned to each data point)
# second element of the list is a vector containing SSE of each cluster
# Additional Information:
# When identifying which cluster to split, choose the one with maximum SSE
# When performing kmeans, pick two random points the cluster with largest SSE as centers at every iteration. 
# Ensure that the two centers being randomly picked are not the same.
# terminating condition: when k clusters have been found
# bisecting k means 
bkm <- function(data.df, iter.max, k){
  # Hint: Remember, the first column is of type ID. 
  # Don't use this column while clustering
  data.df$Label <- 0 #add a new column 'Label'
  clusterSSE <- c(0) #declare a cluster SSE vector
  labelValue <- 0 #initializing reference labelValue to determine which cluster to bisect as 0
  
  while(length(clusterSSE) < k){ #iterate until k clusters are formed
    labelValue <- which.max(clusterSSE) #find the label value corresponding to the cluster with max SSE
    clusterSSE <- clusterSSE[-labelValue] #remove this SSE value as new SSE values will be updated after clustering
    data.df$Label[data.df$Label == labelValue] <- 0 #set label values of data records whose label value is equal to reference label value to 0
    for (i in 1:nrow(data.df)){ #decrement the label values of records having label value greater than refernce label value to have uniform cluster numbering
      if (data.df[i,]$Label > labelValue){
        data.df[i,]$Label = data.df[i,]$Label - 1
      }
    }
    dataBisect <- subset(data.df, Label == 0) #select the cluster whose label values were reset to 0 for bisecting k means operation
    dataRemain <- subset(data.df, Label != 0) #retain the reamining clusters
    clusterUpdate <- kmeans(dataBisect[,2:5],2,iter.max = iter.max, nstart = 20, algorithm = c("Lloyd")) #perform bisecting k means on the selected cluster
    dataBisect$Label <- clusterUpdate$cluster + length(clusterSSE) #update the cluster label
    clusterSSE <- c(clusterSSE,clusterUpdate$withinss) #add new SSE to the SSE vector
    data.df <- rbind(dataRemain, dataBisect) #bind the data records that were earlier split for clustering
  }
  data.df <- data.df[order(data.df$ID),]
  listReturn = list(data.df[,'Label'], clusterSSE) #build list of label values of data records and SSE vector
  return (listReturn) #return generated list
}


# Write code for comparing kmeans with result from bisecting kmeans here - Part b
# Input:
# data.df:  Dataset used for kmeans/bisecting kmeans clustering 
# Result: Variable of type list, obtained on running bkm() function
# k : k value for k-means
# km_centers: ID values of centers for KMeans

#Returns:
# Nothing, just print the observations requested

kmeans_comparison <- function(data.df, result, k, km_centers){
  # First, run KMeans using km_centers and k. 
  # Compare outcomes of kmeans with bisecting kmeans in terms of:
  # 1. Overall SSE
  # 2. Plot the clusters and compare the size and shape of clusters generated
  # 3. Using the plot, also verify (visually) if points are being assigned to different clusters
  centers <- data.frame()  
  for (i in km_centers){   #dataframe of centers preparation
    centers <- rbind(centers,data.df[i,2:5])
  } 
  kmeansClustering <- kmeans(data.df[,2:5],centers = centers, iter.max = 20, algorithm = "Lloyd") #kmeans clustering
  print("K means overall SSE")
  print(kmeansClustering$tot.withinss) #total SSE for the 3 clusters
  plot_clustering_result(data.df, list(kmeansClustering$cluster, kmeansClustering$withinss), "KMeans outcome", k) #plotting kmeans outcome
  
  bkmSSE <- 0 #initializing total SSE of bkm as 0
  for (i in result[[2]]){ #iterating and finding total SSE
    bkmSSE <- bkmSSE + i
  }
  print("Bisecting K means overall SSE")
  print(bkmSSE)
  
  plot_clustering_result(data.df, result, "Bisecting KMeans outcome", k) #plotting outcome of bkm
}

euclideanDistCalc <- function(data1, data2){ #euclidean distance calculation
  dist = sqrt(sum((data1-data2)^2))
  return(dist)
}

# Write code for KNN algorithm
# implement my_knn with euclidean distance, majority vote and 
# randomly resolving ties
# you are NOT allowed to use the built-in knn function or the dist() function
# (i.e., implement the K Nearest Neighbors, also implement a function for euclidean distance calculation)

# Input: 
# train: training data frame
# test: test data frame
# cl: class labels for training data
# k: 'k' value in KNN

# Output:
# A vector of class labels. return variable should be of type factor
my_knn <- function(train, test, cl, k)
{
  testClassFinal <- c() #initialization
  
  for (i in 1:nrow(test)){
    euclideanDist <- numeric() 
    for (j in 1:nrow(train)){ #compute euclidean distance of each test data to all training data
      euclideanDist <- c(euclideanDist,euclideanDistCalc(train[j,],test[i,]))
    }
    trainDistClass <- data.frame(euclideanDist,cl) #construct dataframe of eu distance and class
    trainDistClassSort <- trainDistClass[order(trainDistClass$euclideanDist),] #sort in ascending order of eu distance
    trainDistClassK <- trainDistClassSort[1:k,] #take top k values
    testClass <- table(trainDistClassK$cl) #determine occurences of each class
    testClassMax <- names(testClass)[testClass == max(testClass)] #find max occurence
    if(length(testClassMax) == 1){ #if max occurence is one value, then assign the class
      testClassFinal <- c(testClassFinal, testClassMax)
    }
    else{ #or randomly pick from the classes listed for max occurence
      randomSelection <- sample(testClassMax, 1)
      testClassFinal <- c(testClassFinal, randomSelection)
    }
  }
  return (factor(testClassFinal)) #return class name
}

# Generate accuracy measures for your KNN classification
# Input:
# test_cl: actual class labels for test data
# knn_result: predicted class labels for test data

# Output:
# A vector of size 4 in the following order: 
# (overall accuracy, precision for the class 'setosa', recall for the class 'setosa', F-measure for the class 'setosa')

# DONOT use predefined libraries to calculate the accuracy measures
# Implement the formulae by generating the confusion matrix using the table() function
my_knn_accuracy <- function(test_cl, knn_result){
  results <- as.character(knn_result)
  test_cl <- as.character(test_cl)
  matrix <- as.matrix(table(test_cl = test_cl, knn_result = results)) #confusion matrix
  count <- sum(matrix)  #total data
  classesCount <- nrow(matrix) #data from each class
  correctCount <- diag(matrix) #data points predicted correctly
  rowsums <- apply(matrix, 1, sum) 
  colsums <- apply(matrix, 2, sum)
  accuracy <- (sum(correctCount) / count)*100 #accuracy
  precision <- correctCount / colsums #precision
  recall <- correctCount / rowsums #recall
  f1 <- 2 * precision * recall / (precision + recall) #f1
  matrixDf <- data.frame(precision,recall,f1)
  returnVector <- c(accuracy,matrixDf[1,1],matrixDf[1,2],matrixDf[1,3]) #return vector
  return (returnVector)
}
