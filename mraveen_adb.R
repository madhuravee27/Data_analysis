######
# Adaboost Classifier
# Student Name: Madhumitha Raveendar
# Student Unity ID:mraveen
######

# Do not clear your workspace
require(mlbench)
require(rpart) # for decision stump
require(caret)

# set seed to ensure reproducibility
set.seed(100)

# calculate the alpha value using epsilon
# params:
# Input: 
# epsilon: value from calculate_epsilon (or error, line 7 in algorithm 5.7 from Textbook)
# output: alpha value (single value) (from Line 12 in algorithm 5.7 from Textbook)
###
calculate_alpha <- function(epsilon){
alpha <- ((1/2)*log((1-epsilon)/epsilon))
return (alpha)
}

# calculate the epsilon value  
# input:
# weights: weights generated at the end of the previous iteration
# y_true: actual labels (ground truth)
# y_pred: predicted labels (from your newly generated decision stump)
# n_elements: number of elements in y_true or y_pred
# output:
# just the epsilon or error value (line 7 in algorithm 5.7 from Textbook)
###
calculate_epsilon <- function(weights, y_true, y_pred, n_elements){
  sum_of_weights <- 0
  for (position in 1:length(y_true)){
  if (y_true[position] != y_pred[position]){
    sum_of_weights <- sum_of_weights + weights[position]
  }
  }
  epsilon <- ((1/n_elements)*sum_of_weights)
  return(epsilon)
}


# Calculate the weights using equation 5.69 from the textbook 
# Input:
# old_weights: weights from previous iteration
# alpha: current alpha value from Line 12 in algorithm 5.7 in the textbook
# y_true: actual class labels
# y_pred: predicted class labels
# n_elements: number of values in y_true or y_pred
# Output:
# a vector of size n_elements containing updated weights
###
calculate_weights <- function(old_weights, alpha, y_true, y_pred, n_elements){
new_weights <- c()
  for(i in 1:length(y_true)){
  updated_weight <- 0
  if (y_true[i] != y_pred[i]){
    updated_weight <- (old_weights[i] * exp(alpha))
  }
  else{
    updated_weight <- (old_weights[i] * exp(-(alpha)))
  }
  new_weights <- c(new_weights,updated_weight)
}
new_weights_normalized <- new_weights/sum(new_weights)
return(new_weights_normalized)
}

# implement myadaboost - simple adaboost classification
# use the 'rpart' method from 'rpart' package to create a decision stump 
# Think about what parameters you need to set in the rpart method so that it generates only a decision stump, not a decision tree
# Input: 
# train: training dataset (attributes + class label)
# k: number of iterations of adaboost
# n_elements: number of elements in 'train'
# Output:
# a vector of predicted values for 'train' after all the iterations of adaboost are completed
###
myadaboost <- function(train, k, n_elements){
initial_weight <- (1/n_elements)
train$Weight <- initial_weight
labels_matrix <- double()
for (i in 1:k){
sampled_index <- sample(seq_len(nrow(train)),n_elements,replace = TRUE, prob = train$Weight)
sampled_data <- train[sampled_index,]
decision_stump <- rpart(sampled_data$Label ~ .,sampled_data, method = "class", control = rpart.control(maxdepth = 1, minsplit = 0))
new_Labels <- predict(decision_stump, train, type = "class")
epsilon <- calculate_epsilon(train$Weight,train[,ncol(train)-1], new_Labels, n_elements)
if(epsilon > 0.5){
  train$Weight <- initial_weight
  i <- i - 1 
}
else{
alpha <- calculate_alpha(epsilon)
train$Weight <- calculate_weights(train$Weight, alpha, train[,ncol(train)-1], new_Labels, n_elements)
labels_matrix <- rbind(labels_matrix, as.numeric(as.character(new_Labels)))
}
}
labels_matrix <- t(labels_matrix)
final_labels <- c(nrow(labels_matrix))
for(i in 1:nrow(labels_matrix)){
  vector_labels_matrix <- c(labels_matrix[i,])
  final_labels[i] <- names(which(table(vector_labels_matrix) == max(table(vector_labels_matrix))))
}
return(final_labels)
}

# Code has already been provided here to preprocess the data and then call the adaboost function
# Implement the functions marked with ### before this line
data("Ionosphere")
Ionosphere <- Ionosphere[,-c(1,2)]
# lets convert the class labels into format we are familiar with in class
# -1 for bad, 1 for good (create a column named 'Label' which will serve as class variable)
Ionosphere$Label[Ionosphere$Class == "good"] = 1
Ionosphere$Label[Ionosphere$Class == "bad"] = -1
# remove unnecessary columns
Ionosphere <- Ionosphere[,-(ncol(Ionosphere)-1)]
# class variable
cl <- Ionosphere$Label
# train and predict on training data using adaboost
predictions <- myadaboost(Ionosphere, 5, nrow(Ionosphere))
# generate confusion matrix
print(table(cl, predictions))