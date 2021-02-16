library(readr)
library(dplyr)
library(randomForest)
library(ggplot2)

#loading the train and test sets of MNIST dataset 
train_set <- read_csv("mnist_train.csv", col_names = FALSE)
test_set <- read_csv("mnist_test.csv", col_names = FALSE)

#extracting the labels
#converting digits to factors
train_labels <- as.factor(train_set[, 1]$X1)
test_labels <- as.factor(test_set[, 1]$X1)

#printing the first 10 labels
head(train_labels, 10)

#printing number of records for each digit (0 to 9)
summary(train_labels)

# reduce size of data set if needed using the following 4 lines
#train_set <- train_set[ 1:1000, ]
#train_labels <- train_labels[ 1:1000 ]
#test_set <- test_set[ 1:100, ]
#test_labels <- test_labels[ 1:100 ]

#training the model
rf <- randomForest(x = train_set, y = train_labels, xtest = test_set, ntree = 50)
rf

#1- error rate
#represents the accuracy 
1 - mean(rf$err.rate)

#error rate for every digit
err_df <- as.data.frame(rf$err.rate)
mymeans <- err_df %>%
    select(-"OOB") %>%
    colMeans()

#creating a dataframe containing the indices and means
toplot <- data.frame( index = seq_len( length( mymeans ) ) - 1,
                      col_means = mymeans )

#plotting the numbers vs. error using ggplot2
ggplot( toplot, aes( x = index, y = col_means ) ) +
    geom_line() +
    geom_point() + 
    scale_x_continuous(breaks = seq_len( length( mymeans ) ) - 1 )+
    labs(title = "Handwritten Digit Classifier", x = "Numbers", y = "Error")

