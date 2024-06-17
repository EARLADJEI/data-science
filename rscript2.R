#-----Section 01-------------------------------------------
# get the data

# set working directory
setwd(dirname(file.choose()))
getwd()
# data reading and understanding
fraud <- read.csv("Fraudulent_E-Commerce_Transaction_Data_2.csv", stringsAsFactors = FALSE)
head(fraud)
str(fraud)
summary(fraud)

# general checking for missing values
apply(fraud, MARGIN = 2, FUN = function(x) sum(is.na(x)))
library(Amelia)
missmap(fraud, col = c("black", "grey"), legend = FALSE)

# casewise deletion if necessary
scale_fraud <- na.omit(scale_fraud)
 # explore the dataset
 boxplot(fraud$Customer.Age)
boxplot(fraud[c(3,7,8, 14,15,16)]) 
 summary(fraud$Quantity)
 summary(fraud$Transaction.Amount)
 prop.table(fraud$Quantity)
  
 # create a dataset with relevant variable
 relevant_data <- fraud[, c(3,5,7,8,13,14,15,16)]
boxplot(relevant_data[-2])
relevant_data<- relevant_data[-5]
str(relevant_data)



# Select only numeric columns
numeric_fraud <- select_if(fraud, is.numeric)
# Calculate correlation
correlation_matrix <- cor(numeric_fraud, use = "pairwise.complete.obs", method = "spearman")
scale_fraud<- scale(numeric_fraud)

boxplot(scale_fraud)
#numeric_fraud <- select_if(fraud, is.numeric)
# Convert the scaled data back to a dataframe
#scaled_df <- as.data.frame(numeric_fraud)

scaled_diabetes <- cbind(scaled_df, diabetes[, -which(names(diabetes) %in% numerical_features)])
boxplot 
plot(relevant_data)
ggplot(relevant_data)

# create a label for the target group
relevant_data$Is.Fraudulent <- factor(relevant_data$Is.Fraudulent, levels = c("0", "1"),
                                   labels = c("0", "1"))
str(relevant_data)
#-----Section 03-------------------------------------------
# create a random sample for training and test data
# use set.seed to use the same random number sequence each time
set.seed(12345679)
relevant_data <- relevant_data[order(runif(20000)), ]#-----Section 04-------------------------------------------
# split the data frames

relevant_data.tr <- relevant_data[1:16000, ]
relevant_data.ts <- relevant_data[16001:20000, ]


# compare the credit and credit_rand data frames
summary(diabetes$HighBP)
summary(diabetes_rand$HighBP)
str(diabetes$HighChol)
str(diabetes_rand$HighChol)
summary(diabetes)
prop.table(table(diabetes$Diabetes_binary))

# check the proportion of class variable
prop.table(table(relevant_data.tr$Is.Fraudulent))
prop.table(table(relevant_data.ts$Is.Fraudulent))

#-----Section 05-------------------------------------------
# training a model on the data
# build the simplest decision tree

library(C50)

set.seed(12345)
fraud_model <- C5.0(relevant_data.tr[-5], relevant_data.tr$Is.Fraudulent)

# display simple facts about the tree
fraud_model
lenght()
# display detailed information about the tree
summary(fraud_model)
varImp(fraud_model)
# Visualize decision tree
plot(fraud_model)

#-----Section 06-------------------------------------------
# evaluating model performance

# create a factor vector of predictions on test data
fraud_pred1 <- predict(fraud_model, relevant_data.ts)
# cross tabulation of predicted versus actual classes
library(gmodels)
CrossTable(relevant_data.ts$Is.Fraudulent, fraud_pred1,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

# more diagnostics
library(caret)
confusionMatrix(fraud_pred1, relevant_data.ts$Is.Fraudulent, positive = "0")
