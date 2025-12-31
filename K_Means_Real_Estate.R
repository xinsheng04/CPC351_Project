# Put your working directories over here
setwd("C:/Users/Asus/OneDrive/Desktop/myProjects/Y3S1/CPC 351/CPC351_Project")

#-----------------------------------------------------------------------------
# This R file tackles the real_estate dataset, 
# performing Data Preprocessing, EDA and model experimentation
# in order to answer the problem statement
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Import csv file
#-----------------------------------------------------------------------------
dataset <- read.csv("Project_1_Datasets/real_estate_dataset.csv", header=TRUE)
# Inspect dataset features
cat("Dimensions of the dataset: ", dim(dataset))

# Structure of the dataset
str(dataset)

# Summary of dataset
summary(dataset)

# Observe examples from the dataset
head(dataset)
dataset[sample(nrow(dataset), size=5, replace=TRUE),]

#-----------------------------------------------------------------------------
# Data preprocessing
#-----------------------------------------------------------------------------

#1. Check for duplicates

print(sum(duplicated(dataset)))
print(sum(duplicated(dataset$ID)))

#2. Check for missing values

for(c in colnames(dataset)){
  cat("Number of missing values in column ", c, " is: ", sum(is.na(dataset[c])), "\n")
}

#3. Convert Has_Garden and Has_Pool columns to factors

dataset$Has_Garden_Factor <- factor(dataset$Has_Garden, levels=c(0,1), labels=c("No", "Yes"))
dataset$Has_Pool_Factor <- factor(dataset$Has_Pool, levels=c(0,1), labels=c("No", "Yes"))

# Check created columns

summary(dataset[, c("Has_Garden_Factor", "Has_Pool_Factor")])

#4. Visualize frequency distributions

library(ggplot2)

# Visualize floor and room distributions
cols_starting_with_num <- c("Num_Bathrooms", "Num_Bedrooms", "Num_Floors")

for (c in cols_starting_with_num) {
  p <- ggplot(dataset, aes(x = factor(.data[[c]]))) +
    geom_bar(fill = "steelblue", color = "white") +
    labs(title = paste("Distribution of", c), 
         x = paste("Number of", c), 
         y = "Frequency") +
    theme_minimal()
  
  print(p)
}

# Visualize location, price and other numerical distributions
cols_of_nums <- c("Square_Feet", "Garage_Size", "Location_Score", "Distance_to_Center", "Price")

for (c in cols_of_nums){
  p <- ggplot(dataset) + geom_density(aes(x=.data[[c]]))
  print(p)
}

#-----------------------------------------------------------------------------
# Feature selection & Model training
#-----------------------------------------------------------------------------

# 1. Subset dataset to create a dataset for unsupervised learning
# Remove ID as it only identifies rows uniquely
# Remove Price to prevent simple seggregation of clusters into "rich" and "poor" clusters
# Remove Has_Garden_Factor and Has_Pool_Factor to use the original numeric columns
cols_to_remove <- c("ID", "Square_Feet", "Location_Score", "Distance_to_Center", "Has_Garden_Factor", "Has_Pool_Factor", "Price")
subset_df <- dataset[, setdiff(colnames(dataset), cols_to_remove)]

print(summary(subset_df))

# Scale the data
subset_df <- scale(subset_df)

# 2. Use Within Sum Squares (WSS) method to determine optimal value of k
max_k <- 20
wss <- numeric(max_k)
for (k in 1:max_k){
  wss[k] <- sum(kmeans(subset_df, centers=k, nstart=25)$withinss)
}

plot(1:max_k, wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares")

# Optional: Use factoextra library to predict best k
library(factoextra)
fviz_nbclust(subset_df, kmeans, method = "silhouette")
