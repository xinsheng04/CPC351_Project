# Put your working directories over here
setwd("C:/Users/Jun & Heng/Desktop/CPC351 R Prgramming/CPC351_Project")
setwd("C:/Users/Asus/OneDrive/Desktop/myProjects/Y3S1/CPC 351/CPC351_Project")

#-----------------------------------------------------------------------------
# This R file tackles the uber dataset,
# performing Data Preprocessing, EDA and model experimentation
# in order to answer the problem statement
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Import csv file
#-----------------------------------------------------------------------------
dataset <- read.csv("Project_1_Datasets/uber.csv", header = TRUE)

# Inspect dataset features
cat("Dimensions of the dataset: ", dim(dataset))

# Structure of the dataset
str(dataset)

# Summary of dataset
summary(dataset)

#-----------------------------------------------------------------------------
# Data preprocessing
#-----------------------------------------------------------------------------
# 1. Check for duplicates
print(sum(duplicated(dataset)))

# 2. Check for missing values
for (c in colnames(dataset)) {
  cat("Number of missing values in column ", c, " is: ", sum(is.na(dataset[c])), "\n")
}

# 3. Deal with missing and invalid values
narows <- dataset[is.na(dataset$dropoff_longitude) | is.na(dataset$dropoff_latitude), ]

same_location <- dataset[dataset$pickup_longitude == dataset$dropoff_longitude &
  dataset$pickup_latitude == dataset$dropoff_latitude, ]

negative_fare <- dataset[dataset$fare_amount < 0, ]

# Trips out of NYC: not part of problem statement
# NYC 
area_outboundaries <- dataset[dataset$dropoff_longitude < -80 | dataset$dropoff_longitude > -70 |
  dataset$pickup_longitude < -80 | dataset$pickup_longitude > -70 |
  dataset$dropoff_latitude < 38 | dataset$dropoff_latitude > 43 |
  dataset$pickup_latitude < 38 | dataset$pickup_latitude > 43, ]

invalid_rows <- unique(rbind(narows, same_location, negative_fare, area_outboundaries))
invalidProbability <- nrow(invalid_rows) / nrow(dataset)
print(paste("Invalid probability:", invalidProbability))


# 4. Filter out rows that appear in the invalid_rows table
dataset <- dataset[!(row.names(dataset) %in% row.names(invalid_rows)), ]

# Recheck dataset
print(summary(dataset))

#-----------------------------------------------------------------------------
# K-means model training
#-----------------------------------------------------------------------------
# 1. Extract and derive the relevant features for model classification
# Remove the X and key columns
filtered_dataset <- dataset[, -c(1, 2)]

# Calculate Euclidean Distance
filtered_dataset$dist_euclidean <- sqrt(
  (filtered_dataset$dropoff_longitude - filtered_dataset$pickup_longitude)^2 +
    (filtered_dataset$dropoff_latitude - filtered_dataset$pickup_latitude)^2
)

# Convert to Datetime and extract the Hour
filtered_dataset$pickup_datetime <- as.POSIXct(filtered_dataset$pickup_datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Create a new column for the hour (0-23)
filtered_dataset$pickup_hour <- as.numeric(format(filtered_dataset$pickup_datetime, "%H"))

# Remove the pickup_datetime and passenger_count column by index
filtered_dataset <- filtered_dataset[, -c(2, 7)]

print(summary(filtered_dataset))

# Get the pickup and dropoff location information
pickup_location <- filtered_dataset[, c("pickup_latitude", "pickup_longitude")]
dropoff_location <- filtered_dataset[, c("dropoff_latitude", "dropoff_longitude")]

# 2. Use Within Sum Squares (WSS) method to determine optimal value of k
max_k <- 20
wss_1 <- numeric(max_k)
for (k in 1:max_k) {
  model <- kmeans(pickup_location,
    centers = k, nstart = 25,
    iter.max = 100, algorithm = "MacQueen"
  )
  wss_1[k] <- sum(model$withinss)
}
plot(1:max_k, wss_1, main = "WSS over pickup location", type = "b", xlab = "Number of Clusters", ylab = "Within Sum of Squares")


wss_2 <- numeric(max_k)
for (k in 1:max_k) {
  model <- kmeans(dropoff_location,
    centers = k, nstart = 25,
    iter.max = 100, algorithm = "MacQueen"
  )
  wss_2[k] <- sum(model$withinss)
}
plot(1:max_k, wss_2, main = "WSS over dropoff location", type = "b", xlab = "Number of Clusters", ylab = "Within Sum of Squares")

# 3. Create the kmeans model using the best k values obtained from plotting
k_best_1 <- 5
k_best_2 <- 3

# 3. Create the kmeans model using the best k values obtained from plotting
fit1 <- kmeans(pickup_location,
  centers = k_best_1, nstart = 25,
  iter.max = 100, algorithm = "MacQueen"
)
fit2 <- kmeans(dropoff_location,
  centers = k_best_2, nstart = 25,
  iter.max = 100, algorithm = "MacQueen"
)

# View the model
table(fit1$cluster)
table(fit2$cluster)

# Add the results back to filtered_dataset for reporting
filtered_dataset$pickup_cluster <- as.factor(fit1$cluster)
filtered_dataset$dropoff_cluster <- as.factor(fit2$cluster)

#-----------------------------------------------------------------------------
# Results Diagnosis and Evaluation
#-----------------------------------------------------------------------------
# Plot to see the distribution of data across different clustering models

# Plot the clusters
library("ggplot2")
library("grDevices")

## -------------------- Pick Up Location -----------------------------------##
# This finds the 'outer' points for each cluster to draw the polygons
hulls <- do.call(rbind, lapply(unique(filtered_dataset$pickup_cluster), function(c) {
  # Filter for each specific cluster
  df_sub <- filtered_dataset[filtered_dataset$pickup_cluster == c, ]
  # Calculate the indices of the convex hull points
  hull_indices <- chull(df_sub$pickup_longitude, df_sub$pickup_latitude)
  # Return those specific rows
  return(df_sub[hull_indices, ])
}))

ggplot() +
  # Plot all points as small dots (better than text for large filtered_datasets)
  geom_point(
    data = filtered_dataset,
    aes(x = pickup_latitude, y = pickup_longitude, color = pickup_cluster),
    alpha = 0.3, size = 1
  ) +
  # Draw the polygons around the clusters
  geom_polygon(
    data = hulls,
    aes(
      x = pickup_latitude, y = pickup_longitude,
      fill = pickup_cluster, group = pickup_cluster
    ),
    alpha = 0.4
  ) +
  # Make it look like a map
  coord_fixed() +
  theme_minimal() +
  labs(
    title = "New York City Uber Pickup Clusters",
    x = "Latitude", y = "Longitude"
  ) +
  theme(legend.position = "right")

## -------------------- Drop off Location -----------------------------------##
# This finds the 'outer' points for each cluster to draw the polygons
hulls <- do.call(rbind, lapply(unique(filtered_dataset$dropoff_cluster), function(c) {
  # Filter for each specific cluster
  df_sub <- filtered_dataset[filtered_dataset$dropoff_cluster == c, ]
  # Calculate the indices of the convex hull points
  hull_indices <- chull(df_sub$dropoff_longitude, df_sub$dropoff_latitude)
  # Return those specific rows
  return(df_sub[hull_indices, ])
}))

ggplot() +
  # Plot all points as small dots (better than text for large filtered_datasets)
  geom_point(
    data = filtered_dataset,
    aes(x = dropoff_latitude, y = dropoff_longitude, color = dropoff_cluster),
    alpha = 0.3, size = 1
  ) +
  # Draw the polygons around the clusters
  geom_polygon(
    data = hulls,
    aes(
      x = dropoff_latitude, y = dropoff_longitude,
      fill = dropoff_cluster, group = dropoff_cluster
    ),
    alpha = 0.4
  ) +
  # Make it look like a map
  coord_fixed() +
  theme_minimal() +
  labs(
    title = "New York City Uber Drop Off Clusters",
    x = "Latitude", y = "Longitude"
  ) +
  theme(legend.position = "right")

# Group hours based on cluster
hourly_pickup_distribution <- as.data.frame(
  table(
    filtered_dataset$pickup_cluster,
    filtered_dataset$pickup_hour
  )
)
colnames(hourly_pickup_distribution) <- c(
  "pickup_cluster", "pickup_hour", "trip_count"
)


# Plotting Bar Charts to show peak hour for each cluster
ggplot(hourly_pickup_distribution, aes(
  x = as.numeric(as.character(pickup_hour)),
  y = trip_count, fill = pickup_cluster
)) +
  geom_bar(stat = "identity") +
  facet_wrap(~pickup_cluster, nrow = 2, ncol = 4, scales = "free_y") +
  scale_x_continuous(breaks = seq(0, 23, by = 4)) +
  labs(
    title = "Peak Hours by Pickup Cluster",
    x = "Hour of Day (0–23)",
    y = "Number of Pickups"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
