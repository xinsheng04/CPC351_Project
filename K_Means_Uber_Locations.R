# Put your working directories over here
# setwd("C:/Users/Jun & Heng/Desktop/CPC351 R Prgramming/CPC351_Project")
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
print(paste("Percentage of unused rows: ", invalidProbability * 100, "%"))

# 4. Filter out rows that appear in the invalid_rows table
filtered_dataset <- dataset[!(row.names(dataset) %in% row.names(invalid_rows)), ]

# See filtered dataset values
print(summary(filtered_dataset))

# 5. Calculate Euclidean Distance and hour of the day for each trip
filtered_dataset$dist_euclidean <- sqrt(
  (filtered_dataset$dropoff_longitude - filtered_dataset$pickup_longitude)^2 +
    (filtered_dataset$dropoff_latitude - filtered_dataset$pickup_latitude)^2
)

# Convert to Datetime and extract the Hour
filtered_dataset$pickup_datetime <- as.POSIXct(filtered_dataset$pickup_datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Create a new column for the hour (0-23)
filtered_dataset$pickup_hour <- as.numeric(format(filtered_dataset$pickup_datetime, "%H"))

# Remove the pickup_datetime
filtered_dataset <- filtered_dataset[, -2]

print(summary(filtered_dataset))

#-----------------------------------------------------------------------------
# K-means model training - Task 1: Group pickup hours into time periods
#-----------------------------------------------------------------------------
# 1. Extract and derive the relevant features for model classification
# Remove the X and key columns
filtered_dataset <- filtered_dataset[, -c(1, 2)]

# 2. Use Within Sum Squares (WSS) method to determine optimal value of k
# We want to classify based on pickup hour
max_k <- 20
wss_time <- numeric(max_k)
for (k in 1:max_k) {
  model <- kmeans(filtered_dataset[, 8],
    centers = k, nstart = 5,
    iter.max = 20, algorithm = "MacQueen"
  )
  # Note: tot.withinss is the same as sum(withinss)
  wss_time[k] <- model$tot.withinss
}
plot(1:max_k, wss_time, main = "WSS over pickup hour", type = "b", 
     xlab = "Number of Clusters", ylab = "Within Sum of Squares")

# 3. Create the kmeans model using the best k values obtained from plotting
k_best_time <- 5 # Using 5 time periods
fit1 <- kmeans(filtered_dataset[, 8],
  centers = k_best_time, nstart = 25,
  iter.max = 100, algorithm = "MacQueen"
)

# View booking distributions across time periods
table(fit1$cluster)

# Add the results back to filtered_dataset for reporting
filtered_dataset$hour_cluster <- as.factor(fit1$cluster)

#-----------------------------------------------------------------------------
# Time period Evaluation
#-----------------------------------------------------------------------------
library(ggplot2)

# Interpret each time period using box plots
time_periods <- filtered_dataset[, c("pickup_hour", "hour_cluster")]

ggplot(time_periods, aes(x = as.factor(hour_cluster), y = pickup_hour, fill = as.factor(hour_cluster))) +
  geom_boxplot(alpha = 0.7) +
  scale_y_continuous(breaks = seq(0, 23, by = 2)) +
  labs(title = "Distribution of Pickup Hours by Cluster",
       x = "Cluster ID",
       y = "Hour of Day (0-23)",
       fill = "Cluster") +
  theme_minimal() + coord_flip()

#-----------------------------------------------------------------------------
# K-means model training - Task 2: Group bookings by location for each time cluster
#-----------------------------------------------------------------------------
# 1. Separate bookings into their own respective groups
cluster_datasets <- list()
for (x in 1:k_best_time) {
  cluster_datasets[[x]] <- filtered_dataset[filtered_dataset$hour_cluster == x, 
                                            c("pickup_longitude", "pickup_latitude")]
}

# 2. Use Within Sum Squares (WSS) method to determine optimal value of k for each time cluster
for (x in 1:k_best_time) {
  current_data <- cluster_datasets[[x]]
  
  # Skip if the dataset is empty or has fewer rows than max_k
  if(nrow(current_data) <= max_k) next 
  
  wss_location <- numeric(max_k)
  
  for (k in 1:max_k) {
    model <- kmeans(current_data, centers = k, nstart = 5, iter.max = 20, algorithm = "MacQueen")
    
    wss_location[k] <- model$tot.withinss
  }
  
  plot(1:max_k, wss_location, 
       main = paste("WSS for Location Clusters in Time Group", x), 
       type = "b", xlab = "Number of Clusters (k)", ylab = "WSS")
}

#3. Create the kmeans model based on computed k_best for each cluster
k_best_locations = c(5, 4, 3, 6, 5)
filtered_dataset$location_cluster <- NA

for (x in 1:k_best_time){
  current_data <- cluster_datasets[[x]]
  num_locations <- k_best_locations[x]
  fit_name <- paste("fit_period_", x)
  assign(fit_name, kmeans(current_data, centers = num_locations, 
                          nstart = 25, iter.max = 100, algorithm = "MacQueen"))
  
  filtered_dataset$location_cluster[filtered_dataset$hour_cluster == x] <- get(fit_name)$cluster
}

#4. Gather the top locations (pickup destinations) from each time cluster
top_locations_by_cluster = list()
for(x in 1:k_best_time){
  current_fit <- get(paste("fit_period_", x))
  centers <- as.data.frame(current_fit$centers)
  centers$time_period <- x
  top_locations_by_cluster[[x]] <- centers
}

# For convenience
all_locations <- do.call(rbind, top_locations_by_cluster)

# 5. Plot the top locations 
library(ggplot2)

# Define common NYC coordinates to keep the zoom consistent
nyc_long_limit <- c(-74.05, -73.85)
nyc_lat_limit <- c(40.65, 40.85)

for (x in 1:k_best_time) {
  
  # Filter data for the specific time cluster
  period_data <- filtered_dataset[filtered_dataset$hour_cluster == x, ]
  period_centers <- all_locations[all_locations$time_period == x, ]
  
  p <- ggplot(period_data, aes(x = pickup_longitude, y = pickup_latitude)) +
    # Low alpha to see density
    geom_point(aes(color = as.factor(location_cluster)), alpha = 0.2, size = 0.5) +
    # Plotting the centers
    geom_point(data = period_centers, aes(x = pickup_longitude, y = pickup_latitude), 
               color = "black", shape = 8, size = 4, stroke = 1.5) + 
    # Fix the aspect ratio and zoom
    coord_fixed(ratio = 1.3, xlim = nyc_long_limit, ylim = nyc_lat_limit) +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(title = paste("Most popular pickup locations by Time Period", x),
         x = "Longitude", y = "Latitude")
  
  # 4. Print the plot to the screen
  print(p)
}