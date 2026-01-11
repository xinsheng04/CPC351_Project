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

# Remove trips out of NYC 
long_limits = c( -74.5, -73.7)
lat_limits = c(40.4, 41)
area_outboundaries <- dataset[dataset$dropoff_longitude < long_limits[1] | dataset$dropoff_longitude > long_limits[2] |
  dataset$pickup_longitude < long_limits[1] | dataset$pickup_longitude > long_limits[2] |
  dataset$dropoff_latitude < lat_limits[1] | dataset$dropoff_latitude > lat_limits[2] |
  dataset$pickup_latitude < lat_limits[1] | dataset$pickup_latitude > lat_limits[2], ]

invalid_rows <- unique(rbind(narows, same_location, negative_fare, area_outboundaries))
invalidProbability <- nrow(invalid_rows) / nrow(dataset)
print(paste("Invalid probability:", invalidProbability))


# 4. Filter out rows that appear in the invalid_rows table
filtered_dataset <- dataset[!(row.names(dataset) %in% row.names(invalid_rows)), ]

# Recheck dataset
print(summary(filtered_dataset))

# Visualize pickup locations after out of NYC location removal
library(leaflet)
initial_map <- leaflet(filtered_dataset[, c("pickup_longitude", "pickup_latitude")]) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    lng = ~pickup_longitude, 
    lat = ~pickup_latitude,
    radius = 1,
    color = "blue",
    stroke = FALSE,
    fillOpacity = 0.1
  )

print(initial_map)

#-----------------------------------------------------------------------------
# K-means model training
#-----------------------------------------------------------------------------
set.seed(500)
# 1. Extract and derive the relevant features for model classification

# Convert to Datetime and extract the Hour
filtered_dataset$pickup_datetime <- as.POSIXct(filtered_dataset$pickup_datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Create a new column for the hour (0-23)
filtered_dataset$pickup_hour <- as.numeric(format(filtered_dataset$pickup_datetime, "%H"))

# Only retain the pickup location and pickup hour columns
filtered_dataset <- filtered_dataset[, c("pickup_longitude", "pickup_latitude", "pickup_hour")]

print(summary(filtered_dataset))

# Get the pickup location information
pickup_location <- filtered_dataset[, c("pickup_latitude", "pickup_longitude")]

# 2. Use Within Sum Squares (WSS) method to determine optimal value of k
max_k <- 20
wss <- numeric(max_k)
for (k in 1:max_k) {
  model <- kmeans(pickup_location,
    centers = k, nstart = 25,
    iter.max = 100, algorithm = "Hartigan-Wong"
  )
  wss[k] <- sum(model$withinss)
}
plot(1:max_k, wss, main = "WSS over pickup location", type = "b", xlab = "Number of Clusters", ylab = "Within Sum of Squares")

# 3. Create the kmeans model using the best k values obtained from plotting
k_best <- 5

fit <- kmeans(pickup_location,
  centers = k_best, nstart = 25,
  iter.max = 100, algorithm = "Hartigan-Wong"
)

# View the model
table(fit$cluster)

# Add the results back to filtered_dataset for reporting
filtered_dataset$pickup_cluster <- as.factor(fit$cluster)

#-----------------------------------------------------------------------------
# Results Diagnosis and Evaluation
#-----------------------------------------------------------------------------
# Plot to see the distribution of data across different clustering models
library("ggplot2")
library("grDevices")
library(leaflet)
library(dplyr)

hulls <- do.call(rbind, lapply(unique(filtered_dataset$pickup_cluster), function(c) {
  
  # Filter for each specific cluster
  
  df_sub <- filtered_dataset[filtered_dataset$pickup_cluster == c, ]
  
  # Calculate the indices of the convex hull points and return those rows
  
  hull_indices <- chull(df_sub$pickup_longitude, df_sub$pickup_latitude)
  
  return(df_sub[hull_indices, ])
  
}))

# 1. Create a color palette based on the clusters
pal <- colorFactor(palette = "Set1", domain = filtered_dataset$pickup_cluster)

map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) # A clean, grey map perfect for data

# 2. Add the Hulls (Polygons)
for (c in unique(filtered_dataset$pickup_cluster)) {
  hull_data <- hulls %>% filter(pickup_cluster == c)
  
  map <- map %>%
    addPolygons(
      data = hull_data,
      lng = ~pickup_longitude, 
      lat = ~pickup_latitude,
      fillColor = ~pal(c),
      weight = 2,
      opacity = 1,
      fillOpacity = 0.4,
      group = paste("Cluster", c),
      label = paste("Cluster", c)
    )
}

# 3. Add the individual points for detail
map <- map %>%
  addCircleMarkers(
    data = filtered_dataset,
    lng = ~pickup_longitude, 
    lat = ~pickup_latitude,
    radius = 1,
    color = ~pal(pickup_cluster),
    stroke = FALSE,
    fillOpacity = 0.2,
    group = "Points"
  ) %>%
  addLayersControl(
    overlayGroups = c("Points", paste("Cluster", unique(filtered_dataset$pickup_cluster))),
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  addLegend(
    data = filtered_dataset,
    position = "topright",
    pal = pal,
    values = ~pickup_cluster,
    title = "Pickup Clusters",
    opacity = 1
  )


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
  theme(legend.position = "none", panel.spacing.y = unit(2, "lines"))

print(map)