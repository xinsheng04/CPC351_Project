#-----------------------------------------------------------------------------
# This R file tackles the co2 dataset, 
# performing Data Preprocessing, EDA and model experimentation
# in order to answer the problem statement
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Import csv file
#-----------------------------------------------------------------------------
dataset <- read.csv("co2.csv", header= TRUE)

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

#1. Check for duplication and remove it (if presence)
#before cleaning

#show total duplicated rows
sum(duplicated(dataset))
#show the entry duplicated
dataset[duplicated(dataset),]
dataset_clean <- unique(dataset)

#after cleaning
sum(duplicated(dataset_clean))
dataset_clean[duplicated(dataset_clean), ]

#2. Check for missing values
# paste[0] to beautify the output (concatenates without extra spaces) 
for(c in colnames(dataset_clean)){
  cat(paste0('Number of missing values in column "',c, '" is: ', sum(is.na(dataset_clean[c]))), "\n")
}

#------------------------------------------------------
# Feature selection & Multiple Linear Regression
#------------------------------------------------------
# Select relevant variables for regression
regression_df <- dataset_clean[, c("CO2.Emissions.g.km.","Fuel.Consumption.Comb..L.100.km.",
  "Engine.Size.L.","Cylinders")]

#check for the df structure
str(regression_df)

library(ggplot2)
# Visualize the relationship between Fuel Consumption and CO2
a <- ggplot(regression_df, aes(
  x = Fuel.Consumption.Comb..L.100.km.,
  y = CO2.Emissions.g.km.
)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Fuel Consumption vs CO2 Emissions")

#print the graph
a

#save the graph
ggsave(
  filename = "Fuel Consumption VS CO2.png",
  plot = a,
  width = 16,
  height = 10,
  dpi = 400
)

linear_a <- lm( CO2.Emissions.g.km. ~ Fuel.Consumption.Comb..L.100.km., data = regression_df)
summary(linear_a)

#-------------------------------------------------------------
# Visualize the relationship between Engine Size and CO2
#-------------------------------------------------------------
b <- ggplot(regression_df, aes(
  x = Engine.Size.L.,
  y = CO2.Emissions.g.km.
)) + geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Engine Size vs CO2 Emissions")

#print the graoh
b

#save the graph
ggsave(
  filename = "Engine Size VS CO2.png",
  plot = b,
  width = 16,
  height = 10,
  dpi = 400
)

linear_b <- lm( CO2.Emissions.g.km. ~ Engine.Size.L., data = regression_df)
summary(linear_b)

#-----------------------------------------------------------------
# Visualize the relationship between Cylinders and CO2
#-----------------------------------------------------------------
c <- ggplot(regression_df, aes(
  x = Cylinders,
  y = CO2.Emissions.g.km.
)) + geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Cylinders vs CO2 Emissions")

#print the graph
c

#save the graph
ggsave(
  filename = "Cylinders VS CO2.png",
  plot = c,
  width = 16,
  height = 10,
  dpi = 400
)

linear_c <- lm( CO2.Emissions.g.km. ~ Cylinders, data = regression_df)
summary(linear_c)
#-------------------------------------------------------------------------------
# Perform the multi linear regression 
#calculate the relationship between CO2 and others facors
#-------------------------------------------------------------------------------
# Plot all relationship 
plot(regression_df)

multiplelr_model <- lm(
  CO2.Emissions.g.km. ~
    Fuel.Consumption.Comb..L.100.km. +
    Engine.Size.L. +
    Cylinders,
  data = regression_df
)

summary(multiplelr_model)

#-------------------------------------------------------------------------------
# Predict the CO2
#-------------------------------------------------------------------------------
Fuel <- as.numeric(readline(prompt = "Enter the Fuel Consumption in L/100km:"))
Engine <- as.numeric(readline(prompt = "Enter the Engine Size in L:"))
Cylinders <- as.numeric(readline(prompt = "Enter the Cylinders:"))

prediction_CO2_df <- data.frame(
  Fuel.Consumption.Comb..L.100.km. = Fuel,
  Engine.Size.L. = Engine,
  Cylinders = Cylinders
)

predicted_CO2 <- predict(multiplelr_model, prediction_CO2_df)

cat("The predicted CO2 Emission is: ", round(predicted_CO2,3), "g/km \n")