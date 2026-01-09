# Put your working directories over here
setwd("C:/Users/Jun & Heng/Desktop/CPC351 R Prgramming/CPC351_Project")
setwd("C:/Users/Asus/OneDrive/Desktop/myProjects/Y3S1/CPC 351/CPC351_Project")

#-----------------------------------------------------------------------------
# This R file tackles the uber df,
# performing Data Preprocessing, EDA and model experimentation
# in order to answer the problem statement
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Import csv file
#-----------------------------------------------------------------------------
df <- read.csv("Project_1_Datasets/Course_Completion_Dataset.csv", header = TRUE)

# Inspect df features
cat("Dimensions of the df: ", dim(df))

# Structure of the df
str(df)

# Summary of df
summary(df)

#-----------------------------------------------------------------------------
# Data preprocessing
#-----------------------------------------------------------------------------
# 1. Check for duplicates
print(sum(duplicated(df)))

# 2. Check for missing values
print("Missing values per column")
for (c in colnames(df)) {
  cat(c, " : ", sum(is.na(df[c])), "\n")
}

# 3. Check for invalid values
print(unique(df$Education_Level))
print(unique(df$Employment_Status))
print(unique(df$City))
print(unique(df$Device_Type))
print(unique(df$Internet_Connection_Quality))
print(unique(df$Course_Name))
print(unique(df$Category))
print(unique(df$Course_Level))
print(unique(df$Completed))

# 4. Convert Completed column to factors
df$Completed <- as.factor(df$Completed)
#-----------------------------------------------------------------------------
# Decision Tree Construction and Hypothesis Testing
#-----------------------------------------------------------------------------
library(rpart)
library(rattle)
library(vip)


# Hypotheses:
# 1. Students who spend more time on the course are more likely to complete it
# IF Login_Frequency is HIGH and Average_Session_Duration_Min is HIGH and
# Video_Completion_Rate is HIGH and Days_Since_Last_Login is LOW then
# Completed is more likely to be true

vars_h1 <- c("Login_Frequency", "Average_Session_Duration_Min",
  "Video_Completion_Rate", "Days_Since_Last_Login", "Completed"
)

df_h1 <- df[, vars_h1]
tree_h1 <- rpart(
  Completed ~ ., 
  data=df_h1, 
  method="class",
  control = rpart.control(
    maxdepth = 4,
    minsplit = 30,
    cp = 0.01
  )
)
fancyRpartPlot(tree_h1)
vip(tree_h1)

vars_h1b <- c("Login_Frequency", "Average_Session_Duration_Min",
 "Days_Since_Last_Login", "Completed"
)

# Remove Video_Completion_Rate and see how other variables affect course completion

df_h1b <- df[, vars_h1b]

tree_h1b <- rpart(
  Completed ~ ., 
  data=df_h1b, 
  method="class",
  control = rpart.control(
    maxdepth = 4,
    minsplit = 30,
    cp = 0.01
  )
)
fancyRpartPlot(tree_h1b)
vip(tree_h1b)

# 2. Students who regularly submit assignments, have good quiz scores and good 
# project scores are more likely to complete the course
# IF Assignments_Submitted is HIGH and Quiz_Score_Avg is HIGH and Project_Grade
# is HIGH then Completion is HIGH

vars_h2 <- c("Quiz_Score_Avg",
  "Project_Grade", "Completed"
)

df_h2 <- df[, vars_h2]

tree_h2 <- rpart(
  Completed ~ ., 
  data = df_h2, 
  method = "class",
  control = rpart.control(
    maxdepth = 4,
    minsplit = 25,
    cp = 0.01
  )
)

fancyRpartPlot(tree_h2)
vip(tree_h2)

# Do not use

# 3. Students who proactively engage with peers and look forward to notifications
# are more likely to be satisfied with the course
# IF Discussion_Participation is HIGH and Notifications_Checked is HIGH and 
# Peer_Interaction_Score is HIGH then Satisfaction_Rating is HIGH
vars_h3 <- c("Discussion_Participation", "Notifications_Checked", 
  "Peer_Interaction_Score", "Satisfaction_Rating"
)

df_h3 <- df[, vars_h3]
tree_h3 <- rpart(
  Satisfaction_Rating ~ .,
  data = df_h3,
  method = "anova",
  control = rpart.control(
    maxdepth = 4,
    minsplit = 30,
    cp = 0.01
  )
)

fancyRpartPlot(tree_h3)
vip(tree_h3)

# 4. Good instructors lead to high student engagement, good scores and favorable 
# satisfaction and completion rates
# IF Instructor_Rating is HIGH then Login_Frequency is HIGH and Quiz_Score_Avg 
# is HIGH and Satisfaction_Rating is HIGH and Completed is more likely to be true

tree_h4a <- rpart(
  Login_Frequency ~ Instructor_Rating,
  data = df[, c("Login_Frequency", "Instructor_Rating")],
  method = "anova",
)
fancyRpartPlot(tree_h4a)
vip(tree_h4a)


tree_h4b <- rpart(
  Quiz_Score_Avg ~ Instructor_Rating,
  data = df[, c("Quiz_Score_Avg", "Instructor_Rating")],
  method = "anova"
)
fancyRpartPlot(tree_h4b)
vip(tree_h4b)


tree_h4c <- rpart(
  Satisfaction_Rating ~ Instructor_Rating,
  data = df[, c("Satisfaction_Rating", "Instructor_Rating")],
  method = "class"
)
fancyRpartPlot(tree_h4c)
vip(tree_h4c)

