# Importing the dataset 
#library(readxl)
#Apartments_Data_Initial <- read_excel("Data Files/BUAN 6356/Project/Apartments Data.xlsx", sheet = "Apartments Data (10K)") 

library(readxl)
Apartments_Data_Initial <- read_excel("~/Downloads/Apartments Data.xlsx")
View(Apartments_Data_Initial)

### 
# Data cleaning and Pre-processing 
### 

# Making a copy of the Dataset 
Apartments_Data <- Apartments_Data_Initial 

# Getting the names of columns with missing values'
print(names(Apartments_Data)[colSums(is.na(Apartments_Data)) > 0]) 

# The above function returned: "bedrooms"  "latitude"  "longitude" 
# Replacing the N.A. values in the 'bedrooms' column with the average value of the column 
avg_bedrooms <- mean(Apartments_Data$bedrooms, na.rm = TRUE) 
Apartments_Data$bedrooms <- ifelse(is.na(Apartments_Data$bedrooms), avg_bedrooms, Apartments_Data$bedrooms) 

# Removing the N.A. values in the dataset but only 'latitude' and 'longitude' columns contain N.A. values 
Apartments_Data <- na.omit(Apartments_Data) 

# Verifying that no columns have missing values anymore 
print(names(Apartments_Data)[colSums(is.na(Apartments_Data)) > 0]) 

# The above function returned: 0 

# Removing columns with "null" values 
# Replacing amenities = "null" with amenities = "None" 
Apartments_Data$amenities <- ifelse(Apartments_Data$amenities == "null", "None", Apartments_Data$amenities) 

# Replacing bathrooms = "null" with bathrooms = 0 
Apartments_Data$bathrooms <- ifelse(Apartments_Data$bathrooms == "null", 0, Apartments_Data$bathrooms) 

# Replacing pets_allowed = "null" with pets_allowed = "None" 
Apartments_Data$pets_allowed <- ifelse(Apartments_Data$pets_allowed == "null", "None", Apartments_Data$pets_allowed) 

# Replacing address = "null" with address = "Unavailable" 
Apartments_Data$address <- ifelse(Apartments_Data$address == "null", "Unavailable", Apartments_Data$address) 

# Replacing cityname = "null" with cityname = "Unavailable" 
Apartments_Data$cityname <- ifelse(Apartments_Data$cityname == "null", "Unavailable", Apartments_Data$cityname) 

# Replacing state = "null" with state = "Unavailable" 
Apartments_Data$state <- ifelse(Apartments_Data$state == "null", "Unavailable", Apartments_Data$state) 

# Formatting the 'bathrooms' column as numeric 
Apartments_Data$bathrooms <- as.numeric(Apartments_Data$bathrooms) 

# Checking if all the prices are for the same amount of time 
print(unique(Apartments_Data$price_type)) 

# The above function returned: "Monthly" "Weekly" "Monthly|Weekly" 

# Updating 'Weekly' and 'Monthly|Weekly' prices to 'Monthly' prices to maintain consistency 
Apartments_Data$price[Apartments_Data$price_type == "Weekly" | Apartments_Data$price_type == "Monthly|Weekly"] <- Apartments_Data$price[Apartments_Data$price_type == "Weekly" | Apartments_Data$price_type == "Monthly|Weekly"] * 4 
Apartments_Data$price_type[Apartments_Data$price_type == "Weekly" | Apartments_Data$price_type == "Monthly|Weekly"] <- "Monthly" 

#Clustering

library(cluster)
library(ggplot2)
library(factoextra)

Apartments_Data$source <- as.numeric(Apartments_Data$source)
Apartments_Data$state <- as.numeric(Apartments_Data$state)

# Select relevant variables for clustering
selected_vars <- c( "price", "square_feet")
cluster_data <- Apartments_Data[selected_vars]

#run pam algorithm, metric =“euclidean”
set.seed(2)
km <- kmeans(cluster_data, 4)

km$cluster

#### Table 15.10
# centroids
km$centers
km$withinss
km$size

min(km$centers)
max(km$centers)

fviz_cluster(km, cluster_data, ellipse.type ="euclid",  ggtheme = theme_minimal())


# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l", 
     ylim = c(min(km$centers), max(km$centers)), xlim = c(1, 2))

# label x-axes
axis(1, at = c(1:2), labels = names(cluster_data))

# plot centroids
for (i in c(1:4))
  lines(km$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 2),
                                                       "black", "purple"))

# name clusters
text(x =0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:2)))


Apartments_Data$source <- as.numeric(Apartments_Data$source)
Apartments_Data$state <- as.numeric(Apartments_Data$state)

# Select relevant variables for clustering
selected_vars <- c( "source", "state")
cluster_data1 <- Apartments_Data[selected_vars]

#run pam algorithm, metric =“euclidean”
set.seed(2)
km <- kmeans(cluster_data1, 4)

km$cluster

# centroids
km$centers
km$withinss
km$size

fviz_cluster(km, cluster_data1, ellipse.type ="euclid",  ggtheme = theme_minimal())

# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l", 
     ylim = c(min(km$centers), max(km$centers)), xlim = c(1, 2))

# label x-axes
axis(1, at = c(1:2), labels = names(cluster_data1))

axis(2, at = axTicks(2, axp = c(0, 55, 55))) 

# plot centroids
for (i in c(1:4))
  lines(km$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 2),
                                                       "blue", "orange"))

# name clusters
text(x =0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:2)))

# Adding a new region column 
region <- "" 
Apartments_Data <- cbind(Apartments_Data, region) 

Apartments_Data$region[Apartments_Data$state == "NY" | Apartments_Data$state == "MA" | Apartments_Data$state == "NJ" | Apartments_Data$state == "PA" | Apartments_Data$state == "CT" | Apartments_Data$state == "RI" | Apartments_Data$state == "NH" | Apartments_Data$state == "VT" | Apartments_Data$state == "ME"] <- "Northeast" 
Apartments_Data$region[Apartments_Data$state == "VA" | Apartments_Data$state == "NC" | Apartments_Data$state == "GA" | Apartments_Data$state == "FL" | Apartments_Data$state == "AL" | Apartments_Data$state == "MD" | Apartments_Data$state == "TN" | Apartments_Data$state == "DE" | Apartments_Data$state == "SC" | Apartments_Data$state == "KY" | Apartments_Data$state == "LA" | Apartments_Data$state == "AR" | Apartments_Data$state == "WV" | Apartments_Data$state == "MS"] <- "South" 
Apartments_Data$region[Apartments_Data$state == "IN" | Apartments_Data$state == "IL" | Apartments_Data$state == "IA" | Apartments_Data$state == "MN" | Apartments_Data$state == "MI" | Apartments_Data$state == "WI" | Apartments_Data$state == "OH" | Apartments_Data$state == "MO"] <- "Midwest" 
Apartments_Data$region[Apartments_Data$state == "DC" | Apartments_Data$state == "WA" | Apartments_Data$state == "CA" | Apartments_Data$state == "AZ" | Apartments_Data$state == "TX" | Apartments_Data$state == "CO" | Apartments_Data$state == "NM" | Apartments_Data$state == "AK" | Apartments_Data$state == "OR" | Apartments_Data$state == "NV" | Apartments_Data$state == "UT" | Apartments_Data$state == "OK" | Apartments_Data$state == "NE" | Apartments_Data$state == "ND" | Apartments_Data$state == "KS" | Apartments_Data$state == "ID" | Apartments_Data$state == "HI" | Apartments_Data$state == "MT" | Apartments_Data$state == "SD" | Apartments_Data$state == "WY"] <- "West" 
Apartments_Data$region[Apartments_Data$state == "Unavailable"] <- "Unavailable" 



### 
# Forming the Regression 
### 


# Running the regression 
square_feet3 <- Apartments_Data$square_feet * Apartments_Data$square_feet  * Apartments_Data$square_feet 
reg_model <- lm(price ~ square_feet + square_feet3 + bathrooms + bedrooms + square_feet*bathrooms + square_feet3*bedrooms + state + cityname, data = Apartments_Data)

# Regression Summary 
summary(reg_model) 

###
# Removing outliers 
### 

# Calculating Cook's distance 
cooksd <- cooks.distance(reg_model) 

# Identifying influential observations (outliers) 
influential_obs <- which(cooksd > 4 / length(cooksd)) 

# Printing influential observations 
cat("Influential Observations (Outliers):", influential_obs, "\n")

# Removing influential observations from the dataset 
Apartments_Data_no_outliers <- Apartments_Data[-influential_obs, ] 

# Fitting a new model without outliers 
square_feet3 <- Apartments_Data_no_outliers$square_feet * Apartments_Data_no_outliers$square_feet * Apartments_Data_no_outliers$square_feet 
reg_model_no_outliers <- lm(price ~ square_feet3 + bathrooms + bedrooms + square_feet3*bathrooms + square_feet*bedrooms + region, data = Apartments_Data_no_outliers) 

summary(reg_model_no_outliers) 

# Calculating errors for the regression 
predicted_values <- predict(reg_model_no_outliers, Apartments_Data_no_outliers, type = "response") 
actual_values <- Apartments_Data_no_outliers$price 

mse <- mean((actual_values - predicted_values)^2, na.rm = TRUE) 
rmse <- sqrt(mse) 

# Calculating relative RMSE 
cv_rmse <- (rmse / (max(predicted_values) - min(predicted_values))) * 100 
print(cv_rmse) 



### 
# Apply Neural Network 
### 

library(neuralnet) 

# Function to standardize values in terms of z-scores 
z_score_standardize <- function(x) { 
  if (is.numeric(x)) { 
    return((x - mean(x)) / sd(x)) 
  } else { 
    return(x) 
  } 
} 

Apartments_Data_standardized_initial <- as.data.frame(lapply(Apartments_Data, z_score_standardize)) 

# Encoding region into numeric variables 
encoded_data <- model.matrix(~ region - 1, data = Apartments_Data_standardized_initial) 
Apartments_Data_standardized <- cbind(Apartments_Data_standardized_initial[, c("price", "square_feet", "bathrooms", "bedrooms")], encoded_data) 

# Identifying the odd and even rows 
odd_rows <- seq(1, nrow(Apartments_Data_standardized), by = 2) 
even_rows <- seq(2, nrow(Apartments_Data_standardized), by = 2) 

# Creating the training and validation datasets 
training_data <- Apartments_Data_standardized[odd_rows, ] 
validation_data <- Apartments_Data_standardized[even_rows, ] 

# Defining the neural network model 
nn <- neuralnet(price ~ square_feet + bathrooms + bedrooms + regionMidwest + regionNortheast + regionSouth + regionUnavailable + regionWest, 
                data = training_data, 
                linear.output = F, 
                hidden = 5,            # Number of hidden layers and neurons 
                learningrate = 1.5)    # Adjust the learning rate as needed 

plot(nn, rep = "best") 

predictions <- predict(nn, validation_data, type = "response") 
actual_values <- validation_data$price 

# Calculate Mean Absolute Error (MAE) 
mae <- mean(abs(predictions - actual_values)) 

# Calculate Mean Squared Error (MSE) 
mse <- mean((predictions - actual_values)^2) 

# Calculate Root Mean Squared Error (RMSE) 
rmse <- sqrt(mse) 

# Calculating relative RMSE 
cv_rmse <- (rmse / (max(predictions) - min(predictions))) * 100 

# Calculating R-squared 
rsquared <- 1 - sum((actual_values - predictions)^2) / sum((actual_values - mean(actual_values))^2) 

cat("Mean Absolute Error (MAE):", mae, "\n") 
cat("Mean Squared Error (MSE):", mse, "\n") 
cat("Root Mean Squared Error (RMSE):", rmse, "\n") 
cat("Relative Root Mean Squared Error (RMSE):", cv_rmse, "\n") 
cat("R-Squared:", rsquared, "\n") 
