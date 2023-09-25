library(ggplot2)
library(h2o)

# Step 1: Add ggplot2::mpg dataset
data("mpg")

# Step 2: Preprocessing
mpg <- mpg[, c("cty", "year", "cyl", "displ")]

# Check for missing values
missing_values <- sum(is.na(mpg))
if (missing_values > 0) {
  cat("Warning: There are missing values in the dataset. Imputing...\n")
  mpg[is.na(mpg)] <- median(mpg, na.rm = TRUE)  # Impute with median
}

# Log-transform 'displ' variable
mpg$displ <- log(mpg$displ)

# Step 3: Initialize and Start H2O Cluster
h2o.init()

# Step 4: Fit Generalized Linear Model (GLM)
# Assuming you want to predict "cty" based on "year", "cyl", and "displ".
mpg_h2o <- as.h2o(mpg)  # Convert the data to H2O frame
model <- h2o.glm(y = "cty", x = c("year", "cyl", "displ"), training_frame = mpg_h2o)

# Step 5: Print coefficients table and give interpretation
summary(model)
