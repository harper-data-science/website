
# Load necessary libraries
library(ggplot2)

# set your working dir

# Read the dataset
data <- read.csv('winter_holiday_agriculture_data.csv')

# View the first few rows of the dataset
head(data)

# Simple Linear Regression: Crop Yield vs. Fertilizer Amount
model <- lm(Crop_Yield ~ Fertilizer_Amount, data = data)

# Summary of the model to see the effect
summary(model)

# Plotting
# Scatter plot with regression line
ggplot(data, aes(x = Fertilizer_Amount, y = Crop_Yield)) +
  geom_point(aes(color = Fertilizer_Amount)) +  # Colored points by fertilizer amount
  geom_smooth(method = "lm", se = FALSE) +      # Linear model line without standard error
  scale_color_gradient(low = "red", high = "darkgreen") + # Red to green color ramp
    labs(title = "Effect of Fertilizer on Crop Yield",
       x = "Fertilizer Amount (kg)",
       y = "Crop Yield (quintals)") +
  theme_minimal()
