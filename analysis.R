#data source: https://www.fueleconomy.gov/feg/download.shtml

# Print the cars2018 object
cars2018

# Plot the histogram
ggplot(cars2018, aes(x = MPG)) +
  geom_histogram(bins = 25) +
  labs(y = "Number of cars",
       x = "Fuel efficiency (mpg)")

# Deselect the 2 columns to create cars_vars
cars_vars <- cars2018 %>%
  select(-Model, -`Model Index`)

# Fit a linear model
fit_all <- lm(MPG ~ ., data = cars_vars)

# Print the summary of the model
summary(fit_all)