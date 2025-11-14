library(tidyverse)
library(modelr)
library(performance)
library(report)
library(broom)

mush <- read_csv("mushroom_growth.csv")

glimpse(mush)
summary(mush)

#growthrate vs. light
print(
  ggplot(mush, aes(x = Light, y = GrowthRate)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()
)

# growthrate vs. nitrogen
print(
  ggplot(mush, aes(x = Nitrogen, y = GrowthRate)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()
)

# growthrate vs. temp
print(
  ggplot(mush, aes(x = Temperature, y = GrowthRate)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()
)

# growthrate vs. species
print(
  ggplot(mush, aes(x = Species, y = GrowthRate)) +
  geom_boxplot() +
  theme_minimal()
)

# growthrate vs. humidity
print(
  ggplot(mush, aes(x = Humidity, y = GrowthRate)) +
  geom_boxplot() +
  theme_minimal()
) 

# models
mod1 <- lm(GrowthRate ~ Light, data = mush)
mod2 <- lm(GrowthRate ~ Nitrogen, data = mush)
mod3 <- lm(GrowthRate ~ Light + Nitrogen, data = mush)
mod4 <- lm(GrowthRate ~ Light + Nitrogen + Temperature + Species + Humidity,
           data = mush)

mse_mod1 <- mean(mod1$residuals^2)
mse_mod2 <- mean(mod2$residuals^2)
mse_mod3 <- mean(mod3$residuals^2)
mse_mod4 <- mean(mod4$residuals^2)

mse_mod1
mse_mod2
mse_mod3
mse_mod4

# predictions = model4 has the lowest mean squared value = best
new_data <- data.frame(
  Light = c(0, 10, 20),
  Nitrogen = c(0, 10, 20),
  Temperature = c(20, 20, 20),
  Species = c("P.ostreotus", "P.ostreotus", "P.ostreotus"),
  Humidity = c("Low", "Low", "Low")  
)
pred_values <- predict(mod4, newdata = new_data)
pred_df <- data.frame(new_data, PredictedGrowth = pred_values)
pred_df

# real 
real_pred <- mush %>%
  add_predictions(mod4)
real_pred$Type <- "Real Predicted"
pred_df$Type <- "Hypothetical"
combined <- bind_rows(
  real_pred %>% select(Light, GrowthRate, pred, Type),
  pred_df %>% rename(pred = PredictedGrowth) %>%
    mutate(GrowthRate = NA)
)

# plot
print(
  ggplot() +
  geom_point(data = real_pred,
             aes(x = Light, y = GrowthRate),
             color = "black", size = 2) +
  geom_point(data = real_pred,
             aes(x = Light, y = pred),
             color = "red", size = 3) +
  geom_point(data = pred_df,
             aes(x = Light, y = PredictedGrowth),
             color = "blue", size = 4) +
  theme_minimal() +
  labs(title = "Real vs Predicted GrowthRate (mod4)",
       x = "Light",
       y = "Growth Rate")
)