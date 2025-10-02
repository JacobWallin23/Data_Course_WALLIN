library(survival)
library(ggplot2)
library(broom)

data("heart", package = "survival")

surv_obj <- Surv(time = heart$stop, event = heart$event)
fit <- survfit(surv_obj ~ transplant, data = heart)

plot(fit, col = c("red","blue"), lty = 1:2,
     xlab = "Days since acceptance",
     ylab = "Survival probability",
     main = "Kaplan-Meier Survival (Stanford Data)")
legend("bottomleft",
       legend = c("No transplant","Transplant"),
       col = c("red","blue"), lty = 1:2)

plot_data <- tidy(fit)
p <- ggplot(plot_data, aes(x = time, y = estimate, color = strata)) +
  geom_step(size = 1) +
  labs(title = "Kaplan-Meier Survival Curves (Stanford Dataset)",
       x = "Days since acceptance",
       y = "Survival probability",
       color = "Group") +
  theme_minimal()

print(p)
ggsave("/Users/jakewallin/Desktop/Data_Course_WALLIN/Assignments/Assignment_4/example_plot.png",
       plot = p, width = 6, height = 4)
