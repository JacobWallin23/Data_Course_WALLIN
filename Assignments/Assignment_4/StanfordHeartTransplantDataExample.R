library(survival)
data("heart", package = "survival")

write.csv(heart, 
          "/Users/jakewallin/Desktop/Data_Course_WALLIN/Assignments/Assignment_4/stanford_heart.csv", 
          row.names = FALSE)
