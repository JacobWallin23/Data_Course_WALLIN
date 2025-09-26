list.files(path = "Data", pattern = "\\.csv$", full.names = TRUE)
length(list.files(path = "Data", pattern = "\\.csv$", full.names = TRUE))
df <- read.csv("Data/wingspan_vs_mass.csv")
head(df, 5)
b_files <- list.files(path = "Data", pattern = "^b", recursive = TRUE, full.names = TRUE)
for (file in b_files) {
  cat("File:", file, "\n")
  print(readLines(file, n = 1))
  cat("\n")
}
csv_files <- list.files(path = "Data", pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
for (file in csv_files) {
  cat("File:", file, "\n")
  print(readLines(file, n = 1))
  cat("\n")
}