library(tidyverse)
options(readr.show_col_types = FALSE)
csv_path <- file.path("..","..","Data","BioLog_Plate_Data.csv")
message("Working dir: ", getwd())
message("CSV exists? ", file.exists(csv_path), "  ->  ", csv_path)
if (!file.exists(csv_path)) {
  message("Trying to locate the file…")
  print(list.files(path = here::here("."), recursive = TRUE,
                   pattern = "BioLog_Plate_Data\\.csv$", ignore.case = TRUE))
  stop("Couldn't find the CSV at the expected relative path. Adjust csv_path above.")}
dat_raw <- read_csv(csv_path)
glimpse(dat_raw)
names(dat_raw)
head(dat_raw, 3)
library(stringr)
dat_tidy <- dat_raw %>%
  pivot_longer(
    cols = starts_with("Hr_"),
    names_to = "Hour",
    values_to = "Absorbance"
  ) %>%
  mutate(
    Hour = readr::parse_number(Hour),
    Sample_Type = if_else(str_detect(`Sample ID`, regex("soil", ignore_case = TRUE)),
                          "soil", "water"),
    Substrate = str_squish(Substrate)
  )
glimpse(dat_tidy)
distinct(dat_tidy, `Sample ID`, Sample_Type) %>% arrange(`Sample ID`) %>% print(n = Inf)
dat_plot <- dat_tidy %>%
  filter(Dilution == 0.1) %>%
  group_by(`Sample ID`, Sample_Type, Hour) %>%
  summarise(MeanAbs = mean(Absorbance, na.rm = TRUE), .groups = "drop")

p_static <- ggplot(dat_plot, aes(x = Hour, y = MeanAbs, color = `Sample ID`)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ Sample_Type, nrow = 1) +
  scale_x_continuous(breaks = c(24, 48, 144)) +
  labs(title = "Biolog: Mean Absorbance over Time (Dilution 0.1)",
       x = "Hour", y = "Mean Absorbance") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

print(p_static)
ggsave("Biolog_Dilution0.1_static.png", p_static, width = 9, height = 4.5, dpi = 200)

p_static

library(gganimate)
library(gifski)

dat_ita <- dat_tidy %>%
  filter(Substrate == "Itaconic Acid") %>%
  group_by(`Sample ID`, Sample_Type, Dilution, Hour) %>%
  summarise(MeanAbs = mean(Absorbance, na.rm = TRUE), .groups = "drop")

p_anim <- ggplot(dat_ita, aes(x = Dilution, y = MeanAbs, color = `Sample ID`)) +
  geom_point(size = 3, alpha = 0.9) +             
  facet_wrap(~ Sample_Type, nrow = 1) +
  scale_x_log10() +
  labs(title = "Itaconic Acid Utilization — Hour = {current_frame}",
       x = "Dilution", y = "Mean Absorbance") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom") +
  transition_manual(Hour)                          

anim <- animate(p_anim, nframes = length(unique(dat_ita$Hour)),
                fps = 2, width = 800, height = 400,
                renderer = gifski_renderer())

anim_save("Itaconic_Acid_Animation.gif", animation = anim)
print(p_static)