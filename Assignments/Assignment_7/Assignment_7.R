# --- Libraries ---
library(readr)
library(janitor)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# --- Load and tidy data ---
df_raw <- read_csv("Utah_Religions_by_County.csv", show_col_types = FALSE) |>
  clean_names()

df_tidy <- df_raw |>
  rename(population = pop_2010) |>
  pivot_longer(
    cols = -c(county, population),
    names_to = "religion",
    values_to = "proportion"
  ) |>
  mutate(
    religion = religion |> 
      str_replace_all("_", " ") |> 
      str_replace_all("-", " ") |> 
      str_to_title()
  ) |>
  # Remove totals so only individual religions remain
  filter(!religion %in% c("Religious", "Non Religious"))

# --- Create the plot ---
p_by_county <- ggplot(df_tidy, aes(
  x = reorder(religion, proportion),
  y = proportion * 100,
  fill = religion
)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ county, scales = "free_y") +
  labs(
    title = "Religious Composition of Utah Counties (2010)",
    x = "Religious Group",
    y = "Percent of County Population"
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    axis.text.x = element_text(
      angle = 90,         # vertical labels
      vjust = 0.5,        # centered vertically
      hjust = 1,          # aligned under tick marks
      size = 7            # smaller font to fit
    ),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 11, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 11, face = "bold", margin = margin(r = 10)),
    strip.text = element_text(face = "bold", size = 11),
    panel.spacing = unit(1, "lines")
  )

# --- Display and save ---
print(p_by_county)

if (!dir.exists("figs")) dir.create("figs")
ggsave("figs/religion_by_county_faceted_vertical.png",
       p_by_county, width = 18, height = 12, dpi = 300)
