library(tidyverse)

dat <- read_csv("cleaned_covid_data.csv")

A_states <- dat %>% 
  filter(str_starts(Province_State, "A"))

p1 <- ggplot(A_states, aes(x = Last_Update, y = Deaths)) +
  geom_point(alpha = 0.5, size = 0.8) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ Province_State, scales = "free")
print(p1)

state_max_fatality_rate <- dat %>% 
  group_by(Province_State) %>% 
  summarize(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE)) %>% 
  mutate(Maximum_Fatality_Ratio = ifelse(is.infinite(Maximum_Fatality_Ratio), NA_real_, Maximum_Fatality_Ratio)) %>% 
  arrange(desc(Maximum_Fatality_Ratio))

p2 <- ggplot(state_max_fatality_rate,
             aes(x = fct_reorder(Province_State, Maximum_Fatality_Ratio, .desc = TRUE),
                 y = Maximum_Fatality_Ratio)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(p2)

us_deaths <- dat %>%
  group_by(Last_Update) %>%
  summarize(US_Cumulative_Deaths = sum(Deaths, na.rm = TRUE))

p3 <- ggplot(us_deaths, aes(x = Last_Update, y = US_Cumulative_Deaths)) +
  geom_line()
print(p3)
