df_dove <- read_csv("data/processed/doves.csv") %>%
  mutate(date = lubridate::date(date)) %>%
  complete(date = seq(lubridate::date("2016-01-01"), lubridate::date("2022-12-31"), by = "day")) %>%
  arrange(date) %>%
  mutate(number = replace_na(number, 0)) %>%
  mutate(number = cumsum(number))

p_dove <- ggplot(df_dove) +
  geom_line(aes(x = date, y = number)) +
  labs(
    x = "Time",
    y = "Cumulated number of launched satellites"
  ) +
  scale_x_date(breaks = "year") +
  ggthemes::theme_few()
