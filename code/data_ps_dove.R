df_dove <- read_csv(str_c(.path$input, "ps/doves.csv")) %>%
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

# save figure
if (.fig_save) {
  ggsave(
    plot = p_dove,
    filename = str_c(.path$output, "supp/supp_dove.pdf"),
    width = 7,
    height = 5,
    device = pdf
  )
}
