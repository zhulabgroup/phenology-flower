# linear regression
df_clim_reg <- df_lag_clim %>%
  group_by(taxa) %>%
  filter(n() >= 3) %>%
  do(broom::tidy(lm(lag ~ mat, .))) %>%
  filter(term %in% c("mat")) %>%
  select(-statistic)

p_slope <- ggplot(df_clim_reg) +
  geom_point(aes(x = taxa, y = estimate)) +
  geom_errorbar(aes(x = taxa, ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.2) +
  geom_hline(yintercept = 0, lty = 2) +
  ggthemes::theme_few() +
  ylab("Slope (day/°C)") +
  xlab("Genus") +
  theme(axis.text.x = element_text(face = "italic"))

fit_lme <- lmerTest::lmer(lag ~ mat + 1 | taxa, data = df_lag_clim %>%
  filter(group == "early"))
