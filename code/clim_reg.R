# linear regression
df_clim_reg <- df_lag_clim %>%
  filter(site != "SJ") %>%
  group_by(taxa) %>%
  do(broom::tidy(lm(lag ~ mat, .))) %>%
  filter(term %in% c("mat")) %>%
  dplyr::select(-statistic)
# summary(df_clim_reg$p.value < 0.05)

p_slope <- ggplot(df_clim_reg %>%
  filter(!taxa %in% c("Poaceae early", "Poaceae late", "Ambrosia"))) +
  geom_point(aes(x = taxa, y = estimate)) +
  geom_errorbar(aes(x = taxa, ymin = estimate - 1.95 * std.error, ymax = estimate + 1.95 * std.error), width = 0.2) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 9.5, lty = 1) +
  theme_classic() +
  ylab("Slope (day/°C)") +
  xlab("Taxa")



fit_lme <- nlme::lme(lag ~ mat,
  random = ~ 1 | taxa, data = df_lag_clim %>%
    filter(!taxa %in% c("Poaceae early", "Poaceae late", "Ambrosia")) %>%
    filter(site != "SJ") %>%
    filter(group == "early"),
  control = nlme::lmeControl(opt = "optim", optimMethod = "SANN")
)
