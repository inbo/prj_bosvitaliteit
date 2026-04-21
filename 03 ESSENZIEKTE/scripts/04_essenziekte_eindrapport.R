# Steekproef bespreking

# symptomen levende bomen 2014-2023

# Trend essenziekte (model incl bodem + omtrek_start (brms))

source("03 ESSENZIEKTE/scripts/00_Essenziekte_data.R")

proefvlakken <- unique(df_es_ana$proefvlak_nummer)
n_plots <- length(proefvlakken)

# only data plot
p <-
  ggplot(
    df_es_ana |>
      filter(proefvlak_nummer %in% proefvlakken),
    aes(x = jaar, y = bladverlies, group = boom_id)
  ) +
  xlim(2014, 2025) +
  geom_line() +
  geom_point(size = 0.5) +
  theme(
    strip.text = element_text(size = 8),
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8)
  ) +
  facet_wrap(~proefvlak_nummer, ncol = 6, nrow = 5)
print(p)

# naive plot (only 1 100%)
ggplot(df_es_ana, aes(x = jaar, y = bladverlies)) +
  geom_line(aes(group = boom_id), alpha = 0.8, color = "darkgrey") +
  geom_smooth(method = "loess", color = inbo_donkergroen, se = FALSE) +
  facet_wrap(~proefvlak_nummer) +
  theme_minimal()

# naive plot (keep 100%)
ggplot(df_es_cc, aes(x = jaar, y = bladverlies)) +
  geom_line(aes(group = boom_id), alpha = 0.9, color = "darkgrey") +
  geom_smooth(method = "loess", color = inbo_donkergroen, se = FALSE) +
  facet_wrap(~proefvlak_nummer) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
