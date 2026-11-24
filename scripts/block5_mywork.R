library(readr)
library(dplyr)
library(ggplot2)

my_data <- read_hh("data/raw/unzipped")
worldclim_monthly <- read_csv(
  "data/external/worldclim/worldclim_monthly_normals_my_sites.csv",
  show_col_types = FALSE
)

sites <- names(my_data)
valid_worldclim <- all(c("site_id", "month", "tavg") %in% names(worldclim_monthly)) &&
  !anyDuplicated(worldclim_monthly[c("site_id", "month")]) &&
  all(worldclim_monthly$month %in% 1:12) &&
  all(is.finite(worldclim_monthly$tavg)) &&
  all(vapply(sites, function(site) {
    setequal(as.integer(worldclim_monthly$month[worldclim_monthly$site_id == site]), 1:12)
  }, logical(1)))
if (!valid_worldclim) {
  stop("WorldClim data must have unique, finite monthly normals (months 1-12) for every tower.")
}

mean_or_na <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)

worldclim_annual <- worldclim_monthly |>
  group_by(site_id) |>
  summarise(BIO1_normal = mean(tavg), .groups = "drop")

dheed_events <- read_csv(
  "data/external/dheed/dheed_events_my_sites.csv",
  show_col_types = FALSE
)
if (!all(c("site_id", "first_day_at_tower") %in% names(dheed_events))) {
  stop("DHEED events data must contain site_id and first_day_at_tower columns.")
}
if (anyNA(as.Date(dheed_events$first_day_at_tower))) {
  stop("DHEED events data contains invalid first_day_at_tower dates.")
}

dheed_counts <- dheed_events |>
  mutate(year = as.integer(format(as.Date(first_day_at_tower), "%Y"))) |>
  count(site_id, year, name = "dheed_event_count")

annual_data <- bind_rows(my_data, .id = "site_id") |>
  mutate(year = as.integer(format(time, "%Y")),
         month = format(time, "%m")) |>
  group_by(site_id, year) |>
  summarise(
    n_months = n_distinct(month[!is.na(TA_F)]),
    across(where(is.numeric), mean_or_na),
    .groups = "drop"
  ) |>
  filter(n_months == 12) |>
  left_join(worldclim_annual, by = "site_id") |>
  mutate(TA_F_anomaly = TA_F - BIO1_normal) |>
  select(-n_months, -BIO1_normal) |>
  left_join(dheed_counts, by = c("site_id", "year")) |>
  mutate(
    dheed_event_count = if_else(
      year >= 1950 & year <= 2023,
      coalesce(dheed_event_count, 0L),
      NA_integer_
    )
  )

my_data <- split(annual_data, annual_data$site_id)

plot_dir <- file.path("reports", "annual_temperature_anomalies")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

test_site_anomaly <- function(site_id) {
  anomalies <- annual_data$TA_F_anomaly[annual_data$site_id == site_id]
  anomalies <- anomalies[is.finite(anomalies)]

  if (length(anomalies) < 2) {
    return(tibble(
      site_id = site_id, n_years = length(anomalies),
      mean_anomaly = mean_or_na(anomalies), sd_anomaly = NA_real_,
      conf_low = NA_real_, conf_high = NA_real_, p_value = NA_real_,
      status = "At least two valid years are required"
    ))
  }
  if (sd(anomalies) == 0) {
    return(tibble(
      site_id = site_id, n_years = length(anomalies),
      mean_anomaly = mean(anomalies), sd_anomaly = 0,
      conf_low = NA_real_, conf_high = NA_real_, p_value = NA_real_,
      status = "No variation; t-test is undefined"
    ))
  }

  test <- t.test(anomalies, mu = 0)
  tibble(
    site_id = site_id, n_years = length(anomalies),
    mean_anomaly = mean(anomalies), sd_anomaly = sd(anomalies),
    conf_low = unname(test$conf.int[1]),
    conf_high = unname(test$conf.int[2]),
    p_value = test$p.value, status = "ok"
  )
}

statistical_results <- bind_rows(lapply(sites, test_site_anomaly)) |>
  mutate(p_holm = NA_real_)
tested_sites <- is.finite(statistical_results$p_value)
statistical_results$p_holm[tested_sites] <- p.adjust(
  statistical_results$p_value[tested_sites],
  method = "holm"
)
write_csv(
  statistical_results,
  file.path(plot_dir, "annual_temperature_anomaly_tests.csv")
)

test_event_correlation <- function(site_id) {
  paired <- annual_data |>
    filter(.data$site_id == .env$site_id, !is.na(dheed_event_count),
           is.finite(TA_F_anomaly))

  if (nrow(paired) < 3 || n_distinct(paired$TA_F_anomaly) < 2 ||
      n_distinct(paired$dheed_event_count) < 2) {
    return(tibble(
      site_id = site_id, n_years = nrow(paired), spearman_rho = NA_real_,
      p_value = NA_real_, status = "At least three paired years and variation in both variables are required"
    ))
  }

  test <- cor.test(
    paired$TA_F_anomaly,
    paired$dheed_event_count,
    method = "spearman",
    exact = FALSE
  )
  tibble(
    site_id = site_id, n_years = nrow(paired),
    spearman_rho = unname(test$estimate),
    p_value = test$p.value, status = "ok"
  )
}

event_correlations <- bind_rows(lapply(sites, test_event_correlation)) |>
  mutate(p_holm = NA_real_)
tested_correlations <- is.finite(event_correlations$p_value)
event_correlations$p_holm[tested_correlations] <- p.adjust(
  event_correlations$p_value[tested_correlations],
  method = "holm"
)
write_csv(
  event_correlations,
  file.path(plot_dir, "temperature_anomaly_dheed_correlations.csv")
)

for (site_id in names(my_data)) {
  site_plot <- ggplot(my_data[[site_id]], aes(year, TA_F_anomaly)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_line(color = "steelblue", na.rm = TRUE) +
    labs(title = paste("Annual temperature anomaly:", site_id),
         x = "Year", y = "Temperature anomaly (°C)") +
    theme_minimal()
  safe_site_id <- gsub("[^A-Za-z0-9_-]", "_", site_id)
  ggsave(file.path(plot_dir, paste0(safe_site_id, "_annual_temperature_anomaly.png")),
         site_plot, width = 9, height = 5, dpi = 300)
}

all_sites_plot <- ggplot(
  annual_data, aes(year, TA_F_anomaly, color = site_id, group = site_id)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(na.rm = TRUE) +
  labs(title = "Annual temperature anomalies by tower",
       x = "Year", y = "Temperature anomaly (°C)", color = "Tower") +
  theme_minimal()

ggsave(file.path(plot_dir, "all_sites_annual_temperature_anomaly.png"),
       all_sites_plot, width = 10, height = 6, dpi = 300)

event_plot <- ggplot(
  annual_data |> filter(!is.na(dheed_event_count)),
  aes(TA_F_anomaly, dheed_event_count)
) +
  geom_point(color = "steelblue", alpha = 0.8) +
  facet_wrap(vars(site_id), scales = "free_y") +
  labs(
    title = "Annual temperature anomalies and DHEED events",
    x = "Temperature anomaly (°C)",
    y = "DHEED events starting at tower that year"
  ) +
  theme_minimal()

ggsave(file.path(plot_dir, "temperature_anomaly_vs_dheed_events.png"),
       event_plot, width = 10, height = 7, dpi = 300)
