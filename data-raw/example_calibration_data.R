library(simmsm)

set.seed(42L)

tmat <- as_TransitionMatrix(
  matrix(
    c(NA,  1,  2, NA,
      NA, NA,  3, NA,
      NA, NA, NA,  4,
      NA, NA, NA, NA),
    nrow = 4,
    byrow = TRUE
  ),
  state_labels = c("start", "response", "progression", "death")
)

msm <- MSM(
  Exponential(log(2) / 3),
  Exponential(log(2) / 6),
  Exponential(log(2) / 12),
  Exponential(log(2) / 3),
  tmat = tmat,
  tmax = 60
)

tbl_data <- simulate(msm, nsim = 2^12)
time_points <- c(0, 3, 6, 12, 24, 36)

example_calibration_data <- list(
  pr_start_response = next_state_probabilities(msm, tbl_data, as_matrix = TRUE)["start", "response"],
  pr_start_response_se = 0.05
)

tbl_km_pfs <- kaplan_meier(tbl_data, "start", c("progression", "death"))
example_calibration_data$tbl_km_pfs <- with(tbl_km_pfs,
  tibble::tibble(
    t = time_points,
    est = approx(time, estimate, xout = time_points, yleft = 1, rule = 2, method = "constant")$y,
    se = approx(time, std.error, xout = time_points, yleft = 1, rule = 2, method = "constant")$y
  )
)

tbl_km_os <- kaplan_meier(tbl_data, "start", "death")
example_calibration_data$tbl_km_os <- with(tbl_km_os,
  tibble::tibble(
    t = time_points,
    est = approx(time, estimate, xout = time_points, yleft = 1, rule = 2, method = "constant")$y,
    se = approx(time, std.error, xout = time_points, yleft = 1, rule = 2, method = "constant")$y
  )
)

tbl_km_pd <- kaplan_meier(tbl_data, "progression", "death")
example_calibration_data$tbl_km_progression_death <- with(tbl_km_pd,
  tibble::tibble(
    t = 6,
    est = approx(time, estimate, xout = 6, yleft = 1, rule = 2, method = "constant")$y,
    se = approx(time, std.error, xout = 6, yleft = 1, rule = 2, method = "constant")$y
  )
)

usethis::use_data(example_calibration_data, overwrite = TRUE)
