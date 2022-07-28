#devtools::load_all("./wader")
devtools::document("./evercast")

devtools::load_all("./evercast")

main <- "~/testing"
setup_dir(main = main)

counts <- read_counts(main)
covariates <- read_covariates(main)

head(counts)
head(covariates)


forecast_years <- 2022:2025
nforecast_years <- length(forecast_years)

par(mfrow = c(3, 2))

# Poisson GARCH
#  no water

fun  <- tsglm
args <- list(ts    = counts$count,
             model = list(past_obs  = 1,
                          past_mean = NULL),
             link  = "log",
             distr = "poisson")
fit <- do.call(what = fun, 
               args = args)
forecast <- predict(fit, nforecast_years, level = 0.95)

par(mar = c(3, 5, 3, 1))
plot(counts$year, counts$count,
     ylim = c(0, 150000),
     xlim = c(1985, 2027),
     xlab = "",
     ylab = "",
     main = "Total, Everglades-Wide, pGARCH", las = 1, bty = "L", xaxt = "n", yaxt = "n")
axis(1, at = seq(1980, 2020, 10), cex.axis = 1.25)
axis(1, at = seq(1980, 2025, 5), labels = FALSE, tck = -0.01)
axis(1, at = seq(1980, 2027, 1), labels = FALSE, tck = -0.005)

axis(2, at = seq(0, 150000, 25000), labels = seq(0, 150000, 25000) / 1000, las = 1, cex.axis = 1.25)
mtext(side = 2, "Colony Count (x 1,000)", line = 3.25, cex = 1.25)

points(counts$year, fit$fitted.values, type = "l", lwd = 3)

last_year   <- counts$year[nrow(counts)]
last_count  <- counts$count[nrow(counts)]
last_fitted <- fit$fitted.values[nrow(counts)]

points(c(last_year, forecast_years), c(last_fitted, forecast$pred), type = "l", lty = 2, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, forecast$interval[ , "upper"]), type = "l", lty = 3, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, forecast$interval[ , "lower"]), type = "l", lty = 3, lwd = 2)



# Negative Binomial GARCH
#  no water

fun  <- tsglm
args <- list(ts    = counts$count,
             model = list(past_obs  = 1,
                          past_mean = NULL),
             link  = "log",
             distr = "nbinom")
fit <- do.call(what = fun, 
               args = args)
forecast <- predict(fit, nforecast_years, level = 0.95)

par(mar = c(3, 5, 3, 1))
plot(counts$year, counts$count,
     ylim = c(0, 150000),
     xlim = c(1985, 2027),
     xlab = "",
     ylab = "",
     main = "Total, Everglades-Wide, nbGARCH", las = 1, bty = "L", xaxt = "n", yaxt = "n")
axis(1, at = seq(1980, 2020, 10), cex.axis = 1.25)
axis(1, at = seq(1980, 2025, 5), labels = FALSE, tck = -0.01)
axis(1, at = seq(1980, 2027, 1), labels = FALSE, tck = -0.005)

axis(2, at = seq(0, 150000, 25000), labels = seq(0, 150000, 25000) / 1000, las = 1, cex.axis = 1.25)
mtext(side = 2, "Colony Count (x 1,000)", line = 3.25, cex = 1.25)

points(counts$year, fit$fitted.values, type = "l", lwd = 3)

last_year   <- counts$year[nrow(counts)]
last_count  <- counts$count[nrow(counts)]
last_fitted <- fit$fitted.values[nrow(counts)]

points(c(last_year, forecast_years), c(last_fitted, forecast$pred), type = "l", lty = 2, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, forecast$interval[ , "upper"]), type = "l", lty = 3, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, forecast$interval[ , "lower"]), type = "l", lty = 3, lwd = 2)


# Including forecasted covariate: water
# note that depth starts in 1991, counts in 1986
#  extrapolate back to 1986
#   assume distributed according to the "low phase" depths

low_phase_years <- c(1991:1999, 2012:2021)
mean_low_phase_init_depth <- mean(covariates$init_depth[covariates$year %in% low_phase_years])
init_depth <- c(rep(mean_low_phase_init_depth, 5), covariates$init_depth[covariates$year < 2022])

# Given the bi phasic nature of depth, just using the low-phase mean for the forecast years as well

forecast_init_depth <- rep(mean_low_phase_init_depth, nforecast_years)


par(mar = c(3, 5, 3, 1))
plot(counts$year, init_depth,
     ylim = c(0, 250),
     xlim = c(1985, 2027),
     xlab = "",
     ylab = "",
     main = "", las = 1, bty = "L", xaxt = "n", yaxt = "n")
axis(1, at = seq(1980, 2020, 10), cex.axis = 1.25)
axis(1, at = seq(1980, 2025, 5), labels = FALSE, tck = -0.01)
axis(1, at = seq(1980, 2027, 1), labels = FALSE, tck = -0.005)

axis(2, at = seq(0, 250, 50), las = 1, cex.axis = 1.25)
mtext(side = 2, "Initial Depth", line = 3.25, cex = 1.25)


points(forecast_years, forecast_init_depth, pch = 16)
points(1986:1990, rep(mean_low_phase_init_depth, 5), pch = 16)



low_phase_years <- c(1991:1999, 2012:2021)
mean_low_phase_breed_season_depth <- mean(covariates$breed_season_depth[covariates$year %in% low_phase_years])
breed_season_depth <- c(rep(mean_low_phase_breed_season_depth, 5), covariates$breed_season_depth[covariates$year < 2022])

# Given the bi phasic nature of depth, just using the low-phase mean for the forecast years as well

forecast_breed_season_depth <- rep(mean_low_phase_breed_season_depth, nforecast_years)


par(mar = c(3, 5, 3, 1))
plot(counts$year, breed_season_depth,
     ylim = c(0, 250),
     xlim = c(1985, 2027),
     xlab = "",
     ylab = "",
     main = "", las = 1, bty = "L", xaxt = "n", yaxt = "n")
axis(1, at = seq(1980, 2020, 10), cex.axis = 1.25)
axis(1, at = seq(1980, 2025, 5), labels = FALSE, tck = -0.01)
axis(1, at = seq(1980, 2027, 1), labels = FALSE, tck = -0.005)

axis(2, at = seq(0, 250, 50), las = 1, cex.axis = 1.25)
mtext(side = 2, "Breeding Season Depth", line = 3.25, cex = 1.25)


points(forecast_years, forecast_breed_season_depth, pch = 16)
points(1986:1990, rep(mean_low_phase_breed_season_depth, 5), pch = 16)




# Poisson GARCH
#  with water

fun  <- tsglm
args <- list(ts    = counts$count,
             model = list(past_obs  = 1,
                          past_mean = NULL),
             xreg  = init_depth,
             link  = "log",
             distr = "poisson")
fit <- do.call(what = fun, 
               args = args)
forecast <- predict(object  = fit, 
                    n.ahead = nforecast_years, 
                    newxreg = forecast_init_depth,
                    level   = 0.95)

par(mar = c(3, 5, 3, 1))
plot(counts$year, counts$count,
     ylim = c(0, 150000),
     xlim = c(1985, 2027),
     xlab = "",
     ylab = "",
     main = "Total, Everglades-Wide, pwGARCH", las = 1, bty = "L", xaxt = "n", yaxt = "n")
axis(1, at = seq(1980, 2020, 10), cex.axis = 1.25)
axis(1, at = seq(1980, 2025, 5), labels = FALSE, tck = -0.01)
axis(1, at = seq(1980, 2027, 1), labels = FALSE, tck = -0.005)

axis(2, at = seq(0, 150000, 25000), labels = seq(0, 150000, 25000) / 1000, las = 1, cex.axis = 1.25)
mtext(side = 2, "Colony Count (x 1,000)", line = 3.25, cex = 1.25)

points(counts$year, fit$fitted.values, type = "l", lwd = 3)

last_year   <- counts$year[nrow(counts)]
last_count  <- counts$count[nrow(counts)]
last_fitted <- fit$fitted.values[nrow(counts)]

points(c(last_year, forecast_years), c(last_fitted, forecast$pred), type = "l", lty = 2, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, forecast$interval[ , "upper"]), type = "l", lty = 3, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, forecast$interval[ , "lower"]), type = "l", lty = 3, lwd = 2)



# Negative Binomial GARCH
#  with water

fun  <- tsglm
args <- list(ts    = counts$count,
             model = list(past_obs  = 1,
                          past_mean = NULL),
             xreg  = init_depth,
             link  = "log",
             distr = "nbinom")
fit <- do.call(what = fun, 
               args = args)
forecast <- predict(object  = fit, 
                    n.ahead = nforecast_years, 
                    newxreg = forecast_init_depth,
                    level   = 0.95)

par(mar = c(3, 5, 3, 1))
plot(counts$year, counts$count,
     ylim = c(0, 150000),
     xlim = c(1985, 2027),
     xlab = "",
     ylab = "",
     main = "Total, Everglades-Wide, nbwGARCH", las = 1, bty = "L", xaxt = "n", yaxt = "n")
axis(1, at = seq(1980, 2020, 10), cex.axis = 1.25)
axis(1, at = seq(1980, 2025, 5), labels = FALSE, tck = -0.01)
axis(1, at = seq(1980, 2027, 1), labels = FALSE, tck = -0.005)

axis(2, at = seq(0, 150000, 25000), labels = seq(0, 150000, 25000) / 1000, las = 1, cex.axis = 1.25)
mtext(side = 2, "Colony Count (x 1,000)", line = 3.25, cex = 1.25)

points(counts$year, fit$fitted.values, type = "l", lwd = 3)

last_year   <- counts$year[nrow(counts)]
last_count  <- counts$count[nrow(counts)]
last_fitted <- fit$fitted.values[nrow(counts)]

points(c(last_year, forecast_years), c(last_fitted, forecast$pred), type = "l", lty = 2, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, forecast$interval[ , "upper"]), type = "l", lty = 3, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, forecast$interval[ , "lower"]), type = "l", lty = 3, lwd = 2)



