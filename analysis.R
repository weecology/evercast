devtools::document()

main <- "~/testing"
setup_dir(main = main)

counts_g <- read_counts(main, "all_GREG")
counts_w <- read_counts(main, "all_WOSTWHIB")

covariates <- read_covariates(main)

head(counts_g)
head(counts_w)
head(covariates)


forecast_years <- 2022:2026
nforecast_years <- length(forecast_years)


init_depth         <- covariates$init_depth
breed_season_depth <- covariates$breed_season_depth
cor.test(init_depth, breed_season_depth)
plot(init_depth, breed_season_depth)

plot(covariates$year, breed_season_depth, type = "l")


covariates$year

covariate_forecast_years <- forecast_years[!(forecast_years %in% covariates$year)]
ncovariate_forecast_years <- length(covariate_forecast_years)

forecast_model_breed_season_depth <- auto.arima(breed_season_depth)
forecast_breed_season_depth <- forecast(forecast_model_breed_season_depth, ncovariate_forecast_years)

future_breed_season_depth <- c(covariates$breed_season_depth[covariates$year %in% forecast_years] , forecast_breed_season_depth$mean)

nmissing_early_years <- min(covariates$year) - min(counts_g$year)

breed_season_depth <- c(rep(mean(breed_season_depth[1:5]), nmissing_early_years), breed_season_depth[covariates$year %in% counts_g$year])

# Poisson

fun  <- tsglm
args_g <- list(ts    = counts_g$count,
               model = list(past_obs  = 1,
                            past_mean = NULL),
               xreg  = breed_season_depth,
               link  = "log",
               distr = "poisson")
fit_g <- do.call(what = fun, 
                 args = args_g)
forecast_g <- predict(object  = fit_g, 
                      n.ahead = nforecast_years, 
                      newxreg = future_breed_season_depth,
                      level   = 0.95)



args_w <- list(ts    = counts_w$count,
               model = list(past_obs  = 1,
                            past_mean = NULL),
               xreg  = breed_season_depth,
               link  = "log",
               distr = "poisson")
fit_w <- do.call(what = fun, 
                 args = args_w)
forecast_w <- predict(object  = fit_w, 
                      n.ahead = nforecast_years, 
                      newxreg = future_breed_season_depth,
                      level   = 0.95)



par(mar = c(3, 5, 3, 1))
plot(counts_w$year, counts_w$count,
     cex = 1.25, lwd = 2,
     ylim = c(0, 100000),
     xlim = c(1985, 2027),
     xlab = "",
     ylab = "",
     main = "Everglades-Wide, Poisson GARCH, Breeding Water Level", las = 1, bty = "L", xaxt = "n", yaxt = "n")
axis(1, at = seq(1980, 2020, 10), cex.axis = 1.25)
axis(1, at = seq(1980, 2025, 5), labels = FALSE, tck = -0.01)
axis(1, at = seq(1980, 2027, 1), labels = FALSE, tck = -0.005)

axis(2, at = seq(0, 100000, 25000), labels = seq(0, 100000, 25000) / 1000, las = 1, cex.axis = 1.25)
axis(2, at = seq(0, 100000, 5000), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.01)
axis(2, at = seq(0, 100000, 10000), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.02)
mtext(side = 2, "Colony Count (x 1,000)", line = 3.25, cex = 1.25)

points(counts_w$year, fit_w$fitted.values, type = "l", lwd = 3)

last_year   <- counts_w$year[nrow(counts_w)]
last_count  <- counts_w$count[nrow(counts_w)]
last_fitted <- fit_w$fitted.values[nrow(counts_w)]

points(c(last_year, forecast_years), c(last_fitted, forecast_w$pred), type = "l", lty = 2, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, forecast_w$interval[ , "upper"]), type = "l", lty = 3, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, forecast_w$interval[ , "lower"]), type = "l", lty = 3, lwd = 2)



points(counts_g$year, counts_g$count, pch = 0, lwd = 2, cex = 1.25, col = grey(0.6))

points(counts_g$year, fit_g$fitted.values, type = "l", lwd = 3, col = grey(0.6))

last_year   <- counts_g$year[nrow(counts_g)]
last_count  <- counts_g$count[nrow(counts_g)]
last_fitted <- fit_g$fitted.values[nrow(counts_g)]

points(c(last_year, forecast_years), c(last_fitted, forecast_g$pred), type = "l", lty = 2, lwd = 2, col = grey(0.6))
points(c(last_year, forecast_years), c(last_fitted, forecast_g$interval[ , "upper"]), type = "l", lty = 3, lwd = 2, col = grey(0.6))
points(c(last_year, forecast_years), c(last_fitted, forecast_g$interval[ , "lower"]), type = "l", lty = 3, lwd = 2, col = grey(0.6))


text(rep(1986, 2), c(95000, 90000), c("WOST & WHIB", "GREG"), cex = 1.125, font = 2, col = c(grey(0), grey(0.6)), adj = 0)



# NB

fun  <- tsglm
args_g_nb <- list(ts    = counts_g$count,
                  model = list(past_obs  = 1,
                               past_mean = NULL),
                  xreg  = breed_season_depth,
                  link  = "log",
                  distr = "nbinom")
fit_g_nb <- do.call(what = fun, 
                    args = args_g_nb)
forecast_g_nb <- predict(object  = fit_g_nb, 
                         n.ahead = nforecast_years, 
                         newxreg = future_breed_season_depth,
                         level   = 0.95)



args_w_nb <- list(ts    = counts_w$count,
                  model = list(past_obs  = 1,
                               past_mean = NULL),
                  xreg  = breed_season_depth,
                  link  = "log",
                  distr = "nbinom")
fit_w_nb <- do.call(what = fun, 
                    args = args_w_nb)
forecast_w_nb <- predict(object  = fit_w_nb, 
                         n.ahead = nforecast_years, 
                         newxreg = future_breed_season_depth,
                         level   = 0.95)




par(mar = c(3, 5, 3, 1))
plot(counts_w$year, counts_w$count,
     cex = 1.25, lwd = 2,
     ylim = c(0, 100000),
     xlim = c(1985, 2027),
     xlab = "",
     ylab = "",
     main = "Everglades-Wide, NB GARCH, Breeding Water Level", las = 1, bty = "L", xaxt = "n", yaxt = "n")
axis(1, at = seq(1980, 2020, 10), cex.axis = 1.25)
axis(1, at = seq(1980, 2025, 5), labels = FALSE, tck = -0.01)
axis(1, at = seq(1980, 2027, 1), labels = FALSE, tck = -0.005)

axis(2, at = seq(0, 100000, 25000), labels = seq(0, 100000, 25000) / 1000, las = 1, cex.axis = 1.25)
axis(2, at = seq(0, 100000, 5000), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.01)
axis(2, at = seq(0, 100000, 10000), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.02)
mtext(side = 2, "Colony Count (x 1,000)", line = 3.25, cex = 1.25)

points(counts_w$year, fit_w$fitted.values, type = "l", lwd = 3)

last_year   <- counts_w$year[nrow(counts_w)]
last_count  <- counts_w$count[nrow(counts_w)]
last_fitted <- fit_w_nb$fitted.values[nrow(counts_w)]

points(c(last_year, forecast_years), c(last_fitted, forecast_w_nb$pred), type = "l", lty = 2, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, forecast_w_nb$interval[ , "upper"]), type = "l", lty = 3, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, forecast_w_nb$interval[ , "lower"]), type = "l", lty = 3, lwd = 2)



points(counts_g$year, counts_g$count, pch = 0, lwd = 2, cex = 1.25, col = grey(0.6))

points(counts_g$year, fit_g$fitted.values, type = "l", lwd = 3, col = grey(0.6))

last_year   <- counts_g$year[nrow(counts_g)]
last_count  <- counts_g$count[nrow(counts_g)]
last_fitted <- fit_g_nb$fitted.values[nrow(counts_g)]

points(c(last_year, forecast_years), c(last_fitted, forecast_g_nb$pred), type = "l", lty = 2, lwd = 2, col = grey(0.6))
points(c(last_year, forecast_years), c(last_fitted, forecast_g_nb$interval[ , "upper"]), type = "l", lty = 3, lwd = 2, col = grey(0.6))
points(c(last_year, forecast_years), c(last_fitted, forecast_g_nb$interval[ , "lower"]), type = "l", lty = 3, lwd = 2, col = grey(0.6))


text(rep(1986, 2), c(95000, 90000), c("WOST & WHIB", "GREG"), cex = 1.125, font = 2, col = c(grey(0), grey(0.6)), adj = 0)



summary(fit_g)
summary(fit_w)
summary(fit_g_nb)
summary(fit_w_nb)
