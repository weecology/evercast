# currently won't work to fully establish the datasets, given that 3A covariates and counts are not available at that level specifically
# the dataset controls are written for them, with only partial implementation completed. not sure what form the data will come so holding.

devtools::document()

main <- "~/testing"
#setup_dir(main = main)

fill_data(main)

counts_greg <- read_counts(main, "all_GREG")
counts_wost <- read_counts(main, "all_WOST")
counts_whib <- read_counts(main, "all_WHIB")
counts_rosp <- read_counts(main, "all_ROSP")

covariates <- read_covariates(main)

head(counts_greg)
head(counts_wost)
head(counts_whib)
head(counts_rosp)
head(covariates)


forecast_years <- 2022:2026
nforecast_years <- length(forecast_years)


init_depth         <- covariates$init_depth
breed_season_depth <- covariates$breed_season_depth
cor.test(init_depth, breed_season_depth)
plot(init_depth, breed_season_depth)

plot(covariates$year, breed_season_depth, type = "l")


breed_season_depth <- breed_season_depth[!(covariates$year %in% forecast_years)]
covariate_forecast_years <- forecast_years[!(forecast_years %in% covariates$year)]
ncovariate_forecast_years <- length(covariate_forecast_years)

forecast_model_breed_season_depth <- auto.arima(breed_season_depth)
forecast_breed_season_depth <- forecast(forecast_model_breed_season_depth, ncovariate_forecast_years)

future_breed_season_depth <- c(covariates$breed_season_depth[covariates$year %in% forecast_years] , forecast_breed_season_depth$mean)



# Poisson

fun  <- tsglm
args_greg_p <- list(ts    = counts_greg$count,
                    model = list(past_obs  = 1,
                                 past_mean = NULL),
                    xreg  = breed_season_depth,
                    link  = "log",
                    distr = "poisson")
fit_greg_p <- do.call(what = fun, 
                      args = args_greg_p)
forecast_greg_p <- predict(object  = fit_greg_p, 
                           n.ahead = nforecast_years, 
                           newxreg = future_breed_season_depth,
                           level   = 0.95)



fun  <- tsglm
args_whib_p <- list(ts    = counts_whib$count,
                    model = list(past_obs  = 1,
                                 past_mean = NULL),
                    xreg  = breed_season_depth,
                    link  = "log",
                    distr = "poisson")
fit_whib_p <- do.call(what = fun, 
                      args = args_whib_p)
forecast_whib_p <- predict(object  = fit_whib_p, 
                           n.ahead = nforecast_years, 
                           newxreg = future_breed_season_depth,
                           level   = 0.95)


fun  <- tsglm
args_wost_p <- list(ts    = counts_wost$count,
                    model = list(past_obs  = 1,
                                 past_mean = NULL),
                    xreg  = breed_season_depth,
                    link  = "log",
                    distr = "poisson")
fit_wost_p <- do.call(what = fun, 
                      args = args_wost_p)
forecast_wost_p <- predict(object  = fit_wost_p, 
                           n.ahead = nforecast_years, 
                           newxreg = future_breed_season_depth,
                           level   = 0.95)


fun  <- tsglm
args_rosp_p <- list(ts    = counts_rosp$count,
                    model = list(past_obs  = 1,
                                 past_mean = NULL),
                    xreg  = breed_season_depth,
                    link  = "log",
                    distr = "poisson")
fit_rosp_p <- do.call(what = fun, 
                      args = args_rosp_p)
forecast_rosp_p <- predict(object  = fit_rosp_p, 
                           n.ahead = nforecast_years, 
                           newxreg = future_breed_season_depth,
                           level   = 0.95)




# nbinom

fun  <- tsglm
args_greg_nb <- list(ts    = counts_greg$count,
                    model = list(past_obs  = 1,
                                 past_mean = NULL),
                    xreg  = breed_season_depth,
                    link  = "log",
                    distr = "nbinom")
fit_greg_nb <- do.call(what = fun, 
                      args = args_greg_nb)
forecast_greg_nb <- predict(object  = fit_greg_nb, 
                           n.ahead = nforecast_years, 
                           newxreg = future_breed_season_depth,
                           level   = 0.95)



fun  <- tsglm
args_whib_nb <- list(ts    = counts_whib$count,
                    model = list(past_obs  = 1,
                                 past_mean = NULL),
                    xreg  = breed_season_depth,
                    link  = "log",
                    distr = "nbinom")
fit_whib_nb <- do.call(what = fun, 
                      args = args_whib_nb)
forecast_whib_nb <- predict(object  = fit_whib_nb, 
                           n.ahead = nforecast_years, 
                           newxreg = future_breed_season_depth,
                           level   = 0.95)


fun  <- tsglm
args_wost_nb <- list(ts    = counts_wost$count,
                    model = list(past_obs  = 1,
                                 past_mean = NULL),
                    xreg  = breed_season_depth,
                    link  = "log",
                    distr = "nbinom")
fit_wost_nb <- do.call(what = fun, 
                      args = args_wost_nb)
forecast_wost_nb <- predict(object  = fit_wost_nb, 
                           n.ahead = nforecast_years, 
                           newxreg = future_breed_season_depth,
                           level   = 0.95)


fun  <- tsglm
args_rosp_nb <- list(ts    = counts_rosp$count,
                    model = list(past_obs  = 1,
                                 past_mean = NULL),
                    xreg  = breed_season_depth,
                    link  = "log",
                    distr = "nbinom")
fit_rosp_nb <- do.call(what = fun, 
                      args = args_rosp_nb)
forecast_rosp_nb <- predict(object  = fit_rosp_nb, 
                           n.ahead = nforecast_years, 
                           newxreg = future_breed_season_depth,
                           level   = 0.95)


#### Figures ####

par(mar = c(3, 5, 3, 1))
plot(counts_whib$year, counts_whib$count,
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

points(counts_whib$year, fit_whib_p$fitted.values, type = "l", lwd = 3)

last_year   <- counts_whib$year[nrow(counts_whib)]
last_count  <- counts_whib$count[nrow(counts_whib)]
last_fitted <- fit_whib_p$fitted.values[nrow(counts_whib)]

points(c(last_year, forecast_years), c(last_fitted, forecast_whib_p$pred), type = "l", lty = 2, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, forecast_whib_p$interval[ , "upper"]), type = "l", lty = 3, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, forecast_whib_p$interval[ , "lower"]), type = "l", lty = 3, lwd = 2)



points(counts_wost$year, counts_wost$count, pch = 0, lwd = 2, cex = 1.25, col = grey(0.3))

points(counts_wost$year, fit_wost_p$fitted.values, type = "l", lwd = 3, col = grey(0.3))

last_year   <- counts_wost$year[nrow(counts_wost)]
last_count  <- counts_wost$count[nrow(counts_wost)]
last_fitted <- fit_wost_p$fitted.values[nrow(counts_wost)]

points(c(last_year, forecast_years), c(last_fitted, forecast_wost_p$pred), type = "l", lty = 2, lwd = 2, col = grey(0.3))
points(c(last_year, forecast_years), c(last_fitted, forecast_wost_p$interval[ , "upper"]), type = "l", lty = 3, lwd = 2, col = grey(0.3))
points(c(last_year, forecast_years), c(last_fitted, forecast_wost_p$interval[ , "lower"]), type = "l", lty = 3, lwd = 2, col = grey(0.3))



points(counts_greg$year, counts_greg$count, pch = 3, lwd = 2, cex = 1.25, col = grey(0.5))

points(counts_greg$year, fit_greg_p$fitted.values, type = "l", lwd = 3, col = grey(0.5))

last_year   <- counts_greg$year[nrow(counts_greg)]
last_count  <- counts_greg$count[nrow(counts_greg)]
last_fitted <- fit_greg_p$fitted.values[nrow(counts_greg)]

points(c(last_year, forecast_years), c(last_fitted, forecast_greg_p$pred), type = "l", lty = 2, lwd = 2, col = grey(0.5))
points(c(last_year, forecast_years), c(last_fitted, forecast_greg_p$interval[ , "upper"]), type = "l", lty = 3, lwd = 2, col = grey(0.5))
points(c(last_year, forecast_years), c(last_fitted, forecast_greg_p$interval[ , "lower"]), type = "l", lty = 3, lwd = 2, col = grey(0.5))



points(counts_rosp$year, counts_rosp$count, pch = 2, lwd = 2, cex = 1.25, col = grey(0.7))

points(counts_rosp$year, fit_rosp_p$fitted.values, type = "l", lwd = 3, col = grey(0.7))

last_year   <- counts_rosp$year[nrow(counts_rosp)]
last_count  <- counts_rosp$count[nrow(counts_rosp)]
last_fitted <- fit_rosp_p$fitted.values[nrow(counts_rosp)]

points(c(last_year, forecast_years), c(last_fitted, forecast_rosp_p$pred), type = "l", lty = 2, lwd = 2, col = grey(0.7))
points(c(last_year, forecast_years), c(last_fitted, forecast_rosp_p$interval[ , "upper"]), type = "l", lty = 3, lwd = 2, col = grey(0.7))
points(c(last_year, forecast_years), c(last_fitted, forecast_rosp_p$interval[ , "lower"]), type = "l", lty = 3, lwd = 2, col = grey(0.7))



text(rep(1986, 4), seq(100000, 85000, -5000), c("WHIB", "WOST", "GREG", "ROSP"), cex = 1.125, font = 2, col = c(grey(0), grey(0.3), grey(0.5), grey(0.7)), adj = 0)





par(mar = c(3, 5, 3, 1))
plot(counts_whib$year, counts_whib$count,
     cex = 1.25, lwd = 2,
     ylim = c(0, 100000),
     xlim = c(1985, 2027),
     xlab = "",
     ylab = "",
     main = "Everglades-Wide, NBinom GARCH, Breeding Water Level", las = 1, bty = "L", xaxt = "n", yaxt = "n")
axis(1, at = seq(1980, 2020, 10), cex.axis = 1.25)
axis(1, at = seq(1980, 2025, 5), labels = FALSE, tck = -0.01)
axis(1, at = seq(1980, 2027, 1), labels = FALSE, tck = -0.005)

axis(2, at = seq(0, 100000, 25000), labels = seq(0, 100000, 25000) / 1000, las = 1, cex.axis = 1.25)
axis(2, at = seq(0, 100000, 5000), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.01)
axis(2, at = seq(0, 100000, 10000), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.02)
mtext(side = 2, "Colony Count (x 1,000)", line = 3.25, cex = 1.25)

points(counts_whib$year, fit_whib_nb$fitted.values, type = "l", lwd = 3)

last_year   <- counts_whib$year[nrow(counts_whib)]
last_count  <- counts_whib$count[nrow(counts_whib)]
last_fitted <- fit_whib_nb$fitted.values[nrow(counts_whib)]

points(c(last_year, forecast_years), c(last_fitted, forecast_whib_nb$pred), type = "l", lty = 2, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, forecast_whib_nb$interval[ , "upper"]), type = "l", lty = 3, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, forecast_whib_nb$interval[ , "lower"]), type = "l", lty = 3, lwd = 2)



points(counts_wost$year, counts_wost$count, pch = 0, lwd = 2, cex = 1.25, col = grey(0.3))

points(counts_wost$year, fit_wost_nb$fitted.values, type = "l", lwd = 3, col = grey(0.3))

last_year   <- counts_wost$year[nrow(counts_wost)]
last_count  <- counts_wost$count[nrow(counts_wost)]
last_fitted <- fit_wost_nb$fitted.values[nrow(counts_wost)]

points(c(last_year, forecast_years), c(last_fitted, forecast_wost_nb$pred), type = "l", lty = 2, lwd = 2, col = grey(0.3))
points(c(last_year, forecast_years), c(last_fitted, forecast_wost_nb$interval[ , "upper"]), type = "l", lty = 3, lwd = 2, col = grey(0.3))
points(c(last_year, forecast_years), c(last_fitted, forecast_wost_nb$interval[ , "lower"]), type = "l", lty = 3, lwd = 2, col = grey(0.3))



points(counts_greg$year, counts_greg$count, pch = 3, lwd = 2, cex = 1.25, col = grey(0.5))

points(counts_greg$year, fit_greg_nb$fitted.values, type = "l", lwd = 3, col = grey(0.5))

last_year   <- counts_greg$year[nrow(counts_greg)]
last_count  <- counts_greg$count[nrow(counts_greg)]
last_fitted <- fit_greg_nb$fitted.values[nrow(counts_greg)]

points(c(last_year, forecast_years), c(last_fitted, forecast_greg_nb$pred), type = "l", lty = 2, lwd = 2, col = grey(0.5))
points(c(last_year, forecast_years), c(last_fitted, forecast_greg_nb$interval[ , "upper"]), type = "l", lty = 3, lwd = 2, col = grey(0.5))
points(c(last_year, forecast_years), c(last_fitted, forecast_greg_nb$interval[ , "lower"]), type = "l", lty = 3, lwd = 2, col = grey(0.5))



points(counts_rosp$year, counts_rosp$count, pch = 2, lwd = 2, cex = 1.25, col = grey(0.7))

points(counts_rosp$year, fit_rosp_nb$fitted.values, type = "l", lwd = 3, col = grey(0.7))

last_year   <- counts_rosp$year[nrow(counts_rosp)]
last_count  <- counts_rosp$count[nrow(counts_rosp)]
last_fitted <- fit_rosp_nb$fitted.values[nrow(counts_rosp)]

points(c(last_year, forecast_years), c(last_fitted, forecast_rosp_nb$pred), type = "l", lty = 2, lwd = 2, col = grey(0.7))
points(c(last_year, forecast_years), c(last_fitted, forecast_rosp_nb$interval[ , "upper"]), type = "l", lty = 3, lwd = 2, col = grey(0.7))
points(c(last_year, forecast_years), c(last_fitted, forecast_rosp_nb$interval[ , "lower"]), type = "l", lty = 3, lwd = 2, col = grey(0.7))



text(rep(1986, 4), seq(100000, 85000, -5000), c("WHIB", "WOST", "GREG", "ROSP"), cex = 1.125, font = 2, col = c(grey(0), grey(0.3), grey(0.5), grey(0.7)), adj = 0)


 

# without WHIB



par(mar = c(3, 5, 3, 1))
plot(counts_whib$year, counts_whib$count,
     cex = 1.25, lwd = 2,
     ylim = c(0, 20000),
     xlim = c(1985, 2027),
     xlab = "",
     ylab = "",
     main = "Everglades-Wide, Poisson GARCH, Breeding Water Level", las = 1, type = "n", bty = "L", xaxt = "n", yaxt = "n")
axis(1, at = seq(1980, 2020, 10), cex.axis = 1.25)
axis(1, at = seq(1980, 2025, 5), labels = FALSE, tck = -0.01)
axis(1, at = seq(1980, 2027, 1), labels = FALSE, tck = -0.005)


axis(2, at = seq(0, 20000, 5000), labels = seq(0, 20000, 5000) / 1000, las = 1, cex.axis = 1.25)
axis(2, at = seq(0, 20000, 1000), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.01)
mtext(side = 2, "Colony Count (x 1,000)", line = 3.25, cex = 1.25)




points(counts_wost$year, counts_wost$count, pch = 0, lwd = 2, cex = 1.25, col = grey(0.3))

points(counts_wost$year, fit_wost_p$fitted.values, type = "l", lwd = 3, col = grey(0.3))

last_year   <- counts_wost$year[nrow(counts_wost)]
last_count  <- counts_wost$count[nrow(counts_wost)]
last_fitted <- fit_wost_p$fitted.values[nrow(counts_wost)]

points(c(last_year, forecast_years), c(last_fitted, forecast_wost_p$pred), type = "l", lty = 2, lwd = 2, col = grey(0.3))
points(c(last_year, forecast_years), c(last_fitted, forecast_wost_p$interval[ , "upper"]), type = "l", lty = 3, lwd = 2, col = grey(0.3))
points(c(last_year, forecast_years), c(last_fitted, forecast_wost_p$interval[ , "lower"]), type = "l", lty = 3, lwd = 2, col = grey(0.3))



points(counts_greg$year, counts_greg$count, pch = 3, lwd = 2, cex = 1.25, col = grey(0.5))

points(counts_greg$year, fit_greg_p$fitted.values, type = "l", lwd = 3, col = grey(0.5))

last_year   <- counts_greg$year[nrow(counts_greg)]
last_count  <- counts_greg$count[nrow(counts_greg)]
last_fitted <- fit_greg_p$fitted.values[nrow(counts_greg)]

points(c(last_year, forecast_years), c(last_fitted, forecast_greg_p$pred), type = "l", lty = 2, lwd = 2, col = grey(0.5))
points(c(last_year, forecast_years), c(last_fitted, forecast_greg_p$interval[ , "upper"]), type = "l", lty = 3, lwd = 2, col = grey(0.5))
points(c(last_year, forecast_years), c(last_fitted, forecast_greg_p$interval[ , "lower"]), type = "l", lty = 3, lwd = 2, col = grey(0.5))



points(counts_rosp$year, counts_rosp$count, pch = 2, lwd = 2, cex = 1.25, col = grey(0.7))

points(counts_rosp$year, fit_rosp_p$fitted.values, type = "l", lwd = 3, col = grey(0.7))

last_year   <- counts_rosp$year[nrow(counts_rosp)]
last_count  <- counts_rosp$count[nrow(counts_rosp)]
last_fitted <- fit_rosp_p$fitted.values[nrow(counts_rosp)]

points(c(last_year, forecast_years), c(last_fitted, forecast_rosp_p$pred), type = "l", lty = 2, lwd = 2, col = grey(0.7))
points(c(last_year, forecast_years), c(last_fitted, forecast_rosp_p$interval[ , "upper"]), type = "l", lty = 3, lwd = 2, col = grey(0.7))
points(c(last_year, forecast_years), c(last_fitted, forecast_rosp_p$interval[ , "lower"]), type = "l", lty = 3, lwd = 2, col = grey(0.7))



text(rep(1986, 3), seq(20000, 18000, -1000), c("WOST", "GREG", "ROSP"), cex = 1.125, font = 2, col = c(grey(0.3), grey(0.5), grey(0.7)), adj = 0)










par(mar = c(3, 5, 3, 1))
plot(counts_whib$year, counts_whib$count,
     cex = 1.25, lwd = 2,
     ylim = c(0, 20000),
     xlim = c(1985, 2027),
     xlab = "",
     ylab = "",
     main = "Everglades-Wide, NBinom GARCH, Breeding Water Level", las = 1, bty = "L", type = "n", xaxt = "n", yaxt = "n")
axis(1, at = seq(1980, 2020, 10), cex.axis = 1.25)
axis(1, at = seq(1980, 2025, 5), labels = FALSE, tck = -0.01)
axis(1, at = seq(1980, 2027, 1), labels = FALSE, tck = -0.005)

axis(2, at = seq(0, 20000, 5000), labels = seq(0, 20000, 5000) / 1000, las = 1, cex.axis = 1.25)
axis(2, at = seq(0, 20000, 1000), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.01)
mtext(side = 2, "Colony Count (x 1,000)", line = 3.25, cex = 1.25)



points(counts_wost$year, counts_wost$count, pch = 0, lwd = 2, cex = 1.25, col = grey(0.3))

points(counts_wost$year, fit_wost_nb$fitted.values, type = "l", lwd = 3, col = grey(0.3))

last_year   <- counts_wost$year[nrow(counts_wost)]
last_count  <- counts_wost$count[nrow(counts_wost)]
last_fitted <- fit_wost_nb$fitted.values[nrow(counts_wost)]

points(c(last_year, forecast_years), c(last_fitted, forecast_wost_nb$pred), type = "l", lty = 2, lwd = 2, col = grey(0.3))
points(c(last_year, forecast_years), c(last_fitted, forecast_wost_nb$interval[ , "upper"]), type = "l", lty = 3, lwd = 2, col = grey(0.3))
points(c(last_year, forecast_years), c(last_fitted, forecast_wost_nb$interval[ , "lower"]), type = "l", lty = 3, lwd = 2, col = grey(0.3))



points(counts_greg$year, counts_greg$count, pch = 2, lwd = 2, cex = 1.25, col = grey(0.5))

points(counts_greg$year, fit_greg_nb$fitted.values, type = "l", lwd = 3, col = grey(0.5))

last_year   <- counts_greg$year[nrow(counts_greg)]
last_count  <- counts_greg$count[nrow(counts_greg)]
last_fitted <- fit_greg_nb$fitted.values[nrow(counts_greg)]

points(c(last_year, forecast_years), c(last_fitted, forecast_greg_nb$pred), type = "l", lty = 2, lwd = 2, col = grey(0.5))
points(c(last_year, forecast_years), c(last_fitted, forecast_greg_nb$interval[ , "upper"]), type = "l", lty = 3, lwd = 2, col = grey(0.5))
points(c(last_year, forecast_years), c(last_fitted, forecast_greg_nb$interval[ , "lower"]), type = "l", lty = 3, lwd = 2, col = grey(0.5))



points(counts_rosp$year, counts_rosp$count, pch = 3, lwd = 2, cex = 1.25, col = grey(0.7))

points(counts_rosp$year, fit_rosp_nb$fitted.values, type = "l", lwd = 3, col = grey(0.7))

last_year   <- counts_rosp$year[nrow(counts_rosp)]
last_count  <- counts_rosp$count[nrow(counts_rosp)]
last_fitted <- fit_rosp_nb$fitted.values[nrow(counts_rosp)]

points(c(last_year, forecast_years), c(last_fitted, forecast_rosp_nb$pred), type = "l", lty = 2, lwd = 2, col = grey(0.7))
points(c(last_year, forecast_years), c(last_fitted, forecast_rosp_nb$interval[ , "upper"]), type = "l", lty = 3, lwd = 2, col = grey(0.7))
points(c(last_year, forecast_years), c(last_fitted, forecast_rosp_nb$interval[ , "lower"]), type = "l", lty = 3, lwd = 2, col = grey(0.7))



text(rep(1986, 3), seq(20000, 18000, -1000), c("WOST", "GREG", "ROSP"), cex = 1.125, font = 2, col = c(grey(0.3), grey(0.5), grey(0.7)), adj = 0)










