# currently won't work to fully establish the datasets, given that 3A covariates and counts are not available at that level specifically
# the dataset controls are written for them, with only partial implementation completed. not sure what form the data will come so holding.

devtools::document()

main <- "~/testing"
#setup_dir(main = main)

#fill_data(main)

counts_greg <- read_counts(main, "all_GREG")
counts_wost <- read_counts(main, "all_WOST")
counts_whib <- read_counts(main, "all_WHIB")
counts_rosp <- read_counts(main, "all_ROSP")

covariates <- read_covariates(main)
covariates$init_depth2 <- covariates$init_depth ^ 2

head(counts_greg)
head(counts_wost)
head(counts_whib)
head(counts_rosp)
head(covariates)

train_years <- 1991:2019
forecast_years <- 2020:2022
nforecast_years <- length(forecast_years)

counts_greg$train <- c(rep(TRUE, 29), rep(FALSE, 2))
counts_wost$train <- c(rep(TRUE, 29), rep(FALSE, 2))
counts_whib$train <- c(rep(TRUE, 29), rep(FALSE, 2))
counts_rosp$train <- c(rep(TRUE, 29), rep(FALSE, 2))

covariates$train <- c(rep(TRUE, 29), rep(FALSE, 3))



init_depth         <- covariates$init_depth
init_depth2        <- covariates$init_depth2
reversals          <- covariates$reversals
pre_recession      <- covariates$pre_recession
post_recession     <- covariates$post_recession

covariates_wost <- data.frame(init_depth, init_depth2, pre_recession, post_recession)

plot(data.frame(init_depth, init_depth2, reversals, pre_recession, post_recession))
cor(data.frame(init_depth, init_depth2, reversals, pre_recession, post_recession))



# Poisson

fun  <- tsglm
args_greg_p <- list(ts    = counts_greg$count[counts_greg$train],
                    model = list(past_obs  = 1,
                                 past_mean = NULL),
                    xreg  = reversals[covariates$train],
                    link  = "log",
                    distr = "poisson")
fit_greg_p <- do.call(what = fun, 
                      args = args_greg_p)
forecast_greg_p <- predict(object  = fit_greg_p, 
                           n.ahead = nforecast_years, 
                           newxreg = reversals[!covariates$train],
                           level   = 0.95)



fun  <- tsglm
args_whib_p <- list(ts    = counts_whib$count[counts_whib$train],
                    model = list(past_obs  = 1,
                                 past_mean = NULL),
                    xreg  = reversals[covariates$train],
                    link  = "log",
                    distr = "poisson")
fit_whib_p <- do.call(what = fun, 
                      args = args_whib_p)
forecast_whib_p <- predict(object  = fit_whib_p, 
                           n.ahead = nforecast_years, 
                           newxreg = reversals[!covariates$train],
                           level   = 0.95)


fun  <- tsglm
args_wost_p <- list(ts    = counts_wost$count[counts_wost$train],
                    model = list(past_obs  = 1,
                                 past_mean = NULL),
                    xreg  = covariates_wost[covariates$train, ],
                    link  = "log",
                    distr = "poisson")
fit_wost_p <- do.call(what = fun, 
                      args = args_wost_p)
forecast_wost_p <- predict(object  = fit_wost_p, 
                           n.ahead = nforecast_years, 
                           newxreg = covariates_wost[!covariates$train, ],
                           level   = 0.95)

# N Binom

fun  <- tsglm
args_greg_nb <- list(ts    = counts_greg$count[counts_greg$train],
                    model = list(past_obs  = 1,
                                 past_mean = NULL),
                    xreg  = reversals[covariates$train],
                    link  = "log",
                    distr = "nbinom")
fit_greg_nb <- do.call(what = fun, 
                      args = args_greg_nb)
forecast_greg_nb <- predict(object  = fit_greg_nb, 
                           n.ahead = nforecast_years, 
                           newxreg = reversals[!covariates$train],
                           level   = 0.95)



fun  <- tsglm
args_whib_nb <- list(ts    = counts_whib$count[counts_whib$train],
                    model = list(past_obs  = 1,
                                 past_mean = NULL),
                    xreg  = reversals[covariates$train],
                    link  = "log",
                    distr = "nbinom")
fit_whib_nb <- do.call(what = fun, 
                      args = args_whib_nb)
forecast_whib_nb <- predict(object  = fit_whib_nb, 
                           n.ahead = nforecast_years, 
                           newxreg = reversals[!covariates$train],
                           level   = 0.95)


fun  <- tsglm
args_wost_nb <- list(ts    = counts_wost$count[counts_wost$train],
                    model = list(past_obs  = 1,
                                 past_mean = NULL),
                    xreg  = covariates_wost[covariates$train, ],
                    link  = "log",
                    distr = "nbinom")
fit_wost_nb <- do.call(what = fun, 
                      args = args_wost_nb)
forecast_wost_nb <- predict(object  = fit_wost_nb, 
                           n.ahead = nforecast_years, 
                           newxreg = covariates_wost[!covariates$train, ],
                           level   = 0.95)




#### Figures ####
par(mfrow = c(2, 3))
par(mar = c(3, 5, 3, 1))
plot(counts_whib$year, counts_whib$count,
     cex = 1.25, lwd = 2,
     ylim = c(0, 100000),
     xlim = c(1985, 2027),
     xlab = "",
     ylab = "",
     main = "Everglades-Wide, Poisson GARCH, WHIB", las = 1, bty = "L", xaxt = "n", yaxt = "n")
axis(1, at = seq(1980, 2020, 10), cex.axis = 1.25)
axis(1, at = seq(1980, 2025, 5), labels = FALSE, tck = -0.01)
axis(1, at = seq(1980, 2027, 1), labels = FALSE, tck = -0.005)

axis(2, at = seq(0, 100000, 25000), labels = seq(0, 100000, 25000) / 1000, las = 1, cex.axis = 1.25)
axis(2, at = seq(0, 100000, 5000), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.01)
axis(2, at = seq(0, 100000, 10000), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.02)
mtext(side = 2, "Colony Count (x 1,000)", line = 3.25, cex = 1.25)

points(counts_whib$year[counts_whib$train], fit_whib_p$fitted.values, type = "l", lwd = 3)

last_year   <- counts_whib$year[nrow(counts_whib[counts_whib$train, ])]
last_count  <- counts_whib$count[nrow(counts_whib[counts_whib$train, ])]
last_fitted <- fit_whib_p$fitted.values[nrow(counts_whib[counts_whib$train, ])]

points(c(last_year, forecast_years), c(last_fitted, forecast_whib_p$pred), type = "l", lty = 2, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, forecast_whib_p$interval[ , "upper"]), type = "l", lty = 3, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, forecast_whib_p$interval[ , "lower"]), type = "l", lty = 3, lwd = 2)



par(mar = c(3, 5, 3, 1))
plot(counts_wost$year, counts_wost$count,
     cex = 1.25, lwd = 2,
     ylim = c(0, 5000),
     xlim = c(1985, 2027),
     xlab = "",
     ylab = "",
     main = "Everglades-Wide, Poisson GARCH, WOST", las = 1, bty = "L", xaxt = "n", yaxt = "n")
axis(1, at = seq(1980, 2020, 10), cex.axis = 1.25)
axis(1, at = seq(1980, 2025, 5), labels = FALSE, tck = -0.01)
axis(1, at = seq(1980, 2027, 1), labels = FALSE, tck = -0.005)

axis(2, at = seq(0, 5000, 1000), labels = seq(0, 5000, 1000) / 1000, las = 1, cex.axis = 1.25)
axis(2, at = seq(0, 5000, 500), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.01)
axis(2, at = seq(0, 5000, 100), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.005)
mtext(side = 2, "Colony Count (x 1,000)", line = 3.25, cex = 1.25)


points(counts_wost$year, counts_wost$count, lwd = 2, cex = 1.25)

points(counts_wost$year[counts_wost$train], fit_wost_p$fitted.values, type = "l", lwd = 3)


last_year   <- counts_wost$year[nrow(counts_wost[counts_wost$train, ])]
last_count  <- counts_wost$count[nrow(counts_wost[counts_wost$train, ])]
last_fitted <- fit_wost_p$fitted.values[nrow(counts_wost[counts_wost$train, ])]

points(c(last_year, forecast_years), c(last_fitted, forecast_wost_p$pred), type = "l", lty = 2, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, forecast_wost_p$interval[ , "upper"]), type = "l", lty = 3, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, forecast_wost_p$interval[ , "lower"]), type = "l", lty = 3, lwd = 2)




par(mar = c(3, 5, 3, 1))
plot(counts_greg$year, counts_greg$count,
     cex = 1.25, lwd = 2,
     ylim = c(0, 15000),
     xlim = c(1985, 2027),
     xlab = "",
     ylab = "",
     main = "Everglades-Wide, Poisson GARCH, GREG", las = 1, bty = "L", xaxt = "n", yaxt = "n")
axis(1, at = seq(1980, 2020, 10), cex.axis = 1.25)
axis(1, at = seq(1980, 2025, 5), labels = FALSE, tck = -0.01)
axis(1, at = seq(1980, 2027, 1), labels = FALSE, tck = -0.005)

axis(2, at = seq(0, 15000, 5000), labels = seq(0, 15000, 5000) / 1000, las = 1, cex.axis = 1.25)
axis(2, at = seq(0, 15000, 1000), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.01)

mtext(side = 2, "Colony Count (x 1,000)", line = 3.25, cex = 1.25)


points(counts_greg$year, counts_greg$count, lwd = 2, cex = 1.25)

points(counts_greg$year[counts_greg$train], fit_greg_p$fitted.values, type = "l", lwd = 3)


last_year   <- counts_greg$year[nrow(counts_greg[counts_greg$train, ])]
last_count  <- counts_greg$count[nrow(counts_greg[counts_greg$train, ])]
last_fitted <- fit_greg_p$fitted.values[nrow(counts_greg[counts_greg$train, ])]

points(c(last_year, forecast_years), c(last_fitted, forecast_greg_p$pred), type = "l", lty = 2, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, forecast_greg_p$interval[ , "upper"]), type = "l", lty = 3, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, forecast_greg_p$interval[ , "lower"]), type = "l", lty = 3, lwd = 2)





par(mar = c(3, 5, 3, 1))
plot(counts_whib$year, counts_whib$count,
     cex = 1.25, lwd = 2,
     ylim = c(0, 100000),
     xlim = c(1985, 2027),
     xlab = "",
     ylab = "",
     main = "Everglades-Wide, N. Binomial GARCH, WHIB", las = 1, bty = "L", xaxt = "n", yaxt = "n")
axis(1, at = seq(1980, 2020, 10), cex.axis = 1.25)
axis(1, at = seq(1980, 2025, 5), labels = FALSE, tck = -0.01)
axis(1, at = seq(1980, 2027, 1), labels = FALSE, tck = -0.005)

axis(2, at = seq(0, 100000, 25000), labels = seq(0, 100000, 25000) / 1000, las = 1, cex.axis = 1.25)
axis(2, at = seq(0, 100000, 5000), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.01)
axis(2, at = seq(0, 100000, 10000), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.02)
mtext(side = 2, "Colony Count (x 1,000)", line = 3.25, cex = 1.25)

points(counts_whib$year[counts_whib$train], fit_whib_nb$fitted.values, type = "l", lwd = 3)

last_year   <- counts_whib$year[nrow(counts_whib[counts_whib$train, ])]
last_count  <- counts_whib$count[nrow(counts_whib[counts_whib$train, ])]
last_fitted <- fit_whib_nb$fitted.values[nrow(counts_whib[counts_whib$train, ])]

points(c(last_year, forecast_years), c(last_fitted, forecast_whib_nb$pred), type = "l", lty = 2, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, forecast_whib_nb$interval[ , "upper"]), type = "l", lty = 3, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, forecast_whib_nb$interval[ , "lower"]), type = "l", lty = 3, lwd = 2)



par(mar = c(3, 5, 3, 1))
plot(counts_wost$year, counts_wost$count,
     cex = 1.25, lwd = 2,
     ylim = c(0, 5000),
     xlim = c(1985, 2027),
     xlab = "",
     ylab = "",
     main = "Everglades-Wide, N. Binomial GARCH, WOST", las = 1, bty = "L", xaxt = "n", yaxt = "n")
axis(1, at = seq(1980, 2020, 10), cex.axis = 1.25)
axis(1, at = seq(1980, 2025, 5), labels = FALSE, tck = -0.01)
axis(1, at = seq(1980, 2027, 1), labels = FALSE, tck = -0.005)

axis(2, at = seq(0, 5000, 1000), labels = seq(0, 5000, 1000) / 1000, las = 1, cex.axis = 1.25)
axis(2, at = seq(0, 5000, 500), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.01)
axis(2, at = seq(0, 5000, 100), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.005)
mtext(side = 2, "Colony Count (x 1,000)", line = 3.25, cex = 1.25)


points(counts_wost$year, counts_wost$count, lwd = 2, cex = 1.25)

points(counts_wost$year[counts_wost$train], fit_wost_nb$fitted.values, type = "l", lwd = 3)


last_year   <- counts_wost$year[nrow(counts_wost[counts_wost$train, ])]
last_count  <- counts_wost$count[nrow(counts_wost[counts_wost$train, ])]
last_fitted <- fit_wost_nb$fitted.values[nrow(counts_wost[counts_wost$train, ])]

points(c(last_year, forecast_years), c(last_fitted, forecast_wost_nb$pred), type = "l", lty = 2, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, forecast_wost_nb$interval[ , "upper"]), type = "l", lty = 3, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, forecast_wost_nb$interval[ , "lower"]), type = "l", lty = 3, lwd = 2)




par(mar = c(3, 5, 3, 1))
plot(counts_greg$year, counts_greg$count,
     cex = 1.25, lwd = 2,
     ylim = c(0, 15000),
     xlim = c(1985, 2027),
     xlab = "",
     ylab = "",
     main = "Everglades-Wide, N. Binomial GARCH, GREG", las = 1, bty = "L", xaxt = "n", yaxt = "n")
axis(1, at = seq(1980, 2020, 10), cex.axis = 1.25)
axis(1, at = seq(1980, 2025, 5), labels = FALSE, tck = -0.01)
axis(1, at = seq(1980, 2027, 1), labels = FALSE, tck = -0.005)

axis(2, at = seq(0, 15000, 5000), labels = seq(0, 15000, 5000) / 1000, las = 1, cex.axis = 1.25)
axis(2, at = seq(0, 15000, 1000), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.01)

mtext(side = 2, "Colony Count (x 1,000)", line = 3.25, cex = 1.25)


points(counts_greg$year, counts_greg$count, lwd = 2, cex = 1.25)

points(counts_greg$year[counts_greg$train], fit_greg_nb$fitted.values, type = "l", lwd = 3)


last_year   <- counts_greg$year[nrow(counts_greg[counts_greg$train, ])]
last_count  <- counts_greg$count[nrow(counts_greg[counts_greg$train, ])]
last_fitted <- fit_greg_nb$fitted.values[nrow(counts_greg[counts_greg$train, ])]

points(c(last_year, forecast_years), c(last_fitted, forecast_greg_nb$pred), type = "l", lty = 2, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, forecast_greg_nb$interval[ , "upper"]), type = "l", lty = 3, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, forecast_greg_nb$interval[ , "lower"]), type = "l", lty = 3, lwd = 2)







