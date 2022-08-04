# currently won't work to fully establish the datasets, given that 3A covariates and counts are not available at that level specifically
# the dataset controls are written for them, with only partial implementation completed. not sure what form the data will come so holding.

devtools::document()

main <- "~/testing"
setup_dir(main = main)

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

greg_train <- data.frame(counts_greg[counts_greg$train, ], covariates[covariates$train, ])
wost_train <- data.frame(counts_wost[counts_wost$train, ], covariates[covariates$train, ])
whib_train <- data.frame(counts_whib[counts_whib$train, ], covariates[covariates$train, ])
rosp_train <- data.frame(counts_rosp[counts_rosp$train, ], covariates[covariates$train, ])

greg_test <- data.frame(count = c(counts_greg$count[!counts_greg$train], NA), covariates[!covariates$train, ])
wost_test <- data.frame(count = c(counts_wost$count[!counts_wost$train], NA), covariates[!covariates$train, ])
whib_test <- data.frame(count = c(counts_whib$count[!counts_whib$train], NA), covariates[!covariates$train, ])
rosp_test <- data.frame(count = c(counts_rosp$count[!counts_rosp$train], NA), covariates[!covariates$train, ])



# Poisson

fun  <- glm
args_greg_p <- list(formula  = count ~ year + reversals,
                    data     = greg_train,
                    family   = "poisson")
fit_greg_p <- do.call(what = fun, 
                      args = args_greg_p)
forecast_greg_p <- predict(object  = fit_greg_p, 
                           newdata = greg_test,
                           se.fit  = TRUE,
                           type    = "link", dispersion = sd(fit_greg_p$residuals))
 

fun  <- glm
args_whib_p <- list(formula  = count ~ year + reversals,
                    data     = whib_train,
                    family   = "poisson")
fit_whib_p <- do.call(what = fun, 
                      args = args_whib_p)
forecast_whib_p <- predict(object  = fit_whib_p, 
                           newdata = whib_test,
                           se.fit  = TRUE,
                           type    = "link", dispersion = sd(fit_whib_p$residuals))


fun  <- glm
args_wost_p <- list(formula  = count ~ year + init_depth + init_depth2 + pre_recession + post_recession,
                    data     = wost_train,
                    family   = "poisson")
fit_wost_p <- do.call(what = fun, 
                      args = args_wost_p)
forecast_wost_p <- predict(object  = fit_wost_p, 
                           newdata = wost_test,
                           se.fit  = TRUE,
                           type    = "link", dispersion = sd(fit_wost_p$residuals))


# N Binom
glm.nb <- MASS::glm.nb


fun  <- glm.nb
args_greg_nb <- list(formula  = count ~ year + reversals,
                    data     = greg_train)
fit_greg_nb <- do.call(what = fun, 
                      args = args_greg_nb)
forecast_greg_nb <- predict(object  = fit_greg_nb, 
                           newdata = greg_test,
                           se.fit  = TRUE,
                           type    = "link", dispersion = sd(fit_greg_nb$residuals))
 

fun  <- glm.nb
args_whib_nb <- list(formula  = count ~ year + reversals,
                    data     = whib_train)
fit_whib_nb <- do.call(what = fun, 
                      args = args_whib_nb)
forecast_whib_nb <- predict(object  = fit_whib_nb, 
                           newdata = whib_test,
                           se.fit  = TRUE,
                           type    = "link", dispersion = sd(fit_whib_nb$residuals))


fun  <- glm.nb
args_wost_nb <- list(formula  = count ~ year + init_depth + init_depth2 + pre_recession + post_recession,
                    data     = wost_train)
fit_wost_nb <- do.call(what = fun, 
                      args = args_wost_nb)
forecast_wost_nb <- predict(object  = fit_wost_nb, 
                           newdata = wost_test,
                           se.fit  = TRUE,
                           type    = "link", dispersion = sd(fit_wost_nb$residuals))




#### Figures ####
par(mfrow = c(2, 3))
par(mar = c(3, 5, 3, 1))
plot(counts_whib$year, counts_whib$count,
     cex = 1.25, lwd = 2,
     ylim = c(0, 100000),
     xlim = c(1985, 2027),
     xlab = "",
     ylab = "",
     main = "Everglades-Wide, Poisson GLM, WHIB", las = 1, bty = "L", xaxt = "n", yaxt = "n")
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


points(c(last_year, forecast_years), c(last_fitted, exp(forecast_whib_p$fit)), type = "l", lty = 2, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, exp(forecast_whib_p$fit + 1.96 * forecast_whib_p$se.fit)), type = "l", lty = 3, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, exp(forecast_whib_p$fit - 1.96 * forecast_whib_p$se.fit)), type = "l", lty = 3, lwd = 2)



par(mar = c(3, 5, 3, 1))
plot(counts_wost$year, counts_wost$count,
     cex = 1.25, lwd = 2,
     ylim = c(0, 5000),
     xlim = c(1985, 2027),
     xlab = "",
     ylab = "",
     main = "Everglades-Wide, Poisson GLM, WOST", las = 1, bty = "L", xaxt = "n", yaxt = "n")
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


points(c(last_year, forecast_years), c(last_fitted, exp(forecast_wost_p$fit)), type = "l", lty = 2, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, exp(forecast_wost_p$fit + 1.96 * forecast_wost_p$se.fit)), type = "l", lty = 3, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, exp(forecast_wost_p$fit - 1.96 * forecast_wost_p$se.fit)), type = "l", lty = 3, lwd = 2)




par(mar = c(3, 5, 3, 1))
plot(counts_greg$year, counts_greg$count,
     cex = 1.25, lwd = 2,
     ylim = c(0, 15000),
     xlim = c(1985, 2027),
     xlab = "",
     ylab = "",
     main = "Everglades-Wide, Poisson GLM, GREG", las = 1, bty = "L", xaxt = "n", yaxt = "n")
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

points(c(last_year, forecast_years), c(last_fitted, exp(forecast_greg_p$fit)), type = "l", lty = 2, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, exp(forecast_greg_p$fit + 1.96 * forecast_greg_p$se.fit)), type = "l", lty = 3, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, exp(forecast_greg_p$fit - 1.96 * forecast_greg_p$se.fit)), type = "l", lty = 3, lwd = 2)





par(mar = c(3, 5, 3, 1))
plot(counts_whib$year, counts_whib$count,
     cex = 1.25, lwd = 2,
     ylim = c(0, 100000),
     xlim = c(1985, 2027),
     xlab = "",
     ylab = "",
     main = "Everglades-Wide, N. Binomial GLM, WHIB", las = 1, bty = "L", xaxt = "n", yaxt = "n")
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


points(c(last_year, forecast_years), c(last_fitted, exp(forecast_whib_nb$fit)), type = "l", lty = 2, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, exp(forecast_whib_nb$fit + 1.96 * forecast_whib_nb$se.fit)), type = "l", lty = 3, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, exp(forecast_whib_nb$fit - 1.96 * forecast_whib_nb$se.fit)), type = "l", lty = 3, lwd = 2)




par(mar = c(3, 5, 3, 1))
plot(counts_wost$year, counts_wost$count,
     cex = 1.25, lwd = 2,
     ylim = c(0, 5000),
     xlim = c(1985, 2027),
     xlab = "",
     ylab = "",
     main = "Everglades-Wide, N. Binomial GLM, WOST", las = 1, bty = "L", xaxt = "n", yaxt = "n")
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


points(c(last_year, forecast_years), c(last_fitted, exp(forecast_wost_nb$fit)), type = "l", lty = 2, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, exp(forecast_wost_nb$fit + 1.96 * forecast_wost_nb$se.fit)), type = "l", lty = 3, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, exp(forecast_wost_nb$fit - 1.96 * forecast_wost_nb$se.fit)), type = "l", lty = 3, lwd = 2)




par(mar = c(3, 5, 3, 1))
plot(counts_greg$year, counts_greg$count,
     cex = 1.25, lwd = 2,
     ylim = c(0, 15000),
     xlim = c(1985, 2027),
     xlab = "",
     ylab = "",
     main = "Everglades-Wide, N. Binomial GLM, GREG", las = 1, bty = "L", xaxt = "n", yaxt = "n")
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


points(c(last_year, forecast_years), c(last_fitted, exp(forecast_greg_nb$fit)), type = "l", lty = 2, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, exp(forecast_greg_nb$fit + 1.96 * forecast_greg_nb$se.fit)), type = "l", lty = 3, lwd = 2)
points(c(last_year, forecast_years), c(last_fitted, exp(forecast_greg_nb$fit - 1.96 * forecast_greg_nb$se.fit)), type = "l", lty = 3, lwd = 2)






