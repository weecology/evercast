devtools::document()

main <- "./testing"
setup_dir(main = main)

fill_data(main)

counts_greg <- read_counts(main, "all_GREG")
counts_wost <- read_counts(main, "all_WOST")
counts_whib <- read_counts(main, "all_WHIB")
counts_rosp <- read_counts(main, "all_ROSP")


covariates <- read_covariates(main)
covariates$init_depth2 <- covariates$init_depth ^ 2

init_depth         <- covariates$init_depth
init_depth2        <- covariates$init_depth2
reversals          <- covariates$reversals
pre_recession      <- covariates$pre_recession
post_recession     <- covariates$post_recession

origins  <- 2000:2016
norigins <- length(origins)
horizon  <- 5
nsample  <- 1e5

fit_greg_p      <- vector("list", norigins)
forecast_greg_p <- vector("list", norigins)
score_greg_p    <- vector("list", norigins)
draws_greg_p    <- vector("list", norigins)
draws_greg_p_lim  <- vector("list", norigins)

fit_wost_p      <- vector("list", norigins)
forecast_wost_p <- vector("list", norigins)
score_wost_p    <- vector("list", norigins)
draws_wost_p    <- vector("list", norigins)
draws_wost_p_lim  <- vector("list", norigins)

fit_whib_p      <- vector("list", norigins)
forecast_whib_p <- vector("list", norigins)
score_whib_p    <- vector("list", norigins)
draws_whib_p    <- vector("list", norigins)
draws_whib_p_lim  <- vector("list", norigins)

fit_rosp_p      <- vector("list", norigins)
forecast_rosp_p <- vector("list", norigins)
score_rosp_p    <- vector("list", norigins)
draws_rosp_p    <- vector("list", norigins)
draws_rosp_p_lim  <- vector("list", norigins)


fit_greg_nb      <- vector("list", norigins)
forecast_greg_nb <- vector("list", norigins)
score_greg_nb    <- vector("list", norigins)
draws_greg_nb   <- vector("list", norigins)
draws_greg_nb_lim  <- vector("list", norigins)

fit_wost_nb      <- vector("list", norigins)
forecast_wost_nb <- vector("list", norigins)
score_wost_nb    <- vector("list", norigins)
draws_wost_nb   <- vector("list", norigins)
draws_wost_nb_lim  <- vector("list", norigins)

fit_whib_nb      <- vector("list", norigins)
forecast_whib_nb <- vector("list", norigins)
score_whib_nb    <- vector("list", norigins)
draws_whib_nb   <- vector("list", norigins)
draws_whib_nb_lim  <- vector("list", norigins)

fit_rosp_nb      <- vector("list", norigins)
forecast_rosp_nb <- vector("list", norigins)
score_rosp_nb    <- vector("list", norigins)
draws_rosp_nb   <- vector("list", norigins)
draws_rosp_nb_lim  <- vector("list", norigins)


summary <- data.frame(species  = NULL,
                      model    = NULL,
                      origin   = NULL,
                      lead     = NULL,
                      target   = NULL,
                      true_y   = NULL,
                      pred_y   = NULL,
                      sd_pred  = NULL,
                      logs     = NULL,
                      logs_lim = NULL,
                      crps     = NULL)


cols <- c(greg = rgb(33, 23, 144, maxColorValue = 256), 
          wost = rgb(192, 58, 131, maxColorValue = 256),
          whib = rgb(129, 33, 167, maxColorValue = 256),
          rosp = rgb(233, 112, 88, maxColorValue = 256))

for (origin in 1:norigins) {

  message("modeling origin ", origins[origin], " ...")

  train_years  <- 1991:origins[origin]
  test_years   <- (origins[origin] + 1):(origins[origin] + horizon)
  ntrain_years <- length(train_years)
  ntest_years  <- length(test_years)

  counts_greg$train <- c(rep(TRUE, ntrain_years), rep(FALSE, nrow(counts_greg) - ntrain_years))
  counts_wost$train <- c(rep(TRUE, ntrain_years), rep(FALSE, nrow(counts_wost) - ntrain_years))
  counts_whib$train <- c(rep(TRUE, ntrain_years), rep(FALSE, nrow(counts_whib) - ntrain_years))
  counts_rosp$train <- c(rep(TRUE, ntrain_years), rep(FALSE, nrow(counts_rosp) - ntrain_years))

  covariates$train  <- c(rep(TRUE, ntrain_years), rep(FALSE, nrow(covariates) - ntrain_years))

  counts_greg$test <- c(rep(FALSE, ntrain_years), rep(TRUE, ntest_years), rep(FALSE, nrow(counts_greg) - ntrain_years - ntest_years))
  counts_wost$test <- c(rep(FALSE, ntrain_years), rep(TRUE, ntest_years), rep(FALSE, nrow(counts_wost) - ntrain_years - ntest_years))
  counts_whib$test <- c(rep(FALSE, ntrain_years), rep(TRUE, ntest_years), rep(FALSE, nrow(counts_whib) - ntrain_years - ntest_years))
  counts_rosp$test <- c(rep(FALSE, ntrain_years), rep(TRUE, ntest_years), rep(FALSE, nrow(counts_rosp) - ntrain_years - ntest_years))

  covariates$test  <- c(rep(FALSE, ntrain_years), rep(TRUE, ntest_years), rep(FALSE, nrow(covariates) - ntrain_years - ntest_years))


  greg_train <- data.frame(counts_greg[counts_greg$train, ], covariates[covariates$train, ])
  wost_train <- data.frame(counts_wost[counts_wost$train, ], covariates[covariates$train, ])
  whib_train <- data.frame(counts_whib[counts_whib$train, ], covariates[covariates$train, ])
  rosp_train <- data.frame(counts_rosp[counts_rosp$train, ], covariates[covariates$train, ])

  greg_test <- data.frame(count = counts_greg$count[counts_greg$test], covariates[covariates$test, ])
  wost_test <- data.frame(count = counts_wost$count[counts_wost$test], covariates[covariates$test, ])
  whib_test <- data.frame(count = counts_whib$count[counts_whib$test], covariates[covariates$test, ])
  rosp_test <- data.frame(count = counts_rosp$count[counts_rosp$test], covariates[covariates$test, ])


  # Poisson

    fun  <- glm

    args_greg_p          <- list(formula  = count ~ year + reversals,
                                 data     = greg_train,
                                 family   = "poisson", 
                                 control  = glm.control(maxit = 1000))
    fit_greg_p[[origin]]      <- do.call(what = fun, 
                                         args = args_greg_p)
    forecast_greg_p[[origin]] <- predict(object     = fit_greg_p[[origin]], 
                                         newdata    = greg_test,
                                         se.fit     = TRUE,
                                         type       = "link", 
                                         dispersion = sd(fit_greg_p[[origin]]$residuals))
    draws_greg_p[[origin]] <- t(round(exp(mapply(rnorm, n = nsample, mean = forecast_greg_p[[origin]]$fit, sd = forecast_greg_p[[origin]]$se.fit))))
    draws_greg_p_lim[[origin]] <- draws_greg_p[[origin]]
    draws_greg_p_lim[[origin]][ , nsample] <- greg_test$count

    score_greg_p[[origin]] <- list(crps = crps_sample(greg_test$count, draws_greg_p[[origin]]),
                                   logs = logs_sample(greg_test$count, draws_greg_p[[origin]]),
                                   logs_lim = logs_sample(greg_test$count, draws_greg_p_lim[[origin]]))
   
    tiff(file.path(main, "forecasts", paste0("greg_p_origin_", origins[[origin]], ".tiff")), height = 4, width = 8, units = "in", res = 200, compression = "lzw")

    ymax <- 10^ceiling(log10(max(counts_greg$count, na.rm = TRUE)))
    ystep1 <- ymax / 4
    ystep2 <- ymax / 10
    ystep3 <- ymax / 20
    ylabs  <- seq(0, ymax, ystep1) / 1000

    par(mar = c(3, 5, 1, 1))
    plot(counts_greg$year[counts_greg$train], counts_greg$count[counts_greg$train],
         cex = 1.25, lwd = 2,
         ylim = c(0, ymax),
         xlim = c(1985, 2027),
         xlab = "",
         ylab = "",
         main = "", las = 1, bty = "L", xaxt = "n", yaxt = "n", col = cols["greg"], pch = 16)

    axis(1, at = seq(1980, 2020, 10), cex.axis = 1.25)
    axis(1, at = seq(1980, 2025, 5), labels = FALSE, tck = -0.01)
    axis(1, at = seq(1980, 2027, 1), labels = FALSE, tck = -0.005)

    axis(2, at = seq(0, ymax, ystep1), labels = ylabs, las = 1, cex.axis = 1.25)
    axis(2, at = seq(0, ymax, ystep2), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.02)
    axis(2, at = seq(0, ymax, ystep3), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.01)
    mtext(side = 2, "Colony Count (x 1,000)", line = 3.25, cex = 1.25)

    points(counts_greg$year[counts_greg$train], fit_greg_p[[origin]]$fitted.values, type = "l", lwd = 3, col = cols["greg"])

    last_year   <- counts_greg$year[nrow(counts_greg[counts_greg$train, ])]
    last_count  <- counts_greg$count[nrow(counts_greg[counts_greg$train, ])]
    last_fitted <- fit_greg_p[[origin]]$fitted.values[nrow(counts_greg[counts_greg$train, ])]

    points(c(last_year, test_years), 
           c(last_fitted, exp(forecast_greg_p[[origin]]$fit)), 
           type = "l", lty = 2, lwd = 2, col = cols["greg"])
    points(c(last_year, test_years),
           c(last_fitted, exp(forecast_greg_p[[origin]]$fit + 1.96 * forecast_greg_p[[origin]]$se.fit)), 
           type = "l", lty = 3, lwd = 2, col = cols["greg"])
    points(c(last_year, test_years), 
           c(last_fitted, exp(forecast_greg_p[[origin]]$fit - 1.96 * forecast_greg_p[[origin]]$se.fit)), 
           type = "l", lty = 3, lwd = 2, col = cols["greg"])
    points(counts_greg$year[counts_greg$test], counts_greg$count[counts_greg$test], lwd = 2, cex = 1.25, pch = 1, lty = 3, col = cols["greg"])

    dev.off()


    summary <- rbind(summary, 
                     data.frame(species  = "GREG",
                                model    = "p",
                                origin   = origins[origin],
                                lead     = 1:ntest_years,
                                target   = greg_test$year,
                                true_y   = greg_test$count,
                                pred_y   = apply(draws_greg_p[[origin]], 1, mean),
                                sd_pred  = apply(draws_greg_p[[origin]], 1, sd),
                                logs     = score_greg_p[[origin]]$logs,
                                logs_lim = score_greg_p[[origin]]$logs_lim,
                                crps     = score_greg_p[[origin]]$crps))



    args_whib_p          <- list(formula  = count ~ year + reversals,
                                 data     = whib_train,
                                 family   = "poisson", 
                                 control  = glm.control(maxit = 1000))
    fit_whib_p[[origin]]      <- do.call(what = fun, 
                                         args = args_whib_p)
    forecast_whib_p[[origin]] <- predict(object     = fit_whib_p[[origin]], 
                                         newdata    = whib_test,
                                         se.fit     = TRUE,
                                         type       = "link", 
                                         dispersion = sd(fit_whib_p[[origin]]$residuals))
    draws_whib_p[[origin]] <- t(round(exp(mapply(rnorm, n = nsample, mean = forecast_whib_p[[origin]]$fit, sd = forecast_whib_p[[origin]]$se.fit))))
    draws_whib_p_lim[[origin]] <- draws_whib_p[[origin]]
    draws_whib_p_lim[[origin]][ , nsample] <- whib_test$count

    score_whib_p[[origin]] <- list(crps = crps_sample(whib_test$count, draws_whib_p[[origin]]),
                                   logs = logs_sample(whib_test$count, draws_whib_p[[origin]]),
                                   logs_lim = logs_sample(whib_test$count, draws_whib_p_lim[[origin]]))


    tiff(file.path(main, "forecasts", paste0("whib_p_origin_", origins[[origin]], ".tiff")), height = 4, width = 8, units = "in", res = 200, compression = "lzw")

    ymax <- 10^ceiling(log10(max(counts_whib$count, na.rm = TRUE)))
    ystep1 <- ymax / 4
    ystep2 <- ymax / 10
    ystep3 <- ymax / 20
    ylabs  <- seq(0, ymax, ystep1) / 1000

    par(mar = c(3, 5, 1, 1))
    plot(counts_whib$year[counts_whib$train], counts_whib$count[counts_whib$train],
         cex = 1.25, lwd = 2,
         ylim = c(0, ymax),
         xlim = c(1985, 2027),
         xlab = "",
         ylab = "",
         main = "", las = 1, bty = "L", xaxt = "n", yaxt = "n", col = cols["whib"], pch = 16)

    axis(1, at = seq(1980, 2020, 10), cex.axis = 1.25)
    axis(1, at = seq(1980, 2025, 5), labels = FALSE, tck = -0.01)
    axis(1, at = seq(1980, 2027, 1), labels = FALSE, tck = -0.005)

    axis(2, at = seq(0, ymax, ystep1), labels = ylabs, las = 1, cex.axis = 1.25)
    axis(2, at = seq(0, ymax, ystep2), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.02)
    axis(2, at = seq(0, ymax, ystep3), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.01)
    mtext(side = 2, "Colony Count (x 1,000)", line = 3.25, cex = 1.25)

    points(counts_whib$year[counts_whib$train], fit_whib_p[[origin]]$fitted.values, type = "l", lwd = 3, col = cols["whib"])

    last_year   <- counts_whib$year[nrow(counts_whib[counts_whib$train, ])]
    last_count  <- counts_whib$count[nrow(counts_whib[counts_whib$train, ])]
    last_fitted <- fit_whib_p[[origin]]$fitted.values[nrow(counts_whib[counts_whib$train, ])]

    points(c(last_year, test_years), 
           c(last_fitted, exp(forecast_whib_p[[origin]]$fit)), 
           type = "l", lty = 2, lwd = 2, col = cols["whib"])
    points(c(last_year, test_years),
           c(last_fitted, exp(forecast_whib_p[[origin]]$fit + 1.96 * forecast_whib_p[[origin]]$se.fit)), 
           type = "l", lty = 3, lwd = 2, col = cols["whib"])
    points(c(last_year, test_years), 
           c(last_fitted, exp(forecast_whib_p[[origin]]$fit - 1.96 * forecast_whib_p[[origin]]$se.fit)), 
           type = "l", lty = 3, lwd = 2, col = cols["whib"])
    points(counts_whib$year[counts_whib$test], counts_whib$count[counts_whib$test], lwd = 2, cex = 1.25, pch = 1, lty = 3, col = cols["whib"])

    dev.off()



    summary <- rbind(summary, 
                     data.frame(species  = "WHIB",
                                model    = "p",
                                origin   = origins[origin],
                                lead     = 1:ntest_years,
                                target   = whib_test$year,
                                true_y   = whib_test$count,
                                pred_y   = apply(draws_whib_p[[origin]], 1, mean),
                                sd_pred  = apply(draws_whib_p[[origin]], 1, sd),
                                logs     = score_whib_p[[origin]]$logs,
                                logs_lim = score_whib_p[[origin]]$logs_lim,
                                crps     = score_whib_p[[origin]]$crps))



    args_wost_p          <- list(formula  = count ~ year + init_depth + init_depth2 + pre_recession + post_recession,
                                 data     = wost_train,
                                 family   = "poisson", 
                                 control  = glm.control(maxit = 1000))
    fit_wost_p[[origin]]      <- do.call(what = fun, 
                                         args = args_wost_p)
    forecast_wost_p[[origin]] <- predict(object     = fit_wost_p[[origin]], 
                                         newdata    = wost_test,
                                         se.fit     = TRUE,
                                         type       = "link",
                                         dispersion = sd(fit_wost_p[[origin]]$residuals))
    draws_wost_p[[origin]] <- t(round(exp(mapply(rnorm, n = nsample, mean = forecast_wost_p[[origin]]$fit, sd = forecast_wost_p[[origin]]$se.fit))))
    draws_wost_p_lim[[origin]] <- draws_wost_p[[origin]]
    draws_wost_p_lim[[origin]][ , nsample] <- wost_test$count

    score_wost_p[[origin]] <- list(crps = crps_sample(wost_test$count, draws_wost_p[[origin]]),
                                   logs = logs_sample(wost_test$count, draws_wost_p[[origin]]),
                                   logs_lim = logs_sample(wost_test$count, draws_wost_p_lim[[origin]]))


    tiff(file.path(main, "forecasts", paste0("wost_p_origin_", origins[[origin]], ".tiff")), height = 4, width = 8, units = "in", res = 200, compression = "lzw")

    ymax <- 10^ceiling(log10(max(counts_wost$count, na.rm = TRUE)))
    ystep1 <- ymax / 4
    ystep2 <- ymax / 10
    ystep3 <- ymax / 20
    ylabs  <- seq(0, ymax, ystep1) / 1000

    par(mar = c(3, 5, 1, 1))
    plot(counts_wost$year[counts_wost$train], counts_wost$count[counts_wost$train],
         cex = 1.25, lwd = 2,
         ylim = c(0, ymax),
         xlim = c(1985, 2027),
         xlab = "",
         ylab = "",
         main = "", las = 1, bty = "L", xaxt = "n", yaxt = "n", pch = 16, col = cols["wost"])

    axis(1, at = seq(1980, 2020, 10), cex.axis = 1.25)
    axis(1, at = seq(1980, 2025, 5), labels = FALSE, tck = -0.01)
    axis(1, at = seq(1980, 2027, 1), labels = FALSE, tck = -0.005)

    axis(2, at = seq(0, ymax, ystep1), labels = ylabs, las = 1, cex.axis = 1.25)
    axis(2, at = seq(0, ymax, ystep2), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.02)
    axis(2, at = seq(0, ymax, ystep3), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.01)
    mtext(side = 2, "Colony Count (x 1,000)", line = 3.25, cex = 1.25)

    points(counts_wost$year[counts_wost$train], fit_wost_p[[origin]]$fitted.values, type = "l", lwd = 3, col = cols["wost"])

    last_year   <- counts_wost$year[nrow(counts_wost[counts_wost$train, ])]
    last_count  <- counts_wost$count[nrow(counts_wost[counts_wost$train, ])]
    last_fitted <- fit_wost_p[[origin]]$fitted.values[nrow(counts_wost[counts_wost$train, ])]

    points(c(last_year, test_years), 
           c(last_fitted, exp(forecast_wost_p[[origin]]$fit)), 
           type = "l", lty = 2, lwd = 2, col = cols["wost"])
    points(c(last_year, test_years),
           c(last_fitted, exp(forecast_wost_p[[origin]]$fit + 1.96 * forecast_wost_p[[origin]]$se.fit)), 
           type = "l", lty = 3, lwd = 2, col = cols["wost"])
    points(c(last_year, test_years), 
           c(last_fitted, exp(forecast_wost_p[[origin]]$fit - 1.96 * forecast_wost_p[[origin]]$se.fit)), 
           type = "l", lty = 3, lwd = 2, col = cols["wost"])
    points(counts_wost$year[counts_wost$test], counts_wost$count[counts_wost$test], lwd = 2, cex = 1.25, pch = 1, lty = 3, col = cols["wost"])

    dev.off()


    summary <- rbind(summary, 
                     data.frame(species  = "WOST",
                                model    = "p",
                                origin   = origins[origin],
                                lead     = 1:ntest_years,
                                target   = wost_test$year,
                                true_y   = wost_test$count,
                                pred_y   = apply(draws_wost_p[[origin]], 1, mean),
                                sd_pred  = apply(draws_wost_p[[origin]], 1, sd),
                                logs     = score_wost_p[[origin]]$logs,
                                logs_lim = score_wost_p[[origin]]$logs_lim,
                                crps     = score_wost_p[[origin]]$crps))



  # N Binom

    fun  <- MASS::glm.nb

    args_greg_nb <- list(formula  = count ~ year + reversals,
                         data     = greg_train, 
                         control  = glm.control(maxit = 1000))
    fit_greg_nb[[origin]] <- do.call(what = fun, 
                                     args = args_greg_nb)
    forecast_greg_nb[[origin]] <- predict(object     = fit_greg_nb[[origin]], 
                                          newdata    = greg_test,
                                          se.fit     = TRUE,
                                          type       = "link", 
                                          dispersion = sd(fit_greg_nb[[origin]]$residuals))
    draws_greg_nb[[origin]] <- t(round(exp(mapply(rnorm, n = nsample, mean = forecast_greg_nb[[origin]]$fit, sd = forecast_greg_nb[[origin]]$se.fit))))
    draws_greg_nb_lim[[origin]] <- draws_greg_nb[[origin]]
    draws_greg_nb_lim[[origin]][ , nsample] <- greg_test$count

    score_greg_nb[[origin]] <- list(crps = crps_sample(greg_test$count, draws_greg_nb[[origin]]),
                                   logs = logs_sample(greg_test$count, draws_greg_nb[[origin]]),
                                   logs_lim = logs_sample(greg_test$count, draws_greg_nb_lim[[origin]]))

 
    tiff(file.path(main, "forecasts", paste0("greg_nb_origin_", origins[[origin]], ".tiff")), height = 4, width = 8, units = "in", res = 200, compression = "lzw")

    ymax <- 10^ceiling(log10(max(counts_greg$count, na.rm = TRUE)))
    ystep1 <- ymax / 4
    ystep2 <- ymax / 10
    ystep3 <- ymax / 20
    ylabs  <- seq(0, ymax, ystep1) / 1000

    par(mar = c(3, 5, 1, 1))
    plot(counts_greg$year[counts_greg$train], counts_greg$count[counts_greg$train],
         cex = 1.25, lwd = 2,
         ylim = c(0, ymax),
         xlim = c(1985, 2027),
         xlab = "",
         ylab = "",
         main = "", las = 1, bty = "L", xaxt = "n", yaxt = "n", pch = 16, col = cols["greg"])

    axis(1, at = seq(1980, 2020, 10), cex.axis = 1.25)
    axis(1, at = seq(1980, 2025, 5), labels = FALSE, tck = -0.01)
    axis(1, at = seq(1980, 2027, 1), labels = FALSE, tck = -0.005)

    axis(2, at = seq(0, ymax, ystep1), labels = ylabs, las = 1, cex.axis = 1.25)
    axis(2, at = seq(0, ymax, ystep2), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.02)
    axis(2, at = seq(0, ymax, ystep3), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.01)
    mtext(side = 2, "Colony Count (x 1,000)", line = 3.25, cex = 1.25)

    points(counts_greg$year[counts_greg$train], fit_greg_nb[[origin]]$fitted.values, type = "l", lwd = 3, col = cols["greg"])

    last_year   <- counts_greg$year[nrow(counts_greg[counts_greg$train, ])]
    last_count  <- counts_greg$count[nrow(counts_greg[counts_greg$train, ])]
    last_fitted <- fit_greg_nb[[origin]]$fitted.values[nrow(counts_greg[counts_greg$train, ])]

    points(c(last_year, test_years), 
           c(last_fitted, exp(forecast_greg_nb[[origin]]$fit)), 
           type = "l", lty = 2, lwd = 2, col = cols["greg"])
    points(c(last_year, test_years),
           c(last_fitted, exp(forecast_greg_nb[[origin]]$fit + 1.96 * forecast_greg_nb[[origin]]$se.fit)), 
           type = "l", lty = 3, lwd = 2, col = cols["greg"])
    points(c(last_year, test_years), 
           c(last_fitted, exp(forecast_greg_nb[[origin]]$fit - 1.96 * forecast_greg_nb[[origin]]$se.fit)), 
           type = "l", lty = 3, lwd = 2, col = cols["greg"])
    points(counts_greg$year[counts_greg$test], counts_greg$count[counts_greg$test], lwd = 2, cex = 1.25, pch = 1, lty = 3, col = cols["greg"])

    dev.off()



    summary <- rbind(summary, 
                     data.frame(species  = "GREG",
                                model    = "nb",
                                origin   = origins[origin],
                                lead     = 1:ntest_years,
                                target   = greg_test$year,
                                true_y   = greg_test$count,
                                pred_y   = apply(draws_greg_nb[[origin]], 1, mean),
                                sd_pred  = apply(draws_greg_nb[[origin]], 1, sd),
                                logs     = score_greg_nb[[origin]]$logs,
                                logs_lim = score_greg_nb[[origin]]$logs_lim,
                                crps     = score_greg_nb[[origin]]$crps))



    args_whib_nb <- list(formula  = count ~ year + reversals,
                         data     = whib_train, 
                         control  = glm.control(maxit = 1000))
    fit_whib_nb[[origin]] <- do.call(what = fun, 
                                     args = args_whib_nb)
    forecast_whib_nb[[origin]] <- predict(object     = fit_whib_nb[[origin]], 
                                          newdata    = whib_test,
                                          se.fit     = TRUE,
                                          type       = "link", 
                                          dispersion = sd(fit_whib_nb[[origin]]$residuals))
    draws_whib_nb[[origin]] <- t(round(exp(mapply(rnorm, n = nsample, mean = forecast_whib_nb[[origin]]$fit, sd = forecast_whib_nb[[origin]]$se.fit))))
    draws_whib_nb_lim[[origin]] <- draws_whib_nb[[origin]]
    draws_whib_nb_lim[[origin]][ , nsample] <- whib_test$count

    score_whib_nb[[origin]] <- list(crps = crps_sample(whib_test$count, draws_whib_nb[[origin]]),
                                   logs = logs_sample(whib_test$count, draws_whib_nb[[origin]]),
                                   logs_lim = logs_sample(whib_test$count, draws_whib_nb_lim[[origin]]))


    tiff(file.path(main, "forecasts", paste0("whib_nb_origin_", origins[[origin]], ".tiff")), height = 4, width = 8, units = "in", res = 200, compression = "lzw")

    ymax <- 10^ceiling(log10(max(counts_whib$count, na.rm = TRUE)))
    ystep1 <- ymax / 4
    ystep2 <- ymax / 10
    ystep3 <- ymax / 20
    ylabs  <- seq(0, ymax, ystep1) / 1000

    par(mar = c(3, 5, 1, 1))
    plot(counts_whib$year[counts_whib$train], counts_whib$count[counts_whib$train],
         cex = 1.25, lwd = 2,
         ylim = c(0, ymax),
         xlim = c(1985, 2027),
         xlab = "",
         ylab = "",
         main = "", las = 1, bty = "L", xaxt = "n", yaxt = "n", pch = 16, col = cols["whib"])

    axis(1, at = seq(1980, 2020, 10), cex.axis = 1.25)
    axis(1, at = seq(1980, 2025, 5), labels = FALSE, tck = -0.01)
    axis(1, at = seq(1980, 2027, 1), labels = FALSE, tck = -0.005)

    axis(2, at = seq(0, ymax, ystep1), labels = ylabs, las = 1, cex.axis = 1.25)
    axis(2, at = seq(0, ymax, ystep2), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.02)
    axis(2, at = seq(0, ymax, ystep3), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.01)
    mtext(side = 2, "Colony Count (x 1,000)", line = 3.25, cex = 1.25)

    points(counts_whib$year[counts_whib$train], fit_whib_nb[[origin]]$fitted.values, type = "l", lwd = 3, col = cols["whib"])

    last_year   <- counts_whib$year[nrow(counts_whib[counts_whib$train, ])]
    last_count  <- counts_whib$count[nrow(counts_whib[counts_whib$train, ])]
    last_fitted <- fit_whib_nb[[origin]]$fitted.values[nrow(counts_whib[counts_whib$train, ])]

    points(c(last_year, test_years), 
           c(last_fitted, exp(forecast_whib_nb[[origin]]$fit)), 
           type = "l", lty = 2, lwd = 2, col = cols["whib"])
    points(c(last_year, test_years),
           c(last_fitted, exp(forecast_whib_nb[[origin]]$fit + 1.96 * forecast_whib_nb[[origin]]$se.fit)), 
           type = "l", lty = 3, lwd = 2, col = cols["whib"])
    points(c(last_year, test_years), 
           c(last_fitted, exp(forecast_whib_nb[[origin]]$fit - 1.96 * forecast_whib_nb[[origin]]$se.fit)), 
           type = "l", lty = 3, lwd = 2, col = cols["whib"])
    points(counts_whib$year[counts_whib$test], counts_whib$count[counts_whib$test], lwd = 2, cex = 1.25, pch = 1, lty = 3, col = cols["whib"])

    dev.off()


    summary <- rbind(summary, 
                     data.frame(species  = "WHIB",
                                model    = "nb",
                                origin   = origins[origin],
                                lead     = 1:ntest_years,
                                target   = whib_test$year,
                                true_y   = whib_test$count,
                                pred_y   = apply(draws_whib_nb[[origin]], 1, mean),
                                sd_pred  = apply(draws_whib_nb[[origin]], 1, sd),
                                logs     = score_whib_nb[[origin]]$logs,
                                logs_lim = score_whib_nb[[origin]]$logs_lim,
                                crps     = score_whib_nb[[origin]]$crps))



    args_wost_nb          <- list(formula  = count ~ year + init_depth + init_depth2 + pre_recession + post_recession,
                                  data     = wost_train, 
                                   control  = glm.control(maxit = 1000))
    fit_wost_nb[[origin]]      <- do.call(what = fun, 
                                          args = args_wost_nb)
    forecast_wost_nb[[origin]] <- predict(object     = fit_wost_nb[[origin]], 
                                          newdata    = wost_test,
                                          se.fit     = TRUE,
                                          type       = "link", 
                                          dispersion = sd(fit_wost_nb[[origin]]$residuals))
    draws_wost_nb[[origin]] <- t(round(exp(mapply(rnorm, n = nsample, mean = forecast_wost_nb[[origin]]$fit, sd = forecast_wost_nb[[origin]]$se.fit))))
    draws_wost_nb_lim[[origin]] <- draws_wost_nb[[origin]]
    draws_wost_nb_lim[[origin]][ , nsample] <- wost_test$count

    score_wost_nb[[origin]] <- list(crps = crps_sample(wost_test$count, draws_wost_nb[[origin]]),
                                   logs = logs_sample(wost_test$count, draws_wost_nb[[origin]]),
                                   logs_lim = logs_sample(wost_test$count, draws_wost_nb_lim[[origin]]))


    tiff(file.path(main, "forecasts", paste0("wost_nb_origin_", origins[[origin]], ".tiff")), height = 4, width = 8, units = "in", res = 200, compression = "lzw")

    ymax <- 10^ceiling(log10(max(counts_wost$count, na.rm = TRUE)))
    ystep1 <- ymax / 4
    ystep2 <- ymax / 10
    ystep3 <- ymax / 20
    ylabs  <- seq(0, ymax, ystep1) / 1000

    par(mar = c(3, 5, 1, 1))
    plot(counts_wost$year[counts_wost$train], counts_wost$count[counts_wost$train],
         cex = 1.25, lwd = 2,
         ylim = c(0, ymax),
         xlim = c(1985, 2027),
         xlab = "",
         ylab = "",
         main = "", las = 1, bty = "L", xaxt = "n", yaxt = "n", pch = 16, col = cols["wost"])

    axis(1, at = seq(1980, 2020, 10), cex.axis = 1.25)
    axis(1, at = seq(1980, 2025, 5), labels = FALSE, tck = -0.01)
    axis(1, at = seq(1980, 2027, 1), labels = FALSE, tck = -0.005)

    axis(2, at = seq(0, ymax, ystep1), labels = ylabs, las = 1, cex.axis = 1.25)
    axis(2, at = seq(0, ymax, ystep2), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.02)
    axis(2, at = seq(0, ymax, ystep3), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.01)
    mtext(side = 2, "Colony Count (x 1,000)", line = 3.25, cex = 1.25)

    points(counts_wost$year[counts_wost$train], fit_wost_nb[[origin]]$fitted.values, type = "l", lwd = 3, col = cols["wost"])

    last_year   <- counts_wost$year[nrow(counts_wost[counts_wost$train, ])]
    last_count  <- counts_wost$count[nrow(counts_wost[counts_wost$train, ])]
    last_fitted <- fit_wost_nb[[origin]]$fitted.values[nrow(counts_wost[counts_wost$train, ])]

    points(c(last_year, test_years), 
           c(last_fitted, exp(forecast_wost_nb[[origin]]$fit)), 
           type = "l", lty = 2, lwd = 2, col = cols["wost"])
    points(c(last_year, test_years),
           c(last_fitted, exp(forecast_wost_nb[[origin]]$fit + 1.96 * forecast_wost_nb[[origin]]$se.fit)), 
           type = "l", lty = 3, lwd = 2, col = cols["wost"])
    points(c(last_year, test_years), 
           c(last_fitted, exp(forecast_wost_nb[[origin]]$fit - 1.96 * forecast_wost_nb[[origin]]$se.fit)), 
           type = "l", lty = 3, lwd = 2, col = cols["wost"])
    points(counts_wost$year[counts_wost$test], counts_wost$count[counts_wost$test], lwd = 2, cex = 1.25, pch = 1, lty = 3, col = cols["wost"])

    dev.off()


    summary <- rbind(summary, 
                     data.frame(species  = "WOST",
                                model    = "nb",
                                origin   = origins[origin],
                                lead     = 1:ntest_years,
                                target   = wost_test$year,
                                true_y   = wost_test$count,
                                pred_y   = apply(draws_wost_nb[[origin]], 1, mean),
                                sd_pred  = apply(draws_wost_nb[[origin]], 1, sd),
                                logs     = score_wost_nb[[origin]]$logs,
                                logs_lim = score_wost_nb[[origin]]$logs_lim,
                                crps     = score_wost_nb[[origin]]$crps))

}


write.csv(summary, file.path(main, "forecasts", "forecast_summary_table.csv"), row.names = FALSE)


cmax   <- 1000 * ceiling(1/1000 * (max(c(summary$true_y, summary$pred_y))))
cstep1 <- 50000
cstep2 <- 25000
cstep3 <- 5000
clabs  <- seq(0, cmax, cstep1) / 1000
ymax   <- cmax


for (origin in 1:norigins) {


  # Poisson

    tiff(file.path(main, "forecasts", paste0("p_origin_", origins[[origin]], ".tiff")), height = 6, width = 6, units = "in", res = 200, compression = "lzw")

    par(mar = c(5, 5, 1, 1))
    plot(1, 1, type = "n",
         cex = 1.25, lwd = 2,
         ylim = c(0, ymax),
         xlim = c(0, ymax),
         xlab = "",
         ylab = "",
         main = "", las = 1, bty = "L", xaxt = "n", yaxt = "n")

    axis(1, at = seq(0, cmax, cstep1), labels = clabs, cex.axis = 1.25)
    axis(1, at = seq(0, cmax, cstep2), labels = FALSE, tck = -0.01)
    axis(1, at = seq(0, cmax, cstep3), labels = FALSE, tck = -0.005)
    mtext(side = 1, "Predicted Count (x 1,000)", line = 3.25, cex = 1.25)

    axis(2, at = seq(0, cmax, cstep1), labels = clabs, las = 1, cex.axis = 1.25)
    axis(2, at = seq(0, cmax, cstep2), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.02)
    axis(2, at = seq(0, cmax, cstep3), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.01)
    mtext(side = 2, "Observed Count (x 1,000)", line = 3.25, cex = 1.25, las = 0)

    abline(a = 0, b = 1, lwd = 2)

    x <- summary$pred_y[summary$origin == origins[origin] & summary$species == "WOST" & summary$model == "p"]
    y <- summary$true_y[summary$origin == origins[origin] & summary$species == "WOST" & summary$model == "p"]
    points(x, y, pch = 16, col = cols["wost"])

    x <- summary$pred_y[summary$origin == origins[origin] & summary$species == "WHIB" & summary$model == "p"]
    y <- summary$true_y[summary$origin == origins[origin] & summary$species == "WHIB" & summary$model == "p"]
    points(x, y, pch = 16, col = cols["whib"])


    x <- summary$pred_y[summary$origin == origins[origin] & summary$species == "GREG" & summary$model == "p"]
    y <- summary$true_y[summary$origin == origins[origin] & summary$species == "GREG" & summary$model == "p"]
    points(x, y, pch = 16, col = cols["greg"])

    dev.off()


  # NB

    tiff(file.path(main, "forecasts", paste0("nb_origin_", origins[[origin]], ".tiff")), height = 6, width = 6, units = "in", res = 200, compression = "lzw")

    par(mar = c(5, 5, 1, 1))
    plot(1, 1, type = "n",
         cex = 1.25, lwd = 2,
         ylim = c(0, ymax),
         xlim = c(0, ymax),
         xlab = "",
         ylab = "",
         main = "", las = 1, bty = "L", xaxt = "n", yaxt = "n")

    axis(1, at = seq(0, cmax, cstep1), labels = clabs, cex.axis = 1.25)
    axis(1, at = seq(0, cmax, cstep2), labels = FALSE, tck = -0.01)
    axis(1, at = seq(0, cmax, cstep3), labels = FALSE, tck = -0.005)
    mtext(side = 1, "Predicted Count (x 1,000)", line = 3.25, cex = 1.25)

    axis(2, at = seq(0, cmax, cstep1), labels = clabs, las = 1, cex.axis = 1.25)
    axis(2, at = seq(0, cmax, cstep2), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.02)
    axis(2, at = seq(0, cmax, cstep3), labels = FALSE, las = 1, cex.axis = 1.25, tck = -0.01)
    mtext(side = 2, "Observed Count (x 1,000)", line = 3.25, cex = 1.25, las = 0)

    abline(a = 0, b = 1, lwd = 2)

    x <- summary$pred_y[summary$origin == origins[origin] & summary$species == "WOST" & summary$model == "nb"]
    y <- summary$true_y[summary$origin == origins[origin] & summary$species == "WOST" & summary$model == "nb"]
    points(x, y, pch = 16, col = cols["wost"])

    x <- summary$pred_y[summary$origin == origins[origin] & summary$species == "WHIB" & summary$model == "nb"]
    y <- summary$true_y[summary$origin == origins[origin] & summary$species == "WHIB" & summary$model == "nb"]
    points(x, y, pch = 16, col = cols["whib"])


    x <- summary$pred_y[summary$origin == origins[origin] & summary$species == "GREG" & summary$model == "nb"]
    y <- summary$true_y[summary$origin == origins[origin] & summary$species == "GREG" & summary$model == "nb"]
    points(x, y, pch = 16, col = cols["greg"])

    dev.off()




}


