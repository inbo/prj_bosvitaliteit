#functies trend


calc_sen_slope <- function(data, sen_boot = 0){
  if (sen_boot < 1) {
    test <- rkt::rkt(date = data[["Jaar"]], y = data[["mean_value"]], block = data[["PlotNr"]])
    lmerfit <- summary(lmer(mean_value ~ Jaar + (1|PlotNr), data = data))$coef[,1]
    return(data.frame(sen_int1 = median(data[["mean_value"]] - test$B * data[["Jaar"]]),
                      sen_slope1 = test$B,
                      tau = test$tau,
                      lmer_intercept = lmerfit[1],
                      lmer_slope = lmerfit[2]))
  }
  else {
    senslope <- numeric(sen_boot)
    senintercept <- numeric(sen_boot)
    allplots <- unique(data[["PlotNr"]])
    for (i in 1:sen_boot) {
      print(paste0(i/sen_boot*100,"%"))
      randplots <- sample(allplots, size = length(allplots), replace = TRUE)
      bd <- NULL
      for (j in 1:length(randplots)) {
        bd <- rbind(bd, (filter(data, PlotNr == randplots[j]) %>% mutate(PlotNr = j)))
      }
      senslope[i] <- rkt::rkt(bd[["Jaar"]], y = bd[["mean_value"]], block = bd[["PlotNr"]])$B
      senintercept[i] <- median(bd[["mean_value"]] - senslope[i] * bd[["Jaar"]])
    }
    return(data.frame(Nboot = sen_boot,
                      sen_intercept = mean(senintercept),
                      sen_slope = mean(senslope),
                      sen_lcl = quantile(senslope, probs = 0.025, na.rm = TRUE),
                      sen_ucl = quantile(senslope, probs = 0.975, na.rm = TRUE)))
  }
}


###############################################################################



pred_sen_slope <- function(data, sen_boot = 0) {

  #print(data$selectie[1])
  allplots <- unique(data[["PlotNr"]])
  years <- sort(unique(data[["Jaar"]]))
  n_years <- length(years)
  yearmeans <- data %>%
    group_by(Jaar) %>%
    summarize(mean_year = mean(mean_value, na.rm = TRUE),
              mean_se = sd(mean_value, na.rm = TRUE) / sqrt(n()),
              mean_lcl = mean_year - 1.96 * mean_se,
              mean_ucl = mean_year + 1.96 * mean_se)

  test <- rkt::rkt(date = data[["Jaar"]],  y = data[["mean_value"]], block = data[["PlotNr"]])
  sen_int <- mean(data[["mean_value"]] - test$B * data[["Jaar"]])
  sen_slope <- test$B
  preds <- data.frame(Jaar = min(data[["Jaar"]]):max(data[["Jaar"]]))
  preds$fit <- sen_int + preds$Jaar * sen_slope
  preds <- left_join(preds, yearmeans)


  if (sen_boot == 0) {
    return(cbind(preds, sen_int = sen_int, sen_slope = sen_slope))
  }

  bootdata <- NULL
  sen_ints <- numeric(sen_boot)
  sen_slopes <- numeric(sen_boot)
  predsboot <- data.frame(matrix(ncol = sen_boot, nrow = n_years, data = NA))
  predsboot$Jaar = years


  for (i in 1:sen_boot) {
    print(paste("iter:", i))
    randplots <- sample(allplots, size = length(allplots), replace = TRUE)
    print(paste("unieke proefvlakken:", length(unique(randplots))))
    bootdata <- NULL
    for (j in 1:length(randplots)) {
      newbootdata <- filter(data, PlotNr == randplots[j]) %>%
        select(Jaar, PlotNr, mean_value) %>%
        mutate(PlotNr = j) #om duplicaten te vermijden in plotnummer, waar rkt niet mee overweg kan
      bootdata <- rbind(bootdata, newbootdata)
    }
    print(paste("rows bootdata: ", nrow(bootdata)))
    test <- rkt::rkt(date = bootdata[["Jaar"]],  y = bootdata[["mean_value"]], block = bootdata[["PlotNr"]])
    sen_ints[i] <- mean(bootdata[["mean_value"]] - test$B * bootdata[["Jaar"]])
    sen_slopes[i] <- test$B
    predsboot[, paste0('X', i)] <- sen_ints[i] + predsboot$Jaar * sen_slopes[i]
  }
  mypreds <<- predsboot
  mysen_slopes <<- sen_slopes
  confboot <- data.frame(
    Jaar = predsboot$Jaar,
    bootfit = rowMeans(select(predsboot, -Jaar)),
    boot_lcl = apply(select(predsboot, -Jaar), 1, quantile, 0.025),
    boot_ucl = apply(select(predsboot, -Jaar), 1, quantile, 0.975),
    sen_slope,
    sen_slope_boot = mean(sen_slopes),
    sen_slope_lcl = quantile(sen_slopes, 0.025),
    sen_slope_ucl = quantile(sen_slopes, 0.975),
    sen_int,
    sen_int_boot = mean(sen_ints),
    sen_int_lcl = quantile(sen_ints, 0.025),
    sen_int_ucl = quantile(sen_ints, 0.975))

  return(left_join(preds, confboot, by = "Jaar"))
}
