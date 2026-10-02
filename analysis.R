# =============================================================================
# Did the heat recovery retrofit pay off?
# Energy intensity analysis for a fictional Nordic packaging plant
#
# Author : Rajan Kumar V K, D.Sc. (Tech.)
# Tool   : R (base R only; svglite is used for figures when installed)
# Run    : source("analysis.R")   or   Rscript analysis.R
# Output : data/plant_energy.csv, output/results.txt, figures/*.svg, index.html
#
# The company and the data are fictional. The data are simulated below with a
# fixed seed, so every number in the report can be reproduced exactly.
# =============================================================================

set.seed(2026)
dir.create("data", showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

# ---- 0. Assumptions used for the business case -------------------------------
energy_price  <- 90        # EUR per MWh
co2_factor    <- 0.10      # kg CO2 per kWh (illustrative grid factor)
hours_per_day <- 16        # two shifts
days_per_year <- 250
investment    <- 420000    # EUR, heat recovery unit on line B

# ---- 1. Simulate 36 weeks of daily production data for two lines -------------
weeks <- 36; days <- 5; retrofit_week <- 19
grid <- expand.grid(day = 1:days, week = 1:weeks, line = c("A", "B"))
n <- nrow(grid)
date <- as.Date("2026-01-05") + (grid$week - 1) * 7 + (grid$day - 1)

season      <- -8 + 25 * (grid$week - 1) / (weeks - 1)       # winter to late summer
temp        <- round(season + rnorm(n, 0, 3), 1)
throughput  <- round(rnorm(n, 12, 1.2), 2)
moisture    <- round(rnorm(n, 8, 1), 2)
grade       <- factor(ifelse(runif(n) < 0.35, "Heavy", "Standard"), levels = c("Standard", "Heavy"))
changeovers <- rpois(n, 1.2)
period      <- factor(ifelse(grid$week >= retrofit_week, "After", "Before"), levels = c("Before", "After"))
treated     <- as.integer(grid$line == "B" & period == "After")

energy <- 620 - 14 * (throughput - 12) + 11 * (moisture - 8) - 2.2 * temp +
  35 * (grade == "Heavy") + 9 * changeovers + 12 * (grid$line == "B") -
  38 * treated + rnorm(n, 0, 18)

logit <- -2.3 + 0.55 * (moisture - 8) + 0.35 * changeovers + 0.6 * (grade == "Heavy") -
  0.25 * (throughput - 12)
quality_dev <- rbinom(n, 1, plogis(logit))

raw <- data.frame(date, week = grid$week, line = factor(grid$line), period, treated,
                  throughput_tph = throughput, moisture_pct = moisture, outdoor_temp_c = temp,
                  grade, changeovers, energy_kwh_t = round(energy, 1), quality_dev)
raw$moisture_pct[sample(n, 5)] <- NA      # a few lost sensor readings, as in real life
write.csv(raw, "data/plant_energy.csv", row.names = FALSE)

# ---- 2. Data quality ----------------------------------------------------------
d <- raw[complete.cases(raw), ]
n_dropped <- nrow(raw) - nrow(d)

out <- character(0); sec <- list()
keep <- function(title, expr) {
  res <- capture.output(expr)
  out <<- c(out, paste0("## ", title), res, "")
  sec[[title]] <<- res
  invisible(res)
}
keep("Rows read, rows used", cat(nrow(raw), "rows read;", n_dropped, "dropped for a missing moisture reading;", nrow(d), "used\n"))
keep("Summary of numeric variables", print(summary(d[, c("energy_kwh_t", "throughput_tph", "moisture_pct", "outdoor_temp_c", "changeovers")])))

# ---- 3. Naive answer: before against after on line B (Welch t test) ----------
b <- d[d$line == "B", ]
a <- d[d$line == "A", ]
t_b <- t.test(energy_kwh_t ~ period, data = b)
t_a <- t.test(energy_kwh_t ~ period, data = a)
naive     <- unname(t_b$estimate[2] - t_b$estimate[1])
naive_ci  <- -rev(t_b$conf.int)
keep("Welch t test, line B, before against after", print(t_b))
keep("Welch t test, line A (no retrofit), before against after", print(t_a))

# ---- 4. Difference in differences ---------------------------------------------
hc3 <- function(m) {                       # heteroskedasticity robust covariance (HC3)
  X <- model.matrix(m); e <- resid(m); h <- hatvalues(m)
  bread <- solve(crossprod(X)); meat <- crossprod(X * (e / (1 - h)))
  bread %*% meat %*% bread
}
robust_table <- function(m) {
  est <- coef(m); se <- sqrt(diag(hc3(m))); tval <- est / se
  p <- 2 * pt(abs(tval), df.residual(m), lower.tail = FALSE)
  q <- qt(0.975, df.residual(m))
  round(cbind(Estimate = est, Robust_SE = se, t = tval, p = p, CI_low = est - q * se, CI_high = est + q * se), 3)
}

did_simple <- lm(energy_kwh_t ~ line * period, data = d)
did_adj    <- lm(energy_kwh_t ~ line * period + throughput_tph + moisture_pct + outdoor_temp_c + grade + changeovers, data = d)
tab_simple <- robust_table(did_simple)
tab_adj    <- robust_table(did_adj)
keep("Difference in differences, no covariates (robust SE)", print(tab_simple))
keep("Difference in differences with process covariates (robust SE)", print(tab_adj))
keep("Model fit, adjusted model", cat("R squared", round(summary(did_adj)$r.squared, 3), "| adjusted", round(summary(did_adj)$adj.r.squared, 3), "| residual SE", round(sigma(did_adj), 1), "kWh/t\n"))

eff    <- tab_adj["lineB:periodAfter", ]
eff_s  <- tab_simple["lineB:periodAfter", ]

# ---- 5. Checks on the model ----------------------------------------------------
# 5a. Parallel trends before the retrofit
pre <- d[d$period == "Before", ]
pt_model <- lm(energy_kwh_t ~ line * week + throughput_tph + moisture_pct + outdoor_temp_c + grade + changeovers, data = pre)
pt_row <- round(summary(pt_model)$coefficients["lineB:week", ], 3)
keep("Parallel trends check: line by week interaction in the pre period", print(pt_row))

# 5b. Multicollinearity (variance inflation factors)
X <- model.matrix(did_adj)[, -1]
vif <- sapply(seq_len(ncol(X)), function(j) 1 / (1 - summary(lm(X[, j] ~ X[, -j]))$r.squared))
names(vif) <- colnames(X)
keep("Variance inflation factors", print(round(vif, 2)))

# 5c. Residual checks
sw <- shapiro.test(resid(did_adj))
aux <- lm(I(resid(did_adj)^2) ~ X)
bp_stat <- nrow(d) * summary(aux)$r.squared
bp_p <- pchisq(bp_stat, ncol(X), lower.tail = FALSE)
keep("Residual checks", cat("Shapiro-Wilk W =", round(sw$statistic, 4), "p =", round(sw$p.value, 3),
                             "\nBreusch-Pagan (studentised) LM =", round(bp_stat, 2), "df =", ncol(X), "p =", round(bp_p, 3), "\n"))

# ---- 6. Did the retrofit hurt quality? -----------------------------------------
q_tab <- table(Period = b$period, Deviation = b$quality_dev)
q_test <- chisq.test(q_tab)
keep("Quality deviations on line B, before and after", print(q_tab))
keep("Chi-squared test", print(q_test))

logit_m <- glm(quality_dev ~ moisture_pct + changeovers + grade + throughput_tph + treated, data = d, family = binomial)
or_tab <- round(cbind(Odds_ratio = exp(coef(logit_m)), exp(confint.default(logit_m)), p = summary(logit_m)$coefficients[, 4]), 3)
colnames(or_tab)[2:3] <- c("CI_low", "CI_high")
keep("Logistic regression for a quality deviation day (odds ratios)", print(or_tab))

auc <- function(y, p) { r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0); (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) }
fold <- sample(rep(1:10, length.out = nrow(d)))
oof <- numeric(nrow(d))
for (k in 1:10) {
  fit <- glm(formula(logit_m), data = d[fold != k, ], family = binomial)
  oof[fold == k] <- predict(fit, d[fold == k, ], type = "response")
}
auc_cv <- auc(d$quality_dev, oof)
auc_in <- auc(d$quality_dev, fitted(logit_m))
keep("Discrimination", cat("AUC in sample", round(auc_in, 3), "| AUC 10-fold cross-validated", round(auc_cv, 3), "\n"))

# ---- 7. Business case -----------------------------------------------------------
tonnes <- mean(b$throughput_tph) * hours_per_day * days_per_year
value  <- function(kwh_t) -kwh_t * tonnes / 1000 * energy_price      # EUR per year
biz <- data.frame(
  Estimate   = c("Naive before/after", "Difference in differences"),
  kWh_per_t  = round(c(naive, eff["Estimate"]), 1),
  MWh_year   = round(-c(naive, eff["Estimate"]) * tonnes / 1000),
  EUR_year   = round(value(c(naive, eff["Estimate"])), -3),
  Payback_y  = round(investment / value(c(naive, eff["Estimate"])), 1)
)
eur_ci <- round(value(c(eff["CI_high"], eff["CI_low"])), -3)
co2_t  <- round(-eff["Estimate"] * tonnes * co2_factor / 1000)
keep("Business case, line B", print(biz, row.names = FALSE))
keep("Range for the annual saving (95 % CI)", cat("EUR", eur_ci[1], "to", eur_ci[2], "| CO2 avoided about", co2_t, "t per year | tonnes per year", round(tonnes), "\n"))

keep("Session", cat(R.version.string, "\n"))
writeLines(out, "output/results.txt")

# ---- 8. Figures ------------------------------------------------------------------
teal <- "#0e6b66"; amber <- "#c4741f"; ink <- "#13212b"; grey <- "#74828c"
open_svg <- function(file, w = 7.2, h = 3.8) {
  if (requireNamespace("svglite", quietly = TRUE)) svglite::svglite(file, width = w, height = h) else svg(file, width = w, height = h)
  par(mar = c(4, 4.2, 1, 1), las = 1, bty = "l", col.axis = grey, col.lab = ink, fg = grey, cex.axis = 0.85, cex.lab = 0.95)
}

wk <- aggregate(energy_kwh_t ~ week + line, data = d, FUN = mean)
open_svg("figures/fig1_weekly.svg")
plot(NA, xlim = c(1, weeks), ylim = range(wk$energy_kwh_t) + c(-8, 8), xlab = "Week of 2026", ylab = "Energy intensity, kWh per tonne")
abline(v = retrofit_week - 0.5, lty = 2, col = grey)
text(retrofit_week - 0.2, max(wk$energy_kwh_t) + 5, "retrofit on line B", adj = 0, cex = 0.8, col = grey)
lines(energy_kwh_t ~ week, data = wk[wk$line == "A", ], col = amber, lwd = 2)
lines(energy_kwh_t ~ week, data = wk[wk$line == "B", ], col = teal, lwd = 2)
legend("bottomleft", c("Line A (no retrofit)", "Line B (retrofit)"), col = c(amber, teal), lwd = 2, bty = "n", cex = 0.85, text.col = ink)
dev.off()

est <- rbind(c(naive, naive_ci), eff_s[c("Estimate", "CI_low", "CI_high")], eff[c("Estimate", "CI_low", "CI_high")])
open_svg("figures/fig2_estimates.svg", h = 2.9)
par(mar = c(4, 13, 1, 1))
plot(est[, 1], 3:1, xlim = range(est, 0) + c(-6, 4), ylim = c(0.5, 3.5), pch = 19, col = c(amber, teal, teal), cex = 1.4, yaxt = "n", ylab = "", xlab = "Change in energy intensity on line B, kWh per tonne (95 % CI)")
segments(est[, 2], 3:1, est[, 3], 3:1, col = c(amber, teal, teal), lwd = 2.5)
abline(v = 0, lty = 2, col = grey)
axis(2, at = 3:1, labels = c("Naive before/after", "Difference in differences", "DiD + process covariates"), tick = FALSE, col.axis = ink)
dev.off()

open_svg("figures/fig3_residuals.svg")
plot(fitted(did_adj), resid(did_adj), pch = 16, cex = 0.6, col = teal, xlab = "Fitted energy intensity, kWh per tonne", ylab = "Residual, kWh per tonne")
abline(h = 0, lty = 2, col = grey)
lines(lowess(fitted(did_adj), resid(did_adj)), col = amber, lwd = 2)
dev.off()

thr <- sort(unique(c(0, oof, 1)), decreasing = TRUE)
tpr <- sapply(thr, function(s) mean(oof[d$quality_dev == 1] >= s))
fpr <- sapply(thr, function(s) mean(oof[d$quality_dev == 0] >= s))
open_svg("figures/fig4_roc.svg", w = 4.6, h = 3.8)
plot(fpr, tpr, type = "l", col = teal, lwd = 2, xlab = "False positive rate", ylab = "True positive rate", xlim = c(0, 1), ylim = c(0, 1))
abline(0, 1, lty = 2, col = grey)
text(0.98, 0.06, paste("Cross-validated AUC", format(round(auc_cv, 2), nsmall = 2)), adj = 1, cex = 0.85, col = ink)
dev.off()

# ---- 9. Build the web report (index.html) from the results above -----------------
if (file.exists("report.R")) source("report.R")
cat("Done. See output/results.txt, figures/ and index.html\n")
