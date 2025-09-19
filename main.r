# install.packages("ggplot2")
library(ggplot2)

set.seed(42)

dir.create("plots", showWarnings = FALSE)

############################
# 1) Load data & basic EDA
############################

csv_path <- "index_1.csv"
if (!file.exists(csv_path)) stop("CSV file 'index_1.csv' not found.")

raw <- read.csv(csv_path, stringsAsFactors = FALSE, check.names = TRUE)
raw <- subset(raw, select = -card)

cat("\n--- Head ---\n"); print(utils::head(raw, 3))
cat("\n--- Str ---\n"); print(str(raw))

coffee <- raw

############################
# 2) Data preparation, cleaning, feature engineering
############################

required_cols <- c("date","datetime","cash_type","money","coffee_name")
missing_req <- setdiff(required_cols, names(coffee))
if (length(missing_req)) {
  stop(paste0("Required column(s) missing: ", paste(missing_req, collapse = ", ")))
}

# 2.1 Trim whitespace
char_ix <- sapply(coffee, is.character)
coffee[char_ix] <- lapply(coffee[char_ix], trimws)

# 2.2 Parse dates & datetimes
coffee$date <- as.Date(coffee$date, format = "%Y-%m-%d")
coffee$datetime <- as.POSIXct(coffee$datetime, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")

# 2.3 Coerce money to numeric & basic validity checks
coffee$money <- suppressWarnings(as.numeric(coffee$money))

# Remove rows with missing critical fields
coffee <- subset(coffee, !is.na(date) & !is.na(datetime) & !is.na(money) & !is.na(coffee_name))

# Drop exact duplicate rows (if any)
coffee <- coffee[!duplicated(coffee), ]

stopifnot(all(coffee$money > 0))

# 2.4 Feature engineering: hour, time_of_day, weekend, month
coffee$hour <- as.integer(format(coffee$datetime, "%H"))

# Time-of-day
# 05-09 Breakfast, 09-12 LateMorning, 12-16 Afternoon, 16-20 Evening, 20-05 Night
cut_td <- cut(coffee$hour,
              breaks = c(-Inf, 5, 9, 12, 16, 20, Inf),
              labels = c("Night","Breakfast","LateMorning","Afternoon","Evening","NightLate"),
              right = FALSE)
cut_td <- factor(ifelse(cut_td == "NightLate", "Night", as.character(cut_td)),
                 levels = c("Breakfast","LateMorning","Afternoon","Evening","Night"))
coffee$time_of_day <- cut_td

# Weekend from date (Sunday=0, Saturday=6)
wday <- as.POSIXlt(coffee$date)$wday
coffee$weekend <- factor(ifelse(wday %in% c(0,6), "Weekend", "Weekday"),
                         levels = c("Weekday","Weekend"))

# Month
coffee$month <- factor(format(coffee$date, "%Y-%m"))

# 2.5 Define binary outcome is_cash
ct <- tolower(as.character(coffee$cash_type))
valid_ct <- ct %in% c("cash","card")
if (!all(valid_ct)) {
  warning("Some cash_type values are neither 'cash' nor 'card'")
}
coffee$is_cash <- ifelse(ct == "cash", 1L,
                         ifelse(ct == "card", 0L, NA_integer_))

# 2.6 Light outlier flagging
q1 <- stats::quantile(coffee$money, 0.25, na.rm = TRUE)
q3 <- stats::quantile(coffee$money, 0.75, na.rm = TRUE)
iqr <- q3 - q1
upper <- q3 + 3 * iqr
lower <- max(0, q1 - 3 * iqr)
coffee$outlier_money <- (coffee$money < lower) | (coffee$money > upper)

cat("\n# Rows:", nrow(coffee),
    "\n# Date range:", format(min(coffee$date), "%Y-%m-%d"), "to", format(max(coffee$date), "%Y-%m-%d"),
    "\n# Unique coffee types:", length(unique(coffee$coffee_name)),
    "\n# % Outlier money (flag only):", round(mean(coffee$outlier_money)*100, 2), "%\n")

############################
# 3) EDA + Visualizations
############################


# 3.1 Descriptive summaries
cat("\n--- Descriptive statistics (money) ---\n")
print(summary(coffee$money))


cat("\n--- Descriptive by weekend ---\n")
print(aggregate(money ~ weekend, data = coffee, function(z) c(n=length(z), mean=mean(z), sd=sd(z), median=median(z))))


cat("\n--- Descriptive by time_of_day ---\n")
print(aggregate(money ~ time_of_day, data = coffee, function(z) c(n=length(z), mean=mean(z), sd=sd(z), median=median(z))))

# 3.2 Visualization 1: Boxplot of money by time-of-day, faceted by weekend
p1 <- ggplot(coffee, aes(x = time_of_day, y = money)) +
  geom_boxplot(outlier.alpha = 0.4) +
  facet_wrap(~ weekend) +
  labs(title = "Money by Time-of-Day and Weekend",
       x = "Time of day", y = "Money") +
  theme_minimal(base_size = 12)


ggsave("plots/box_money_time_weekend.png", p1, width = 9, height = 5, dpi = 150)
cat('Saved: plots/box_money_time_weekend.png\n')

# 3.3 Visualization 2: Heatmap of counts coffee_name x time_of_day
N_TOP <- 8
ct_counts <- sort(table(coffee$coffee_name), decreasing = TRUE)
keep_types <- names(ct_counts)[seq_len(min(N_TOP, length(ct_counts)))]
coffee$coffee_simple <- ifelse(coffee$coffee_name %in% keep_types, coffee$coffee_name, "Other")
coffee$coffee_simple <- factor(coffee$coffee_simple, levels = c(setdiff(keep_types, "Other"), "Other"))


heat_dat <- as.data.frame(table(coffee$coffee_simple, coffee$time_of_day))
colnames(heat_dat) <- c("coffee_simple","time_of_day","n")


# Dynamic label color for contrast
threshold <- median(heat_dat$n, na.rm = TRUE)
heat_dat$label_col <- ifelse(heat_dat$n >= threshold, "white", "black")


p2 <- ggplot(heat_dat, aes(x = time_of_day, y = coffee_simple, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n, colour = label_col), size = 3, fontface = "bold") +
  scale_colour_identity() +
  scale_fill_gradient(low = "#f0f4ff", high = "#0b2e59") +
  labs(title = "Order Counts by Coffee Type and Time-of-Day",
       x = "Time of day", y = "Coffee type", fill = "Count") +
  theme_minimal(base_size = 12)


ggsave("plots/heat_counts_coffee_time.png", p2, width = 9, height = 5, dpi = 150)
cat('Saved: plots/heat_counts_coffee_time.png\n')

#############################################
# 4) Inferential analysis
#############################################

fmt_p <- function(p) ifelse(p < 0.001, "< 0.001", sprintf("= %.3f", p))

# 4.1 Two-way ANOVA
anova_data <- subset(coffee, !is.na(money) & !is.na(time_of_day) & !is.na(weekend))

# 4.2 Estimated cell means with 95% CI plot
cell_stats <- aggregate(money ~ time_of_day + weekend, data = anova_data,
                        function(z) c(mean = mean(z), sd = sd(z), n = length(z)))
cell_stats <- do.call(data.frame, cell_stats)
colnames(cell_stats) <- c("time_of_day","weekend","mean","sd","n")
cell_stats$se <- cell_stats$sd / sqrt(cell_stats$n)
cell_stats$ci_half <- qt(0.975, df = pmax(cell_stats$n - 1, 1)) * cell_stats$se
cell_stats$ci_lo <- cell_stats$mean - cell_stats$ci_half
cell_stats$ci_hi <- cell_stats$mean + cell_stats$ci_half

dodge <- position_dodge(width = 0.4)

p_anova <- ggplot(
  cell_stats,
  aes(x = time_of_day, y = mean,
      group = weekend, color = weekend, shape = weekend, linetype = weekend)
) +
  geom_line(position = dodge, linewidth = 0.8) +
  geom_point(position = dodge, size = 2.8) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), position = dodge, width = 0) +
  labs(
    title = "Estimated means (95% CI): money ~ time_of_day * weekend",
    x = "Time of day", y = "Mean money"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

ggsave("plots/anova_cell_means.png", p_anova, width = 9, height = 5, dpi = 150)
cat('Saved: plots/anova_cell_means.png\n')

anova_fit <- aov(money ~ time_of_day * weekend, data = anova_data)
cat("\n===== Standard Two-way ANOVA: money ~ time_of_day * weekend =====\n"); print(summary(anova_fit))

res <- residuals(anova_fit)
res_shapiro <- try({
  samp <- if (length(res) > 5000) sample(res, 5000) else res
  shapiro.test(samp)
}, silent = TRUE)
if (!inherits(res_shapiro, "try-error")) {
  cat("\nShapiro-Wilk on residuals: W =", round(res_shapiro$statistic, 3), ", p ", fmt_p(res_shapiro$p.value), "\n", sep = "")
}


levene_median_test <- function(y, g1, g2) {
  g <- interaction(g1, g2, drop = TRUE)
  med <- tapply(y, g, median, na.rm = TRUE)
  z <- abs(y - med[g])
  a <- aov(z ~ g)
  out <- summary(a)[[1]]
  list(F = out["g","F value"], p = out["g","Pr(>F)"])
}
lev <- levene_median_test(anova_data$money, anova_data$time_of_day, anova_data$weekend)
cat("Levene-type homogeneity test: F = ", round(lev$F, 3), ", p = ", fmt_p(lev$p), "\n", sep = "")

fit <- fitted(anova_fit)
std_res <- scale(res)[,1]

# Q–Q plot of standardized residuals
qq_df <- data.frame(std_res = std_res)
library(ggplot2)
p_qq <- ggplot(qq_df, aes(sample = std_res)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q–Q plot of standardized residuals",
       x = "Theoretical quantiles", y = "Standardized residuals") +
  theme_minimal(base_size = 12)

ggsave("plots/anova_residuals_qq.png", p_qq, width = 7, height = 5, dpi = 150)
cat('Saved: plots/anova_residuals_qq.png\n')

# Residuals vs fitted
rf_df <- data.frame(fit = fit, std_res = std_res)
p_rv <- ggplot(rf_df, aes(x = fit, y = std_res)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(title = "Residuals vs Fitted",
       x = "Fitted values", y = "Standardized residuals") +
  theme_minimal(base_size = 12)

ggsave("plots/anova_residuals_vs_fitted.png", p_rv, width = 7, height = 5, dpi = 150)
cat('Saved: plots/anova_residuals_vs_fitted.png\n')

# 4.3 Welch-type tests + permutation for interaction if Levene significant
cat("\n===== Variance heterogeneity handling =====\n")
if (!is.na(lev$p) && lev$p < 0.05) {
  cat("Levene significant -> Using Welch-type tests for main effects and a permutation test for interaction.\n")
  # Welch one-way for time_of_day
  welch_time <- oneway.test(money ~ time_of_day, data = anova_data, var.equal = FALSE)
  cat("\nWelch one-way ANOVA for time_of_day:\n"); print(welch_time)
  
  # Welch two-sample t-test
  welch_weekend <- t.test(money ~ weekend, data = anova_data, var.equal = FALSE)
  cat("\nWelch two-sample test for weekend:\n"); print(welch_weekend)
  
  # Permutation (Freedman–Lane) test for the interaction term
  perm_int_test <- function(df, B = 5000, seed = 42) {
    set.seed(seed)
    full <- lm(money ~ time_of_day * weekend, data = df)
    red  <- lm(money ~ time_of_day + weekend, data = df)
    an_cmp <- anova(red, full)
    F_obs <- as.numeric(an_cmp$F[2])
    yhat_red <- fitted(red)
    res_red  <- residuals(red)
    F_perm <- numeric(B)
    for (b in seq_len(B)) {
      y_perm <- yhat_red + sample(res_red, replace = FALSE)
      full_p <- lm(y_perm ~ time_of_day * weekend, data = df)
      red_p  <- lm(y_perm ~ time_of_day + weekend, data = df)
      F_perm[b] <- as.numeric(anova(red_p, full_p)$F[2])
    }
    p_perm <- (1 + sum(F_perm >= F_obs, na.rm = TRUE)) / (B + 1)
    list(F_obs = F_obs, p_perm = p_perm, B = B)
  }
  perm_res <- perm_int_test(anova_data, B = 5000)
  cat(sprintf("\nPermutation test for interaction (Freedman–Lane): F_obs = %.3f, p_perm %s (B = %d)\n",
              perm_res$F_obs, fmt_p(perm_res$p_perm), perm_res$B))
  
} else {
  cat("Levene NOT significant -> Standard ANOVA assumptions acceptable; Welch/permutation not required.\n")
}

## --- 5) Post-hoc for money ~ time_of_day * weekend ---

library(dplyr)
library(rstatix)
library(readr)

dir.create("tables", showWarnings = FALSE, recursive = TRUE)

anova_data <- anova_data %>%
  dplyr::mutate(
    time_of_day = as.factor(time_of_day),
    weekend     = as.factor(weekend)
  )

cat("\n===== Post-hoc: Simple effects (Games–Howell + Welch) =====\n")

# Within each weekend level: pairwise time_of_day via Games–Howell
gh_time_within_weekend <-
  anova_data %>%
  dplyr::group_by(weekend) %>%
  rstatix::games_howell_test(money ~ time_of_day) %>%


  rstatix::adjust_pvalue(method = "holm") %>%
  rstatix::add_significance("p.adj") %>%
  dplyr::ungroup() %>%

  dplyr::select(dplyr::any_of(c(
    "weekend",".y.","group1","group2","n1","n2",
    "estimate","conf.low","conf.high",
    "statistic","t","df","p","p.adj","p.adj.signif"
  ))) %>%
  dplyr::arrange(weekend, group1, group2)

cat("\nPairwise time_of_day (Games–Howell, Holm-adjusted) within each weekend level:\n")
print(gh_time_within_weekend, n = Inf)
readr::write_csv(gh_time_within_weekend, "tables/posthoc_games_howell_time_within_weekend.csv")
cat('Saved: tables/posthoc_games_howell_time_within_weekend.csv\n')

# Within each time_of_day level: weekend difference via Welch two-sample t-test
welch_weekend_within_time <-
  anova_data %>%
  dplyr::group_by(time_of_day) %>%
  rstatix::t_test(money ~ weekend, var.equal = FALSE) %>%
  rstatix::adjust_pvalue(method = "holm") %>%
  rstatix::add_significance("p.adj") %>%
  dplyr::ungroup() %>%

  dplyr::select(dplyr::any_of(c(
    "time_of_day",".y.","group1","group2","n1","n2",
    "estimate","estimate1","estimate2",
    "conf.low","conf.high","statistic","t","df","p","p.adj","p.adj.signif"
  ))) %>%
  dplyr::arrange(time_of_day)

cat("\nWeekend effect (Welch t-test, Holm-adjusted) within each time_of_day level:\n")
print(welch_weekend_within_time, n = Inf)
readr::write_csv(welch_weekend_within_time, "tables/posthoc_welch_weekend_within_time.csv")
cat('Saved: tables/posthoc_welch_weekend_within_time.csv\n')

## C) Summaries of significant contrasts
sig_gh <- dplyr::filter(gh_time_within_weekend, p.adj < 0.05)
sig_wk <- dplyr::filter(welch_weekend_within_time, p.adj < 0.05)

if (nrow(sig_gh)) {
  cat("\nSignificant time_of_day contrasts within weekend (Holm):\n")
  sig_gh %>% dplyr::select(dplyr::any_of(c("weekend","group1","group2","p.adj","p.adj.signif"))) %>% print(n = Inf)
} else {
  cat("\nNo Holm-significant time_of_day contrasts within weekend.\n")
}

if (nrow(sig_wk)) {
  cat("\nSignificant weekend contrasts within time_of_day (Holm):\n")
  sig_wk %>% dplyr::select(dplyr::any_of(c("time_of_day","group1","group2","p.adj","p.adj.signif"))) %>% print(n = Inf)
} else {
  cat("\nNo Holm-significant weekend contrasts within time_of_day.\n")
}

