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

# 3.4 Visualization 3: Payment method shares by time-of-day and weekend
pm <- subset(coffee, !is.na(is_cash))
if (nrow(pm) > 0) {
  pm$payment <- factor(ifelse(pm$is_cash == 1L, "Cash", "Card"), levels = c("Card","Cash"))
  p3 <- ggplot(pm, aes(x = time_of_day, fill = payment)) +
    geom_bar(position = "fill") +
    facet_wrap(~ weekend) +
    scale_y_continuous(labels = function(b) paste0(round(b*100), "%")) +
    labs(title = "Payment Share by Time-of-Day and Weekend",
         x = "Time of day", y = "Share (%)", fill = "Payment") +
    theme_minimal(base_size = 12)
  ggsave("plots/payment_share.png", p3, width = 9, height = 5, dpi = 150)
} else {
  cat("\n[Info] Skipped payment share plot: no non-NA values in is_cash.\n")
}
