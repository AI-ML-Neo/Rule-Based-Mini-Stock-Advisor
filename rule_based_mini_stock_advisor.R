# rule_based_mini_stock_advisor.R
# Requires: ggplot2
library(ggplot2)

# 1) Sample data
df <- data.frame(
  Day = 1:10,
  Open = c(100.00, 103.00, 105.50, 104.00, 101.00),
  Close = c(103.00, 105.50, 104.00, 101.00, 102.00)
)

# 2) Percent change
df$pct_change <- ((df$Close - df$Open) / df$Open) * 100

# 3) Helper: previous day pct (NA for day 1)
df$prev_pct <- c(NA, df$pct_change[-nrow(df)])

# 4) Apply rules (matching the logic used above)
get_signal <- function(today_pct, prev_pct) {
  today_pos <- !is.na(today_pct) && today_pct > 0
  prev_pos  <- !is.na(prev_pct)  && prev_pct > 0
  today_neg <- !is.na(today_pct) && today_pct < 0
  prev_neg  <- !is.na(prev_pct)  && prev_pct < 0

  buy  <- (today_pct > 2) && prev_pos && today_pos
  sell <- (today_pct < -2) && prev_neg && today_neg
  hold_cond <- (today_pct >= -2 && today_pct <= 2) && !buy && !sell

  if (buy) return("BUY")
  if (sell) return("SELL")
  # per assignment: if no buy/sell triggered, treat as HOLD
  return("HOLD")
}

df$signal <- mapply(get_signal, df$pct_change, df$prev_pct)
df$signal <- factor(df$signal, levels=c("BUY","SELL","HOLD"))

# Print dataframe
print(df)

# 5) Bar chart: percent change per day colored by signal
p1 <- ggplot(df, aes(x=factor(Day), y=pct_change, fill=signal)) +
  geom_bar(stat="identity") +
  labs(title="Daily % Change (colored by signal)", x="Day", y="% change") +
  theme_minimal()
print(p1)

# 6) Histogram: distribution of percent changes
p2 <- ggplot(df, aes(x=pct_change)) +
  geom_histogram(bins=8) +
  labs(title="Histogram of Daily % Changes", x="% change", y="Count") +
  theme_minimal()
print(p2)

# 7) Scatter plot: Open vs Close colored by signal (with labels)
p3 <- ggplot(df, aes(x=Open, y=Close, color=signal, label=Day)) +
  geom_point(size=3) +
  geom_text(nudge_y = 0.6, size=3) +
  labs(title="Open vs Close (colored by signal)", x="Open price", y="Close price") +
  theme_minimal()
print(p3)