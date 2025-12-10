
library(tidyverse)    
library(ggpubr)       
library(readr)
library(rstatix)      
library(psych)        
library(Hmisc)        

# Load data
df <- read_csv("flight_data_BOM_BLR.csv", show_col_types = FALSE)

print("Columns present:")
print(colnames(df))
print(glimpse(df))

# Convert "01 h 55 m" -> minutes (numeric)
parse_duration_to_minutes <- function(x) {
  x <- as.character(x)
  # remove any trailing newlines and extra text
  x <- str_replace_all(x, "\\r|\\n|\\+ 1 DAY", "")
  # extract hours and minutes
  h <- as.numeric(str_extract(x, "(\\d+)\\s*h"))
  m <- as.numeric(str_extract(x, "(\\d+)\\s*m"))
  h[is.na(h)] <- 0
  m[is.na(m)] <- 0
  return(h * 60 + m)
}

# Clean price: remove commas, currency symbols, convert to numeric
clean_price <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "[^0-9.]", "")
  as.numeric(x)
}

# --- Apply cleaning ---
df <- df %>%
  mutate(duration_min = parse_duration_to_minutes(Duration),
         price_num = clean_price(Price))

# Check missingness and counts
n_total <- nrow(df)
n_valid <- df %>% filter(!is.na(duration_min) & !is.na(price_num)) %>% nrow()
cat("Total observations:", n_total, "\n")
cat("Observations with valid duration and price:", n_valid, "\n")

# Keep only valid rows for analysis
df_clean <- df %>% filter(!is.na(duration_min) & !is.na(price_num))

# Descriptive stats
desc_table <- df_clean %>%
  summarise(n = n(),
            mean_duration = mean(duration_min),
            sd_duration = sd(duration_min),
            mean_price = mean(price_num),
            sd_price = sd(price_num),
            median_price = median(price_num))
print(desc_table)

# 1) Scatter plot: Duration vs Price
p_scatter <- ggplot(df_clean, aes(x = duration_min, y = price_num)) +
  geom_point(alpha = 0.7) +
  labs(title = "Flight Duration (min) vs Price (INR)",
       x = "Duration (minutes)",
       y = "Price (INR)") +
  theme_minimal()
ggsave("fig_scatter_duration_price.png", p_scatter, width = 8, height = 5, dpi = 300)

# 2) Histogram: Duration
p_hist_dur <- ggplot(df_clean, aes(x = duration_min)) +
  geom_histogram(bins = 10) +
  labs(title = "Histogram of Flight Duration (minutes)",
       x = "Duration (minutes)",
       y = "Count") +
  theme_minimal()
ggsave("fig_hist_duration.png", p_hist_dur, width = 7, height = 4, dpi = 300)

# 3) Histogram: log(price)
p_hist_logp <- ggplot(df_clean, aes(x = log(price_num))) +
  geom_histogram(bins = 10) +
  labs(title = "Histogram of log(Price)",
       x = "log(Price)",
       y = "Count") +
  theme_minimal()
ggsave("fig_hist_logprice.png", p_hist_logp, width = 7, height = 4, dpi = 300)


shapiro_duration <- shapiro_test(df_clean$duration_min)
shapiro_price <- shapiro_test(df_clean$price_num)
cat("Shapiro test for duration: p =", shapiro_duration$p, "\n")
cat("Shapiro test for price: p =", shapiro_price$p, "\n")

pearson_res <- cor.test(df_clean$duration_min, df_clean$price_num, method = "pearson")
cat("Pearson r =", pearson_res$estimate, " p =", pearson_res$p.value, "\n")

# Spearman (rank-based, robust to non-normality)
spearman_res <- cor.test(df_clean$duration_min, df_clean$price_num, method = "spearman")
cat("Spearman rho =", spearman_res$estimate, " p =", spearman_res$p.value, "\n")


# Save a CSV of cleaned data and summary stats for Appendix 
write_csv(df_clean, "flight_data_BOM_BLR_clean.csv")
write_csv(as.data.frame(t(desc_table)), "summary_stats.csv")

# Print sample of cleaned rows for report 
print(head(df_clean %>% select(FlightName, FlightCode, Duration, duration_min, Price, price_num), 10))

cat("All plots saved: fig_scatter_duration_price.png, fig_hist_duration.png, fig_hist_logprice.png\n")
