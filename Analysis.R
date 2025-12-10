
# Load required libraries
library(tidyverse)
library(ggpubr)
library(readr)
library(rstatix)
library(psych)
library(Hmisc)

setwd("E:/TeamResearch")  

df <- read_csv("flight_data_BOM_BLR.csv", show_col_types = FALSE)

# Quick check of columns
print("Columns present:")
print(colnames(df))
print(glimpse(df))

parse_duration_to_minutes <- function(x) {
  x <- as.character(x)
  
  # Remove "+ 1 DAY" text if present
  x <- str_replace_all(x, "\\+ 1 DAY", "")
  
  # Initialize vectors
  hours <- numeric(length(x))
  minutes <- numeric(length(x))
  
  for (i in seq_along(x)) {
    # Extract numbers from each string
    nums <- as.numeric(str_extract_all(x[i], "\\d+")[[1]])
    
    if (length(nums) == 0) {
      hours[i] <- NA
      minutes[i] <- NA
    } else if (length(nums) == 1) {
      # If only one number and contains "h", it's hours
      if (grepl("h", x[i])) {
        hours[i] <- nums[1]
        minutes[i] <- 0
      } else {
        # If no "h" but has "m", it's minutes
        hours[i] <- 0
        minutes[i] <- nums[1]
      }
    } else {
      # First number is hours, second is minutes
      hours[i] <- nums[1]
      minutes[i] <- nums[2]
    }
  }
  
  return(hours * 60 + minutes)
}

clean_price <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "[^0-9.]", "")
  as.numeric(x)
}

df <- df %>%
  mutate(duration_min = parse_duration_to_minutes(Duration),
         price_num = clean_price(Price))


df_clean <- df %>% filter(!is.na(duration_min) & !is.na(price_num))

cat("Total observations:", nrow(df), "\n")
cat("Observations with valid duration and price:", nrow(df_clean), "\n")

desc_table <- df_clean %>%
  summarise(
    n = n(),
    mean_duration = mean(duration_min),
    sd_duration = sd(duration_min),
    mean_price = mean(price_num),
    sd_price = sd(price_num),
    median_price = median(price_num)
  )

print("Summary statistics:")
print(desc_table)


p_scatter <- ggplot(df_clean, aes(x = duration_min, y = price_num)) +
  geom_point(alpha = 0.7) +
  geom_jitter(width = 0.2, height = 0) +  # avoids overlapping points
  labs(title = "Flight Duration (min) vs Price (INR)",
       x = "Duration (minutes)",
       y = "Price (INR)") +
  theme_minimal()
ggsave("fig_scatter_duration_price.png", p_scatter, width = 8, height = 5, dpi = 300)


p_hist_dur <- ggplot(df_clean, aes(x = duration_min)) +
  geom_histogram(bins = 10, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Flight Duration (minutes)",
       x = "Duration (minutes)",
       y = "Count") +
  theme_minimal()
ggsave("fig_hist_duration.png", p_hist_dur, width = 7, height = 4, dpi = 300)


p_hist_logp <- ggplot(df_clean, aes(x = log(price_num))) +
  geom_histogram(bins = 10, fill = "salmon", color = "black") +
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


spearman_res <- cor.test(df_clean$duration_min, df_clean$price_num, method = "spearman")
cat("Spearman rho =", spearman_res$estimate, " p =", spearman_res$p.value, "\n")


write_csv(df_clean, "flight_data_BOM_BLR_clean.csv")
write_csv(as.data.frame(t(desc_table)), "summary_stats.csv")

print(head(df_clean %>% select(FlightName, FlightCode, Duration, duration_min, Price, price_num), 10))

cat("All plots saved: fig_scatter_duration_price.png, fig_hist_duration.png, fig_hist_logprice.png\n")
