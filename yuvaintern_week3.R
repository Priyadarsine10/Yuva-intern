# ============================================================
# WEEK 3 ASSESSMENT
# STATISTICAL ANALYSIS AND PREDICTIVE MODELING USING R
# India Air Quality Dataset: 2020 - October 2025
# ============================================================

# ------------------------------------------------------------
# 1. Load required packages
# ------------------------------------------------------------

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

library(ggplot2)


# ------------------------------------------------------------
# 2. Load the dataset
# ------------------------------------------------------------

file_path <- "C:/Users/PRIYA/OneDrive/Desktop/YuvaIntern_AQI_Analysis/AQI_2020_to_2025_October.csv"

aqi <- read.csv(
  file_path,
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------
# 3. Basic dataset information
# ------------------------------------------------------------

cat("========================================\n")
cat("WEEK 3 DATASET INFORMATION\n")
cat("========================================\n")

cat("Rows:", nrow(aqi), "\n")
cat("Columns:", ncol(aqi), "\n")

cat("\nColumn Names:\n")
print(names(aqi))

cat("\nDataset Structure:\n")
str(aqi)

cat("\nSummary Statistics:\n")
summary(aqi)


# ------------------------------------------------------------
# 4. Prepare variables
# ------------------------------------------------------------

aqi$Date <- as.Date(
  aqi$Date,
  format = "%d-%m-%Y"
)

aqi$Year <- as.numeric(
  format(aqi$Date, "%Y")
)

aqi$Month <- factor(
  format(aqi$Date, "%B"),
  levels = month.name
)

aqi$City <- factor(aqi$City)

aqi$Air.Quality <- factor(aqi$Air.Quality)


# ------------------------------------------------------------
# 5. Check missing values
# ------------------------------------------------------------

cat("\n========================================\n")
cat("MISSING VALUE CHECK\n")
cat("========================================\n")

print(colSums(is.na(aqi)))


# ------------------------------------------------------------
# 6. Descriptive statistics for AQI
# ------------------------------------------------------------

cat("\n========================================\n")
cat("AQI DESCRIPTIVE STATISTICS\n")
cat("========================================\n")

aqi_statistics <- c(
  Mean = mean(aqi$AQI.Value),
  Median = median(aqi$AQI.Value),
  SD = sd(aqi$AQI.Value),
  Minimum = min(aqi$AQI.Value),
  Maximum = max(aqi$AQI.Value)
)

print(aqi_statistics)


# ------------------------------------------------------------
# 7. Correlation analysis
# ------------------------------------------------------------

cat("\n========================================\n")
cat("CORRELATION: YEAR VS AQI\n")
cat("========================================\n")

year_aqi_correlation <- cor(
  aqi$Year,
  aqi$AQI.Value,
  method = "pearson"
)

print(year_aqi_correlation)


# ------------------------------------------------------------
# 8. Normality check
# Use a random sample because the full dataset is very large
# ------------------------------------------------------------

set.seed(123)

aqi_sample <- sample(
  aqi$AQI.Value,
  5000
)

cat("\n========================================\n")
cat("SHAPIRO-WILK NORMALITY TEST\n")
cat("========================================\n")

shapiro_result <- shapiro.test(
  aqi_sample
)

print(shapiro_result)


# ------------------------------------------------------------
# 9. Hypothesis Testing
# One-way ANOVA: Does average AQI differ by month?
# ------------------------------------------------------------

cat("\n========================================\n")
cat("ONE-WAY ANOVA: AQI BY MONTH\n")
cat("========================================\n")

anova_model <- aov(
  AQI.Value ~ Month,
  data = aqi
)

print(summary(anova_model))


# ------------------------------------------------------------
# 10. Visualization for statistical analysis
# ------------------------------------------------------------

p <- ggplot(
  aqi,
  aes(
    x = Month,
    y = AQI.Value
  )
) +
  geom_boxplot() +
  labs(
    title = "AQI Distribution Across Months",
    x = "Month",
    y = "AQI Value"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

print(p)


cat("\n========================================\n")
cat("STEP 1 STATISTICAL ANALYSIS COMPLETED\n")
cat("========================================\n")