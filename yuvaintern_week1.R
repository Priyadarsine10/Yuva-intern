#choose file:

file.choose()

#import csv:

aqi <- read.csv(
  "C:\\Users\\PRIYA\\OneDrive\\Desktop\\YuvaIntern_AQI_Analysis\\AQI_2020_to_2025_October.csv",
  stringsAsFactors = FALSE
)

#confirm it loaded:

dim(aqi)

#check the column name:

names(aqi)

head(aqi)

#understand the raw dataset:

str(aqi)
summary(aqi)

#missing values analysis:

colSums(is.na(aqi))

round(
  colSums(is.na(aqi)) / nrow(aqi) * 100,
  2
)

sum(duplicated(aqi))

#check data quality:
unique(aqi$Air.Quality)
table(aqi$Air.Quality)
unique(aqi$S.No)
unique(aqi$Prominent.Pollutant)
head(aqi$Prominent.Pollutant, 10)
range(aqi$AQI.Value)
range(aqi$AQI.Value)

#check the date properly:
head(aqi$Date)
tail(aqi$Date)
length(unique(aqi$Date))

#convert the date:
aqi$Date <- as.Date(aqi$Date, format = "%d-%m-%Y")
class(aqi$Date)
head(aqi$Date)
range(aqi$Date)

#misleading column name:
table(
  aqi$S.No,
  format(aqi$Date, "%B")
)

#create proper month variable:
aqi$Month <- format(aqi$Date, "%B")
head(aqi[, c("Date", "S.No", "Month")])
sum(aqi$S.No != aqi$Month)

#AQI outlier detection using the IQR method:
aqi$Month <- format(aqi$Date, "%B")

head(aqi[, c("Date", "S.No", "Month")])

sum(aqi$S.No != aqi$Month)

#remove the redundant s.no:
aqi$S.No <- NULL
names(aqi)
dim(aqi)

#outlier detection:
Q1 <- quantile(aqi$AQI.Value, 0.25, na.rm = TRUE)
Q3 <- quantile(aqi$AQI.Value, 0.75, na.rm = TRUE)

Q1
Q3

IQR_AQI <- Q3 - Q1

IQR_AQI

lower_bound <- Q1 - 1.5 * IQR_AQI
upper_bound <- Q3 + 1.5 * IQR_AQI

lower_bound
upper_bound

outliers <- aqi[
  aqi$AQI.Value < lower_bound |
    aqi$AQI.Value > upper_bound,
]

nrow(outliers)

round(
  nrow(outliers) / nrow(aqi) * 100,
  2
)
#investigate the outliers:
summary(outliers$AQI.Value)
table(outliers$Air.Quality)
head(
  outliers[
    order(-outliers$AQI.Value),
    c("Date", "City", "Air.Quality", "AQI.Value")
  ],
  10
)
sum(aqi$AQI.Value < 0)

sum(aqi$AQI.Value > 500)
sum(is.na(aqi$AQI.Value))

#Normalization:
aqi$AQI.ZScore <- as.numeric(
  scale(aqi$AQI.Value)
)
head(
  aqi[, c("AQI.Value", "AQI.ZScore")]
)
mean(aqi$AQI.ZScore)
sd(aqi$AQI.ZScore)

#categorical encoding:
aqi$City <- as.factor(aqi$City)

aqi$Air.Quality <- as.factor(aqi$Air.Quality)

aqi$Month <- factor(
  aqi$Month,
  levels = month.name
)
str(
  aqi[
    ,
    c("City", "Air.Quality", "Month")
  ]
)


air_quality_encoded <- model.matrix(
  ~ Air.Quality - 1,
  data = aqi
)
head(air_quality_encoded)
dim(air_quality_encoded)

#descriptive statistics:
aqi_stats <- c(
  Mean = mean(aqi$AQI.Value),
  Median = median(aqi$AQI.Value),
  SD = sd(aqi$AQI.Value),
  Minimum = min(aqi$AQI.Value),
  Maximum = max(aqi$AQI.Value)
)

aqi_stats

quantile(
  aqi$AQI.Value,
  probs = c(0.25, 0.50, 0.75)
)

#air quality category distribution:
air_quality_count <- table(aqi$Air.Quality)

air_quality_count
air_quality_percent <- round(
  prop.table(air_quality_count) * 100,
  2
)

air_quality_percent


#EDA:
hist(
  aqi$AQI.Value,
  breaks = 50,
  main = "Distribution of AQI Values",
  xlab = "AQI Value",
  ylab = "Frequency"
)
