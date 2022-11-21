install.packages("tidyverse")
library("tidyverse")

install.packages("corrplot")
library("corrplot")

#download and read file
url <- "https://dax-cdn.cdn.appdomain.cloud/dax-airline/1.0.1/lax_to_jfk.tar.gz"
download.file(url, destfile = "lax_to_jfk.tar.gz")
untar("lax_to_jfk.tar.gz", tar = "internal")
sub_airline <- read_csv("lax_to_jfk/lax_to_jfk.csv",
                        col_types = cols('DivDistance' = col_number(), 
                                         'DivArrDelay' = col_number()))


#boxplot
ggplot(data = sub_airline, mapping = aes(x = Reporting_Airline, y = ArrDelay)) +
  geom_boxplot(fill = "bisque",color = "black", alpha = 0.3) +
  geom_jitter(aes(color = 'blue'), alpha=0.2) +
  labs(x = "Airline") +
  ggtitle("Arrival Delays by Airline") +
  guides(color = "none") +
  theme_minimal() +
  coord_cartesian(ylim = quantile(sub_airline$ArrDelay, c(0, 0.99)))

#load Alaska Airlines data and delete rows with missing departure/arrival data
alaska_flights <- sub_airline %>%
  filter(Reporting_Airline == "AS") %>%
  filter(!is.na(DepDelay) & !is.na(ArrDelay)) %>%
  filter(DepDelay < 40)

ggplot(data = alaska_flights, mapping = aes(x = DepDelay, y = ArrDelay)) +
  geom_point() +
  ggtitle("Alaska Flight Depature Delays vs Arrival Delays")

#list data types in the array
str(sub_airline)

#find the correlation between DepDelayMinutes and ArrDelayMinutes
cor(sub_airline$DepDelayMinutes, sub_airline$ArrDelayMinutes)

#test departure delays as a potential predictor of arrival delays
ggplot(data = sub_airline, mapping = aes(x = DepDelayMinutes, y = ArrDelayMinutes)) +
  geom_point() + 
  geom_smooth(method = "lm", na.rm = TRUE)

cor(sub_airline$DepDelayMinutes, sub_airline$ArrDelayMinutes)

#test weather delays as a potential predictor variable of arrival delays
ggplot(data = sub_airline, mapping = aes(x = WeatherDelay, y = ArrDelayMinutes)) +
  geom_point() + 
  geom_smooth(method = "lm", na.rm = TRUE)

cor(sub_airline$WeatherDelay, sub_airline$ArrDelayMinutes, use = "complete.obs")

#test correlation between carrier delay and arrival delay
ggplot(data = sub_airline, mapping = aes(x = CarrierDelay, y = ArrDelayMinutes)) +
geom_point() + 
  geom_smooth(method = "lm", na.rm = TRUE)

cor(sub_airline$CarrierDelay, sub_airline$ArrDelayMinutes, use = "complete.obs")

#summarize and group data
summary_airline_delays <- sub_airline %>%
  group_by(Reporting_Airline) %>%
  summarize(count = n(), 
            mean = mean(ArrDelayMinutes, na.rm = TRUE),
            std_dev = sd(ArrDelayMinutes, na.rm = TRUE), 
            min = min(ArrDelayMinutes, na.rm = TRUE), 
            median = median(ArrDelayMinutes, na.rm=TRUE),
            iqr = IQR(ArrDelayMinutes, na.rm = TRUE), 
            max = max(ArrDelayMinutes, na.rm = TRUE))

summary_airline_delays

#value counts
sub_airline %>%
  count(Reporting_Airline)

#group by reporting airline and day of week
avg_delays <- sub_airline %>%
  group_by(Reporting_Airline, DayOfWeek) %>%
  summarize(mean_delays = mean(ArrDelayMinutes), .groups = 'keep')

head(avg_delays)

#sort the dataframe
sorted <- avg_delays %>% 
  arrange(desc(mean_delays))

head(sorted)

#change the color

avg_delays %>% 
  ggplot(aes(x = Reporting_Airline, 
             y = DayOfWeek, 
             fill = mean_delays)) +
  
#change the border formatting
  
  geom_tile(color = "white", size = 0.2) +
  
#define gradient color scales
  
  scale_fill_gradient(low = "yellow", high = "red")

#test for correlation
sub_airline %>% 
  select(DepDelayMinutes, ArrDelayMinutes) %>% 
  cor(method = "pearson")

sub_airline %>% 
  cor.test(~DepDelayMinutes + ArrDelayMinutes, data = .) 

#correlation between multiple variables
correlation <- sub_airline %>%
  select(ArrDelayMinutes, DepDelayMinutes, 
         CarrierDelay, WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay) %>% 
  cor(use = "pairwise.complete.obs", method = "pearson")

correlation

#corrplot
numerics_airline <- sub_airline %>%
  select(ArrDelayMinutes, DepDelayMinutes, CarrierDelay,
         WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay)

airlines_cor <- cor(numerics_airline, method = "pearson", use='pairwise.complete.obs')

#analysis of variance
summary_airline_delays <- sub_airline %>%
  group_by(Reporting_Airline) %>%
  summarize(Average_Delays = mean(ArrDelayMinutes, na.rm = TRUE))

summary_airline_delays %>%  
  ggplot(aes(x = Reporting_Airline, y = Average_Delays)) + 
  geom_bar(stat = "identity") +
  ggtitle("Average Arrival Delays by Airline")

#american airlines and alaska airlines
aa_as_subset <- sub_airline %>%
  select(ArrDelay, Reporting_Airline) %>%
  filter(Reporting_Airline == 'AA' | Reporting_Airline == 'AS')

ad_aov <- aov(ArrDelay ~ Reporting_Airline, data = aa_as_subset)
summary(ad_aov)

#american airlines and pan am airlines
aa_pa_subset <- sub_airline %>%
  select(ArrDelay, Reporting_Airline) %>%
  filter(Reporting_Airline == 'AA' | Reporting_Airline == 'PA (1)')

ad_aov <- aov(ArrDelay ~ Reporting_Airline, data = aa_pa_subset)
summary(ad_aov)




col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(airlines_cor, method = "color", col = col(200),  
         type = "upper", order = "hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 45, #Text label color and rotation
)

