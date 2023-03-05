# Google Data Analysis Certificate
# Case Study 2: How can a wellness technology company play it smart?

# ***** PREPARING AND CLEANING THE DATA *****
# First, need to import the data using readr.
install.packages("tidyverse")
library(tidyverse)

library(readr)
dailyActivity <- read_csv("raw_data/dailyActivity_merged.csv")
View(dailyActivity)
sleepDay <- read_csv("raw_data/sleepDay_merged.csv")
View(sleepDay)

# Then, we need to clean the data.
library(skimr)
library(here)
library(janitor)

glimpse(dailyActivity)
glimpse(sleepDay)

# need to separate by Id numbers
dailyActivity_ids<- dailyActivity %>% 
  count(Id, sort = TRUE) 
# tells us there are 33 Ids in this dataset
# not everyone used their FitBits everyday, with the lowest being 4 days.
# therefore, need to remove Ids that have less than 31 days

# how many times do logged activities show up
dailyActivity_logged <- dailyActivity %>% 
  select(Id, LoggedActivitiesDistance) %>% 
  filter(LoggedActivitiesDistance > 0) %>% 
  count(Id, sort = TRUE)
# tells us only 4 of the 33 Ids used the logged activity so not as relevant for marketing

# comparing tracker distance with combined very, moderately, light and sedentary active distance to see if they add up
tracker_vs_vmlsdistance <- dailyActivity %>%
  mutate(vmls_distance = VeryActiveDistance + ModeratelyActiveDistance + LightActiveDistance + SedentaryActiveDistance) %>% 
  select(Id, TrackerDistance, vmls_distance)

x = tracker_vs_vmlsdistance$TrackerDistance
y = tracker_vs_vmlsdistance$vmls_distance
cov(x, y, method = "pearson")

# ***** DATA VISUALISATION *****
# compare total steps and calories burnt in the month for varying customers
install.packages(ggplot2)
library(ggplot2)

ggplot(data=dailyActivity, aes(x=TotalSteps, y=Calories, colour=Id)) +  
  geom_point() +
  geom_smooth(method="loess") +
  labs(title="Fitbit Daily Activity: Total Steps vs Calories",
       caption="Data source: https://www.kaggle.com/datasets/arashnic/fitbit") +
  annotate("text", x=25000, y=1500, label="Strong positive relationship.")
# the graph indicates a strong positive relationship between total steps and calories... which is expected.