library(dplyr)
library(readr)
library(ggplot2)
setwd("~/Documents/HousingAnalysis/")

# States that I won't live in (because I know nobody at all)
# (Notably I don't think ill of these places, just they're not for me)
unacceptable_states <- c("FL", "GA", "AZ", "United States", "MO", "MD", "NC",
                         "OK", "KY", "LA", "AL", "NM", "NE", "SC", "AR", "KS",
                         "ID", "IA", "MS", "TN")

prices_df <- read_csv("Metro_ZORI_AllHomesPlusMultifamily_SSA.csv") %>%
  select(RegionName, SizeRank,`2019-03`) %>%
  mutate(state=gsub(".*, ", "", RegionName)) %>%
  filter(! state %in% unacceptable_states)

# ----------------------------------
# Constants for Analysis (Assuming all normalized 0 to 1 except rent, so $$$ units on 0-1 interval)
# COVID-era numbers
AVERAGE_RENT <- 3075 # Rent is actually 3075
HAPPY_COW_HITS <- -2000
PUBLIC_TRANSIT <- -1250
HIKE <- -400
FRIENDS <- -400
AUTOCRACY <- 1500 # i.e. moving to Hong Kong in particular, during autocratic decline
MUM_INDEPENDENCE <- 3000 # Living w Mum (tend to argue), and independence therein
FOREIGN <- 500
NO_MOVE_REQUIRED <- -1000
EAST_COAST <- -500
CONNOR <- -500
NEED_CAR <- 1000



# Reads in Google Sheet
housing_df <- read_csv("cities_to_live_in.csv") %>%
  select(-`Misc Notes`, -`Tech Salary Diff`)

# Normalises Data to 0-1 range
normalise<-function(m){
  (m - min(m))/(max(m)-min(m))
}
for(column in colnames(housing_df)) {
  if (column != "City") {
    print(column)
    housing_df[column] <- normalise(housing_df[column])
    print(housing_df[column])
  }
}

# Applies Utility Constants
housing_utility_df <- housing_df %>%
  mutate(`Average Rent`=AVERAGE_RENT*`Average Rent`,
         `Happy Cow Hits`=HAPPY_COW_HITS*`Happy Cow Hits`,
         `Public Transit`=PUBLIC_TRANSIT*`Public Transit`,
         `Walking Dist Hike`=HIKE*`Walking Dist Hike`,
         `Friends`=FRIENDS*`Friends`,
         `Autocracy`=AUTOCRACY*`Autocracy`,
         `Mum_Indpt`=MUM_INDEPENDENCE * `Mum_Indpt`,
         `Foreign Marker`=FOREIGN*`Foreign Marker`,
         `No Move Required`=NO_MOVE_REQUIRED*`No Move Required`,
         `East Coast Modifier`=EAST_COAST*`East Coast Modifier`,
         `Connor`=CONNOR*`Connor`,
         `Need Car`=NEED_CAR*`Need Car`
         ) %>%
  select(-City)
housing_utility_df$Total <- rowSums(housing_utility_df)
housing_utility_df$City <- housing_df$City

ggplot(housing_utility_df, aes(x=City, y=Total)) + geom_col(fill="purple")

