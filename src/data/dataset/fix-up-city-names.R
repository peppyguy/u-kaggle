# This script cleans up the field that contains city names. There are a lot
# of typos in the city names. The script looks at the city names per zip and
# state, then replaces city names that are almost identical.

# It didnt boost my score, but the code itself might be useful to someone.
# If it helps you, please upvote.

library(readr)
library(dplyr)
library(stringdist)

# Read competition data files:
train <- read_csv("../input/train.csv")

cat("Nr of city names before cleanup:", length(unique(train$VAR_0200)), fill=T)

reviewDupes <- mutate(train, City = VAR_0200, State = VAR_0237, Zip=VAR_0241) %>% 
   select(City, State, Zip) %>%
   mutate(stateZip = paste(Zip, State, sep="_"),
         fullGeoID = paste(City, Zip, State, sep="_")) %>%
   distinct()
potentialDupes <- group_by(reviewDupes, stateZip) %>% 
  dplyr::summarise(n = n(), 
                   altName = first(City), # prettier: most common
                   altID = first(fullGeoID)) %>% 
  filter(n > 1)
dupes <- mutate(left_join(potentialDupes, reviewDupes, by="stateZip"), 
                dist=stringdist(altName, City)) %>% 
                filter(dist >= 1 & dist <= 2)

write_csv(select(dupes, City, State, Zip, altName), "CleanedupCities.csv")

print("Preview:")
print(head(paste(dupes$City, dupes$State, "=>", dupes$altName), 20))

train <- mutate(train, fullGeoID = paste(VAR_0200, VAR_0241, VAR_0237, sep="_"))
train <- left_join(train, select(dupes, altName, fullGeoID), by="fullGeoID") %>%
  mutate(VAR_0200 = ifelse(is.na(altName), VAR_0200, altName)) %>%
  select(-fullGeoID, -altName)
# and do the same for the test set

cat("Nr of city names after cleansing:", length(unique(train$VAR_0200)), fill=T)
