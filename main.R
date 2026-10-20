# IN ORDER TO RUN THE CODE, PLEASE DOWNLOAD LIBRARIES LISTED BELLOW:
#tidyverse
#

library(tidyverse)


#PART 1
# load() returns the NAMES of the objects it created; check what you got
loaded <- load("raw_data/dataset_07.RData")
length(loaded)                       # e.g. "myData"
myData <- get(loaded[1])        # rename to something short (adjust if >1 object)


dim(myData)
names(myData)
str(myData)

sapply(myData, class) # AsIs variables being properties and special_features
sapply(myData, typeof) # 
summary(myData)
head(myData)

#AsIs variables; 
class(myData$special_features)
str(myData$special_features)
class(myData$properties)
str(myData$properties)



anyNA(myData)
colSums(is.na(myData))
sum(is.na(myData))

# --- 5. Possible values of categorical variables ---
#Values of categorical variables ( getting from head function - flat_type, rooms, special_features, properties, orientation, floors and n of floors)

unique(myData$flat_type)
unique(myData$rooms)
unique(unlist(myData$special_features)) # unlist due to being lists of categorical variables
unique(unlist(myData$properties)) 
unique(myData$orientation)
unique(myData$floor)
unique(myData$number_of_floors)



count(myData, special_features, sort = TRUE)

 

myData <- myData %>%
  mutate(
    size = parse_number(size),
    room_count = parse_number(rooms))

summary(myData[sapply(myData, is.numeric)])


head(myData, 8) # heres the first 8 individual observations

#PART 2