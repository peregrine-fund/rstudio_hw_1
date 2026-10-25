# IN ORDER TO RUN THE CODE, PLEASE DOWNLOAD LIBRARIES LISTED BELLOW:
#tidyverse
#

(60500618+27037154+96574995)%%60 #calculating our rData number


library(tidyverse)


#PART 1
loaded <- load("raw_data/dataset_07.RData")
loaded[2] # just curious whether theres another object

print(loaded)# could see n of objects by printing instead of blidnly quessing or by length(loaded)                      

myData <- get(loaded[1])      

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

#Values of categorical variables ( getting from head function - flat_type, rooms, special_features, properties, orientation, floors and n of floors)
unique(myData$flat_type)
unique(myData$rooms)


unique(myData$special_features) # dont want this output, should do unlist
unique(unlist(myData$special_features)) # unlist due to being lists of categorical variables
unique(unlist(myData$properties)) 
unique(myData$orientation)
unique(myData$floor)
unique(myData$number_of_floors)



count(myData, special_features, sort = TRUE)

 

myData <- myData %>%
  mutate(
    size = parse_number(size),
    room_count = parse_number(rooms))# really complex could be done much simpler as demonstated bellow

myData$size <- parse_number(myData$size)
myData$room_count <- parse_number(myData$rooms)


summary(myData[sapply(myData, is.numeric)])


head(myData, 8) # heres the first 8 individual observations

#PART 2
cleaned_mydata <- myData |>
  mutate( #creates new cleaned columns to our dataset
    size_m2 = parse_number(as.character(size)), #It converts size to be a string and then it plucks out the numeric value
    nums_rooms = parse_number(as.character(rooms)), #it just extracts the first number (I was surprised that was that simple)
    price_per_m2 = price / size_m2, 
  ) |>
  separate(location, into = c('latitude', 'longitude'), sep = ', ?', convert = TRUE) #separate function splits this column into 2 defined parts, and as there are no ° the sep = ', ?' just splits the text at comma or at any additional blank spaces

save(cleaned_mydata, file = "cleaned_dataset.RData") #probably useless, I've just saved to my local directory

#Part 3
means_types_apts <- cleaned_mydata |>
  group_by(flat_type) |> #sorts the data by types, penthouse, etc.
  summarise(
    count = n(), 
    mean_price = mean(price, na.rm = TRUE), #calculates the mean and exludes all missing values
    median_price = median(price, na.rm = TRUE), #-//-
    mean_size = mean(size_m2, na.rm = TRUE), #-//-
    mean_price_m2 = mean(price_per_m2, na.rm = TRUE) #-//-
  )
print(arrange(means_types_apts, mean_price_m2)) #prints the summary and also arranges the prices per m2 from lowest to highest

orientation_apartmens <- cleaned_mydata |> 
  group_by(nums_rooms, orientation) |>  #group apartments by orientation and number of rooms
  summarise(
    count= n(),
    mean_price_m2 = mean(price_per_m2, na.rm = TRUE),
  ) |>
  filter(count>4) #only count sufficiently large samples

print(orientation_apartmens)
arrange(orientation_apartmens,mean_price_m2)


# does not work as if is not made for vectors
#app_floor <- cleaned_mydata |>
 # mutate(
  #  if (number_of_floors == floor)
   # {floor_type = "top"
  #  } else if( floor == 0){
  #    floor_type = "ground"
   # } else{
    #  floor_type = "intermediate"
  #  },)|>
#  group_by(floor_type)



app_floor <- cleaned_mydata |>
  mutate(
    floor_type = if_else(floor ==0, "ground",
                if_else(floor == number_of_floors, "top", "intermediate")) #finding where is floor
  ) |>
  group_by(floor_type)|>
  summarise(
    count= n(),
    mean_price_m2 = mean(price_per_m2, na.rm = TRUE),
    price = mean(price, na.rm = TRUE),
    size = mean(size, na.rm = TRUE),
  )

app_floor = na.omit(app_floor) #remove na
print(app_floor)
    
#Part 4
has_property_feature <- function(list_properties_features, target) { 
  sapply(list_properties_features, function(x) target %in% x) #sapply goes through every single apartment and 'target %in% x' it returns TRUE or FALSE, e.g. if the vector contains dishwasher then it returns TRUE to the newly created column in the cleaned dataset
}
cleaned_mydata <- cleaned_mydata |>
  mutate( #adds new columns whether the apartment has these properties
    has_garage = has_property_feature(cleaned_mydata$special_features, 'garage'),
    has_balcony = has_property_feature(cleaned_mydata$special_features, 'balcony'),
    has_dishwasher = has_property_feature(cleaned_mydata$properties, 'dishwasher')
  )
sum(cleaned_mydata$has_dishwasher, na.rm = TRUE)#just to check whether they are properties that are having one of the following features
sum(cleaned_mydata$has_balcony, na.rm = TRUE)
sum(cleaned_mydata$has_garage, na.rm = TRUE)
#Part 5
variables <- c(
  "price",
  "size",
  "room_count",
  "price_per_m2",
  "floor",
  "number_of_floors"
)

observs = integer(length(variables)) #creating vectors for variables 
missings = integer(length(variables))
means = integer(length(variables))
medians = integer(length(variables))
std_devs = integer(length(variables))
mins = integer(length(variables))
maxs = integer(length(variables))



for (i in seq_along(variables)) {
  observs[i] <- length(cleaned_mydata[[variables[i]]]) #calculating values of variables using for loop
  missings[i] <- sum(is.na(cleaned_mydata[[variables[i]]]))
  means[i] <- mean(cleaned_mydata[[variables[i]]], na.rm = TRUE)
  medians[i] <- median(cleaned_mydata[[variables[i]]], na.rm = TRUE)
  std_devs[i] <- sd(cleaned_mydata[[variables[i]]], na.rm = TRUE)
  mins[i] <- min(cleaned_mydata[[variables[i]]], na.rm = TRUE)
  maxs[i] <- max(cleaned_mydata[[variables[i]]], na.rm = TRUE)
}

obs_data = cleaned_mydata  #list for lapply
obs_data = obs_data[variables]


rep_ops <- tibble(
  measured_thing = variables,
  observs = observs,
  observs2 = lapply(obs_data, function(x) length(x)), #calculating values using lapply
  missings,
  missings2 = lapply(obs_data, function(x) sum(is.na(x))),
  means,
  means2 = lapply(obs_data, mean, na.rm = TRUE),
  medians,
  medians2 = lapply(obs_data, median, na.rm = TRUE),
  std_devs,
  std_devs2 = lapply(obs_data, sd, na.rm = TRUE),
  mins,
  mins2 = lapply(obs_data, min, na.rm = TRUE),
  maxs,
  maxs2 = lapply(obs_data, max, na.rm = TRUE),
)         
#Part 6
matching_apartments <- cleaned_mydata |>
  filter( #filtering apartments for potential customer by given parameters
    price <= 12000000,
    size_m2 >= 70,
    nums_rooms >= 3,
    number_of_floors > 0,
   (has_garage | has_balcony), #filters apartments that have garage OR balcony
    has_dishwasher #'no-go' around parameter, the apartments need to have dishwasher
  ) |>
  arrange(price_per_m2) |> #this automatically arranges apartments from lowest to the highest prices
  head(3) #shows us the top 3 apartments with given parameters and lowest price per m2
  
print(matching_apartments)
