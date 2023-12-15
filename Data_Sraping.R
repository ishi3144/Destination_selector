# Loading the required libraries

library(rvest)
library(dplyr)
library(tidyr)

# The data is dispersed over 3 pages
html_list <- paste0("https://www.holidify.com/country/india/places-to-visit.html",
                    "?pageNum=",c("0","1","2"))

# Creating the empty vectors to concat the data onto
# length of 184 is taken to take care of the overflow
places_to_visit <- character(184)
links <- character(184)
best_time <- character(184)
num_attractions <- character(184)

# Initialising the counter
i <- 1
while(i <= 3) {
  html <- read_html(html_list[i])
  
  places_to_visit[42*(i-1)+1:min(42*i,100)] <- html %>% 
    html_elements(".card-heading") %>%
    html_text() %>%
    substring(., regexpr("[A-Z]",.))
  
  raw_data <- html %>% 
    html_elements(".card.content-card") 
  start_indices <- regexpr("/places/", raw_data)[1:42]+8
  end_indices <- regexpr("/\\\"", raw_data)[1:42]
  links[42*(i-1)+1:min(42*i,100)] <- paste0("https://www.holidify.com/places/",
                                            substr(raw_data, start_indices, end_indices))
  
  best_time[42*(i-1)+1:min(42*i,100)] <-  html %>%
    html_elements(".mb-3") %>%
    html_text() %>%
    .[grepl("Best",.)] %>%
    substring(., 13)
  
  raw_data <- html %>%
    html_elements(".objective") %>%
    html_text()
  start_indices <- regexpr("[0-9]{1,}  Tourist",raw_data)[1:42]
  start_indices[start_indices<0]=0
  num_attractions[42*(i-1)+1:min(42*i,100)] <- substr(raw_data,start_indices, start_indices +2)
  
  i <- i+1
}

# Cleaning the data and variables finally
places_to_visit <- places_to_visit[1:100]
links <- links[1:100]
best_time <- best_time[1:100]
num_attractions <- num_attractions[1:100]
num_attractions <- as.numeric(num_attractions)
# NA Occours on National Parks
num_attractions[is.na(num_attractions)]<-1
rm(list=c("end_indices", "start_indices", "raw_data"))


# Creating the empty vectors to concat the data onto
state <- character(100)
rating <- character(100)
ideal <- character(100)

# Initialising the counter
i <- 1
while(i <= 100){
  html <- read_html(links[i])
  state[i] <- html %>%
    html_elements(".mb-2.font-smaller a") %>%
    html_text()
  rawdata <- html %>%
    html_elements(".objective-information.negative-margin-mobile") %>%
    html_text()
  
  rating[i] <- rawdata %>%
    substr(., regexpr("[0-9].[0-9]",.), regexpr("[0-9].[0-9]",.)+3)
  
  ideal[i] <- rawdata %>%
    substr(., regexpr("[0-9]-[0-9]",.), regexpr("[0-9]-[0-9]",.)+3) %>%
    paste0(., "days")
  i <- i+1
}

# Cleaning the data and variables finally
rating <- as.numeric(rating)
rm("rawdata")

links <- links %>%
  paste0(., "packages.html")

# Initialising the counter
i <- 1
cost <- numeric(100)
duration <- numeric(100)
Mean <- numeric(100)

while(i <= 100){
  # To handle missing data
  if(i %in% c(50, 61, 64, 69, 75, 78, 83, 89, 95, 96, 98, 100)){
    cost[i] <- duration[i] <- Mean[i] <- NA 
    i <- i+1
    next}
  html <- read_html(links[i])
  rawdata <- html %>%
    html_table() %>%
    .[[1]]
  cost[i] <- mean(as.integer(rawdata$Price %>%
                               gsub("[^0-9]", "", .)))
  duration[i] <- mean(as.integer(rawdata$Inclusion %>%
                                   substr(., 1, regexpr("N",.)-2)))
  Mean[i] <- cost[i]/duration[i]
  
  i <- i+1
}

# Cleaning the data and variables finally
rating <- as.numeric(rating)
rm(list = c("rawdata", "i", "links"))

#Creating a Dataframe
df <- data.frame("Places to Visit" = places_to_visit,
                 "State or Union-Terr" = state,
                 "Ideal Time to Visit" = best_time,
                 "Ideal Duration of Visit" = ideal,
                 "Average Cost per Trip" = cost,
                 "Average Stay per Trip in Days" = duration,
                 "Average Cost of Stay per Day" = Mean,
                 "Number of Attractions" = num_attractions,
                 "Rating out of 5" = rating)

# Saving it as csv file
write.csv(df, "Tourism.csv") 
