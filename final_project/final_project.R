library(tidyverse)
library(rvest)
library(readxl)

###### Scraping the Wikipedia Page for most populous universities #####
page <- read_html("https://en.wikipedia.org/wiki/List_of_United_States_public_university_campuses_by_enrollment")
campus <- page %>% 
  html_elements("td:nth-child(2) > a") %>% html_text()
city <- page %>% 
  html_elements("td:nth-child(3) > a") %>% html_text()
pop <- page %>% 
  html_elements("td:nth-child(4)") %>% html_text()


city <- city[!city == "Minneapolis"]
city <- city[!city == "Urbana"]
city <- city[!city == "Miami"]
city <- city[!city == "Minnesota"]

city[city == "Saint Paul, Minnesota"] <- "Minneapolis/Saint Paul, Minnesota"
city[city == "Minneapolis-Saint Paul"] <- "Minneapolis/Saint Paul, Minnesota"
city[city == "Champaign, Illinois"] <- "Urbana/Champaign, Illinois"
city[city == "Florida"] <- "Miami, Florida"


pop <- str_remove_all(pop, "\n")
pop <- str_remove_all(pop, ",")
pop <- str_remove_all(pop, ">")
pop <- as.numeric(pop)


df <- data.frame(campus, str_split_fixed(city, ", ", 2), pop)
names(df)[names(df) == "X1"] <- "city"
names(df)[names(df) == "X2"] <- "state"

year1 <- c(rep("2021", 10), rep("2020", 10), rep("2019", 10), rep("2018", 10), 
           rep("2017", 10), rep("2016", 10), rep("2015", 10), rep("2014", 10), 
           rep("2013", 10), rep("2012", 10), rep("2011", 10), rep("2010", 10))
year2<- c(rep("2020", 10), rep("2019", 10), rep("2018", 10), rep("2017", 10), 
          rep("2016", 10), rep("2015", 10), rep("2014", 10), rep("2013", 10), 
          rep("2012", 10), rep("2011", 10), rep("2010", 10), rep("2009", 10))
year1 <- as.numeric(year1)
year2 <- as.numeric(year2)
df$year_finish <- year1
df$year_start <- year2
df <- df %>% 
  select(year_start, year_finish, everything())
df$state[df$state == "TX"] <- "Texas"


df %>%
  ggplot(aes(x=state, y=pop, fill=campus)) +
  geom_col() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  facet_wrap(~year_finish, scales = "free")

df %>%
  ggplot(aes(x=city, y=pop, fill=campus)) +
  geom_col() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  facet_wrap(~year_finish, scales = "free")


sum(str_count(df$campus, "Texas A&M University")) 

appearances <- df %>% count(campus, name = "num")
appearances <- arrange(appearances, desc(num))
most_popular <- appearances[appearances$num > 10,]

most_popular

top_8 <- c("Arizona State University", "Ohio State University",
           "Texas A&M University", "University of Central Florida",
           "University of Florida", "University of Minnesota", 
           "Florida International University", " University of Texas at Austin")



df %>% 
  ggplot(aes(x = year_finish, y =pop)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~campus) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

living <- read_xlsx("data/commuter.xlsx")

merge(x=df, y=living, by.x = df$campus, by.y = living$campus)

write.csv(df, file = "data/population.csv", row.names = FALSE)


df$off_camp <- NA
df$off_camp[df$campus == "Texas A&M University"] <- 0.8
df$off_camp[df$campus == "University of Central Florida"] <- 0.85
df$off_camp[df$campus == "Ohio State University"] <- 0.76
df$off_camp[df$campus == "University of Florida"] <- 0.84
df$off_camp[df$campus == "Florida International University"] <- 0.95
df$off_camp[df$campus == "University of Illinois at Urbana–Champaign"] <- 0.50
df$off_camp[df$campus == "Arizona State University"] <- 0.81
df$off_camp[df$campus == "Georgia State University"] <- 0.85
df$off_camp[df$campus == "University of Minnesota"] <- 0.87
df$off_camp[df$campus == "University of Texas at Austin"] <- 0.92
df$off_camp[df$campus == "University of South Florida"] <- 0.90
df$off_camp[df$campus == "Michigan State University"] <- 0.94
df$off_camp[df$campus == "Indiana University"] <- 0.73
df$off_camp[df$campus == "Pennsylvania State University"] <- 0.74
df$off_camp[df$campus == "Indiana University Bloomington"] <- 0.73

df <- read_csv("data/population_1.csv")

df <- df %>% 
  arrange(desc(year_finish), desc(pop_adjusted))

df %>% 
  group_by(campus) %>% 
  summarise(mean_pop = mean(pop_adjusted)) %>% 
  arrange(desc(mean_pop))

##### Scraping Realtor.com #####
library(tidyverse)
library(rvest)

# College-Station_TX

College_Station_TX = data.frame()

for (page_result in c(1:8)) {
  link = paste0("https://www.realtor.com/realestateandhomes-search/College-Station_TX/beds-1/sqft-500/pg-",page_result, "")
  
  Sys.sleep(time = 10)
  page = read_html(link)
  
  address = page %>% html_nodes(".srp-address-redesign") %>% html_text()
  price = page %>% html_nodes(".srp-page-price > .bowEcH") %>% html_text()
  beds = page %>% html_nodes(".srp_list:nth-child(1)") %>% html_text()
  baths = page %>% html_nodes(".srp_list:nth-child(2)") %>% html_text()
  sq_ft = page %>% html_nodes(".srp_list:nth-child(3)") %>% html_text()
  location = rep("College_Station", length(address))
  
  College_Station_TX = rbind(College_Station_TX, data.frame(address, price, beds, baths, sq_ft, location,
                                                            stringsAsFactors = FALSE))
  
  print(paste("Page:", page_result))
}

write_csv(College_Station_TX, file = "data/College_Station_TX.csv")

# Orlando_FL

Orlando_FL = data.frame()

for (page_result in c(1:8)) {
  link = paste0("https://www.realtor.com/realestateandhomes-search/Orlando_FL/beds-1/sqft-500/pg-",page_result, "")
  
  Sys.sleep(time = 10)
  page = read_html(link)
  
  address = page %>% html_nodes(".srp-address-redesign") %>% html_text()
  price = page %>% html_nodes(".srp-page-price > .bowEcH") %>% html_text()
  beds = page %>% html_nodes(".srp_list:nth-child(1)") %>% html_text()
  baths = page %>% html_nodes(".srp_list:nth-child(2)") %>% html_text()
  sq_ft = page %>% html_nodes(".srp_list:nth-child(3)") %>% html_text()
  location = rep("Orlando", length(address))
  
  Orlando_FL = rbind(Orlando_FL, data.frame(address, price, beds, baths, sq_ft, location,
                                            stringsAsFactors = FALSE))
  
  print(paste("Page:", page_result))
}

write_csv(Orlando_FL, file = "data/Orlando_FL.csv")

#### Miami_FL ###

Miami_FL = data.frame()

for (page_result in c(1:8)) {
  link = paste0("https://www.realtor.com/realestateandhomes-search/Miami_FL/beds-1/sqft-500/pg-",page_result, "")
  
  Sys.sleep(time = 10)
  page = read_html(link)
  
  address = page %>% html_nodes(".srp-address-redesign") %>% html_text()
  price = page %>% html_nodes(".srp-page-price > .bowEcH") %>% html_text()
  beds = page %>% html_nodes(".srp_list:nth-child(1)") %>% html_text()
  baths = page %>% html_nodes(".srp_list:nth-child(2)") %>% html_text()
  sq_ft = page %>% html_nodes(".srp_list:nth-child(3)") %>% html_text()
  location = rep("Miami", length(address))
  
  Miami_FL = rbind(Miami_FL, data.frame(address, price, beds, baths, sq_ft, location,
                                        stringsAsFactors = FALSE))
  
  print(paste("Page:", page_result))
}

write_csv(Miami_FL, file = "data/Miami_FL.csv")


### Columbus_OH ###
Columbus_OH = data.frame()

for (page_result in c(1:8)) {
  link = paste0("https://www.realtor.com/realestateandhomes-search/Columbus_OH/beds-1/sqft-500/pg-",page_result, "")
  
  Sys.sleep(time = 10)
  page = read_html(link)
  
  address = page %>% html_nodes(".srp-address-redesign") %>% html_text()
  price = page %>% html_nodes(".srp-page-price > .bowEcH") %>% html_text()
  beds = page %>% html_nodes(".srp_list:nth-child(1)") %>% html_text()
  baths = page %>% html_nodes(".srp_list:nth-child(2)") %>% html_text()
  sq_ft = page %>% html_nodes(".srp_list:nth-child(3)") %>% html_text()
  location = rep("Columbus", length(address))
  
  Columbus_OH = rbind(Columbus_OH, data.frame(address, price, beds, baths, sq_ft, location,
                                              stringsAsFactors = FALSE))
  
  print(paste("Page:", page_result))
}

write_csv(Columbus_OH, file = "data/Columbus_OH.csv")
### Gainesville_FL ###
Gainesville_FL = data.frame()

for (page_result in c(1:8)) {
  link = paste0("https://www.realtor.com/realestateandhomes-search/Gainesville_FL/beds-1/sqft-500/pg-",page_result, "")
  
  Sys.sleep(time = 10)
  page = read_html(link)
  
  address = page %>% html_nodes(".srp-address-redesign") %>% html_text()
  price = page %>% html_nodes(".srp-page-price > .bowEcH") %>% html_text()
  beds = page %>% html_nodes(".srp_list:nth-child(1)") %>% html_text()
  baths = page %>% html_nodes(".srp_list:nth-child(2)") %>% html_text()
  sq_ft = page %>% html_nodes(".srp_list:nth-child(3)") %>% html_text()
  location = rep("Gainesville", length(address))
  
  Gainesville_FL = rbind(Gainesville_FL, data.frame(address, price, beds, baths, sq_ft, location,
                                                    stringsAsFactors = FALSE))
  
  print(paste("Page:", page_result))
}

write_csv(Gainesville_FL, file = "data/Gainesville_FL.csv")
### Austin_TX ###
Austin_TX = data.frame()

for (page_result in c(1:8)) {
  link = paste0("https://www.realtor.com/realestateandhomes-search/Austin_TX/beds-1/sqft-500/pg-",page_result, "")
  
  Sys.sleep(time = 10)
  page = read_html(link)
  
  address = page %>% html_nodes(".srp-address-redesign") %>% html_text()
  price = page %>% html_nodes(".srp-page-price > .bowEcH") %>% html_text()
  beds = page %>% html_nodes(".srp_list:nth-child(1)") %>% html_text()
  baths = page %>% html_nodes(".srp_list:nth-child(2)") %>% html_text()
  sq_ft = page %>% html_nodes(".srp_list:nth-child(3)") %>% html_text()
  location = rep("Austin", length(address))
  
  Austin_TX = rbind(Austin_TX, data.frame(address, price, beds, baths, sq_ft, location,
                                          stringsAsFactors = FALSE))
  
  print(paste("Page:", page_result))
}

write_csv(Austin_TX, file = "data/Austin_TX.csv")
### Minneapolis_MN ###
Minneapolis_MN = data.frame()

for (page_result in c(1:8)) {
  link = paste0("https://www.realtor.com/realestateandhomes-search/Minneapolis_MN/beds-1/sqft-500/pg-",page_result, "")
  
  Sys.sleep(time = 10)
  page = read_html(link)
  
  address = page %>% html_nodes(".srp-address-redesign") %>% html_text()
  price = page %>% html_nodes(".srp-page-price > .bowEcH") %>% html_text()
  beds = page %>% html_nodes(".srp_list:nth-child(1)") %>% html_text()
  baths = page %>% html_nodes(".srp_list:nth-child(2)") %>% html_text()
  sq_ft = page %>% html_nodes(".srp_list:nth-child(3)") %>% html_text()
  location = rep("Minneapolis", length(address))
  
  Minneapolis_MN = rbind(Minneapolis_MN, data.frame(address, price, beds, baths, sq_ft, location,
                                                    stringsAsFactors = FALSE))
  
  print(paste("Page:", page_result))
}

write_csv(Minneapolis_MN, file = "data/Minneapolis_MN.csv")
# Atlanta_GA
Atlanta_GA = data.frame()

for (page_result in c(1:8)) {
  link = paste0("https://www.realtor.com/realestateandhomes-search/Atlanta_GA/beds-1/sqft-500/pg-",page_result, "")
  
  Sys.sleep(time = 10)
  page = read_html(link)
  
  address = page %>% html_nodes(".srp-address-redesign") %>% html_text()
  price = page %>% html_nodes(".srp-page-price > .bowEcH") %>% html_text()
  beds = page %>% html_nodes(".srp_list:nth-child(1)") %>% html_text()
  baths = page %>% html_nodes(".srp_list:nth-child(2)") %>% html_text()
  sq_ft = page %>% html_nodes(".srp_list:nth-child(3)") %>% html_text()
  location = rep("Atlanta", length(address))
  
  Atlanta_GA = rbind(Atlanta_GA, data.frame(address, price, beds, baths, sq_ft, location,
                                            stringsAsFactors = FALSE))
  
  print(paste("Page:", page_result))
}

write_csv(Atlanta_GA, file = "data/Atlanta_GA.csv")
#Tempe_AZ 
Tempe_AZ = data.frame()

for (page_result in c(1:8)) {
  link = paste0("https://www.realtor.com/realestateandhomes-search/Tempe_AZ/beds-1/sqft-500/pg-",page_result, "")
  
  Sys.sleep(time = 10)
  page = read_html(link)
  
  address = page %>% html_nodes(".srp-address-redesign") %>% html_text()
  price = page %>% html_nodes(".srp-page-price > .bowEcH") %>% html_text()
  beds = page %>% html_nodes(".srp_list:nth-child(1)") %>% html_text()
  baths = page %>% html_nodes(".srp_list:nth-child(2)") %>% html_text()
  sq_ft = page %>% html_nodes(".srp_list:nth-child(3)") %>% html_text()
  location = rep("Tempe", length(address))
  
  Tempe_AZ = rbind(Tempe_AZ, data.frame(address, price, beds, baths, sq_ft, location,
                                        stringsAsFactors = FALSE))
  
  print(paste("Page:", page_result))
}

write_csv(Tempe_AZ, file = "data/Tempe_AZ.csv")
# Champaign_IL
Champaign_IL = data.frame()

for (page_result in c(1:8)) {
  link = paste0("https://www.realtor.com/realestateandhomes-search/Champaign_IL/beds-1/sqft-500/pg-",page_result, "")
  
  Sys.sleep(time = 10)
  page = read_html(link)
  
  address = page %>% html_nodes(".srp-address-redesign") %>% html_text()
  price = page %>% html_nodes(".srp-page-price > .bowEcH") %>% html_text()
  beds = page %>% html_nodes(".srp_list:nth-child(1)") %>% html_text()
  baths = page %>% html_nodes(".srp_list:nth-child(2)") %>% html_text()
  sq_ft = page %>% html_nodes(".srp_list:nth-child(3)") %>% html_text()
  location = rep("Champaign", length(address))
  
  Champaign_IL = rbind(Champaign_IL, data.frame(address, price, beds, baths, sq_ft, location,
                                                stringsAsFactors = FALSE))
  
  print(paste("Page:", page_result))
}

write_csv(Champaign_IL, file = "data/Champaign_IL.csv")
# Tampa_FL
Tampa_FL = data.frame()

for (page_result in c(1:8)) {
  link = paste0("https://www.realtor.com/realestateandhomes-search/Tampa_FL/beds-1/sqft-500/pg-",page_result, "")
  
  Sys.sleep(time = 10)
  page = read_html(link)
  
  address = page %>% html_nodes(".srp-address-redesign") %>% html_text()
  price = page %>% html_nodes(".srp-page-price > .bowEcH") %>% html_text()
  beds = page %>% html_nodes(".srp_list:nth-child(1)") %>% html_text()
  baths = page %>% html_nodes(".srp_list:nth-child(2)") %>% html_text()
  sq_ft = page %>% html_nodes(".srp_list:nth-child(3)") %>% html_text()
  location = rep("Tampa", length(address))
  
  Tampa_FL = rbind(Tampa_FL, data.frame(address, price, beds, baths, sq_ft, location,
                                        stringsAsFactors = FALSE))
  
  print(paste("Page:", page_result))
}

write_csv(Tampa_FL, file = "data/Tampa_FL.csv")
# East-Lansing_MI
East_Lansing_MI = data.frame()

for (page_result in c(1:8)) {
  link = paste0("https://www.realtor.com/realestateandhomes-search/East-Lansing_MI/beds-1/sqft-500/pg-",page_result, "")
  
  Sys.sleep(time = 10)
  page = read_html(link)
  
  address = page %>% html_nodes(".srp-address-redesign") %>% html_text()
  price = page %>% html_nodes(".srp-page-price > .bowEcH") %>% html_text()
  beds = page %>% html_nodes(".srp_list:nth-child(1)") %>% html_text()
  baths = page %>% html_nodes(".srp_list:nth-child(2)") %>% html_text()
  sq_ft = page %>% html_nodes(".srp_list:nth-child(3)") %>% html_text()
  location = rep("East_Lansing", length(address))
  
  East_Lansing_MI = rbind(East_Lansing_MI, data.frame(address, price, beds, baths, sq_ft, location,
                                                      stringsAsFactors = FALSE))
  
  print(paste("Page:", page_result))
}

write_csv(East_Lansing_MI, file = "data/East_Lansing_MI.csv")
# University-Park_IL #
Urbana_IL = data.frame()

for (page_result in c(4)) {
  link = paste0("https://www.realtor.com/realestateandhomes-search/Urbana_IL/beds-1/sqft-500/pg-",page_result, "")
  
  Sys.sleep(time = 10)
  page = read_html(link)
  
  address = page %>% html_nodes(".srp-address-redesign") %>% html_text()
  price = page %>% html_nodes(".srp-page-price > .bowEcH") %>% html_text()
  beds = page %>% html_nodes(".srp_list:nth-child(1)") %>% html_text()
  baths = page %>% html_nodes(".srp_list:nth-child(2)") %>% html_text()
  sq_ft = page %>% html_nodes(".srp_list:nth-child(3)") %>% html_text()
  location = rep("Urbana", length(address))
  
  Urbana_IL = rbind(Urbana_IL, data.frame(address, price, beds, baths, sq_ft, location,
                                          stringsAsFactors = FALSE))
  
  print(paste("Page:", page_result))
}

write_csv(Urbana_IL, file = "data/Urbana_IL.csv")
# Bloomington_IN
Bloomington_IN = data.frame()

for (page_result in c(1:8)) {
  link = paste0("https://www.realtor.com/realestateandhomes-search/Bloomington_IN/beds-1/sqft-500/pg-",page_result, "")
  
  Sys.sleep(time = 10)
  page = read_html(link)
  
  address = page %>% html_nodes(".srp-address-redesign") %>% html_text()
  price = page %>% html_nodes(".srp-page-price > .bowEcH") %>% html_text()
  beds = page %>% html_nodes(".srp_list:nth-child(1)") %>% html_text()
  baths = page %>% html_nodes(".srp_list:nth-child(2)") %>% html_text()
  sq_ft = page %>% html_nodes(".srp_list:nth-child(3)") %>% html_text()
  location = rep("Bloomington", length(address))
  
  Bloomington_IN = rbind(Bloomington_IN, data.frame(address, price, beds, baths, sq_ft, location,
                                                    stringsAsFactors = FALSE))
  
  print(paste("Page:", page_result))
}

write_csv(Bloomington_IN, file = "data/Bloomington_IN.csv")




# merging dataframes 
full <- College_Station_TX
list <- c("Orlando_FL", "Miami_FL", "Columbus_OH", 
          "Gainesville_FL", "Austin_TX", "Minneapolis_MN", "Atlanta_GA",
          "Tempe_AZ", "Champaign_IL", "Tampa_FL", "East_Lansing_MI",
          "Urbana_IL", "Bloomington_IN")

full <- merge(full, Orlando_FL, all.x = TRUE, all.y = TRUE)
full <- merge(full, Miami_FL, all.x = TRUE, all.y = TRUE)
full <- merge(full, Columbus_OH, all.x = TRUE, all.y = TRUE)
full <- merge(full, Gainesville_FL, all.x = TRUE, all.y = TRUE)
full <- merge(full, Austin_TX, all.x = TRUE, all.y = TRUE)
full <- merge(full, Minneapolis_MN, all.x = TRUE, all.y = TRUE)
full <- merge(full, Atlanta_GA, all.x = TRUE, all.y = TRUE)
full <- merge(full, Tempe_AZ, all.x = TRUE, all.y = TRUE)
full <- merge(full, Champaign_IL, all.x = TRUE, all.y = TRUE)
full <- merge(full, Tampa_FL, all.x = TRUE, all.y = TRUE)
full <- merge(full, East_Lansing_MI, all.x = TRUE, all.y = TRUE)
full <- merge(full, Urbana_IL, all.x = TRUE, all.y = TRUE)
full <- merge(full, Bloomington_IN, all.x = TRUE, all.y = TRUE)

write.csv(full, file = "data/housing_metrics.csv", row.names = FALSE)

##### Cleaning Realtor.com data #####
housing_metrics <- read_csv("data/housing_metrics.csv")

housing_metrics$price <- str_remove_all(housing_metrics$price, ",")
housing_metrics$price <- str_remove_all(housing_metrics$price, "\\$")
housing_metrics$price <- str_remove_all(housing_metrics$price, "From")
housing_metrics <- housing_metrics[!housing_metrics$price == "Contact For Price",]
housing_metrics$price <- as.numeric(housing_metrics$price)

housing_metrics$beds <- str_remove_all(housing_metrics$beds, "bed")
housing_metrics$beds <- as.numeric(housing_metrics$beds)

housing_metrics$baths <- str_remove_all(housing_metrics$baths, "bath")
housing_metrics$baths <- str_remove_all(housing_metrics$baths, "\\+")
housing_metrics$baths <- str_remove_all(housing_metrics$baths, "sqft")
housing_metrics$baths <- str_remove_all(housing_metrics$baths, ",")
housing_metrics$baths <- as.numeric(housing_metrics$baths)
housing_metrics <- housing_metrics[!housing_metrics$baths > 10,]

housing_metrics$sq_ft <- str_remove_all(housing_metrics$sq_ft, "sqft")
housing_metrics$sq_ft <- str_remove_all(housing_metrics$sq_ft, ",")
housing_metrics$sq_ft <- as.numeric(housing_metrics$sq_ft)

names(housing_metrics)[names(housing_metrics) == "location"] <- "city"

write.csv(housing_metrics, file = "data/housing_metrics_clean", row.names = FALSE)

##### Testing Realtor.com data #####
df <- read_csv("data/housing_metrics_clean")
df <- df[df$beds < 10,]

one_bed <-  df[df$beds == 1,]
two_bed <-  df[df$beds == 2,]
three_bed <-  df[df$beds == 3,]
four_bed <-  df[df$beds == 4,]
five_plus_bed <- df[df$beds > 4,]

avg_price_one_bed <- one_bed %>% 
  group_by(city) %>% 
  summarise(avg_sale_price = mean(price),
            num = n())

avg_price_two_bed <- two_bed %>% 
  group_by(city) %>% 
  summarise(avg_sale_price = mean(price),
            num = n())

avg_price_three_bed <- three_bed %>% 
  group_by(city) %>% 
  summarise(avg_sale_price = mean(price),
            num = n())

df <- df %>% 
  arrange(price)

three_bed <- df %>% 
  filter(beds > 2, baths > 1)
