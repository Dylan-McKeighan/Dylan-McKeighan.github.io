library(tidyverse)
library(rvest)

cities <- c("College-Station_TX", "Orlando_FL", "Miami_FL", "Columbus_OH", 
            "Gainesville_FL", "Austin_TX", "Minneapolis_MN", "Atlanta_GA",
            "Tempe_AZ", "Champaign_IL", "Tampa_FL", "East-Lansing_MI",
            "University-Park_IL", "Bloomington_IN")

df1 = data.frame()

for (city in cities) {
  city_page = paste0("https://www.realtor.com/realestateandhomes-search/", city, "/beds-1/sqft-500/pg-")
  
  for (page_result in 1:8) {
    
    link = paste0(city_page, page_result, "")
    Sys.sleep(time = 10)
    page = read_html(link)
    
    address = page %>% html_nodes(".srp-address-redesign") %>% html_text()
    price = page %>% html_nodes(".srp-page-price > .bowEcH") %>% html_text()
    beds = page %>% html_nodes(".srp_list:nth-child(1)") %>% html_text()
    baths = page %>% html_nodes(".srp_list:nth-child(2)") %>% html_text()
    sq_ft = page %>% html_nodes(".srp_list:nth-child(3)") %>% html_text()
    location = rep(city, length(address))
    
    df1 = rbind(df1, data.frame(address, price, beds, baths, sq_ft, location,
                                stringsAsFactors = FALSE))
    
    print(paste("Page:", page_result))
    
    Sys.sleep(time = 5)
  }
  
  Sys.sleep(time=20)
  
  
}

write_csv(df1, file = "data/realtor.csv")



