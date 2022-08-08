#Library
library(tidyverse)
library(lubridate)
library(RMySQL)

#Connections
elderbrain = dbConnect(MySQL(), user="...", password="...", host="...")

#Variables
post_code <- "4305"
ssc_suburb <- "Raceview"
street_name <- "Wildey Street"
n_of_bedrooms <- "3"

#Get street data
query <- paste0("
SELECT 
	     sold_date,
	     address,
       suburb,
       price AS sold_price,
       bedrooms,
       bathrooms
FROM real_estate.sold_properties
WHERE suburb = '",ssc_suburb,"'
  	AND address LIKE '%",street_name,"%'
  	AND postcode = '",post_code,"'
    AND address NOT LIKE '%/%'
    AND address NOT LIKE '%Lot%'
    AND address NOT LIKE '%nit%'
    AND price NOT LIKE '%agent%'
    AND price NOT LIKE '%-%'
    AND property_type = 'house'
")
print('Collecting data...')
ssc_data <- dbGetQuery(elderbrain,query) %>% na.omit() %>% tibble()

#sold price transformation
pure_sold_price <- gsub(",", "", ssc_data$sold_price)
pure_sold_price <- substring(pure_sold_price, 2)
pure_sold_price <- as.numeric(str_sub(pure_sold_price,1,nchar(pure_sold_price)-3))
ssc_data$sold_price <- pure_sold_price

#bedrooms filter
bedroom_n <- ssc_data %>% filter(bedrooms == n_of_bedrooms)

#Viz
sold_price_graphs <- bedroom_n %>% ggplot() +
                                   geom_line(aes(x = sold_date, y = sold_price, group=1), size = 1.3, color = "blue") +
                                   geom_point(x = bedroom_n$sold_date, y = bedroom_n$sold_price, size = 2, color = 'orange') +
                                   facet_wrap(~bathrooms, ncol = 1) +
                                   theme(axis.text.x = element_text(angle = 45, face = "bold"),axis.text.y = element_text(face = "bold")) + 
                                   labs(title = paste0("Sold Price for " ,n_of_bedrooms, " Bedrooms Houses on " ,street_name, ", " ,ssc_suburb, ""), subtitle = "By the No. of Bathrooms", caption = "Data Source: Ripehouse Advisory Database", x = "Sold Date", y = "Sold Price ($, K)")

ggsave(plot = sold_price_graphs, filename =paste0(str_to_lower(str_replace_all(street_name," ","_")), '_', n_of_bedrooms, '_bedrooms_data_graph.png'),units = "mm", width = 180, height = 250)

            