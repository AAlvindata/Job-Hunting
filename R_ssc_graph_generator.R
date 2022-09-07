#Library
library(tidyverse)
library(lubridate)
library(RMySQL)
source("...")

#Connections
elderbrain = dbConnect(MySQL(), user="...", password="...", host="...")

#Variables
ssc_id <- "32655"
SSC_NAME <- "Springfield Lakes"
CAPTION = "..."

#Get ssc data
query <- paste0("
  SELECT
    ap.date,
    ap.90_day_q1 AS house_asking_low,
    ap.90_day_median AS house_asking_mid,
    ap.90_day_q3 AS house_asking_high,
    sp.90_day_q1 AS house_sold_low,
    sp.90_day_median AS house_sold_mid,
    sp.90_day_q3 AS house_sold_high,
    ROUND(d.90_day_median*100,2) AS house_discount,
    sv.count AS house_sold,
    immediate.percent*100 AS house_immediate_sale,
    dos.days AS dos,
    rent.90_day_median AS rent,
    ROUND(mv.90_day_median*100,2) AS vacancy,
    sv.count AS house_sold_count
  FROM warehouse.asking_price ap
  LEFT JOIN warehouse.monthly_vacancy mv ON 
    mv.date = ap.date AND
    mv.id = ap.id AND
    mv.type = ap.type
  LEFT JOIN warehouse.discount d ON 
    d.date = ap.date AND
    d.id = ap.id AND
    d.type = ap.type AND
    d.property_category = ap.property_category
  LEFT JOIN warehouse.sold_volume sv ON 
    sv.date = ap.date AND
    sv.id = ap.id AND
    sv.type = ap.type AND
    sv.property_category = ap.property_category
  LEFT JOIN warehouse.sold_price sp ON 
    sp.date = ap.date AND
    sp.id = ap.id AND
    sp.type = ap.type AND
    sp.property_category = ap.property_category
  LEFT JOIN warehouse.immediate_sale immediate ON 
    immediate.date = ap.date AND
    immediate.id = ap.id AND
    immediate.type = ap.type AND
    immediate.property_category = ap.property_category
  LEFT JOIN warehouse.dos ON 
    dos.date = ap.date AND
    dos.id = ap.id AND
    dos.type = ap.type AND
    dos.property_category = ap.property_category
  LEFT JOIN warehouse.rent ON 
    rent.date = ap.date AND
    rent.id = ap.id AND
    rent.type = ap.type AND
    rent.property_category = ap.property_category
  WHERE
    ap.date IS NOT NULL AND
    ap.id = '",ssc_id,"' AND
    ap.type = 'ssc' AND
    ap.property_category = 'house'
")

ssc_data <- dbGetQuery(elderbrain,query) %>%
  mutate(date = as_date(date))

#Get rental listings
ssc_rentals <- dbGetQuery(elderbrain,paste0("SELECT date, count AS rentals FROM warehouse.rent_inventory WHERE date > CURRENT_DATE - INTERVAL 1 YEAR AND id = '",ssc_id,"' AND type = 'ssc' AND property_category = 'all'"))

rentals_graph <- ssc_rentals %>%
  mutate(date = as_date(date)) %>%
  ggplot(aes(x=date,y=rentals)) +
  geom_col(fill="white") +
  labs(x="",y="",caption = CAPTION,title = paste0("Rentals: ",SSC_NAME," All Properties")) +
  scale_y_continuous(labels = scales::comma)
rentals_graph <- staticRHA(rentals_graph,cornerLogo = F)
rentals_graph

sold_house_graph <- ssc_data %>%
  ggplot() +
  geom_line(aes(x=date,y=house_sold_mid),colour="white",size = 1) +
  geom_smooth(aes(x=date,y=house_sold_mid),colour="blue",linetype="dotted") +
  labs(x="",y="",caption = CAPTION,title = paste0("Sold Prices: ",SSC_NAME," Houses")) +
  scale_y_continuous(labels = scales::dollar)
sold_house_graph <- staticRHA(sold_house_graph,cornerLogo = F)
sold_house_graph

asking_house_graph <- ssc_data %>%
  ggplot() +
  geom_line(aes(x=date,y=house_asking_mid),colour="white",size = 1) +
  geom_smooth(aes(x=date,y=house_asking_mid),colour="blue",linetype="dotted") +
  labs(x="",y="",caption = CAPTION,title = paste0("Asking Prices: ",SSC_NAME," Houses")) +
  scale_y_continuous(labels = scales::dollar)
asking_house_graph <- staticRHA(asking_house_graph,cornerLogo = F)
asking_house_graph

discount_house_graph <- ssc_data %>%
  ggplot() +
  geom_hline(yintercept = 0,colour="red") +
  geom_line(aes(x=date,y=house_discount/100),colour="white",size = 1) +
  geom_smooth(aes(x=date,y=house_discount/100),colour="blue",linetype="dotted") +
  labs(x="",y="",caption = CAPTION,title = paste0("Discounting: ",SSC_NAME," Houses")) +
  scale_y_continuous(labels = scales::percent)
discount_house_graph <- staticRHA(discount_house_graph,cornerLogo = F)
discount_house_graph

immediate_house_graph <- ssc_data %>%
  ggplot() +
  geom_col(aes(x=date,y=house_immediate_sale/100),fill = "white") +
  geom_hline(yintercept = 0.20,colour = "red") +
  geom_smooth(aes(x=date,y=house_immediate_sale/100),colour="blue",se=FALSE) +
  labs(x="",y="",caption = CAPTION,title = paste0("Immediate Sales: ",SSC_NAME," Houses")) +
  scale_y_continuous(labels = scales::percent)
immediate_house_graph <- staticRHA(immediate_house_graph,cornerLogo = F)
immediate_house_graph

rent_house_graph <- ssc_data %>%
  ggplot() +
  geom_line(aes(x=date,y=rent),colour = "white") +
  geom_smooth(aes(x=date,y=rent),colour="blue",se=FALSE) +
  labs(x="",y="",caption = CAPTION,title = paste0("Rent: ",SSC_NAME," Houses")) +
  scale_y_continuous(labels = scales::dollar)
rent_house_graph <- staticRHA(rent_house_graph,cornerLogo = F)
rent_house_graph

vacancy_graph <- ssc_data %>%
  ggplot() +
  geom_line(aes(x=date,y=vacancy/100),colour = "white") +
  geom_smooth(aes(x=date,y=vacancy/100),colour="blue",se=FALSE) +
  labs(x="",y="",caption = CAPTION,title = paste0("Vacancy: ",SSC_NAME," All Properties")) +
  scale_y_continuous(labels = scales::percent)
vacancy_graph <- staticRHA(vacancy_graph,cornerLogo = F)
vacancy_graph

sales_graph <- ssc_data %>%
  ggplot() +
  geom_col(aes(x=date,y=house_sold_count),fill = "white") +
  geom_smooth(aes(x=date,y=house_sold_count),colour="blue",se=FALSE) +
  labs(x="",y="",caption = CAPTION,title = paste0("Sold Volume: ",SSC_NAME," Houses")) +
  scale_y_continuous(labels = scales::comma)
sales_graph <- staticRHA(sales_graph,cornerLogo = F)
sales_graph

ggsave(plot = discount_house_graph, filename =paste0(str_to_lower(str_replace_all(SSC_NAME," ","_")), '_discount_house_graph.png'),units = "mm", width = 285, height = 157)
ggsave(plot = asking_house_graph, filename =paste0(str_to_lower(str_replace_all(SSC_NAME," ","_")), '_asking_house_graph.png'),units = "mm", width = 285, height = 157)
ggsave(plot = immediate_house_graph, filename =paste0(str_to_lower(str_replace_all(SSC_NAME," ","_")), '_immediate_house_graph.png'),units = "mm", width = 285, height = 157)
ggsave(plot = rent_house_graph, filename =paste0(str_to_lower(str_replace_all(SSC_NAME," ","_")), '_rent_house_graph.png'),units = "mm", width = 285, height = 157)
ggsave(plot = vacancy_graph, filename =paste0(str_to_lower(str_replace_all(SSC_NAME," ","_")), '_vacancy_graph.png'),units = "mm", width = 285, height = 157)
ggsave(plot = sales_graph, filename =paste0(str_to_lower(str_replace_all(SSC_NAME," ","_")), '_sales_graph.png'),units = "mm", width = 285, height = 157)
ggsave(plot = sold_house_graph, filename =paste0(str_to_lower(str_replace_all(SSC_NAME," ","_")), '_sold_house_graph.png'),units = "mm", width = 285, height = 157)
ggsave(plot = rentals_graph, filename =paste0(str_to_lower(str_replace_all(SSC_NAME," ","_")), '_rentals_graph.png'),units = "mm", width = 285, height = 157)

