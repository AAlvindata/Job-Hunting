#Today is Amazing!

#Split Brisbane buy data into smaller chunks.

#Library
library(tidyverse)
library(lubridate)
library(RMySQL)

#Connections
elderbrain = dbConnect(MySQL(), user="...", password="...", host="...")


# Loop through each month
results <- tibble()
date_range <- seq.Date(as_date("2020-03-24"),Sys.Date(),by="month")
month_start <- floor_date(date_range, unit="months")
month_end <- ceiling_date(date_range, unit="months") - days(1)
for (i in 1:length(month_start)) {
  print(paste0("Now downloading buy data between ",month_start[i]," and ",month_end[i],"..."))
  
  query <- paste0(
  "
  SELECT
      date_range.date,
      bc.u_id,
      tb1.first_offer,
      lga.id AS lga_code,
      TRIM(REGEXP_SUBSTR(lga.name,'[A-z ]*')) AS lga_name,
      ssc.id AS ssc_code,
      TRIM(REGEXP_SUBSTR(ssc.name,'[A-z ]*')) AS ssc_name,
      real_estate.identify_type(bc.address,bc.property_type) AS property_category,
      sp.as_int AS price
  FROM abs.lga lga
  INNER JOIN abs.ssc ssc ON lga.id = ssc.lga_fk
  LEFT JOIN look_up.suburb_ssc s_ssc ON ssc.id = s_ssc.ssc_fk
  INNER JOIN real_estate.buy_campaign bc ON UPPER(bc.suburb) = s_ssc.suburb 
      AND bc.postcode = s_ssc.postcode 
      AND UPPER(bc.state) = UPPER(s_ssc.state)
  LEFT JOIN real_estate.sale_price sp ON sp.as_str = bc.price
  LEFT JOIN look_up.calendar_days AS date_range ON date_range.date BETWEEN bc.first_date AND bc.last_date
  LEFT JOIN (SELECT
                  u_id,
                  min(first_date) as first_offer
              FROM real_estate.buy_campaign
              WHERE status IS NOT NULL
              GROUP BY u_id
  ) tb1 ON tb1.u_id = bc.u_id
  WHERE 
      lga.id = '31000' AND
      date_range.date BETWEEN '",month_start[i],"' AND '",month_end[i],"' AND
      bc.u_id NOT IN (SELECT DISTINCT u_id FROM real_estate.buy_campaign WHERE property_type = 'residential-land')
  ORDER BY date_range.date
  "
  )
  
  tmp <- NULL
  tmp <- dbGetQuery(elderbrain,query) %>%
    tibble()
  results <- bind_rows(results,tmp)
  print(paste0("...download complete"))
}
write_csv(x = results, file = "31000_buy_data.csv")
print("Great job today!")



