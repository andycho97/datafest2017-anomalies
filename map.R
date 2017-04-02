library(readr)
library(dplyr)
library(ggmap)
library(data.table)

setwd('C:/Users/Hui/Desktop/datafest')

all <- read_delim('all_merged',delim ="\t")
data <- data[,c(3,18,16)]
data[1] <- NULL
dest <- read_delim('dest.txt',delim ="\t")
all <- merge(dest, data, by = "srch_destination_id")

usmap <- qmap("United States", zoom = 4)
sizedf <- all %>% filter(user_location_country =="UNITED STATES OF AMERICA") %>%
  group_by(srch_destination_name) %>% summarise(count1 = n(), lat = mean(srch_destination_latitude), lon = mean(srch_destination_longitude)) %>% 
  arrange(desc(count1)) %>% mutate(prop = count1/nrow(all))

circle_scale_amt <- 1

usmap +
geom_point(aes(x=lon, y=lat), data=sizedf, col="orange", alpha=.4, size=sizedf$count1*circle_scale_amt) + 
scale_size_continuous(range=range(sizedf$count1))

