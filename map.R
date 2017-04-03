library(readr)
library(dplyr)
library(ggmap)

all <- read_delim('all_merged.csv',delim =",")
all <- all[,c(7,20,34,36,37,9)]
all <- subset(all, all$srch_children_cnt!=0)
# data <- data[,c(3,18,16)]
# data[1] <- NULL
# dest <- read_delim('dest.txt',delim ="\t")
# all <- merge(dest, data, by = "srch_destination_id")
# usmap <- qmap("United States", zoom = 4)
sizedf <- all %>% filter(user_location_country =="UNITED STATES OF AMERICA") %>%
  group_by(srch_destination_name) %>% summarise(count1 = n(), lat = mean(srch_destination_latitude), lon = mean(srch_destination_longitude)) %>% 
  arrange(desc(count1)) %>% mutate(prop = count1/nrow(all))
usmap <- ggmap(get_googlemap(center=c(-98.5795,39.8282), scale=2, zoom=4))
circle_scale_amt <- 0.0005
usmap +
geom_point(aes(x=lon, y=lat), data=sizedf, col="orange", alpha=.4, size=sizedf$count1*circle_scale_amt) + 
scale_size_continuous(range=range(sizedf$count1))
