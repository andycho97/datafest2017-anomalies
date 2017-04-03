library(readr)
library(dplyr)
library(data.table)
library(stringr)
library(ggrepel)

#`----------bubble plot
us_city = read.csv("us.csv")

us_city$user_location_city = us_city$city

us_city_pop = merge(us_city,all)
us_city_pop$city_income = us_city_pop$data

us_city_pop$city_income = as.numeric(us_city_pop$city_income %>% str_replace_all(",", ""))
us_city_pop$srch_destination_name = us_city_pop$srch_destination_name %>% str_replace_all(",.*","")
# us_city_pop <- subset(us_city_pop, us_city_pop$srch_children_cnt!=0)

df <- aggregate(us_city_pop[, c(5,9)], list(us_city_pop$srch_destination_name), mean)
df2 <- aggregate(us_city_pop[, c(5,9)], list(us_city_pop$srch_destination_name), FUN = length)
df2$cnt = df2$srch_children_cnt
df2 <- df2[,c(1,4)]
dfm <- merge(df,df2)
dfm <- dfm[order(-dfm$cnt),]
dfs <- head(dfm,15)

p <- ggplot(data=dfs, aes(x = as.numeric(city_income), y = as.numeric(srch_children_cnt)))
p + geom_point(color = 'blue', size = 3) + geom_label_repel(aes(label=as.character(Group.1)))  + theme_classic(base_size = 16) +
  xlab("Average income (USD)") + ylab("Average number of children")