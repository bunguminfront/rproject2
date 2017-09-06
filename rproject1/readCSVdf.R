system.time(mydf <- read_csv("all3.csv",
quote = "\"",
progress = TRUE
))

# create separate fields for the topic categories

topics <- paste(mydf$topic, mydf$category, mydf$industry, mydf$country, mydf$product, mydf$money, mydf$statistics, mydf$government, mydf$contributor, collapse = ",")
topic_list <- unlist(strsplit(topics, ","))

for (i in 1:length(topic_list)) {
    lt <- topic_list[i]
    mydf <- mydf %>% rowwise() %>% mutate(!!lt := grepl(lt, topic))
}
