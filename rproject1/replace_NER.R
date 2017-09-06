extract_keywords <- function(ner_list) {

    #list_match_count <- unlist(lapply(lapply(gregexpr(" ", ner_list), attr, which = "match.length"), sum))
    #onematching <- ner_list[which(list_match_count == 1)]
    onlywords <- ner_list[grep("^[[:alpha:]]{2,} [[:alpha:]]{2,}$", ner_list)]
    #atleastwo_after <- onematching[grep(" [[:alpha:]]{2,}", onematching)]
    #atleastwo_before <- atleastwo_after[grep("[[:alpha:]]{2,} ", atleastwo_after)]

    return(unique(sort(onlywords)))
}

system.time(atleasttwo_before <- extract_keywords(unlist(dj.corpus_tok_PAR)))

system.time(replacements <- unlist(lapply(atleasttwo_before, sub, pattern = " ", replacement = "_")))

tokenize_loop <- function(corpus, keywords, replacements) {
    for (i in 1:length(keywords)) {
        #print(keywords[i])
        #print(replacements[i])
        temp <- tm_map(corpus, toSmth, keywords[i], replacements[i])
        corpus <- temp
    }
    return(corpus)
}

replace_lapply <- function(corpus, kwrp) {

    lapply(kwrp, function(x) { temp <- tm_map(corpus, toSmth, x[[1]], x[[2]])
    corpus <- temp})
    return(corpus)
}

system.time(mydf <- read_csv("all3.csv",
quote = "\"",
progress =  TRUE
))

# create separate fields for the topic categories

topics <- paste(mydf$topic, mydf$category, mydf$industry, mydf$country, mydf$product, mydf$money, mydf$statistics, mydf$government, mydf$contributor, collapse = ",")
topic_list <- unlist(strsplit(topics, ","))

for (i in 1:length(topic_list)) {
    lt <- topic_list[i]
    mydf <- mydf %>% rowwise() %>% mutate(!!lt := grepl(lt, topic))
}
