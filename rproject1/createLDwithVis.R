require(tm)
library(topicmodels)
library(readr)

setwd("C:\\Users\\bungum\\Documents")
filename <- "preprocessed50000.csv"

mydata <- read_csv(filename)

b <- VectorSource(mydf$replace)
dj.corpus <- Corpus(b, ## I change DireSource(a) by a
          readerControl = list(language = "eng", reader = readPlain))

system.time(djtopic.dtm <- tm::DocumentTermMatrix(dj.corpus, control = list(stemming = TRUE,
    stopwords = TRUE,
    minWordLength = 2,
    removeNumbers = TRUE,
    removePunctuation = TRUE)))

term_tfidf <- tapply(djtopic.dtm$v / slam::row_sums(djtopic.dtm)[djtopic.dtm$i], djtopic.dtm$j, mean) *
  log2(tm::nDocs(djtopic.dtm) / slam::col_sums(djtopic.dtm > 0))
summary(term_tfidf)
medianVal <- summary(term_tfidf)[3]

djreduced.dtm <- djtopic.dtm[, term_tfidf >= medianVal*12]
summary(slam::col_sums(djreduced.dtm))

# find non-zero rows
rowTotals <- apply(djreduced.dtm, 1, sum)
dtm.new <- djreduced.dtm[rowTotals > 0,]
djcorp.new <- dj.corpus[rowTotals > 0,]

# Run model
system.time(dj.model <- topicmodels::LDA(dtm.new, 27, method = "Gibbs", control = list(iter = 2000, seed = 0622)))

dj.topics <- topicmodels::topics(dj.model, 1)
dj.terms <- as.data.frame(topicmodels::terms(dj.model, 30), stringsAsFactors = FALSE)
dj.terms[1:5]

# label topics
topicTerms <- tidyr::gather(dj.terms, Topic)
topicTerms <- cbind(topicTerms, Rank = rep(1:30))
topTerms <- dplyr::filter(topicTerms, Rank < 4)
topTerms <- dplyr::mutate(topTerms, Topic = stringr::word(Topic, 2))
topTerms$Topic <- as.numeric(topTerms$Topic)
topicLabel <- data.frame()
for (i in 1:27) {
    z <- dplyr::filter(topTerms, Topic == i)
    l <- as.data.frame(paste(z[1, 2], z[2, 2], z[3, 2], sep = " "), stringsAsFactors = FALSE)
    topicLabel <- rbind(topicLabel, l)

}
colnames(topicLabel) <- c("Label")
topicLabel

# correlate topics
theta <- as.data.frame(topicmodels::posterior(dj.model)$topics)
head(theta[1:5])

x <- as.data.frame(row.names(theta), stringsAsFactors = FALSE)
colnames(x) <- c("LessonId")
x$LessonId <- as.numeric(x$LessonId)
theta2 <- cbind(x, theta)
theta2 <- dplyr::left_join(theta2, FirstCategorybyLesson, by = "LessonId")
## Returns column means grouped by catergory
theta.mean.by <- by(theta2[, 2:28], theta2$Category, colMeans)
theta.mean <- do.call("rbind", theta.mean.by)

library(corrplot)
c <- cor(theta.mean)
corrplot(c, method = "circle")


# Visualize LDA model
topicmodels_json_ldavis <- function(fitted, corpus, doc_term) {
    ## Required packages
    library(topicmodels)
    library(dplyr)
    library(stringi)
    library(tm)
    library(LDAvis)

    ## Find required quantities
    phi <- posterior(fitted)$terms %>% as.matrix
    theta <- posterior(fitted)$topics %>% as.matrix
    vocab <- colnames(phi)
    length(vocab)
    doc_length <- vector()
    for (i in 1:length(corpus)) {
        temp <- paste(corpus[[i]]$content, collapse = ' ')
        doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
    }
    #temp_frequency <- inspect(doc_term)
    #freq_matrix <- data.frame(ST = colnames(temp_frequency),
    #                           Freq = colSums(temp_frequency))
    #rm(temp_frequency)
    bar <- slam::col_sums(djreduced.dtm)
    freq_matrix <- data.frame(ST = names(bar), Freq = bar)
    #freq_matrix <- as.data.frame(t(slam::row_sums(doc_term)))
    ## Convert to json
    json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                    vocab = vocab,
                                    doc.length = doc_length,
                                    term.frequency = freq_matrix$Freq)

    return(json_lda)
}

library(qdap)
qdap.freq <- freq_terms(djtopic.dtm, top = 10, at.least = 1, stopwords = Top200Words)

library(LDAvis)
dj.json <- topicmodels_json_ldavis(dj.model, djcorp.new, dtm.new)
serVis(dj.json)


#qdap stuff
tm_dat <- qdap_dat <- DATA[1:4, c(1, 4)]
rownames(tm_dat) <- paste("docs", 1:nrow(tm_dat))
tm_dat <- Corpus(DataframeSource(tm_dat[, 2, drop = FALSE]))

with(qdap_dat, wfm(state, person))

TermDocumentMatrix(tm_dat, control = list(removePunctuation = TRUE, wordLengths = c(0, Inf)))

inspect(TermDocumentMatrix(tm_dat, control = list(removePunctuation = TRUE, wordLengths = c(0, Inf))))


data("crude")
myTdm <- TermDocumentMatrix(crude)
temp <- inspect(myTdm)
FreqMat <- data.frame(ST = rownames(temp), Freq = rowSums(temp))
row.names(FreqMat) <- NULL
FreqMat

as.data.frame(t(slam::row_sums(TermDocumentMatrix(crude))))


phi <- posterior(dj.model)$terms %>% as.matrix
theta <- posterior(dj.model)$topics %>% as.matrix
vocab <- colnames(phi)
length(vocab)
doc_length <- vector()
for (i in 1:length(dj.corpus)) {
    temp <- paste(dj.corpus[[i]]$content, collapse = ' ')
    doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
}
doc_length
#temp_frequency <- inspect(doc_term)
#freq_matrix <- data.frame(ST = colnames(temp_frequency),
#                           Freq = colSums(temp_frequency))
#rm(temp_frequency)
bar <- slam::col_sums(djreduced.dtm)
freq_matrix <- data.frame(ST = names(bar), Freq = bar)
#freq_matrix <- as.data.frame(slam::col_sums(djreduced.dtm))
dim(freq_matrix)
## Convert to json
json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                    vocab = vocab,
                                    doc.length = doc_length,
                                    term.frequency = freq_matrix$Freq)

return(json_lda)
require(tm)
require(readtext)

setwd("C:\\Users\\bungum\\Documents\\GitHub\\financial-news-dataset\\20061020_20131126_bloomberg_news\\2013-11-25")
filenames <- list.files(".")

a <- DirSource(".")
dj.corpus <- Corpus(a, ## I change DireSource(a) by a
          readerControl = list(language = "eng", reader = readPlain))

# Clean corpus
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument, language = "english")

# Create dtm
dtm <- DocumentTermMatrix(docs)

# Remove sparse entries
dtm.sparse <- removeSparseTerms(dtm, 0.93)
dtm.sparse


djtopic.dtm <- tm::DocumentTermMatrix(dj.corpus, control = list(stemming = TRUE, stopwords = TRUE,
                                    minWordLength = 2, removeNumbers = TRUE, removePunctuation = TRUE))

term_tfidf <- tapply(djtopic.dtm$v / slam::row_sums(djtopic.dtm)[djtopic.dtm$i], djtopic.dtm$j, mean) *
  log2(tm::nDocs(djtopic.dtm) / slam::col_sums(djtopic.dtm > 0))
summary(term_tfidf)

djreduced.dtm <- djtopic.dtm[, term_tfidf >= 0.01917]
summary(slam::col_sums(djreduced.dtm))

# Run model
system.time(dj.model <- topicmodels::LDA(djreduced.dtm, 27, method = "Gibbs", control = list(iter = 2000, seed = 0622)))

dj.topics <- topicmodels::topics(llis.model, 1)
dj.terms <- as.data.frame(topicmodels::terms(dj.model, 30), stringsAsFactors = FALSE)
dj.terms[1:5]

# label topics
topicTerms <- tidyr::gather(dj.terms, Topic)
topicTerms <- cbind(topicTerms, Rank = rep(1:30))
topTerms <- dplyr::filter(topicTerms, Rank < 4)
topTerms <- dplyr::mutate(topTerms, Topic = stringr::word(Topic, 2))
topTerms$Topic <- as.numeric(topTerms$Topic)
topicLabel <- data.frame()
for (i in 1:27) {
    z <- dplyr::filter(topTerms, Topic == i)
    l <- as.data.frame(paste(z[1, 2], z[2, 2], z[3, 2], sep = " "), stringsAsFactors = FALSE)
    topicLabel <- rbind(topicLabel, l)

}
colnames(topicLabel) <- c("Label")
topicLabel

# correlate topics
theta <- as.data.frame(topicmodels::posterior(dj.model)$topics)
head(theta[1:5])

x <- as.data.frame(row.names(theta), stringsAsFactors = FALSE)
colnames(x) <- c("LessonId")
x$LessonId <- as.numeric(x$LessonId)
theta2 <- cbind(x, theta)
theta2 <- dplyr::left_join(theta2, FirstCategorybyLesson, by = "LessonId")
## Returns column means grouped by catergory
theta.mean.by <- by(theta2[, 2:28], theta2$Category, colMeans)
theta.mean <- do.call("rbind", theta.mean.by)

library(corrplot)
c <- cor(theta.mean)
corrplot(c, method = "circle")


# Visualize LDA model
topicmodels_json_ldavis <- function(fitted, corpus, doc_term) {
    ## Required packages
    library(topicmodels)
    library(dplyr)
    library(stringi)
    library(tm)
    library(LDAvis)

    ## Find required quantities
    phi <- posterior(fitted)$terms %>% as.matrix
    theta <- posterior(fitted)$topics %>% as.matrix
    vocab <- colnames(phi)
    length(vocab)
    doc_length <- vector()
    for (i in 1:length(corpus)) {
        temp <- paste(corpus[[i]]$content, collapse = ' ')
        doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
    }
    #temp_frequency <- inspect(doc_term)
    #freq_matrix <- data.frame(ST = colnames(temp_frequency),
    #                           Freq = colSums(temp_frequency))
    #rm(temp_frequency)
    bar <- slam::col_sums(djreduced.dtm)
    freq_matrix <- data.frame(ST = names(bar), Freq = bar)
    #freq_matrix <- as.data.frame(t(slam::row_sums(doc_term)))
    ## Convert to json
    json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                    vocab = vocab,
                                    doc.length = doc_length,
                                    term.frequency = freq_matrix$Freq)

    return(json_lda)
}

library(qdap)
qdap.freq <- freq_terms(djtopic.dtm, top = 10, at.least = 1, stopwords = Top200Words)

dj.json <- topicmodels_json_ldavis(dj.model, dj.corpus, djreduced.dtm)
serVis(dj.json)


#qdap stuff
tm_dat <- qdap_dat <- DATA[1:4, c(1, 4)]
rownames(tm_dat) <- paste("docs", 1:nrow(tm_dat))
tm_dat <- Corpus(DataframeSource(tm_dat[, 2, drop = FALSE]))

with(qdap_dat, wfm(state, person))

TermDocumentMatrix(tm_dat, control = list(removePunctuation = TRUE, wordLengths = c(0, Inf)))

inspect(TermDocumentMatrix(tm_dat, control = list(removePunctuation = TRUE, wordLengths = c(0, Inf))))


data("crude")
myTdm <- TermDocumentMatrix(crude)
temp <- inspect(myTdm)
FreqMat <- data.frame(ST = rownames(temp), Freq = rowSums(temp))
row.names(FreqMat) <- NULL
FreqMat

as.data.frame(t(slam::row_sums(TermDocumentMatrix(crude))))


phi <- posterior(dj.model)$terms %>% as.matrix
theta <- posterior(dj.model)$topics %>% as.matrix
vocab <- colnames(phi)
length(vocab)
doc_length <- vector()
for (i in 1:length(dj.corpus)) {
    temp <- paste(dj.corpus[[i]]$content, collapse = ' ')
    doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
}
doc_length
#temp_frequency <- inspect(doc_term)
#freq_matrix <- data.frame(ST = colnames(temp_frequency),
#                           Freq = colSums(temp_frequency))
#rm(temp_frequency)
bar <- slam::col_sums(djreduced.dtm)
freq_matrix <- data.frame(ST = names(bar), Freq = bar)
#freq_matrix <- as.data.frame(slam::col_sums(djreduced.dtm))
dim(freq_matrix)
## Convert to json
json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                    vocab = vocab,
                                    doc.length = doc_length,
                                    term.frequency = freq_matrix$Freq)

return(json_lda)
