library(readr)
library(dplyr)
library(tm)
library(LDAvis)

setwd("C:\\Users\\bungum\\Documents")
# saving code
system.time(save(mydf, file = c("mydf_full_columns_fixed_1346_with_date.RData")))
system.time(save(dj.corpus_tok, file = c("dj.corpus_tok.RData")))
system.time(save(dj.select, file = c("dj.select.RData")))
system.time(save(dj.select, file = c("dj.select_ibm_full_origdate.RData")))
system.time(save(isindf, file = c("isindf_ibm.RData")))
system.time(save(isindf, file = c("isindf_ibm_full_origdate.RData")))

system.time(save(dj.select, file = c("dj.select_ibm_1y.RData")))
system.time(save(isindf, file = c("isindf_ibm_1y_full_origdate.RData")))

# load code
system.time(load("mydf_full_columns_fixed_1346_with_date.RData"))
system.time(load("dtm_train_1372718x16384.RData"))
system.time(load("dj.corpus_tok.RData"))
system.time(load("dj.select.RData"))
system.time(load("isindf_ibm.RData"))
system.time(load("dj.select_ibm_1y.RData"))
system.time(load("isindf_ibm_1y_full_origdate.RData"))

# Function: Visualize LDA model (create JSON object)
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
    bar <- slam::col_sums(doc_term)
    freq_matrix <- data.frame(ST = names(bar), Freq = bar)
    #freq_matrix <- as.data.frame(t(slam::ro    w_sums(doc_term)))
    ## Convert to json
    json_lda <- LDAvis::createJSON(phi = phi,
                                    theta = theta,
                                    vocab = vocab,
                                    doc.length = doc_length,
                                    term.frequency = freq_matrix$Freq)

    return(json_lda)
}

create_dtm_from_isin <- function(isin_selection, dj.select) {

    # load the bigram tokenizer
    library(NLP)
    BigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

    # create a dtm
    djtopic.dtm <- tm::DocumentTermMatrix(dj.select,
    control = list(
    #stemming = TRUE,
            stopwords = TRUE,
            minWordLength = 2,
            removeNumbers = TRUE,
            removePunctuation = TRUE
            )
            )
    return(djtopic.dtm)
}

remove_sparse_terms <- function(djtopic.dtm) {
    # sum up the dtm rows to gather aggregated counts
    term_tfidf <- tapply(djtopic.dtm$v / slam::row_sums(djtopic.dtm)[djtopic.dtm$i],
    djtopic.dtm$j, mean) * log2(tm::nDocs(djtopic.dtm) / slam::col_sums(djtopic.dtm > 0))
    median <- summary(term_tfidf)[3]

    # only include terms above the median
    djreduced.dtm <- djtopic.dtm[, term_tfidf >= median]
    summary(slam::col_sums(djreduced.dtm))

    return(djreduced.dtm)
}

attribute_topics_to_docs <- function(dj.model, dj.topics) {

    # Creates a dataframe to store the News Number and the most likely topic
    doctopics.df <- as.data.frame(dj.topics)
    doctopics.df <- dplyr::transmute(doctopics.df,
    #NewsId = rownames(doctopics.df),
        NewsId = isin_selection,
        Topic = dj.topics)
    doctopics.df$NewsId <- as.integer(doctopics.df$NewsId)

    return(doctopics.df)
}

label_topics <- function(dj.terms) {

    # label topics
    topicTerms <- tidyr::gather(dj.terms, Topic)
    topicTerms <- cbind(topicTerms, Rank = rep(1:30))
    topTerms <- dplyr::filter(topicTerms, Rank < 4)
    topTerms <- dplyr::mutate(topTerms, Topic = stringr::word(Topic, 2))
    topTerms$Topic <- as.numeric(topTerms$Topic)
    topicLabel <- data.frame()

    for (i in 1:topicNumber) {
        z <- dplyr::filter(topTerms, Topic == i)
        l <- as.data.frame(paste(z[1, 2], z[2, 2], z[3, 2], sep = " "),
            stringsAsFactors = FALSE)
        topicLabel <- rbind(topicLabel, l)
    }

    colnames(topicLabel) <- c("Label")
    return(topicLabel)

}

correlate_topics <- function(isin_selection, dj.model) {

    baz <- strsplit(mydf[isin_selection,]$topic, ",")
    seltops <- unlist(lapply(baz, `[[`, 1))

    # correlate topics
    theta <- as.data.frame(topicmodels::posterior(dj.model)$topics)
    head(theta[1:5])
    phi <- as.data.frame(topicmodels::posterior(dj.model)$terms)
    head(phi[1])

    x <- as.data.frame(row.names(theta), stringsAsFactors = FALSE)
    colnames(x) <- c("NewsId")
    x$NewsId <- as.numeric(x$NewsId)
    theta2 <- cbind(x, theta)
    theta2 <- dplyr::left_join(theta2, FirstCategorybyLesson, by = "NewsId")
    ## Returns column means grouped by catergory
    theta.mean.by <- by(theta2[, 2:28], theta2$Category, colMeans)
    theta.mean <- do.call("rbind", theta.mean.by)

    library(corrplot)
    c <- cor(theta.mean)
    corrplot(c, method = "circle")

    return(theta.mean)
}

most_diagnostic_topics <- function(theta.mean) {

    # most diagnostic topics
    theta.mean.ratios <- theta.mean
    for (ii in 1:nrow(theta.mean)) {
        for (jj in 1:ncol(theta.mean)) {
            theta.mean.ratios[ii, jj] <-
            theta.mean[ii, jj] / sum(theta.mean[ii, - jj])
        }
    }
    topics.by.ratio <- apply(theta.mean.ratios, 1,
        function(x) sort(x, decreasing = TRUE, index.return = TRUE)$ix)

    # The most diagnostic topics per category are found in the theta 1st row of the index matrix:
    topics.most.diagnostic <- topics.by.ratio[1,]
    head(topics.most.diagnostic)
}

find_optimal_topic_number <- function(dj.red.dtm) {

    library(Rmpfr)
    harmonicMean <- function(logLikelihoods, precision = 2000L) {
        llMed <- median(logLikelihoods)
        as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
    }
    k <- 25
    burnin <- 1000
    iter <- 1000
    keep <- 50
    fitted <- topicmodels::LDA(dj.red.dtm, k = k, method = "Gibbs", control = list(burnin = burnin, iter = iter, keep = keep))
    ## assuming that burnin is a multiple of keep
    logLiks <- fitted@logLiks[-c(1:(burnin / keep))]

    ## This returns the harmomnic mean for k = 25 topics.
    harmonicMean(logLiks)

    seqk <- seq(2, 100, 1)
    burnin <- 1000
    iter <- 1000
    keep <- 50
    system.time(fitted_many <- lapply(seqk, function(k) topicmodels::LDA(dj.red.dtm, k = k, method = "Gibbs", control = list(burnin = burnin, iter = iter, keep = keep))))

    cl <- makePSOCKcluster(8)
    setDefaultCluster(cl)

    #adder <- function(a, b) a + b
    #clusterExport(NULL, c('adder'))
    #parLapply(NULL, 1:8, function(z) adder(z, 100))

    clusterExport(cl, c('LDA', 'dj.red.dtm', 'burnin', 'iter', 'keep'))
    parLapply(cl, seqk, function(k)
        LDA(dj.red.dtm, k = k, method = "Gibbs", control = list(burnin = burnin, iter = iter, keep = keep)))

    # extract logliks from each topic
    logLiks_many <- lapply(fitted_many, function(L) L@logLiks[-c(1:(burnin / keep))])

    # compute harmonic means
    hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))

    ldaplot <- ggplot(data.frame(seqk, hm_many), aes(x = seqk, y = hm_many)) + geom_path(lwd = 1.5) +
    theme(text = element_text(family = NULL),
        axis.title.y = element_text(vjust = 1, size = 16),
        axis.title.x = element_text(vjust = -.5, size = 16),
        axis.text = element_text(size = 16),
        plot.title = element_text(size = 20)) +
    xlab('Number of Topics') +
    ylab('Harmonic Mean') +
     annotate("text", x = 25, y = -80000, label = paste("The optimal number of topics is", seqk[which.max(hm_many)])) +
    ggtitle(expression(atop("Latent Dirichlet Allocation Analysis of DJ Newswire", atop(italic("How many distinct topics in the abstracts?"), ""))))

    return(ldaplot)
}

toSpace <- content_transformer(function(x, pattern) gsub(pattern, "", x))
toSmth <- content_transformer(function(x, pattern, smth) gsub(pattern, smth, x))
chophead <- content_transformer(function(x, start) { stop <- nchar(x); substring(x, start, stop) })

tokenize_corp <- function(corpus) {
    newcorp <- corpus %>% tm_map(toSpace, "[0-9]{4}.*\\\\r\\\\t\\\\t\\\\t\\\\t\\\\t\\\\t") %>%
    tm_map(toSpace, "\\\\r\\\\t\\\\t\\\\r [[:space:]]+Email.*\\)\\\\r\\\\t") %>%
    tm_map(toSpace, "\\\\r\\\\t\\\\t\\\\r [[:space:]]+Write.*\\)\\\\r\\\\t") %>%
    tm_map(toSpace, "\\\\r\\\\t\\\\t\\\\r [[:space:]]+\\(MORE TO FOLLOW.*\\)\\\\r\\\\t") %>%
    tm_map(toSpace, "\\\\r\\\\t\\\\t\\\\r [[:space:]]+\\(END\\).*\\)\\\\r\\\\t") %>%
    tm_map(toSpace, "\\\\r\\\\t\\\\t\\\\r") %>%
    tm_map(toSpace, "\"")
    #tm_map(toSpace, "\\( \\w+.*\\w+ @.* \\)")
    #tm_map(replacener)
    return(newcorp)
}

# import the column in the dataframe as a tm corpus object (presupposes a mydf from the file cvsreader.R
b <- VectorSource(mydf$replace)
#dj.corpus <- Corpus(b, 
#readerControl = list(language = "eng", reader = readPlain))

# Need VCorpus in order exploit bigram tokenization
dj.corpus <- VCorpus(b,
          readerControl = list(language = "eng", reader = readPlain))
test.select <- test.corpus[isin_selection]

vw <- "DE0007664039"
stl <- "NO0010096985"
ibm <- "US4592001014"
tesla <- "US88160R1014"

isin_selection <- which(grepl(ibm, mydf$isins) & !mydf$NENG & mydf[,]$date > as.Date("2016-07-01", "%Y-%m-%d") & mydf[,]$date < as.Date("2017-06-01", "%Y-%m-%d"))
#isin_selection <- which(grepl(ibm, mydf$isins) & !mydf$NENG)
length(isin_selection)
dj.select <- dj.corpus[isin_selection]

system.time(dj.select_tok <- tokenize_corp(dj.select))
system.time(dj.corpus_tok <- tokenize_corp(dj.corpus))


dj.dtm <- create_dtm_from_isin(isin_selection, dj.select_tok)
dj.red.dtm <- remove_sparse_terms(dj.dtm)
dj.red.dtm

# select number of topics
topicNumber <- 10
# Run model
system.time(dj.model <- topicmodels::LDA(dj.red.dtm,
topicNumber,
method = "Gibbs",
control = list(iter = 2000, seed = 0622)))

## Adds topic number to original dataframe of lessons
## dj.display <- dplyr::inner_join(dj.display, doctopics.df, by = "LessonId")

## Pull out topics and terms 
dj.topics <- topicmodels::topics(dj.model, 1)
dj.terms <- as.data.frame(topicmodels::terms(dj.model, 30), stringsAsFactors = FALSE)
dj.terms[1:5]

doctopics.df <- attribute_topics_to_docs(dj.model, dj.topics)
doctopics.df[which(doctopics.df$Topic == 1),]$NewsId

topicLabel <- label_topics(dj.terms)
topicLabel

dj.json <- topicmodels_json_ldavis(dj.model, dj.select, dj.red.dtm)
serVis(dj.json)