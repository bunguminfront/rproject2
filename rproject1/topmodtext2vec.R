library(stringr)
library(text2vec)
data("movie_review")
# select 500 rows for faster running times
movie_review_test = movie_review[501:1000,]
movie_review_train = movie_review[1:500,]

prep_fun = function(x) {
    x %>%
    # make text lower case
    str_to_lower %>%
    # remove non-alphanumeric symbols
    str_replace_all("[^[:alnum:]]", " ") %>%
    # collapse multiple spaces
    str_replace_all("\\s+", " ")
}
movie_review_train$review = prep_fun(movie_review_train$review)
it = itoken(movie_review_train$review, progressbar = FALSE)
v = create_vocabulary(it) %>%
  prune_vocabulary(doc_proportion_max = 0.1, term_count_min = 5)
vectorizer = vocab_vectorizer(v)
dtm = create_dtm(it, vectorizer)


# LSA
tfidf = TfIdf$new()
lsa = LSA$new(n_topics = 10)

# pipe friendly transformation
dtm_tfidf_lsa = dtm %>%
  fit_transform(tfidf) %>%
  fit_transform(lsa)

# Use it on new data 
new_data = movie_review_test
new_data_dtm_tfidf_lsa =
  new_data$review %>%
  itoken(preprocessor = prep_fun, progressbar = FALSE) %>%
  create_dtm(vectorizer) %>%
  transform(tfidf) %>%
  transform(lsa)
head(new_data_dtm_tfidf_lsa)


#LDA 
tokens = movie_review$review %>%
  tolower %>%
  word_tokenizer
# turn off progressbar because it won't look nice in rmd
it = itoken(tokens, ids = movie_review$id, progressbar = FALSE)
v = create_vocabulary(it) %>%
  prune_vocabulary(term_count_min = 10, doc_proportion_max = 0.2)
vectorizer = vocab_vectorizer(v)
dtm = create_dtm(it, vectorizer, type = "lda_c")

lda_model =
  LDA$new(n_topics = 10, vocabulary = v,
          doc_topic_prior = 0.1, topic_word_prior = 0.01)
doc_topic_distr =
  lda_model$fit_transform(dtm, n_iter = 1000, convergence_tol = 0.01,
                          check_convergence_every_n = 10)

lda_model$plot()