library(text2vec)
library(data.table)
data("movie_review")
setDT(movie_review)
setkey(movie_review, id)
set.seed(2016L)
all_ids = movie_review$id
train_ids = sample(all_ids, 4000)
test_ids = setdiff(all_ids, train_ids)
train = movie_review[J(train_ids)]
test = movie_review[J(test_ids)]

train = mydf

# define preprocessing function and tokenization function
# create vocabulary with a memory friendly way (lazy function?)
prep_fun = tolower
tok_fun = word_tokenizer

it_train = itoken(train$replace,
             preprocessor = prep_fun,
             tokenizer = tok_fun,
             ids = train$newsid,
             progressbar = TRUE)
t1 = Sys.time()
vocab = create_vocabulary(it_train)
print(difftime(Sys.time(), t1, units = 'sec'))

# Alternatively, create vocabulary by preparing a list first
train_tokens = train$review %>%
  prep_fun %>%
  tok_fun
it_train = itoken(train_tokens,
                  ids = train$id,
# turn off progressbar because it won't look nice in rmd
                  progressbar = T)

vocab = create_vocabulary(it_train)
vocab

vectorizer = vocab_vectorizer(vocab)
t1 = Sys.time()
dtm_train = create_dtm(it_train, vectorizer)
print(difftime(Sys.time(), t1, units = 'sec'))

library(glmnet)
NFOLDS = 4
t1 = Sys.time()
glmnet_classifier = cv.glmnet(x = dtm_train, y = train[['CYC']],
                              family = 'binomial',
                                # L1 penalty
                              alpha = 1,
                                # interested in the area under ROC curve
                              type.measure = "auc",
                                # 5-fold cross-validation
                              nfolds = NFOLDS,
                                # high value is less accurate, but has faster training
                              thresh = 1e-3,
                                # again lower number of iterations for faster training
                              maxit = 1e3)
print(difftime(Sys.time(), t1, units = 'sec'))

plot(glmnet_classifier)


# feature hashing 
h_vectorizer = hash_vectorizer(hash_size = 2^14, ngram = c(1L, 2L))

t1 = Sys.time()
dtm_train = create_dtm(it_train, h_vectorizer)
print(difftime(Sys.time(), t1, units = 'sec'))

t1 = Sys.time()
glmnet_classifier = cv.glmnet(x = dtm_train, y = train[['CYC']],
                             family = 'binomial',
                             alpha = 1,
                             type.measure = "auc",
                             nfolds = 5,
                             thresh = 1e-3,
                             maxit = 1e3)
print(difftime(Sys.time(), t1, units = 'sec'))

plot(glmnet_classifier)

ss <- dtm_train[1:1000,]
dim(ss)

library(caret)
ctrl <- trainControl(method = "cv", savePred = T, classProb = T)
mod <- train(Species ~ ., data = ss[1:800], method = "svmLinear", trControl = ctrl)
head(mod$pred)