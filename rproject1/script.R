library(caret)
library(mlbench)
library(magrittr)

library("AzureML")
ws <- workspace(id = "e12f59ece68c40559b1b83e53ba52030", auth = "sNb9YyIk9NBphMIF8gYbXqP+Ts0SKTRxKFZ3Cai5N1l01N7Mqp71qsDxBmmdwXq1rFh8G5kvG/y4nyZbox228w==", api_endpoint = "https://europewest.studioapi.azureml.net")

ds <- download.intermediate.dataset(ws = ws, node_id = "0b4683ab-89b8-4b40-b372-b3bab93255d8-1829108", experiment = "e12f59ece68c40559b1b83e53ba52030.f-id.fe87bfbfb85140858d69aaee28f44eb7", port_name = "Results dataset", data_type_id = "GenericCSV")
names(ds)

ds <- download.intermediate.dataset(  ws = ws,  node_id = "8f087393-3474-4a94-8bb0-e9f979d044e8-80484",  experiment = "e12f59ece68c40559b1b83e53ba52030.f-id.fe87bfbfb85140858d69aaee28f44eb7",  port_name = "Results dataset",  data_type_id = "GenericCSV")
ds <- download.intermediate.dataset(  ws = ws,  node_id = "8f087393-3474-4a94-8bb0-e9f979d044e8-80484",  experiment = "e12f59ece68c40559b1b83e53ba52030.f-id.fe87bfbfb85140858d69aaee28f44eb7",  port_name = "Results dataset",  data_type_id = "GenericCSV")
ds <- download.intermediate.dataset(  ws = ws,  node_id = "8f087393-3474-4a94-8bb0-e9f979d044e8-80484",  experiment = "e12f59ece68c40559b1b83e53ba52030.f-id.fe87bfbfb85140858d69aaee28f44eb7",  port_name = "Results dataset",  data_type_id = "GenericCSV")
ds <- download.intermediate.dataset(  ws = ws,  node_id = "8f087393-3474-4a94-8bb0-e9f979d044e8-80484",  experiment = "e12f59ece68c40559b1b83e53ba52030.f-id.fe87bfbfb85140858d69aaee28f44eb7",  port_name = "Results dataset",  data_type_id = "GenericCSV")




head(ds)
names(allds)

ds <- download.datasets(dataset = ws, name = "10k_EN_R_script")
valds <- download.datasets(dataset = ws,name = "val_peder_csv")
allds <- download.datasets(dataset = ws, name = "10_en_preprocess_allcat")
# subset dataset for processing

doccol <- allds[1:20000,]

library(NLP)

BigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

library(RWeka)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 3))

library(tm)

# Create corpora objects
docs <- Corpus(VectorSource(doccol$Preprocessed.tokenized_text))
valdocs <- Corpus(VectorSource(valds$Preprocessed.tokenized_text))

toSpace <- content_transformer(function(x, pattern) gsub(pattern, "", x))
toSmth <- content_transformer(function(x, pattern, smth) gsub(pattern, smth, x))
chophead <- content_transformer(function(x, start) {stop <- nchar(x); substring(x, start, stop)} )

filter_corp <- function(corpus) {
    corpus %>% tm_map(chophead, 33) %>%
    tm_map(toSpace, "Subscribe to WSJ.* GMT \\)") %>%
    tm_map(toSpace, "More at.* GMT \\)") %>%
    tm_map(toSpace, "Visit http.* GMT \\)") %>%
    tm_map(toSpace, "opyright 2.* GMT \\)") %>%
    tm_map(toSpace, "Insider Data Source.* GMT \\)") %>%
    tm_map(toSpace, "Write to.* GMT \\)") %>%
    tm_map(toSpace, "\\( MORE TO FOLLOW .* GMT \\)") %>%
    tm_map(toSpace, "\\( END .* GMT \\)") %>%
    tm_map(toSpace, "\\( \\w+.*\\w+ @.* \\)") %>%
    tm_map(toSmth, "\\d+\\w", "") %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, c(stopwords("english"), "gmt")) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace) %>%
    tm_map(stemDocument, language = "english")
    return (corpus)
}

# Filter the document collections according to above function
docs <- filter_corp(docs)
valdocs <- filter_corp(valdocs)

# Create dtm
dtm.ng <- DocumentTermMatrix(docs, control = list(tokenize = BigramTokenizer))
dtm <- DocumentTermMatrix(docs)

# Remove sparse entries
dtm.sparse <- removeSparseTerms(dtm, 0.98)
dtm.sparse <- removeSparseTerms(dtm.ng, 0.99)
dtm.sparse

inspect(dtm.sparse[1,])

# output the vocabulary for vectorizing the validation data
trainVocab <- Terms(dtm.sparse)

valdtm_create <- DocumentTermMatrix(valdocs)
#valdtm_createVocab <- Terms(valdtm_create)
valdtm.sparse <- removeSparseTerms(valdtm_create, 0.95)
valdtm_createVocab <- Terms(valdtm.sparse)

valdtm_from_train_dtm <- DocumentTermMatrix(valdocs, list(dictionary = trainVocab))
train_dtm_from_val <- DocumentTermMatrix(docs, list(dictionary = valdtm_createVocab))


prepare_for_classifier_new <- function(dtm, docs, tag) { 
    nmat.df <- as.data.frame(data.matrix(dtm), stringsAsfactors = FALSE)
    nmat.df <- cbind(nmat.df, docs[[tag]])
    colnames(nmat.df)[ncol(nmat.df)] <- tag
    nmat.df[nmat.df[[tag]] == 0,][[tag]] <- "NO"
    nmat.df[nmat.df[[tag]] == 1,][[tag]] <- "YES"
    nmat.df[[tag]] <- as.factor(nmat.df[[tag]])
    return (nmat.df)
}

levels(train_set_for_classifier$outlook_tag)

training_tag <- "FXFI"

train_set_for_classifier <- prepare_for_classifier_new(dtm.sparse, doccol, training_tag)
val_set_for_classifier <- prepare_for_classifier_new(valdtm_from_train_dtm, valds, training_tag)

training <- sample(nrow(train_set_for_classifier), ceiling(nrow(train_set_for_classifier) * .50))
testing <- (1:nrow(train_set_for_classifier))[-training]

grid <- expand.grid(cost = c(1, 1.5, 2))

PP <- c('center', 'scale')

ctrl <- trainControl(method="cv", allowParallel = TRUE, repeats = 2)

system.time(model1 <- train(outlook_tag ~ ., data = train_set_for_classifier[training,], trControl = ctrl, method = "svmLinear2", tuneGrid = grid))

model2 <- svm(FXFI ~ ., data = train_set_for_classifier[training,], kernel = "linear", cost = 2, probability = TRUE)

pred <- predict(model1, train_set_for_classifier[testing,], probability = TRUE)
table(true = train_set_for_classifier[testing,]$outlook_tag, pred = pred)
confusionMatrix(pred, train_set_for_classifier[testing,]$outlook_tag, mode = "prec_recall", positive = "YES")

valpred <- predict(model1, val_set_for_classifier, probability = TRUE)
table(true = val_set_for_classifier$outlook_tag, pred = valpred)
confusionMatrix(valpred, val_set_for_classifier$outlook_tag, mode = "prec_recall", positive = "YES")

head(attr(pred, "probabilities"))

library(qdap)
as.data.frame(valdocs)

displayData <- function(docs, num)
    as.data.frame(docs)[num,] %>% with(., invisible(sapply(text, function(x) { strWrap(x); cat("\n\n") })))

N <- 10
m <- as.matrix(dtm.sparse)
v <- sort(colSums(m), decreasing = TRUE)
head(v, N)