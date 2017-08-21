library("AzureML")

ws <- workspace(
  id = "e12f59ece68c40559b1b83e53ba52030",
  auth = "sNb9YyIk9NBphMIF8gYbXqP+Ts0SKTRxKFZ3Cai5N1l01N7Mqp71qsDxBmmdwXq1rFh8G5kvG/y4nyZbox228w==",
  api_endpoint = "https://europewest.studioapi.azureml.net"
)
phids <- download.intermediate.dataset(
  ws = ws,
  node_id = "d5120482-e51d-4ca6-a241-af3fdc2f1185-902",
  experiment = "e12f59ece68c40559b1b83e53ba52030.f-id.4027c176d1ec46c3a093a009115c817d",
  port_name = "Results dataset",
  data_type_id = "GenericCSV"
)


thetads <- download.intermediate.dataset(
  ws = ws,
  node_id = "adf8c0f7-96bf-4a8d-aa1c-635ebf23325b-92",
  experiment = "e12f59ece68c40559b1b83e53ba52030.f-id.4027c176d1ec46c3a093a009115c817d",
  port_name = "Results dataset",
  data_type_id = "GenericCSV"
)
names(thetads)

vocabds <- download.intermediate.dataset(
  ws = ws,
  node_id = "adf8c0f7-96bf-4a8d-aa1c-635ebf23325b-109414",
  experiment = "e12f59ece68c40559b1b83e53ba52030.f-id.4027c176d1ec46c3a093a009115c817d",
  port_name = "Results dataset",
  data_type_id = "GenericTSV"
)
names(vocabds)


lengthds <- download.intermediate.dataset(
  ws = ws,
  node_id = "adf8c0f7-96bf-4a8d-aa1c-635ebf23325b-200803",
  experiment = "e12f59ece68c40559b1b83e53ba52030.f-id.4027c176d1ec46c3a093a009115c817d",
  port_name = "Results dataset",
  data_type_id = "GenericCSV"
)
names(lengthds)

filename_theta <- "c:\\users\\bungum\\documents\\thetaTSV.tsv"
thetads <- read.csv(filename_theta, header = T, sep = "\t")

filename_TF <- "c:\\users\\bungum\\documents\\TF_LDA.tsv"
TF_LDA <- read.csv(filename_TF, header = T, sep = "\t")
names(TF_LDA)

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

# Transpose to get the right format for the 
phi_transpose_nums <- t(phids[,2:11])
#convert the phi you read in as a data frame
phi.df <- as.data.frame(phi_transpose_nums)
# the vocabulary is in the first row
vocab <- phids[,1]
# give the dataframe this vocabulary as names
colnames(phi.df) <- vocab
# get the first row out of the data frame
#phi.done.df <- phi.df[-1,]
# Convert all columns from factors to numeric
#phi.done.df[] <- lapply(phi.done.df, function(x) as.numeric(as.character(x)))
# make the theta dataset out of he transpose of the vocabulary and the 500 last rows (minus the feature ID).  NB the topic number is hard coded in her
#thetads <- t(ds[, 2:31])
#thetads <- t(thetads)
# Convert the vocabds into a data.frame
vocab.df <- as.data.frame(vocabds)
# remove the first column
vocab.df <- vocab.df[, -c(1)]
# realign column names with the above
colnames(vocab.df) <- vocab
# compute the total frequencies for each term in the corpus
freq_matrix <- data.frame(ST = vocab,
                           Freq = colSums(vocab.df))
# compute document lengths
length_mat <- as.integer(lengthds$length.Preprocessed.dj_clean.)

# Convert phi to matrix
#phi_mat <- as.matrix(phi.done.df)
#phi_mat <- sapply(phi_mat, as.numeric)
impute_df_to_sum_to_one <- function(df) {

    dt <- dim(df)
    R <- dt[1]
    K <- dt[2]

    df_out <- df

    for (r in 1:R) {
        #print(r)
        df_out[r, K] <- df[r, K] + (1 - sum(df[r,]))
        temp <- all.equal(rowSums(df_out)[r], 1, check.attributes = F) == T
        #print(temp)
    }
    return(df_out)
}

theta_imputed <- impute_df_to_sum_to_one(thetads)

library(LDAvis)

json_lda <- LDAvis::createJSON(phi = phi.df, theta = theta_imputed,
                                    vocab = vocab,
                                    doc.length = length_mat,
                                    term.frequency = freq_matrix$Freq)


serVis(json_lda)

dt <- dim(thetads)
K = dt[2]
phi.test <- all.equal(rowSums(phi_transpose_nums), rep(1, K), check.attributes = FALSE)
theta.test <- all.equal(rowSums(thetads), rep(1, dt[1]), check.attributes = FALSE)


