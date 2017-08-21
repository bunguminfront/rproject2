library("AzureML")

# Read in a tab-separated file to the Visual Studio

ws <- workspace(
  id = "e12f59ece68c40559b1b83e53ba52030",
  auth = "sNb9YyIk9NBphMIF8gYbXqP+Ts0SKTRxKFZ3Cai5N1l01N7Mqp71qsDxBmmdwXq1rFh8G5kvG/y4nyZbox228w==",
  api_endpoint = "https://europewest.studioapi.azureml.net"
)
ds <- download.intermediate.dataset(
  ws = ws,
  node_id = "d69d0e83-2a6c-4790-83a2-b0673f017e06-216896",
  experiment = "e12f59ece68c40559b1b83e53ba52030.f-id.4027c176d1ec46c3a093a009115c817d",
  port_name = "Results dataset",
  data_type_id = "GenericTSV"
)

names(ds)
dim(ds)

library(tm)
library(magrittr)

a <- DirSource(".")
b <- VectorSource(ds$dj_clean)
dj.corpus <- Corpus(b, ## I change DireSource(a) by a
          readerControl = list(language = "eng", reader = readPlain))

# Clean corpus
dj.corpus.clean <- tm_map(dj.corpus, content_transformer(tolower)) %>%
tm_map(removeNumbers) %>%
tm_map(removeWords, stopwords("english")) %>%
tm_map(removePunctuation) %>%
tm_map(stripWhitespace) %>%
tm_map(stemDocument, language = "english")

# Create dtm
dtm <- DocumentTermMatrix(docs)

# Remove sparse entries
dtm.sparse <- removeSparseTerms(dtm, 0.93)
dtm.sparse