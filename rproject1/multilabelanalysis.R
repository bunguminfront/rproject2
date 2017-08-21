library("AzureML")

ws <- workspace(
  id = "e12f59ece68c40559b1b83e53ba52030",
  auth = "sNb9YyIk9NBphMIF8gYbXqP+Ts0SKTRxKFZ3Cai5N1l01N7Mqp71qsDxBmmdwXq1rFh8G5kvG/y4nyZbox228w==",
  api_endpoint = "https://europewest.studioapi.azureml.net"
)

ds <- download.intermediate.dataset(
  ws = ws,
  node_id = "59233455-00b1-437e-9052-d1350e968eea-1236753",
  experiment = "e12f59ece68c40559b1b83e53ba52030.f-id.74b01b36f2644d5fa3db68d8c05272a6",
  port_name = "Results dataset",
  data_type_id = "GenericCSV"
)
names(ds)

resds <- download.intermediate.dataset(
  ws = ws,
  node_id = "470e2bcd-fcba-4498-81aa-29f309f6a05a-12901",
  experiment = "e12f59ece68c40559b1b83e53ba52030.f-id.74b01b36f2644d5fa3db68d8c05272a6",
  port_name = "Results dataset",
  data_type_id = "GenericCSV"
)
names(resds)



library(mldr)

subset_size <- 100;

mydf <- ds[1:subset_size, order(colnames(ds))]
newsid_col <- which(colnames(mydf) == c("newsid"))
mydf <- mydf[c(newsid_col, seq(1:(newsid_col - 1)), seq(newsid_col + 1, length(colnames(mydf))))]

mymldr <- mldr_from_dataframe(mydf, c(2:length(names(mydf))))
summary(mymldr)

plot(mymldr, type = "LC")
plot(mymldr, type = "LH")
plot(mymldr, type = "LB")
plot(mymldr, type = "CH")
plot(mymldr, type = "LSB")
plot(mymldr, type = "AT")
plot(mymldr, type = "LSH")

my_predictions <- resds[1:subset_size, seq(2, length(names(resds)), 2)]
my_predictions <- my_predictions[, order(names(my_predictions))]

res <- mldr_evaluate(mymldr, my_predictions)

predictions <- as.matrix(emotions$dataset[, emotions$labels$index])
# and introduce some noise (alternatively get the predictions from some classifier)
predictions[sample(1:593, 100), sample(1:6, 100, replace = TRUE)] <- sample(0:1, 100, replace = TRUE)
# then evaluate predictive performance
res <- mldr_evaluate(emotions, predictions)
str(res)
plot(res$ROC, main = "ROC curve for emotions")