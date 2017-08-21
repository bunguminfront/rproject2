library("AzureML")

ws <- workspace(
  id = "e12f59ece68c40559b1b83e53ba52030",
  auth = "sNb9YyIk9NBphMIF8gYbXqP+Ts0SKTRxKFZ3Cai5N1l01N7Mqp71qsDxBmmdwXq1rFh8G5kvG/y4nyZbox228w==",
  api_endpoint = "https://europewest.studioapi.azureml.net"
)
ds <- download.intermediate.dataset(
  ws = ws,
  node_id = "3643ea86-dd97-4f3c-9e1e-02407ca25d45-24576",
  experiment = "e12f59ece68c40559b1b83e53ba52030.f-id.61d34376f22c4c22bb005a5d43758fc2",
  port_name = "Results dataset",
  data_type_id = "GenericCSV"
)
head(ds)

library("AzureML")

ws <- workspace(
  id = "e12f59ece68c40559b1b83e53ba52030",
  auth = "sNb9YyIk9NBphMIF8gYbXqP+Ts0SKTRxKFZ3Cai5N1l01N7Mqp71qsDxBmmdwXq1rFh8G5kvG/y4nyZbox228w==",
  api_endpoint = "https://europewest.studioapi.azureml.net"
)
ds <- download.intermediate.dataset(
  ws = ws,
  node_id = "3643ea86-dd97-4f3c-9e1e-02407ca25d45-33050",
  experiment = "e12f59ece68c40559b1b83e53ba52030.f-id.61d34376f22c4c22bb005a5d43758fc2",
  port_name = "Results dataset",
  data_type_id = "GenericTSV"
)
head(ds)

filename <- "c:\\users\\bungum\\Documents\\0.2Mheadline2.csv"

mydf <- read_csv(filename, col_names = F, trim_ws = T)

read.csv(filename)