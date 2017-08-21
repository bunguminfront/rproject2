library("AzureML")

ws <- workspace(
  id = "e12f59ece68c40559b1b83e53ba52030",
  auth = "sNb9YyIk9NBphMIF8gYbXqP+Ts0SKTRxKFZ3Cai5N1l01N7Mqp71qsDxBmmdwXq1rFh8G5kvG/y4nyZbox228w==",
  api_endpoint = "https://europewest.studioapi.azureml.net"
)
ds <- download.intermediate.dataset(
  ws = ws,
  node_id = "7ea3268f-ebe0-4ce9-b942-029d2811d2f5-58393",
  experiment = "e12f59ece68c40559b1b83e53ba52030.f-id.03f3f2e2f38e4777b1fed27d00b87aae",
  port_name = "Results dataset",
  data_type_id = "GenericCSV"
)
head(ds)
