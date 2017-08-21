library(mldr)


setwd("C:\\Users\\bungum\\Documents")
#system.time(save(mydf, file = c("mydf_full_columns_fixed.RData")))

system.time(load("mydf_full_columns_fixed.RData"))

system.time(full_mldr <- mldr_from_dataframe(mydf, labelIndices = 20:1202))
system.time(save(dtm_train, file = "dtm_train_1372718x16384.RData"))

system.time(load("dtm_train_1372718x16384.RData"))

mylabels <- c("FCL", "HCR", "TEC", "ENE", "BSC", "CYC", "IDU", "RECN", "NCY", "RTWS")

mylabels_mldr <- mldr_from_dataframe(mydf[, mylabels], labelIndices = 1:10)

system.time(full_mldr <- mldr_from_dataframe(mydf[, 20:length(colnames(mydf))], labelIndices = 1:(length(colnames(mydf)) - 20)))

plot(mylabels_mldr, type = "LC")
plot(mylabels_mldr, type = "LH")
plot(mylabels_mldr, type = "LB")
plot(mylabels_mldr, type = "CH")
plot(mylabels_mldr, type = "AT")
plot(mylabels_mldr, type = "LSH")
plot(mylabels_mldr, type = "LSB")

library(RWeka)
classifier <- IBk(classLabel ~ ., data = emo_lp, control = Weka_control(K = 10))
evaluate_Weka_classifier(classifier, numFolds = 5)