# Transform dtm to matrix to data frame - df is easier to work with
mat.df <- as.data.frame(data.matrix(dtm.sparse), stringsAsfactors = FALSE)
# Column bind category (known classification)
mat.df <- cbind(mat.df, doccol$equities)
# Change name of new column to "category"
colnames(mat.df)[ncol(mat.df)] <- "category"
# Split data by rownumber into two equal portions
training <- sample(nrow(mat.df), ceiling(nrow(mat.df) * .50))
testing <- (1:nrow(mat.df))[-training]
# Isolate classifier
cl <- mat.df[, "category"]
# Create model data and remove "category"
modeldata <- mat.df[, !colnames(mat.df) %in% "category"]
modeldata$equities <- factor(cl == 1)
modeldata$equities <- cl
modeldata[modeldata$equities == 0,]$equities <- "NEI"
modeldata[modeldata$equities == 1,]$equities <- "JA"
modeldata$equities <- as.factor(modeldata$equities)

valmat.df <- as.data.frame(data.matrix(valdtm), stringsAsfactors = FALSE)
valmat.df <- cbind(valmat.df, valds$equities)
colnames(valmat.df)[ncol(valmat.df)] <- "category"
cl <- valmat.df[, "category"]
valmodeldata <- valmat.df[, !colnames(valmat.df) %in% "category"]
valmodeldata$equities <- cl
valmodeldata[valmodeldata$equities == 0,]$equities <- "NEI"
valmodeldata[valmodeldata$equities == 1,]$equities <- "JA"
valmodeldata$equities <- as.factor(valmodeldata$equities)

prepare_for_classifier <- function(dtm, docs) {
    nmat.df <- as.data.frame(data.matrix(dtm), stringsAsfactors = FALSE)
    nmat.df <- cbind(nmat.df, docs$equities)
    colnames(nmat.df)[ncol(nmat.df)] <- c("equities")
    nmat.df[nmat.df$equities == 0,]$equities <- "NO"
    nmat.df[nmat.df$equities == 1,]$equities <- "YES"
    nmat.df$equities <- as.factor(nmat.df$equities)
    return(nmat.df)
}

prepare_for_classifier_new <- function(dtm, docs) {
    nmat.df <- as.data.frame(data.matrix(dtm), stringsAsfactors = FALSE)
    nmat.df <- cbind(nmat.df, docs$outlook)
    colnames(nmat.df)[ncol(nmat.df)] <- "outlook_tag"
    nmat.df[nmat.df$outlook_tag == 0,]$outlook_tag <- "NO"
    nmat.df[nmat.df$outlook_tag == 1,]$outlook_tag <- "YES"
    nmat.df$outlook_tag <- as.factor(nmat.df$outlook_tag)
    return(nmat.df)
}