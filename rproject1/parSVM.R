library(doParallel);
library(caret)

#create a list of seed, here change the seed for each resampling
set.seed(123)

#length is = (n_repeats*nresampling)+1
seeds <- vector(mode = "list", length = 11)

#(3 is the number of tuning parameter, mtry for rf, here equal to ncol(iris)-2)
for (i in 1:10)
    seeds[[i]] <- sample.int(n = 1000, 3)

#for the last model
seeds[[11]] <- sample.int(1000, 1)

#control list
myControl <- trainControl(method = 'cv', seeds = seeds, index = createFolds(iris$Species))
ndfCtrl <- trainControl(method = 'cv', seeds = seeds, index = createFolds(ndf$CYC))

#run model in parallel
cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)


model1 <- train(Species ~ ., iris, method = 'rf', trControl = myControl)
model2 <- train(Species ~ ., iris, method = 'rf', trControl = myControl)

model3 <- train(CYC ~ ., data=ndf, method = 'rf', trControl = ndfCtrl)

stopCluster(cl)

#compare
all.equal(predict(model1, type = 'prob'), predict(model2, type = 'prob'))

ssmdf <- mydf[1:1000,]
ss <- dtm_train[1:1000,]

ndf <- data.frame(cbind(ssmdf$CYC, as.matrix(ss)))
colnames(ndf)[1] <- "CYC"
ndf$CYC <- ifelse(ndf$CYC == T, "YES", "NO")

