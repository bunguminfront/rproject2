library(stm)

# Just import the metadata directly, should be done more systematically when creating the corpus object
for (i in 1:length(dj.select)) {
    meta(dj.select[[i]], tag = "date") <- mydf[meta(dj.select[[i]], tag = "id"), 1346]
}

dj.select <- dj.corpus_tok[isin_selection]

isindf <- data.frame(mydf[isin_selection,])
colnames(isindf)[1346] <- c("date")

isindf$date <- as.integer(isindf$date) - min(as.integer(isindf$date))
# be sure to check the values before you run this one, if it is converted and you run it again they will all be "NO"
isindf$PRL <- ifelse(isindf$PRL == T, "YES", "NO")

sort(colSums(isindf[, 20:1345][colSums(isindf[, 20:1345]) > 0]))

# Duplicate removal (do for entire dataset!)
isindf <- isindf[!duplicated(lapply(dj.select, '[[', 1)),]
dj.select <- dj.select[!duplicated(lapply(dj.select, '[[', 1))]


myprocessed <- textProcessor(lapply(dj.select, '[[', 1), metadata = isindf)
myout <- prepDocuments(myprocessed$documents, myprocessed$vocab, myprocessed$meta)
mydocs <- myout$documents
myvocab <- myout$vocab
mymeta <- myout$meta

plotRemoved(myprocessed$documents, lower.thresh = seq(1, 200, by = 100))
myout <- prepDocuments(myprocessed$documents, myprocessed$vocab, myprocessed$meta, lower.thresh = 15)

poliblogPrevFit <- stm(documents = out$documents,
    vocab = out$vocab, K = 20,
    prevalence = ~rating + s(day),
    max.em.its = 75,
    data = out$meta, init.type = "Spectral")

system.time(ibmPrevFit <- stm(documents = myout$documents,
    vocab = myout$vocab, K = 50,
    prevalence = ~PRL + s(date),
    max.em.its = 75,
    data = myout$meta,
    init.type = "Spectral",
    LDAbeta = TRUE
    ))

ibmSelect <- selectModel(out$documents, out$vocab, K = 5,
    prevalence = ~rating + s(day), max.em.its = 75,
    data = out$meta, runs = 20, seed = 8458159)

storage <- searchK(myout$documents, myout$vocab, K = c(2, 5),
    prevalence = ~s(date),
    data = mymeta)

labelTopics(ibmPrevFit, c(1:50))

myshortdoc <- substr(isindf$replace, 40, 240)
myshortdoc <- substr(lapply(dj.select, '[[', 1), 0, 200)

thoughts3 <- findThoughts(ibmPrevFit, texts = myshortdoc,
    n = 3, topics = 3)$docs[[1]]

thoughts5 <- findThoughts(ibmPrevFit, texts = myshortdoc,
    n = 3, topics = 5)$docs[[1]]

thoughts41 <- findThoughts(ibmPrevFit, texts = myshortdoc,
    n = 3, topics = 41)$docs[[1]]
thoughts42 <- findThoughts(ibmPrevFit, texts = myshortdoc,
    n = 3, topics = 42)$docs[[1]]
thoughts50 <- findThoughts(ibmPrevFit, texts = myshortdoc,
    n = 3, topics = 50)$docs[[1]]

par(mfrow = c(1, 3), mar = c(.5, .5, 1, .5))
plotQuote(thoughts41, width = 30, main = "Topic 41")
plotQuote(thoughts42, width = 30, main = "Topic 42")
plotQuote(thoughts50, width = 30, main = "Topic 50")

myout$meta$PRL <- as.factor(myout$meta$PRL)

prep <- estimateEffect(1:50 ~ PRL + s(date), ibmPrevFit,
    meta = myout$meta,
    uncertainty = "None")

plot(ibmPrevFit, type = "summary", xlim = c(0, .3))

plot(prep, covariate = "PRL", topics = c(42, 49, 50),
    model = ibmPrevFit, method = "difference",
    cov.value1 = "YES", cov.value2 = "NO",
    xlab = "More True ... More False",
    main = "Effect of True vs. False (PRL)",
    xlim = c(-.1, .1), labeltype = "custom",
    custom.labels = c('airline', 'nvidia', 'salesforce'))

plot(prep, "date", method = "continuous", topics = 42,
    model = z, printlegend = FALSE, xaxt = "n", xlab = "Time (2016-2017)")
monthseq <- seq(from = as.Date("2016-07-01"),
    to = as.Date("2017-06-01"), by = "month")
monthnames <- months(monthseq)
axis(1, at = as.numeric(monthseq) - min(as.numeric(monthseq))+541,
    labels = monthnames)

system.time(ibmContent <- stm(myout$documents, myout$vocab, K = 50,
    prevalence = ~ PRL + s(date), content = ~PRL,
    max.em.its = 75, data = myout$meta, init.type = "Spectral"))

plot(ibmContent, type = "perspectives", topics = 42)

plot(ibmPrevFit, type = "perspectives", topics = c(12, 20))

system.time(ibmInteraction <- stm(myout$documents, myout$vocab,
    K = 50,
    prevalence = ~ PRL * date, max.em.its = 75,
    data = myout$meta,
    init.type = "Spectral",
    LDAbeta = TRUE
    )
)

prep <- estimateEffect(c(42) ~ PRL * date, ibmInteraction,
    metadata = myout$meta, uncertainty = "None")
plot(prep, covariate = "date", model = ibmInteraction,
    method = "continuous", xlab = "Date", moderator = "PRL",
    moderator.value = "YES", linecol = "blue", ylim = c(0, .12),
    printlegend = F)
plot(prep, covariate = "date", model = ibmInteraction,
    method = "continuous", xlab = "Days", moderator = "PRL",
    moderator.value = "NO", linecol = "red", add = T,
    printlegend = F)
legend(0, .08, c("YES", "NO"),
    lwd = 2, col = c("blue", "red"))

mod.out.corr <- topicCorr(ibmPrevFit)
cloud(ibmPrevFit, topic = 50, scale = c(2, .25))
plot(mod.out.corr)

for (i in 1:length(dj.select)) {
    print(dj.select[[i]]$content %in% lapply(dj.select[-i], '[[', 1))
}

stmCorrViz(immigration_perceptions[1], "ibmPrevFit.html",
    documents_raw = immigration_perceptions[2],
    documents_matrix = immigration_perceptions[3])

stmCorrViz(immigration_perceptions$model, 
    "corrviz.html",
    documents_raw = immigration_perceptions$raw_documents,
    documents_matrix = immigration_perceptions$documents_matrix)

stmCorrViz(ibmPrevFit,
    "ibm.html",
    documents_raw = myshortdoc,
    documents_matrix = myout)