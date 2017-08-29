library(stm)

polit_url <- "http://scholar.princeton.edu/sites/default/files/bstewart/files/poliblogs2008.csv"

download.file(polit_url, "poliblogs2008.csv")

load(url("http://goo.gl/VPdxlS"))

data <- read.csv("poliblogs2008.csv")
processed <- textProcessor(data$documents, metadata = data)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

plotRemoved(processed$documents, lower.thresh = seq(1, 200, by = 100))
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 15)

poliblogPrevFit <- stm(documents = out$documents,
    vocab = out$vocab, K = 20,
    prevalence = ~rating + s(day),
    max.em.its = 15,
    data = out$meta, init.type = "Spectral")

teslaPrevFit <- stm(documents = out$documents,
    vocab = out$vocab, K = 5,
    prevalence = ~rating + s(day),
    max.em.its = 75,
    data = out$meta, init.type = "Spectral")

poliblogSelect <- selectModel(out$documents, out$vocab, K = 20,
    prevalence = ~rating + s(day), max.em.its = 75,
    data = out$meta, runs = 20, seed = 8458159)

storage <- searchK(out$documents, out$vocab, K = c(7, 10),
    prevalence = ~rating + s(day),
    data = meta)

labelTopics(poliblogPrevFit, c(3, 7, 20))

thoughts3 <- findThoughts(poliblogPrevFit, texts = shortdoc,
    n = 2, topics = 3)$docs[[1]]

par(mfrow = c(1, 2), mar = c(.5, .5, 1, .5))
plotQuote(thoughts3, width = 30, main = "Topic 3")
plotQuote(thoughts20, width = 30, main = "Topic 20")

thoughts20 <- findThoughts(poliblogPrevFit, texts = shortdoc,
    n = 2, topics = 20)$docs[[1]]

out$meta$rating <- as.factor(out$meta$rating)
prep <- estimateEffect(1:20 ~ rating + s(day), poliblogPrevFit,
    meta = out$meta,
    uncertainty = "Global")

plot(poliblogPrevFit, type = "summary", xlim = c(0, .3))

plot(prep, covariate = "rating", topics = c(3, 7, 20),
    model = poliblogPrevFit, method = "difference",
    cov.value1 = "Liberal", cov.value2 = "Conservative",
    xlab = "More Conservative ... More Liberal",
    main = "Effect of Liberal vs. Conservative",
    xlim = c(-.1, .1), labeltype = "custom",
    custom.labels = c('Obama', 'Sarah Palin', 'Bush Presidency'))

plot(prep, "day", method = "continuous", topics = 7,
    model = z, printlegend = TRUE, xaxt = "n", xlab = "Time (2008)")
monthseq <- seq(from = as.Date("2008-01-01"),
    to = as.Date("2008-12-01"), by = "month")
monthnames <- months(monthseq)
axis(1, at = as.numeric(monthseq) - min(as.numeric(monthseq)),
    labels = monthnames)

poliblogContent <- stm(out$documents, out$vocab, K = 20,
    prevalence = ~rating + s(day), content = ~rating,
    max.em.its = 75, data = out$meta, init.type = "Spectral")

plot(poliblogContent, type = "perspectives", topics = 11)

plot(poliblogPrevFit, type = "perspectives", topics = c(12, 20))

poliblogInteraction <- stm(out$documents, out$vocab,
    K = 20,
    prevalence = ~rating * day, max.em.its = 75,
    data = out$meta, init.type = "Spectral")

prep <- estimateEffect(c(20) ~ rating * day, poliblogInteraction,
    metadata = out$meta, uncertainty = "None")
plot(prep, covariate = "day", model = poliblogInteraction,
    method = "continuous", xlab = "Days", moderator = "rating",
    moderator.value = "Liberal", linecol = "blue", ylim = c(0,.12),
    printlegend = F)
plot(prep, covariate = "day", model = poliblogInteraction,
    method = "continuous", xlab = "Days", moderator = "rating",
    moderator.value = "Conservative", linecol = "red", add = T,
    printlegend = F)
legend(0, .08, c("Liberal", "Conservative"),
    lwd = 2, col = c("blue", "red"))