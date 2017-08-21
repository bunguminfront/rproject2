library(parallel)

# Remember to rerun these things before you run the programs, otherwise the processes can get "exhausted"
cl <- makePSOCKcluster(7)
setDefaultCluster(cl)

# Remember when you parallelize that the R connections to Java are references that get cut between 
# the work environment and the parallelization engine

# Load packages

clusterEvalQ(cl, library(NLP));
clusterEvalQ(cl, library(openNLP));
clusterEvalQ(cl, library(RWeka));
clusterEvalQ(cl, library(openNLPmodels.en));

clusterEvalQ(cl, itinerants_pipeline <- list(
    Maxent_Sent_Token_Annotator(),
    Maxent_Word_Token_Annotator(),
    Maxent_Entity_Annotator(kind = "person"),
    Maxent_Entity_Annotator(kind = "location"),
    Maxent_Entity_Annotator(kind = "organization")
    ));


clusterExport(cl, c('annotate_entities'))

system.time(dj.select_tok_par_NER <- parLapply(cl, dj.select_tok$content, function(k) {
    annotate_entities(k, itinerants_pipeline)
}))

# Stop cluster after we are done
stopCluster(cl)

tm_parLapply_engine(cl)