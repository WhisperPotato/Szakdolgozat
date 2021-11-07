install.packages("stm")

library(stm)        # Package for sturctural topic modeling
library(igraph)     # Package for network analysis and visualisation
library(stmCorrViz) # Package for hierarchical correlation view of STMs

data <- read.csv("C:/Users/zabor/Desktop/STM/final_stm.csv") 
processed <- textProcessor(data$txts_contwrd, metadata=data, lowercase = FALSE, removestopwords = FALSE, removenumbers = FALSE, removepunctuation = FALSE, stem = FALSE)

out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

meta <- out$meta

kResult <- searchK(out$documents, out$vocab, K=c(5:19), prevalence=~day_num, data=meta)
plot(kResult)

poliblogPrevFit <- stm(out$documents, out$vocab, K=19, max.em.its=10, data=meta, prevalence=~day_num, init.type="Spectral", 
                       seed=84)
plot(poliblogPrevFit, type="summary", xlim=c(0,.4))

plot(poliblogPrevFit, type="labels", topics=c(9,3,12))

#storage <- manyTopics(out$documents, out$vocab, K=c(5:20), data=meta$onlyday, runs=5)

#model <- selectModel(out$documents, out$vocab, K=19, data=meta$onlyday, runs=5)

#plotModels(model)

#meta$onlyday <- as.factor(out$meta$onlyday)
prep <- estimateEffect(5:19 ~ day_num, poliblogPrevFit, meta=out$meta, 
                       uncertainty="Global")

###   test_STM.png kódja   ###
plot(prep, "day_num", method="continuous", topics=19, model=z, printlegend=FALSE, xaxt="n", 
     xlab="Time (2020)")
monthseq <- seq(from=as.Date("2020-03-11"), to=as.Date("2020-12-31"), by="month")
monthnames <- months(monthseq)
axis(1, at=as.numeric(monthseq)-min(as.numeric(monthseq)), labels=monthnames)
###   test_STM.png kódja   ###

mod.out.corr <- topicCorr(poliblogPrevFit)
plot(mod.out.corr)
