# szükséges csomagok betöltése
install.packages("stm")

library(stm)        # Package for sturctural topic modeling
library(igraph)     # Package for network analysis and visualisation
library(stmCorrViz) # Package for hierarchical correlation view of STMs

#adatbázis beolvasás és szükséges változók létrehozása
data <- read.csv("C:/Users/zabor/Desktop/STM/final_stm.csv") 
processed <- textProcessor(data$txts_contwrd, metadata=data, lowercase = FALSE, removestopwords = FALSE, removenumbers = FALSE, removepunctuation = FALSE, stem = FALSE)

out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

meta <- out$meta
# optimális topikszám keresés - koherencia érték vizsgálat
kResult <- searchK(out$documents, out$vocab, K=c(5:19), data=meta)
plot(kResult)

poliblogPrevFit <- stm(out$documents, out$vocab, K=19, max.em.its=10, data=meta, init.type="Spectral", 
                       seed=84)

# létrejött topikok vizualizációja
plot(poliblogPrevFit, type="summary", xlim=c(0,.4))

plot(poliblogPrevFit, type="labels")

prep <- estimateEffect(5:19 ~ day_num, poliblogPrevFit, meta=out$meta, 
                       uncertainty="Global")

# korreláció vizsgálat
mod.out.corr <- topicCorr(poliblogPrevFit)
plot(mod.out.corr)

##########################    idő (day_num) meta-változó bevonása     #########################################
kResult2 <- searchK(out$documents, out$vocab, K=c(5:19), prevalence=~s(day_num), data=meta)
plot(kResult2)

poliblogPrevFit2 <- stm(out$documents, out$vocab, K=19, max.em.its=10, data=meta, prevalence=~s(day_num), init.type="Spectral", 
                       seed=84)

# létrejött topikok vizualizációja
plot(poliblogPrevFit2, type="summary", xlim=c(0,.4))

plot(poliblogPrevFit2, type="labels")

prep2 <- estimateEffect(5:19 ~ s(day_num), poliblogPrevFit2, meta=out$meta, 
                       uncertainty="Global")

# topic proportions
plot(prep2, "day_num", method="continuous", topics=19, model=z, printlegend=FALSE, xaxt="n", 
     xlab="Time (2020.03.11 - 2020.12.31.)")
monthseq <- seq(from=as.Date("2020-03-11"), to=as.Date("2020-12-31"), by="day")
monthnames <- months(monthseq)  ##ide vmi kéne, hogy naponta legyen
axis(1, at=as.numeric(monthseq)-min(as.numeric(monthseq)), labels=monthnames)
# topic proportions

# korreláció vizsgálat
mod.out.corr2 <- topicCorr(poliblogPrevFit2)
plot(mod.out.corr2)
