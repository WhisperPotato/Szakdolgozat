# szükséges csomagok betöltése
install.packages("stm")
install.packages("tidyverse")
install.packages("ggplot2")

library(stm)        # Package for sturctural topic modeling
library(igraph)     # Package for network analysis and visualisation
library(stmCorrViz) # Package for hierarchical correlation view of STMs
library(ggplot2)
library(tidyverse)

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
plot(prep2, "day_num", method="continuous", topics=3, model=z, printlegend=FALSE, xaxt="n", 
     xlab="Time (2020.03.11 - 2020.12.31.)")
monthseq <- seq(from=as.Date("2020-03-11"), to=as.Date("2020-12-31"), by="day")
monthnames <- months(monthseq)  ##ide vmi kéne, hogy naponta legyen
axis(1, at=as.numeric(monthseq)-min(as.numeric(monthseq)), labels=monthnames)
# topic proportions

# korreláció vizsgálat
mod.out.corr2 <- topicCorr(poliblogPrevFit2)
plot(mod.out.corr2)

### topik prevalencia értékek

## Put labels in a vector
labels <- c("topic 1", "topic 2", "topic 3", "topic 4", "topic 5", "topic 6", "topic 7", "topic 8", "topic 9", "topic 10", "topic 11", "topic 12", "topic 13", "topic 14", "topic 15", "topic 16", "topic 17", "topic 18", "topic 19")

## Extract theta from the stm-model
df <- data.frame(labels)
proportion <- as.data.frame(colSums(poliblogPrevFit2$theta/nrow(poliblogPrevFit2$theta)))
df <- cbind(df, proportion)
colnames(df) <- c("Labels", "Probability")

## Sort the dataframe
df <- df[order(-proportion), ] 
df$Labels <- factor(df$Labels, levels = rev(df$Labels))
df$Probability <- as.numeric(df$Probability)
df$Probability <- round(df$Probability, 4)

## Plot graph
ggplot(df, aes(x = Labels, y = Probability)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(breaks = c(0, 0.15), limits = c(0, 0.15), expand = c(0, 0)) + #change breaks and limits as you need
  coord_flip() + 
  geom_text(aes(label = scales::percent(Probability)), #Scale in percent
            hjust = -0.25, size = 4,
            position = position_dodge(width = 1),
            inherit.aes = TRUE) + 
  theme(panel.border = element_blank())
