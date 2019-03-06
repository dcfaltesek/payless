#payless shoesource code Feb/March 2019

#the data are paragraphs stored as "text" from articles collected about the payless shoesource bankruptcy

#remove blank text
library(dplyr)
payA<-filter(payless, text !="")

#check dimensions
dim(payA)
#three vars, 179 paragraphs

#configure for analysis of text
library(mallet)

#produce sequential var to use as an ID
P<-1:270
payB<-mutate(payA, Id=as.character(P))

#stopwords
library(stopwords)
#very generic set of stopwords
stoppy<-stopwords("en")
#write this as a meaningful stopwords file
write.csv(stoppy, "stoppy.csv", row.names = FALSE)

#iterated on the basis of result
docs <- mallet.import(payB$Id, payB$text, "~/stoppy.csv")
#number of topics
topic_model <- MalletLDA(num.topics = 8)
topic_model$loadDocuments(docs)
#how many runs - alpha and beta parameters left at defaults
topic_model$train(50000)
#store results of doc topics
doc.topics<-mallet.doc.topics(topic_model, normalized = TRUE, smoothed = TRUE)
#topic words
topic.words<-mallet.topic.words(topic_model, normalized = TRUE, smoothed = TRUE)
#overview tags
mallet.topic.labels(topic_model, topic.words, num.top.words = 6)

#dendrogram code
plot(mallet.topic.hclust(doc.topics, topic.words, balance = .7))
View(doc.topics)

#doc topics as a dataframe
DT<-data.frame(doc.topics)
View(DT)

library(stringr)
#string detects for key terms
G<-str_count(payB$text, "Gate")
D<-str_count(payB$text, "debt")

#attach to completed dataset Z
Z<-bind_cols(payB, DT)
View(Z)
G <- data.frame(G)
D <- data.frame(D)
Z<-bind_cols(Z, D)
Z<-bind_cols(Z, G)
View(Z) 

#now plot those
library(ggplot2)
ggplot(Z, aes(X1, D, colour = G))+geom_jitter()
cor.test(Z$X1, Z$D)
ggplot(Z, aes(D, X1, colour = G))+geom_jitter()
ggplot(Z, aes(D, X7, colour = X8))+geom_jitter()
qplot(Z$D)

#cor among topics
cor.test(Z$X1, Z$D)
cor.test(Z$X2, Z$D)
cor.test(Z$X3, Z$D)
cor.test(Z$X4, Z$D)
cor.test(Z$X5, Z$D)
cor.test(Z$X6, Z$D)
cor.test(Z$X7, Z$D)
cor.test(Z$X8, Z$D)


#debt in the key topic
ggplot(Z, aes(X7, D, colour = X3))+geom_jitter()+facet_wrap(~publication )

#exploratory filters for probability
filter(Z, D > 1)
filter(Z, X3 > .7)%>%
  filter(D > 1)

filter(Z, X8 > .7)%>%
  filter(D > 1)

filter(Z, X5 > .2)%>%
  filter(D > 1)

filter(Z, G >0)

filter(Z, X7 > .7)

ggplot(Z, aes(X7, D, colour = X3))+geom_jitter()+facet_wrap(~publication )

#convert to long format data
library(tidyr)
Q<-gather(Z, "topic", "prob", 5:12)
View(Q)

library(dplyr)

Q%>%
  filter(prob>.5)%>%
  ggplot(aes(as.numeric(Id)), prob, colour=as.numeric(Id))+geom_density2d() 
             


group_by(Q, topic)%>%
  summarize(p2 = sum(prob))

F<-filter(Z, X7>.7)
View(F)

Q%>%
  filter(publication=="Motley Fool" | publication=="Reuters" | publication == "Business Insider" | publication == "Valley Morning Star")%>%
ggplot(aes(as.numeric(Id), prob, colour = topic))+geom_density2d()+facet_wrap(~publication)

cor.test(Z$X6, Z$D)

Z%>%
  filter(X2>.2)%>%
  ggplot(aes(X2, X8, colour = X))+geom_jitter()+facet_wrap(~publication)

Q%>%
  filter(topic != "X7")%>%
  filter(topic != "X6")%>%
ggplot(aes(as.numeric(Id), prob))+geom_density2d()+facet_wrap(~topic)


ggplot(Q, aes(as.numeric(Id), prob))+geom_density2d()+facet_wrap(~topic)

#standard deviation by topic
Q%>%
  group_by(topic)%>%
  summarize(sd(prob))


ggplot(Q, aes(as.numeric(Id), prob, colour = publication))+geom_jitter()+facet_wrap(~topic)
