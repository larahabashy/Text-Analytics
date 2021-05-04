#Load Required Packages
if (!require ("stm")) {
  install.packages("stm", dependencies = TRUE )
  library(stm)
}

#Load data
data <- read.csv("news.csv") #10k articles
data$id <- NULL
data.stm <- as.data.frame(data)
 
processed <- textProcessor(data.stm$content, metadata = data) #convert and processes the data to ready it for analysis in the stm package
#each article is a row in the .csv file, with the text contained in a variable called content
#builds corpus, converts text to lower case, removes punctuation and numbers, stems the data and finally creating an output

out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
#does pre-processing steps like stemming, removing white spaces, numbers, punctations, removing very common and very rare words

docs <- out$documents
vocab <- out$vocab
meta <-out$meta


stm_model<- stm(documents = out$documents, vocab = out$vocab, K= 10, #k number of topics (assume - naivly)
                prevalence = ~s(year) + s(month), #the prevalence of topics should be influenced by year and month(smoothing) (think of reg model)
                max.em.its = 75, data = out$meta, #max number of iterations
                init.type = "Spectral", verbose = FALSE)
plot(stm_model)
                
#find exemplary passages
z<-data.stm[-out$docs.removed,]
length(z)
findThoughts(stm_model, texts = data.stm$content, n=3, topics = 10)

#choosing k - number of topics
search_k <- searchK(out$documents, out$vocab, K=c(10,30), prevalence = ~s(year) + s(month),data = meta,verbose = FALSE)
plot(search_k)
#outputs goodness of fit measures 
#1.coherence score
#2.Log likelihood
#3.Topic Exclusivity
#range of values of optimal values of k, run those models to validate

#estimate effect function: reg model that uses meta data to predict topic prevalance
pred_topic <- estimateEffect(formula = 1:10 ~ s(year)+s(month), stmobj =stm_model,
                             metadata = out$meta)
#plot




#Metadata/topic relationship visualization
