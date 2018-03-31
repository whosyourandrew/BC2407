# BC2407
setwd('C:/Users/Andrew/Desktop/BC2407/Adzuna job prediction')
set.seed(2014)

#Install Packages
{
  install.packages(c("ggplot2", "e1071", "caret", "quanteda", 
                   "irlba", "randomForest", "ggmap", "glmnet"))
}


#Read CSVs
{
ITJobs <- read.csv('ITJobs.csv', stringsAsFactors = TRUE)
names(ITJobs)
  }


#Call Libraries
{
  library(quanteda)
  library(glmnet)
}

###ITJOBS

  ##ITJobs Pre-process Code
  {
    ITJobs<-ITJobs[,c(3,11)]
    ITJobs$FullDescription<-as.character(ITJobs$FullDescription)
    
    #Tokenize
    train.tokens <- tokens(ITJobs$FullDescription, what = "word", 
                           remove_numbers = TRUE, remove_punct = TRUE,
                           remove_symbols = TRUE, remove_hyphens = TRUE)
    
    #Change tokens to lower case
    train.tokens <- tokens_tolower(train.tokens)
    
    #Stopword list
    train.tokens <- tokens_select(train.tokens, stopwords(), 
                                  selection = "remove")
    
    #Stemming
    train.tokens <- tokens_wordstem(train.tokens, language = "english")
    train.tokens[[357]]
    
    #Bag of words
    train.tokens.dfm <- dfm(train.tokens, remove = c(stopwords("english"),"can","clear","look","job","want","aim","etc", "use", "http", "also", "take", "part", "work", "one", "find", "role", "along", "right", "said", "www.totaljobs.com", "www.cwjobs.co.uk"), stem = TRUE, remove_punct = TRUE, tolower = FALSE)
    
    train.tokens.dfm<-dfm_trim(train.tokens.dfm,min_docfreq = 0.01)
    textplot_wordcloud(train.tokens.dfm)
    ITJ<- cbind(Salary = ITJobs$SalaryNormalized, data.frame(train.tokens.dfm))
    ITJ<-ITJ[,-2]
    
    
    #tfidf with trimmed
    ITJ.tfidf<-dfm_tfidf(train.tokens.dfm)
    textplot_wordcloud(ITJ.tfidf)
    ITJ.tfidf.df <- cbind(Salary = ITJobs$SalaryNormalized, data.frame(ITJ.tfidf))
    ITJ.tfidf.df<-ITJ.tfidf.df[,-2]
    
