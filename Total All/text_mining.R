# Text mining part for the final project of COMSE 6998
# Ming Li, 20171120
# 2:00 PM @ Sci Lib, Columbia University

# this is the first time I m doing text mining, hoping to have time to
# integrate some NLP algorithms 
# Okay, let's rock.

# Firstly, let's bless that the information can be extracted 
# from the PDF via my code
# setwd("~/Google Drive/Trade the world/text")
library(tm)
library(pdftools) # used for read pdf
library(hunspell) # used for collection of the words
library(RWeka)
library(NLP)
library(wordcloud)
library(SnowballC)
library(tokenizers)
library(quanteda)
library(plyr)
library(data.table)
# library(spacyr)
# spacy_initialize()
library(readxl)
library(Himsc)
files.name <- list.files(pattern = "pdf$")
# Rpdf <- readPDF(control = list(text = "-layout"))
# opinions <- Corpus(URISource(files), 
#                    readerControl = list(reader = Rpdf))

# ... TO BE CONTINUED IF NEEDED
# The purpose of the function readPDF() is to create a function 
# that reads in PDF files! So I wil use it latter if needed
# http://data.library.virginia.edu/reading-pdf-files-into-r-for-text-mining/

# second branch, using the pdftools package
# text07 <- pdf_text(files.name[12])
# waiting here as it costs time
# text07.info <- pdf_info(files_name[2])
# grep(x = text07, pattern = "SUMMARY OF CONTENTS") # the CONTENTS page

clean.punct <- function(a){
#  a <- gsub(a, pattern = "\n+ | [0-9]+ | \\s+ | \\?+ | \\.+ | :+ | [:punct:]+", replacement = " ")
  a <- gsub(a, pattern = "\n", replacement = " ")
  a <- gsub(a, pattern = "[0-9]+", replacement = " ")
  a <- gsub(a, pattern = "\\.+", replacement = " ")
  a <- gsub(a, pattern = "\\s+", replacement = " ")
  a <- gsub(a, pattern = "[[:punct:]]", replacement = " ")
  a <- gsub(a, pattern = "( | )", replacement = " ")
  a <- gsub(a, pattern = "\"", replacement = " ")
  return(a)
}

# text07 <- paste(clean.punct(text07), collapse = "")
# 
# a <- tokenize_word_stems(text07)
# tab <- table(a)
# tab <- data.frame(word = names(tab),
#                   count = as.numeric(tab),
#                   stringsAsFactors = F)
# setorder(tab, -count)

# todo, I don't know why the wordcloud can't be created.
# wordcloud(words = d$word, freq = d$freq, min.freq = 1,
#           max.words=200, random.order=FALSE, rot.per=0.35, 
#           colors=brewer.pal(8, "Dark2"))

# tab$word <- apply(tab$word, FUN = correct.words)

# So here is a collector open-source

# raw_text <- paste(readLines("big.txt"), collapse = " ")
# # Make the text lowercase and split it up creating a huge vector of word tokens.
# split_text <- strsplit(tolower(raw_text), "[^a-z]+")
# # Count the number of different type of words.
# word_count <- table(split_text)
# # Sort the words and create an ordered vector with the most common type of words first.
# sorted_words <- names(sort(word_count, decreasing = TRUE))
# write(sorted_words, file = "sorted_word.txt")

sorted_words <- readLines("sorted_words.txt")
correct.words <- function(word) {
  # Calculate the edit distance between the word and all other words in sorted_words.
  edit_dist <- adist(word, sorted_words)
  # Calculate the minimum edit distance to find a word that exists in big.txt 
  # with a limit of two edits.
  min_edit_dist <- min(edit_dist, 2)
  # Generate a vector with all words with this minimum edit distance.
  # Since sorted_words is ordered from most common to least common, the resulting
  # vector will have the most common / probable match first.
  proposals_by_prob <- c(sorted_words[ edit_dist <= min(edit_dist, 2)])
  # In case proposals_by_prob would be empty we append the word to be corrected...
  proposals_by_prob <- c(proposals_by_prob, word)
  # ... and return the first / most probable word in the vector.
  proposals_by_prob[1]
}

# http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know

## the following is the trial for tm package by using the Corpus object to process data.
# dir17 <- paste(getwd(), "/17", sep = "")
# corpus17 <- Corpus(DirSource(dir17))
# corpus17.rm.num <- tm_map(corpus17, removeNumbers)
# corpus17 <- tm_map(corpus17.rm.num, removePunctuation)
# # writeLines(as.character(corpus17$content))
# corpus17 <- tm_map(corpus17, stemDocument)
# # dtm <- DocumentTermMatrix(corpus17)
# 

# todo, make it a function to run.
goods.list <- unique(newer$sitc_sdesc)
# goods.list is the original name for the sitcs
write(goods.list, "goods.origin.txt")
goods.token <- paste(clean.punct(goods.list), collapse = " ")
goods.token <- tokenize_words(goods.token)
goods.tab <- table(goods.token)
goods.tab <- data.frame(word = names(goods.tab),
                  count = as.numeric(goods.tab),
                  stringsAsFactors = F)
setorder(goods.tab, -count)
write.csv(goods.tab, "goods.csv")


## choosing for the goods
text17 <- pdf_text(files.name[16])
text17 <- paste(clean.punct(text17), collapse = "")

text17.token <- tokenize_words(text17)
tab <- table(text17.token)
tab <- data.frame(word = names(tab),
                  count = as.numeric(tab),
                  stringsAsFactors = F)
setorder(tab, -count)


delete.words <- c("the","and", "of", "or", "for", "not", "products", "other", "parts", "preparations", "in", "to", "on", "a")

# common.words <- goods.tab[goods.tab$word %in% tab$word,]
# common.words <- common.words[which(common.words$word %in% delete.words == F), ]
# common.words$year <- 2017
# 
# wordcloud(words = common.words$word, freq = common.words$count, min.freq = 1,
#           max.words=200, random.order=FALSE, rot.per=0.35, 
#           colors=brewer.pal(8, "Dark2"))
# 
# write.csv(common.words, "common17.csv")

# initialize the data frame to store the word data
common.words <- as.data.frame(matrix(ncol = 3))
colnames(common.words) <- c("word", "count", "year")

for (i in 1:length(files.name)){
  # here, generate the year for data label first
  year <- gsub(files.name[i], pattern = "world_trade_report",
                          replacement = "" )
  year <- paste("20", gsub(year, pattern = "_e.pdf",
               replacement = "" ), sep = "")
  year <- as.numeric(year)
  
  # then, tokenize the text data
  text17 <- pdf_text(files.name[i])
  text17 <- paste(clean.punct(text17), collapse = "")
  
  text17.token <- tokenize_words(text17)
  tab <- table(text17.token)
  tab <- data.frame(word = names(tab),
                    count = as.numeric(tab),
                    stringsAsFactors = F)
  setorder(tab, -count)
  
  # find the corresponding word for in the goods.tab
  # add the label of year
  common.words.tmp <- goods.tab[goods.tab$word %in% tab$word,]
  common.words.tmp$year <- year
  
  # find the corresponding term in the sitc names
  tab <- tab[which(tab$word %in% delete.words == F),]
  
  common.item.tmp <- goods.list[toupper(common.words.tmp$word) %in% goods.list]
  # todo, find the exactly items.
  
  # then, merge the data from different years
  common.words <- rbind(common.words, common.words.tmp)
  common.words <- common.words[which(common.words$word %in% delete.words == F), ]
}

common.words <- common.words[-1, ]
write.csv(common.words, "common.words.0317.csv", row.names = F)
