# Jared Brewer
# Revised Rhetorical Analysis of 2016 Presidential Candidates
# Created: 7 February 2016
# Last modified: 7 Feburary 2016

library(qdap)
library(data.table)
library(wordcloud)
library(ggplot2)
library(scales)

raw.speech <- read.csv("./speech.csv", header = T)
speech <- scrubber(raw.speech, num2word = T, fix.comma = T, fix.space = T)
attach(speech)
raw.speech2 <- read.csv("./speech2.csv", header = T)
all.speech2 <- scrubber(raw.speech2, num2word = T, fix.comma = T, fix.space = T)
attach(speech2)

speech.sent <- data.table(speech)
add.syllables <- speech.sent[, syllables := syllable_sum(Sentence)]

johnkasich <- paste(scan("./john.txt", what="character"), collapse = " ")

kasich.df <- data.table(speech=johnkasich, person="John Kasich")
kasich.sent <- data.table(sentSplit(kasich.df, "speech"))
# Add a sentence counter and remove unnecessary variables
kasich.sub <- kasich.sent[, sentence.num := seq(nrow(kasich.sent))]
noperson <- kasich.sub[, person := NULL]
notot <- noperson[, tot := NULL]
colorder <- setcolorder(noperson, c("sentence.num", "speech"))

# Syllables per sentence
john.syl <- john.sub[, syllables := syllable_sum(speech)]
