# Jared Brewer
# Rhetorical Reframing
# Created: 7 February 2016
# Last modified: 7 February 2016

library(qdap)
library(data.table)
library(wordcloud)
library(ggplot2)
library(scales)

raw.donald <- paste(scan("./donald.txt", what="character"), collapse = " ")
donald <- scrubber(raw.donald, num2word = T, fix.comma = T, fix.space = T)
attach(donald)
wordcloud(donald)
freq_terms(donald)

donald.df <- data.table(speech=donald, person = "Donald Trump")
donald.sent <- data.table(sentSplit(donald.df, "speech"))

donald.num <- seq(nrow(donald.sent))
donald.syl <- syllable_sum(donald)
donald.wc <- word_count(donald)

donald.sentavg <- c(donald.syl/donald.num)
summary(donald.sentavg)
donald.wordavg <- c(donald.syl/donald.wc)
summary(donald.wordavg)

raw.hillary <- paste(scan("./hillary.txt", what="character"), collapse = " ")
hillary <- scrubber(raw.hillary, num2word = T, fix.comma = T, fix.space = T)
attach(hillary)
wordcloud(hillary)
freq_terms(hillary)

hillary.df <- data.table(speech=hillary, person = "Hillary Clinton")
hillary.sent <- data.table(sentSplit(hillary.df, "speech"))

hillary.num <- seq(nrow(hillary.sent))
hillary.syl <- syllable_sum(hillary)
hillary.wc <- word_count(hillary)

hillary.sentavg <- c(hillary.syl/hillary.num)
summary(hillary.sentavg)
hillary.wordavg <- c(hillary.syl/hillary.wc)
summary(hillary.wordavg)

raw.bernie <- paste(scan("./bernie.txt", what="character"), collapse = " ")
bernie <- scrubber(raw.bernie, num2word = T, fix.comma = T, fix.space = T)
attach(bernie)
wordcloud(bernie)
freq_terms(bernie)

bernie.df <- data.table(speech=bernie, person = "Bernie Sanders")
bernie.sent <- data.table(sentSplit(bernie.df, "speech"))

bernie.num <- seq(nrow(bernie.sent))
bernie.syl <- syllable_sum(bernie)
bernie.wc <- word_count(bernie)

bernie.sentavg <- c(bernie.syl/bernie.num)
summary(bernie.sentavg)
bernie.wordavg <- c(bernie.syl/bernie.wc)
summary(bernie.wordavg)

raw.ted <- paste(scan("./ted.txt", what="character"), collapse = " ")
ted <- scrubber(raw.ted, num2word = T, fix.comma = T, fix.space = T)
attach(ted)
wordcloud(ted)
freq_terms(ted)

ted.df <- data.table(speech=ted, person = "Ted Cruz")
ted.sent <- data.table(sentSplit(ted.df, "speech"))

ted.num <- seq(nrow(ted.sent))
ted.syl <- syllable_sum(ted)
ted.wc <- word_count(ted)

ted.sentavg <- c(ted.syl/ted.num)
summary(ted.sentavg)
ted.wordavg <- c(ted.syl/ted.wc)
summary(ted.wordavg)

raw.marco <- paste(scan("./marco.txt", what="character"), collapse = " ")
marco <- scrubber(raw.marco, num2word = T, fix.comma = T, fix.space = T)
attach(marco)
wordcloud(marco)
freq_terms(marco)

marco.df <- data.table(speech=marco, person = "Marco Rubio")
marco.sent <- data.table(sentSplit(marco.df, "speech"))

marco.num <- seq(nrow(marco.sent))
marco.syl <- syllable_sum(marco)
marco.wc <- word_count(marco)

marco.sentavg <- c(marco.syl/marco.num)
summary(marco.sentavg)
marco.wordavg <- c(marco.syl/marco.wc)
summary(marco.wordavg)

raw.ben <- paste(scan("./ben.txt", what="character"), collapse = " ")
ben <- scrubber(raw.ben, num2word = T, fix.comma = T, fix.space = T)
attach(ben)
wordcloud(ben)
freq_terms(ben)

ben.df <- data.table(speech=ben, person = "Ben Carson")
ben.sent <- data.table(sentSplit(ben.df, "speech"))

ben.num <- seq(nrow(ben.sent))
ben.syl <- syllable_sum(ben)
ben.wc <- word_count(ben)

ben.sentavg <- c(ben.syl/ben.num)
summary(ben.sentavg)
ben.wordavg <- c(ben.syl/ben.wc)
summary(ben.wordavg)

raw.chris <- paste(scan("./chris.txt", what="character"), collapse = " ")
chris <- scrubber(raw.chris, num2word = T, fix.comma = T, fix.space = T)
attach(chris)
wordcloud(chris)
freq_terms(chris)

chris.df <- data.table(speech=chris, person = "Chris Christie")
chris.sent <- data.table(sentSplit(chris.df, "speech"))

chris.num <- seq(nrow(chris.sent))
chris.syl <- syllable_sum(chris)
chris.wc <- word_count(chris)

chris.sentavg <- c(chris.syl/chris.num)
summary(chris.sentavg)
chris.wordavg <- c(chris.syl/chris.wc)
summary(chris.wordavg)

raw.rand <- paste(scan("./rand.txt", what="character"), collapse = " ")
rand <- scrubber(raw.rand, num2word = T, fix.comma = T, fix.space = T)
attach(rand)
wordcloud(rand)
freq_terms(rand)

rand.df <- data.table(speech=rand, person = "Rand Paul")
rand.sent <- data.table(sentSplit(rand.df, "speech"))

rand.num <- seq(nrow(rand.sent))
rand.syl <- syllable_sum(rand)
rand.wc <- word_count(rand)

rand.sentavg <- c(rand.syl/rand.num)
summary(rand.sentavg)
rand.wordavg <- c(rand.syl/rand.wc)
summary(rand.wordavg)

raw.jeb <- paste(scan("./jeb.txt", what="character"), collapse = " ")
jeb <- scrubber(raw.jeb, num2word = T, fix.comma = T, fix.space = T)
attach(jeb)
wordcloud(jeb)
freq_terms(jeb)

jeb.df <- data.table(speech=jeb, person = "Jeb Bush")
jeb.sent <- data.table(sentSplit(jeb.df, "speech"))

jeb.num <- seq(nrow(jeb.sent))
jeb.syl <- syllable_sum(jeb)
jeb.wc <- word_count(jeb)

jeb.sentavg <- c(jeb.syl/jeb.num)
summary(jeb.sentavg)
jeb.wordavg <- c(jeb.syl/jeb.wc)
summary(jeb.wordavg)

raw.john <- paste(scan("./john.txt", what="character"), collapse = " ")
john <- scrubber(raw.john, num2word = T, fix.comma = T, fix.space = T)
attach(john)
wordcloud(john)
freq_terms(john)

john.df <- data.table(speech=john, person = "John Kasich")
john.sent <- data.table(sentSplit(john.df, "speech"))

john.num <- seq(nrow(john.sent))
john.syl <- syllable_sum(john)
john.wc <- word_count(john)

john.sentavg <- c(john.syl/john.num)
summary(john.sentavg)
john.wordavg <- c(john.syl/john.wc)
summary(john.wordavg)

raw.martin <- paste(scan("./martin.txt", what="character"), collapse = " ")
martin <- scrubber(raw.martin, num2word = T, fix.comma = T, fix.space = T)
attach(martin)
wordcloud(martin)
freq_terms(martin)

martin.df <- data.table(speech=martin, person = "Martin O'Malley")
martin.sent <- data.table(sentSplit(martin.df, "speech"))

martin.num <- seq(nrow(martin.sent))
martin.syl <- syllable_sum(martin)
martin.wc <- word_count(martin)

martin.sentavg <- c(martin.syl/martin.num)
summary(martin.sentavg)
martin.wordavg <- c(martin.syl/martin.wc)
summary(martin.wordavg)