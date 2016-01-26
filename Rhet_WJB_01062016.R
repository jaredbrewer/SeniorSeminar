# Jared Brewer
# Rhetorical Analysis of 2016 Presidential Candidates
# Created: 6 January 2016
# Last modified: 26 January 2016

library(qdap)
library(data.table)
library(wordcloud)
library(ggplot2)
library(scales)

# General remaining quirks: 
# setcolorder(dd1.sent, c("dd1.num", "speech")) does not work. It is unclear why it does not work.
# If it were to work, what would it add to the analysis, if anything? 
# Would it even be better than lapply? Probably not.
# Need to smooth out the plots and configure statistical analysis.

# Load data from the first Republican presidential debate from a local file.
# 'paste' loads data and concatenates it into character-based data.
# 'scan' reads list data.

# Donald Trump
dd1 <- paste(scan("./DonaldDebate1.txt", what="character"), collapse = " ")
attach(dd1)
wordcloud(dd1)

# Convert the data into a table that can later be used for more intensive analysis.
# Then, I divide the data into sentences.
dd1.df <- data.table(speech=dd1, person = "Donald Trump")
dd1.sent <- data.table(sentSplit(dd1.df, "speech"))

dd1.num <- seq(nrow(dd1.sent))
lapply(dd1.sent, as.numeric)
dd1.syl <- syllable_sum(dd1)

pol.dd1 <- polarity(dd1.sent$speech) $all
dd1.words <- pol.dd1$wc
dd1.pol <- pol.dd1$polarity
dd1.wc <- word_count(dd1)
freq_terms(dd1)

dd1sentavg <- c(dd1.syl/dd1.num)
summary(dd1sentavg)
dd1wordavg <- c(dd1.syl/dd1.wc)
summary(dd1wordavg)

plot(pol.dd1, dd1.words)

# 

dd2 <- paste(scan("./DonaldDebate2.txt", what="character"), collapse = " ")
attach(dd2)
wordcloud(dd2)

dd2.df <- data.table(speech=dd2, person = "Donald Trump")
dd2.sent <- data.table(sentSplit(dd2.df, "speech"))

dd2.num <- seq(nrow(dd2.sent))
lapply(dd2.sent, as.numeric)
dd2.syl <- syllable_sum(dd2)

pol.dd2 <- polarity(dd2.sent$speech) $all
dd2.words <- pol.dd2$wc
dd2.pol <- pol.dd2$polarity
dd2.wc <- word_count(dd2)
freq_terms(dd2)

dd2sentavg <- c(dd2.syl/dd2.num)
summary(dd2sentavg)
dd2wordavg <- c(dd2.syl/dd2.wc)
summary(dd2wordavg)

plot(pol.dd2, dd2.words)

dd3 <- paste(scan("./DonaldDebate3.txt", what="character"), collapse = " ")
attach(dd3)
wordcloud(dd3)

dd3.df <- data.table(speech=dd3, person = "Donald Trump")
dd3.sent <- data.table(sentSplit(dd3.df, "speech"))

dd3.num <- seq(nrow(dd3.sent))
lapply(dd3.sent, as.numeric)
dd3.syl <- syllable_sum(dd3)

pol.dd3 <- polarity(dd3.sent$speech) $all
dd3.words <- pol.dd3$wc
dd3.pol <- pol.dd3$polarity
dd3.wc <- word_count(dd3)
freq_terms(dd3)

dd3sentavg <- c(dd3.syl/dd3.num)
summary(dd3sentavg)
dd3wordavg <- c(dd3.syl/dd3.wc)
summary(dd3wordavg)

plot(pol.dd3, dd3.words)

dd4 <- paste(scan("./DonaldDebate4.txt", what="character"), collapse = " ")
attach(dd4)
wordcloud(dd4)

dd4.df <- data.table(speech=dd4, person = "Donald Trump")
dd4.sent <- data.table(sentSplit(dd4.df, "speech"))

dd4.num <- seq(nrow(dd4.sent))
lapply(dd4.sent, as.numeric)
dd4.syl <- syllable_sum(dd4)

pol.dd4 <- polarity(dd4.sent$speech) $all
dd4.words <- pol.dd4$wc
dd4.pol <- pol.dd4$polarity
dd4.wc <- word_count(dd4)
freq_terms(dd4)

dd4sentavg <- c(dd4.syl/dd4.num)
summary(dd4sentavg)
dd4wordavg <- c(dd4.syl/dd4.wc)
summary(dd4wordavg)

plot(pol.dd4, dd4.words)

dd5 <- paste(scan("./DonaldDebate5.txt", what="character"), collapse = " ")
attach(dd5)
wordcloud(dd5)

dd5.df <- data.table(speech=dd5, person = "Donald Trump")
dd5.sent <- data.table(sentSplit(dd5.df, "speech"))

dd5.num <- seq(nrow(dd5.sent))
lapply(dd5.sent, as.numeric)
dd5.syl <- syllable_sum(dd5)

pol.dd5 <- polarity(dd5.sent$speech) $all
dd5.words <- pol.dd5$wc
dd5.pol <- pol.dd5$polarity
dd5.wc <- word_count(dd5)
freq_terms(dd5)

dd5sentavg <- c(dd5.syl/dd5.num)
summary(dd5sentavg)
dd5wordavg <- c(dd5.syl/dd5.wc)
summary(dd5wordavg)

plot(pol.dd5, dd5.words)

dd6 <- paste(scan("./DonaldDebate6.txt", what="character"), collapse = " ")
attach(dd6)
wordcloud(dd6)

dd6.df <- data.table(speech=dd6, person = "Donald Trump")
dd6.sent <- data.table(sentSplit(dd6.df, "speech"))

dd6.num <- seq(nrow(dd6.sent))
lapply(dd6.sent, as.numeric)
dd6.syl <- syllable_sum(dd6)

pol.dd6 <- polarity(dd6.sent$speech) $all
dd6.words <- pol.dd6$wc
dd6.pol <- pol.dd6$polarity
dd6.wc <- word_count(dd6)
freq_terms(dd6)

dd6sentavg <- c(dd6.syl/dd6.num)
summary(dd6sentavg)
dd6wordavg <- c(dd6.syl/dd6.wc)
summary(dd6wordavg)

plot(pol.dd6, dd6.words)

## ##

AllDon <- paste(scan("./Donald.txt", what="character"), collapse = " ")
attach(AllDon)
wordcloud(AllDon)

AllDon.df <- data.table(speech=AlLDon, person = "Donald Trump")
AllDon.sent <- data.table(sentSplit(AllDon.df, "speech"))

AllDon.num <- seq(nrow(AllDon.sent))
lapply(AllDon.sent, as.numeric)
AllDon.syl <- syllable_sum(AllDon)

pol.AllDon <- polarity(AllDon.sent$speech) $all
AllDon.words <- pol.AllDon$wc
AllDon.pol <- pol.AllDon$polarity
AllDon.wc <- word_count(AllDon)
freq_terms(AllDon)

AlLDonsentavg <- c(AllDon.syl/AllDon.num)
summary(AllDonsentavg)
AllDonwordavg <- c(AllDon.syl/AllDon.wc)
summary(AllDonwordavg)

plot(pol.AllDon, dd6.AllDon)

# Function appears to be broken, will need to find a way to fix soon.
readability <- automated_readability_index(speech, sentence.num) $Automated_Readability_Index
summary(readability)

# Jeb Bush

jebd1 <- paste(scan("./JebDebate1.txt", what="character"), collapse = " ")
attach(jebd1)
wordcloud(jebd1)
                     
jebd1.df <- data.table(speech=jebd1, person = "Jeb Bush")
jebd1.sent <- data.table(sentSplit(jebd1.df, "speech"))

jebd1.num <- seq(nrow(jebd1.sent))
setcolorder(jebd1.sent, c("jebd1.num", "speech"))
jebd1.syl <- syllable_sum(jebd1)
summary(jebd1.syl)

pol.jebd1 <- polarity(jebd1.sent$speech) $all
jebd1.words <- pol.jebd1$wc
jebd1.wc <- word_count(jebd1)
jebd1.pol <- pol.jebd1$polarity
summary(pol.jebd1)
freq_terms(jebd1)

jebd1sentavg <- c(jebd1.sent/jebd1.num)
summary(jebd1sentavg)
jebd1wordavg <- c(jebd1.syl/jebd1.wc)
summary(jebd1wordavg)
                     
plot(jebd1.pol, jebd1.words)
                     
# Ted Cruz

tedd1 <- paste(scan("./TedDebate1.txt", what="character"), collapse = " ")
attach(tedd1)
wordcloud(tedd1)
                     
tedd1.df <- data.table(speech=tedd1, person = "Ted Cruz")
tedd1.sent <- data.table(sentSplit(tedd1.df, "speech"))
                     
tedd1.num <- seq(nrow(tedd1.sent))
setcolorder(tedd1.sent, c("tedd1.num", "speech"))
tedd1.syl <- syllable_sum(tedd1)
summary(tedd1.syl)
                     
pol.tedd1 <- polarity(tedd1.sent$speech) $all
tedd1.words <- pol.tedd1$wc
tedd1.wc <- word_count(tedd1)
tedd1.pol <- pol.tedd1$polarity
summary(pol.tedd1)
freq_terms(tedd1)
                     
tedd1sentavg <- c(tedd1.sent/tedd1.num)
summary(tedd1sentavg)
tedd1wordavg <- c(tedd1.syl/tedd1.wc)
summary(tedd1wordavg)

plot(tedd1.pol, tedd1.words)

# Marco Rubio

md1 <- paste(scan("./MarcoDebate1.txt", what="character"), collapse = " ")
attach(md1)
wordcloud(md1)
                   
md1.df <- data.table(speech=md1, person = "Marco Rubio")
md1.sent <- data.table(sentSplit(md1.df, "speech"))
                   
md1.num <- seq(nrow(md1.sent))
setcolorder(md1.sent, c("md1.num", "speech"))
md1.syl <- syllable_sum(md1)
summary(md1.syl)
                   
pol.md1 <- polarity(md1.sent$speech) $all
md1.words <- pol.md1$wc
md1.wc <- word_count(md1)
md1.pol <- pol.md1$polarity
summary(pol.md1)
freq_terms(md1)
                   
md1sentavg <- c(md1.sent/md1.num)
summary(md1sentavg)
md1wordavg <- c(md1.syl/md1.wc)
summary(md1wordavg)
                   
plot(md1.pol, md1.words)

# Ben Carson

bend1 <- paste(scan("./BenDebate1.txt", what="character"), collapse = " ")
attach(bend1)
wordcloud(bend1)

bend1.df <- data.table(speech=bend1, person = "Ben Carson")
bend1.sent <- data.table(sentSplit(bend1.df, "speech"))

bend1.num <- seq(nrow(bend1.sent))
setcolorder(bend1.sent, c("bend1.num", "speech"))
bend1.syl <- syllable_sum(bend1)
summary(bend1.syl)

pol.bend1 <- polarity(bend1.sent$speech) $all
bend1.words <- pol.bend1$wc
bend1.wc <- word_count(bend1)
bend1.pol <- pol.bend1$polarity
summary(pol.bend1)
freq_terms(bend1)

bend1sentavg <- c(bend1.sent/bend1.num)
summary(bend1sentavg)
bend1wordavg <- c(bend1.syl/bend1.wc)
summary(bend1wordavg)

plot(bend1.pol, bend1.words)

# Rand Paul

rd1 <- paste(scan("./RandDebate1.txt", what="character"), collapse = " ")
attach(rd1)
wordcloud(rd1)

rd1.df <- data.table(speech=rd1, person = "Rand Paul")
rd1.sent <- data.table(sentSplit(rd1.df, "speech"))

rd1.num <- seq(nrow(rd1.sent))
setcolorder(rd1.sent, c("rd1.num", "speech"))
rd1.syl <- syllable_sum(rd1)
summary(rd1.syl)

pol.rd1 <- polarity(rd1.sent$speech) $all
rd1.words <- pol.rd1$wc
rd1.wc <- word_count(rd1)
rd1.pol <- pol.rd1$polarity
summary(pol.rd1)
freq_terms(rd1)

rd1sentavg <- c(rd1.sent/rd1.num)
summary(rd1sentavg)
rd1wordavg <- c(rd1.syl/rd1.wc)
summary(rd1wordavg)

plot(rd1.pol, rd1.words)

# Chris Christie

cd1 <- paste(scan("./ChrisDebate1.txt", what="character"), collapse = " ")
attach(cd1)
wordcloud(cd1)

cd1.df <- data.table(speech=cd1, person = "Chris Christie")
cd1.sent <- data.table(sentSplit(cd1.df, "speech"))

cd1.num <- seq(nrow(cd1.sent))
setcolorder(cd1.sent, c("cd1.num", "speech"))
cd1.syl <- syllable_sum(cd1)
summary(cd1.syl)

pol.cd1 <- polarity(cd1.sent$speech) $all
cd1.words <- pol.cd1$wc
cd1.wc <- word_count(cd1)
cd1.pol <- pol.cd1$polarity
summary(pol.cd1)
freq_terms(cd1)

cd1sentavg <- c(cd1.sent/cd1.num)
summary(cd1sentavg)
cd1wordavg <- c(cd1.syl/cd1.wc)
summary(cd1wordavg)

plot(cd1.pol, cd1.words)

# John Kasich

jd1 <- paste(scan("./JohnDebate1.txt", what="character"), collapse = " ")
attach(jd1)
wordcloud(jd1)

jd1.df <- data.table(speech=jd1, person = "John Kasich")
jd1.sent <- data.table(sentSplit(jd1.df, "speech"))

jd1.num <- seq(nrow(jd1.sent))
setcolorder(jd1.sent, c("jd1.num", "speech"))
jd1.syl <- syllable_sum(jd1)
summary(jd1.syl)

pol.jd1 <- polarity(jd1.sent$speech) $all
jd1.words <- pol.jd1$wc
jd1.wc <- word_count(jd1)
jd1.pol <- pol.jd1$polarity
summary(pol.jd1)
freq_terms(jd1)

jd1sentavg <- c(jd1.sent/jd1.num)
summary(jd1sentavg)
jd1wordavg <- c(jd1.syl/jd1.wc)
summary(jd1wordavg)

plot(jd1.pol, jd1.words)

#########
# Bernie Sanders

bd1 <- paste(scan("./SandersDebate1.txt", what="character"), collapse = " ")
attach(bd1)
wordcloud(bd1)

bd1.df <- data.table(speech=bd1, person = "Bernie Sanders")
bd1.sent <- data.table(sentSplit(bd1.df, "speech"))

bd1.num <- seq(nrow(bd1.sent))
setcolorder(bd1.sent, c("bd1.num", "speech"))
bd1.syl <- syllable_sum(bd1)
summary(bd1.syl)

pol.bd1 <- polarity(bd1.sent$speech) $all
bd1.words <- pol.bd1$wc
bd1.wc <- word_count(bd1)
bd1.pol <- pol.bd1$polarity
summary(pol.bd1)
freq_terms(bd1)

bd1sentavg <- c(bd1.syl/bd1.num)
summary(bd1sentavg)
bd1wordavg <- c(bd1.syl/bd1.wc)
summary(bd1wordavg)

plot(bd1.pol, bd1.words)

bd2 <- paste(scan("./SandersDebate2.txt", what="character"), collapse = " ")
attach(bd2)
wordcloud(bd2)

bd2.df <- data.table(speech=bd2, person = "Bernie Sanders")
bd2.sent <- data.table(sentSplit(bd2.df, "speech"))

bd2.num <- seq(nrow(bd2.sent))
setcolorder(bd2.sent, c("bd2.num", "speech"))
bd2.syl <- syllable_sum(bd2)
summary(bd2.syl)

pol.bd2 <- polarity(bd2.sent$speech) $all
bd2.words <- pol.bd2$wc
bd2.wc <- word_count(bd2)
bd2.pol <- pol.bd2$polarity
summary(pol.bd2)
freq_terms(bd2)

bd2sentavg <- c(bd2.sent/bd2.num)
summary(bd2sentavg)
bd2wordavg <- c(bd2.syl/bd2.wc)
summary(bd2wordavg)

plot(bd2.pol, bd2.words)

bd3 <- paste(scan("./SandersDebate3.txt", what="character"), collapse = " ")
attach(bd3)
wordcloud(bd3)

bd3.df <- data.table(speech=bd3, person = "Bernie Sanders")
bd3.sent <- data.table(sentSplit(bd3.df, "speech"))

bd3.num <- seq(nrow(bd3.sent))
setcolorder(bd3.sent, c("bd3.num", "speech"))
bd3.syl <- syllable_sum(bd3)
summary(bd3.syl)

pol.bd3 <- polarity(bd3.sent$speech) $all
bd3.words <- pol.bd3$wc
bd3.wc <- word_count(bd3)
bd3.pol <- pol.bd3$polarity
summary(pol.bd3)
freq_terms(bd3)

bd3sentavg <- c(bd3.sent/bd3.num)
summary(bd3sentavg)
bd3wordavg <- c(bd3.syl/bd3.wc)
summary(bd3wordavg)

plot(bd3.pol, bd3.words)

bd4 <- paste(scan("./SandersDebate4.txt", what="character"), collapse = " ")
attach(bd4)
wordcloud(bd4)

bd4.df <- data.table(speech=bd4, person = "Bernie Sanders")
bd4.sent <- data.table(sentSplit(bd4.df, "speech"))

bd4.num <- seq(nrow(bd4.sent))
setcolorder(bd4.sent, c("bd4.num", "speech"))
bd4.syl <- syllable_sum(bd4)
summary(bd4.syl)

pol.bd4 <- polarity(bd4.sent$speech) $all
bd4.words <- pol.bd4$wc
bd4.wc <- word_count(bd4)
bd4.pol <- pol.bd4$polarity
summary(pol.bd4)
freq_terms(bd4)

bd4sentavg <- c(bd4.sent/bd4.num)
summary(bd4sentavg)
bd4wordavg <- c(bd4.syl/bd4.wc)
summary(bd4wordavg)

plot(bd4.pol, bd4.words)

readability <- automated_readability_index(pol.bernie, berniesentence.num)

# Hillary Clinton

hd1 <- paste(scan("./HillaryDebate1.txt", what="character"), collapse = " ")
attach(hd1)
wordcloud(hd1)

hd1.df <- data.table(speech=hd1, person = "Hillary Clinton")
hd1.sent <- data.table(sentSplit(hd1.df, "speech"))

hd1.num <- seq(nrow(hd1.sent))
setcolorder(hd1.sent, c("hd1.num", "speech"))
hd1.syl <- syllable_sum(hd1)
summary(hd1.syl)

pol.hd1 <- polarity(hd1.sent$speech) $all
hd1.words <- pol.hd1$wc
hd1.wc <- word_count(hd1)
hd1.pol <- pol.hd1$polarity
summary(pol.hd1)
freq_terms(hd1)

hd1sentavg <- c(hd1.sent/hd1.num)
summary(hd1sentavg)
hd1wordavg <- c(hd1.syl/hd1.wc)
summary(hd1wordavg)

plot(hd1.pol, hd1.words)

hd2 <- paste(scan("./HillaryDebate2.txt", what="character"), collapse = " ")
attach(hd2)
wordcloud(hd2)

hd2.df <- data.table(speech=hd2, person = "Hillary Clinton")
hd2.sent <- data.table(sentSplit(hd2.df, "speech"))

hd2.num <- seq(nrow(hd2.sent))
setcolorder(hd2.sent, c("hd2.num", "speech"))
hd2.syl <- syllable_sum(hd2)
summary(hd2.syl)

pol.hd2 <- polarity(hd2.sent$speech) $all
hd2.words <- pol.hd2$wc
hd2.wc <- word_count(hd2)
hd2.pol <- pol.hd2$polarity
summary(pol.hd2)
freq_terms(hd2)

hd2sentavg <- c(hd2.sent/hd2.num)
summary(hd2sentavg)
hd2wordavg <- c(hd2.syl/hd2.wc)
summary(hd2wordavg)

plot(hd2.pol, hd2.words)

hd3 <- paste(scan("./HillaryDebate3.txt", what="character"), collapse = " ")
attach(hd3)
wordcloud(hd3)

hd3.df <- data.table(speech=hd3, person = "Hillary Clinton")
hd3.sent <- data.table(sentSplit(hd3.df, "speech"))

hd3.num <- seq(nrow(hd3.sent))
setcolorder(hd3.sent, c("hd3.num", "speech"))
hd3.syl <- syllable_sum(hd3)
summary(hd3.syl)

pol.hd3 <- polarity(hd3.sent$speech) $all
hd3.words <- pol.hd3$wc
hd3.wc <- word_count(hd3)
hd3.pol <- pol.hd3$polarity
summary(pol.hd3)
freq_terms(hd3)

hd3sentavg <- c(hd3.sent/hd3.num)
summary(hd3sentavg)
hd3wordavg <- c(hd3.syl/hd3.wc)
summary(hd3wordavg)

plot(hd3.pol, hd3.words)

hd4 <- paste(scan("./HillaryDebate4.txt", what="character"), collapse = " ")
attach(hd4)
wordcloud(hd4)

hd4.df <- data.table(speech=hd4, person = "Hillary Clinton")
hd4.sent <- data.table(sentSplit(hd4.df, "speech"))

hd4.num <- seq(nrow(hd4.sent))
setcolorder(hd4.sent, c("hd4.num", "speech"))
hd4.syl <- syllable_sum(hd4)
summary(hd4.syl)

pol.hd4 <- polarity(hd4.sent$speech) $all
hd4.words <- pol.hd4$wc
hd4.wc <- word_count(hd4)
hd4.pol <- pol.hd4$polarity
summary(pol.hd4)
freq_terms(hd4)

hd4sentavg <- c(hd4.sent/hd4.num)
summary(hd4sentavg)
hd4wordavg <- c(hd4.syl/hd4.wc)
summary(hd4wordavg)

plot(hd4.pol, hd4.words)

# Martin O'Malley

mom1 <- paste(scan("./MartinDebate1.txt", what="character"), collapse = " ")
attach(mom1)
wordcloud(mom1)

mom1.df <- data.table(speech=mom1, person = "Martin O’Malley")
mom1.sent <- data.table(sentSplit(mom1.df, "speech"))

mom1.num <- seq(nrow(mom1.sent))
summary(mom1.num)
setcolorder(mom1.sent, c("mom1.num", "speech"))
mom1.syl <- syllable_sum(mom1)
summary(mom1.syl)

pol.mom1 <- polarity(mom1.sent$speech) $all
mom1.words <- pol.mom1$wc
mom1.wc <- word_count(mom1)
mom1.pol <- pol.mom1$polarity
summary(pol.mom1)
freq_terms(mom1)

mom1sentavg <- c(mom1.syl/mom1.num)
summary(mom1sentavg)
mom1wordavg <- c(mom1.syl/mom1.wc)
summary(mom1wordavg)

plot(mom1.pol, mom1.words)

mom2 <- paste(scan("./MartinDebate2.txt", what="character"), collapse = " ")
attach(mom2)
wordcloud(mom2)

mom2.df <- data.table(speech=mom2, person = "Martin O’Malley")
mom2.sent <- data.table(sentSplit(mom2.df, "speech"))

mom2.num <- seq(nrow(mom2.sent))
setcolorder(mom2.sent, c("mom2.num", "speech"))
mom2.syl <- syllable_sum(mom2)
summary(mom2.syl)

pol.mom2 <- polarity(mom2.sent$speech) $all
mom2.words <- pol.mom2$wc
mom2.wc <- word_count(mom2)
mom2.pol <- pol.mom2$polarity
summary(pol.mom2)
freq_terms(mom2)

mom2sentavg <- c(mom2.sent/mom2.num)
summary(mom2sentavg)
mom2wordavg <- c(mom2.syl/mom2.wc)
summary(mom2wordavg)

plot(mom2.pol, mom2.words)

mom3 <- paste(scan("./MartinDebate3.txt", what="character"), collapse = " ")
attach(mom3)
wordcloud(mom3)

mom3.df <- data.table(speech=mom3, person = "Martin O’Malley")
mom3.sent <- data.table(sentSplit(mom3.df, "speech"))

mom3.num <- seq(nrow(mom3.sent))
setcolorder(mom3.sent, c("mom3.num", "speech"))
mom3.syl <- syllable_sum(mom3)
summary(mom3.syl)

pol.mom3 <- polarity(mom3.sent$speech) $all
mom3.words <- pol.mom3$wc
mom3.wc <- word_count(mom3)
mom3.pol <- pol.mom3$polarity
summary(pol.mom3)
freq_terms(mom3)

mom3sentavg <- c(mom3.sent/mom3.num)
summary(mom3sentavg)
mom3wordavg <- c(mom3.syl/mom3.wc)
summary(mom3wordavg)

plot(mom3.pol, mom3.words)

mom4 <- paste(scan("./MartinDebate4.txt", what="character"), collapse = " ")
attach(mom4)
wordcloud(mom4)

mom4.df <- data.table(speech=mom4, person = "Martin O’Malley")
mom4.sent <- data.table(sentSplit(mom4.df, "speech"))

mom4.num <- seq(nrow(mom4.sent))
setcolorder(mom4.sent, c("mom4.num", "speech"))
mom4.syl <- syllable_sum(mom4)
summary(mom4.syl)

pol.mom4 <- polarity(mom4.sent$speech) $all
mom4.words <- pol.mom4$wc
mom4.wc <- word_count(mom4)
mom4.pol <- pol.mom4$polarity
summary(pol.mom4)
freq_terms(mom4)

mom4sentavg <- c(mom4.sent/mom4.num)
summary(mom4sentavg)
mom4wordavg <- c(mom4.syl/mom4.wc)
summary(mom4wordavg)

plot(mom4.pol, mom4.words)