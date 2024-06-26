rm(list=ls())

library(tidyverse)
library(magrittr)
library(lubridate)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textstats)
library(quanteda.textplots)
library(tm)
library(sentimentr)
library(parallel)
library(rvest)

setwd("/Users/brutus/Desktop/Computational Social Science/Datasets")


# styling: font size etc.
# https://stackoverflow.com/questions/29274501/r-markdown-changing-font-size-and-font-type-in-html-output


## QUERY on Swissdox@LiRI
# -> mostly french, rheato-romanic, so probably case sensitive search
# Keywords: covid, corona, Covid, Corona, covid*, corona*, Covid*, Corona*
df <- read_tsv(file = '1b4bcb1e-d578-4ce3-bc74-b9f3362b1600__2022_06_04T12_55_03.tsv')

## SAMPLE FOR INITIAL ANALYSIS: 1%: 11284 of 1128420
set.seed(1234)
dat <- slice_sample(df, n=11284, replace = F)
dat <- distinct(dat, content_id, .keep_all = T) #11089 from 11284
saveRDS(dat, "sample mediadata 11284.RDS")
rm(df)
dat <- readRDS("sample mediadata 11284.RDS") # this is data BEFORE cleaning!


## DATA PREPARATION & CLEANING
# remove HTML tags
cleanHTMLtags <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))}
dat$content <- cleanHTMLtags(dat$content)

#media <- data.frame(table(dat$medium_name))
# classify this thing!
# Kategorien: fög, Jahrbuch Qualität der Medien, S. 144
# https://www.foeg.uzh.ch/dam/jcr:80fd64b0-c078-4ba7-8bba-e2c79bf1a654/2021_Gesamtausgabe.pdf
dat <- mutate(dat, medium_type = case_when(
  medium_name == "20 minuten" ~ "Pendlerzeitung",
  medium_name == "20 minuten online" ~ "Pendlerzeitung Online",
  medium_name == "20 minutes" ~ "Pendlerzeitung",
  medium_name == "20 minutes online" ~ "Pendlerzeitung Online",
  medium_name == "24 heures" ~ "Abonnement",
  medium_name == "24heures.ch" ~ "Abonnement Online",
  medium_name == "Aargauer Zeitung / MLZ" ~ "Abonnement",
  medium_name == "aargauerzeitung.ch" ~ "Abonnement Online",
  medium_name == "Appenzeller Zeitung" ~ "Abonnement",
  medium_name == "Badener Tagblatt" ~ "Abonnement",
  medium_name == "badenertagblatt.ch" ~ "Abonnement Online",
  medium_name == "Basler Zeitung" ~ "Abonnement",
  medium_name == "bazonline.ch" ~ "Abonnement Online",
  medium_name == "bellevue.nzz.ch" ~ "Magazin",
  medium_name == "Beobachter" ~ "Magazin",
  medium_name == "beobachter.ch" ~ "Magazin Online",
  medium_name == "Beobachter.ch" ~ "Magazin Online",
  medium_name == "Berner Oberländer" ~ "Abonnement",
  medium_name == "Berner Zeitung" ~ "Abonnement",
  medium_name == "berneroberlaender.ch" ~ "Abonnement Online",
  medium_name == "bernerzeitung.ch" ~ "Abonnement Online",
  medium_name == "Bilan" ~ "Magazin",
  medium_name == "Bilanz" ~ "Magazin",
  medium_name == "bilanz.ch" ~ "Magazin Online",
  medium_name == "Blick" ~ "Boulevard",
  medium_name == "blick.ch" ~ "Boulevard Online",
  medium_name == "BZ - Langenthaler Tagblatt" ~ "Abonnement",
  medium_name == "bz - Zeitung für die Region Basel" ~ "Abonnement",
  medium_name == "bzbasel.ch" ~ "Abonnement Online",
  medium_name == "cash.ch" ~ "Magazin Online",
  medium_name == "Das Magazin" ~ "Magazin",
  medium_name == "Der Bund" ~ "Abonnement",
  medium_name == "Der Landbote" ~ "Abonnement",
  medium_name == "derbund.ch" ~ "Abonnement Online",
  medium_name == "Die Wochenzeitung" ~ "Abonnement",
  medium_name == "Femina" ~ "Magazin",
  medium_name == "Finanz und Wirtschaft" ~ "Abonnement",
  medium_name == "fuw.ch" ~ "Abonnement Online",
  medium_name == "Glückspost" ~ "Boulevard",
  medium_name == "Grenchner Tagblatt" ~ "Abonnement",
  medium_name == "grenchnertagblatt.ch" ~ "Abonnement Online",
  medium_name == "Handelszeitung" ~ "Abonnement",
  medium_name == "handelszeitung.ch" ~ "Abonnement Online",
  medium_name == "HZ Insurance" ~ "Magazin",
  medium_name == "illustre.ch" ~ "Magazin Online",
  medium_name == "L'Illustré" ~ "Magazin",
  medium_name == "landbote.ch" ~ "Abonnement Online",
  medium_name == "langenthalertagblatt.ch" ~ "Abonnement Online",
  medium_name == "Le Matin Dimanche" ~ "Sonntagszeitung",
  medium_name == "Le Temps" ~ "Abonnement",
  medium_name == "lematin.ch" ~ "Boulevard Online",
  medium_name == "letemps.ch" ~ "Abonnement Online",
  medium_name == "Limmattaler Zeitung" ~ "Abonnement",
  medium_name == "Limmattaler Zeitung / MLZ" ~ "Abonnement",
  medium_name == "limmattalerzeitung.ch" ~ "Abonnement Online",
  medium_name == "Luzerner Zeitung" ~ "Abonnement",
  medium_name == "luzernerzeitung.ch" ~ "Abonnement Online",
  medium_name == "Neue Zürcher Zeitung" ~ "Abonnement",
  medium_name == "Newsnet / 24 heures" ~ "Abonnement Online",
  medium_name == "Newsnet / Berner Zeitung" ~ "Abonnement Online",
  medium_name == "Newsnet / Der Bund" ~ "Abonnement Online",
  medium_name == "Nidwaldner Zeitung" ~ "Abonnement",
  medium_name == "NZZ am Sonntag" ~ "Sonntagszeitung",
  medium_name == "NZZ am Sonntag Magazin" ~ "Magazin",
  medium_name == "nzz.ch" ~ "Abonnement Online",
  medium_name == "Obwaldner Zeitung" ~ "Abonnement",
  medium_name == "Oltner Tagblatt / MLZ" ~ "Abonnement",
  medium_name == "oltnertagblatt.ch" ~ "Abonnement Online",
  medium_name == "PME Magazine" ~ "Magazin",
  medium_name == "pme.ch" ~ "Magazin Online",
  medium_name == "rsi.ch" ~ "SRG Online",
  medium_name == "rts.ch" ~ "SRG Online",
  medium_name == "Schweizer Familie" ~ "Magazin",
  medium_name == "Schweizer Illustrierte" ~ "Boulevard",
  medium_name == "Schweizer LandLiebe" ~ "Magazin",
  medium_name == "schweizer‐illustrierte.ch" ~ "Boulevard Online",
  medium_name == "Solothurner Zeitung / MLZ" ~ "Abonnement",
  medium_name == "solothurnerzeitung.ch" ~ "Abonnement Online",
  medium_name == "Sonntagsblick" ~ "Boulevard",
  medium_name == "SonntagsZeitung" ~ "Sonntagszeitung",
  medium_name == "srf Audio" ~ "SRG Online",
  medium_name == "srf Video" ~ "SRG Online",
  medium_name == "srf.ch" ~ "SRG Online",
  medium_name == "St. Galler Tagblatt" ~ "Abonnement",
  medium_name == "swissinfo.ch" ~ "SRG Online",
  medium_name == "tagblatt.ch" ~ "Abonnement Online",
  medium_name == "Tages-Anzeiger" ~ "Abonnement",
  medium_name == "tagesanzeiger.ch" ~ "Abonnement Online",
  medium_name == "tdg.ch" ~ "Abonnement Online",
  medium_name == "Tele" ~ "Magazin",
  medium_name == "Thalwiler Anzeiger/Sihltaler" ~ "Abonnement",
  medium_name == "Thuner Tagblatt" ~ "Abonnement",
  medium_name == "thunertagblatt.ch" ~ "Abonnement Online",
  medium_name == "Thurgauer Zeitung" ~ "Abonnement",
  medium_name == "Toggenburger Tagblatt" ~ "Abonnement",
  medium_name == "Tribune de Genève" ~ "Abonnement",
  medium_name == "TV Star" ~ "Magazin",
  medium_name == "Urner  Zeitung" ~ "Abonnement",
  medium_name == "Werdenberger & Obertoggenburger" ~ "Abonnement",
  medium_name == "Wiler Zeitung" ~ "Abonnement",
  medium_name == "Zofinger Tagblatt / MLZ" ~ "Abonnement",
  medium_name == "zsz.ch" ~ "Abonnement Online",
  medium_name == "Zuger Presse" ~ "Abonnement",
  medium_name == "Zuger Zeitung" ~ "Abonnement",
  medium_name == "Zugerbieter" ~ "Abonnement",
  medium_name == "zuonline.ch" ~ "Abonnement Online",
  medium_name == "Zürcher Unterländer" ~ "Abonnement",
  medium_name == "Zürichsee-Zeitung" ~ "Abonnement",
  medium_name == "züritipp (Tages-Anzeiger)" ~ "Magazin",
  TRUE ~ "other"))

dat <- mutate(dat, medium_type_group = case_when(
  medium_type == "Abonnement" ~ "Abonnement",
  medium_type == "Abonnement Online" ~ "Abonnement",
  medium_type == "Boulevard" ~ "Boulevard",
  medium_type == "Boulevard Online" ~ "Boulevard",
  medium_type == "Magazin" ~ "Magazin",
  medium_type == "Magazin Online" ~ "Magazin",
  medium_type == "Pendlerzeitung" ~ "Pendlerzeitung",
  medium_type == "Pendlerzeitung Online" ~ "Pendlerzeitung",
  medium_type == "Sonntagszeitung" ~ "Sonntagszeitung",
  medium_type == "SRG Online" ~ "SRG Online",
  TRUE ~ "other"))


## EXPLORE DATA
# language
data.frame(table(dat$language)) %>%
  ggplot(.,  aes(x=reorder(Var1, Freq), y=Freq)) +
  geom_col() + 
  coord_flip() +
  theme_minimal() 
prop.table(table(dat$language))


# regional
data.frame(table(dat$regional)) %>%
  ggplot(.,  aes(x=reorder(Var1, Freq), y=Freq)) +
  geom_col() + 
  coord_flip() +
  theme_minimal() 
# hm. a bit unstructured, headers etc. maybe you should clean. is it worth it..?

# doctype_description: not neccessary, newly coded as medium_type
data.frame(table(dat$doctype_description)) %>%
  ggplot(.,  aes(x=reorder(Var1, Freq), y=Freq)) +
  geom_col() + 
  coord_flip() +
  theme_minimal() 
prop.table(table(dat$doctype_description))

# medium type
data.frame(table(dat$medium_type)) %>%
  ggplot(.,  aes(x=reorder(Var1, Freq), y=Freq)) +
  geom_col() + 
  coord_flip() +
  theme_minimal() 
prop.table(table(dat$medium_type))

# medium type group
data.frame(table(dat$medium_type_group)) %>%
  ggplot(.,  aes(x=reorder(Var1, Freq), y=Freq)) +
  geom_col() + 
  coord_flip() +
  theme_minimal() 
prop.table(table(dat$medium_type_group))

# rubric
# maybe classify with machine learning?
# https://d1wqtxts1xzle7.cloudfront.net/43371696/310585-with-cover-page-v2.pdf?Expires=1656941925&Signature=K-Iq49JzG7V1czn3jl4o0IlUR6fbgbS-v1uy437U7JumEKEKjfTp5Eu6sFshGv1pjXGw7YMDFkDJ5FNFi3~sAvYz5gWNhMnKmVybWRZut9Lk1u-8Lgyqmoqqiig~tRjjVu1NK6CFS9j3v6B2F4IiZ6gAWquPc2sUvmSuC33DvKIhU2IZ0eccemkYLhQcnW-Lx-5lVu1vYbEnD-Sq9KA3eVuVJx5FYL-5E1yf1rbepEY6oOr39kiI90Truj4CDWu07iev0l7wvDEm-A7-JVivDXobGDWA9QXPhNJOOO8xLXfNBXb0kw8yJk80ee08zh0fS7jI87VzpyO-q7CQvAwTfQ__&Key-Pair-Id=APKAJLOHF5GGSLRBV4ZA
dat_de <- dat[dat$language=="de",]
# must be separated into languages

dat_de$rubric <- tolower(dat_de$rubric)

# initial categories: eco, pol, health, sport, culture, international, regional, opinion, entertainment
dat_de <- mutate(dat_de, rub = case_when(
  rubric == "wirtschaft" ~ "eco",
  rubric == "schweiz" ~ "none",
  rubric == "front" ~ "none",
  rubric == "ausland" ~ "international",
  rubric == "coronavirus" ~ "health",
  rubric == "meinung" ~ "opinion",
  rubric == "news - schweiz" ~ "none",
  rubric == "kultur" ~ "culture",
  rubric == "schwerpunkt" ~ "none",
  rubric == "forum" ~ "culture",
  rubric == "zürich" ~ "regional",
  rubric == "thema" ~ "none",
  rubric == "aargau" ~ "regional",
  rubric == "news" ~ "none",
  rubric == "zentralschweiz" ~ "regional",
  rubric == "piazza" ~ "culture",
  rubric == "ostschweiz" ~ "regional",
  rubric == "usa" ~ "international",
  rubric == "news - wirtschaft" ~ "eco",
  rubric == "regionen" ~ "regional",
  rubric == "märkte" ~ "eco",
  rubric == "sport -fussball" ~ "sport",
  rubric == "unternehmen" ~ "eco",
  rubric == "letzte seite" ~ "none",
  rubric == "inland" ~ "none",
  rubric == "focus" ~ "none",
  rubric == "kanton solothurn" ~ "regional",
  rubric == "magazin" ~ "none",
  rubric == "winterthur" ~ "regional",
  rubric == "bern" ~ "regional",
  rubric == "regionalsport" ~ "sport",
  rubric == "leserbriefe" ~ "opinion",
  rubric == "blick sport" ~ "sport",
  rubric == "meinungen" ~ "opinion",
  rubric == "meinungen und debatte" ~ "opinion",
  rubric == "limmattal" ~ "regional",
  rubric == "kultur & gesellschaft" ~ "culture",
  rubric == "panorama" ~ "none",
  rubric == "news - international" ~ "international",
  rubric == "leben & wissen" ~ "none",
  rubric == "kanton bern" ~ "regional",
  TRUE ~ "none"))
 
data.frame(sort(table(dat_de$rub), decreasing=TRUE)[1:40]) %>%
  ggplot(., aes(x=reorder(Var1, Freq), y=Freq)) +
  geom_col() + 
  coord_flip() +
  theme_minimal()
#prop.table(table(dat$rubric)[1:50])
#sum(is.na(dat_de$rub)) #2819 na out of 9761
#sort(table(dat$rubric), decreasing=TRUE)[1:10]

data.frame(sort(table(dat_de$rub), decreasing=TRUE)) %>%
  ggplot(., aes(x=reorder(Var1, Freq), y=Freq)) +
  geom_col() + 
  coord_flip() +
  theme_minimal()

  

# https://www.youtube.com/watch?v=xex5C9cKLyw, on lda qda and svm
# https://cran.r-project.org/web/packages/RTextTools/RTextTools.pdf

train.df <- dat_de[dat_de$rub != "none",] #2188
predict.df <- dat_de[dat_de$rub == "none",] #7573
matrix.rub <- rbind.data.frame(train.df, predict.df)

doc_matrix <- create_matrix(matrix.rub$content, language="german", 
                            removeNumbers=TRUE,
                            stemWords=TRUE)

container <- create_container(doc_matrix, matrix.rub$rub, trainSize=1:2188,
                              testSize=2189:9761, virgin=FALSE)

SVM <- train_model(container,"SVM")
rub.pred<- classify_model(container, SVM)
matrix.rub$content[2188+3] #works... ok
#NOW: train different algorithms and test in train.df to assess accuracy (train 1:1800, test 1801:2188)

matrix.rub$rub[2189:9761] <- rub.pred$SVM_LABEL
data.frame(sort(table(matrix.rub$rub), decreasing=TRUE)) %>%
  ggplot(., aes(x=reorder(Var1, Freq), y=Freq)) +
  geom_col() + 
  coord_flip() +
  theme_minimal()

matrix.rub$date <- as.Date(matrix.rub$pubtime)
data.frame(table(matrix.rub$date, matrix.rub$rub))%>%
  ggplot(., aes(x=Var1, group=Var2)) +
  geom_line(aes(y=Freq, col=Var2))
#hm, group by month






# medium name: not useful
data.frame(table(dat$medium_name)) %>%
  ggplot(., aes(x=reorder(Var1, Freq), y=Freq)) +
  geom_col() + 
  coord_flip() +
  theme_minimal()

# salience over time
dat$count <- 1
dat$datum <- as.Date(dat$pubtime)
tl <- dat %>% select(datum, count) %>%
  group_by(datum) %>%
  summarize_all(sum)

tl <- dat %>% select(datum) %>%
  group_by(datum) %>%
  summarize(count)

ggplot(tl, aes(x=datum, y = count)) +
  geom_line() + theme_bw()
# moving average
tl$ma <- zoo::rollmean(tl$count, 30, fill = NA)
ggplot(tl, aes(x=datum, y = ma)) +
  geom_line() + theme_bw()

# load data on covid salience, cases per day
dat_covid <- read_csv("/Users/brutus/Desktop/Computational Social Science/Datasets/bag_covid_19_data_csv_31_May_2022/data/COVID19Cases_geoRegion.csv")
#geoRegion: CH and all cantons
ggplot(dat_covid, aes(x=datum, y = entries)) + geom_line()

# distribution of character length per article
hist(dat$char_count[dat$char_count <=12000], breaks = seq(from=0, to=12000, length.out = 121))
mean(dat$char_count) #4841


# what analysis?
# topics..?
# keyword: vaccination: sentiments, course over time
# keyword: bundesrat: sentiments, course over time
# sentiment generally, course over time
# sentiments per mediatype
# sentiments per linguistic region

vars <- colnames(dat)

# split into languages
dat_de <- dat[dat$language=="de",]
dat_fr <- dat[dat$language=="fr",]
dat_it <- dat[dat$language=="it",]

corp_de <- corpus(dat_de, text_field = "content")
corp_fr <- corpus(dat_fr, text_field = "content")
corp_it <- corpus(dat_it, text_field = "content")

tok_de <- tokens(corp_de, 
                 remove_separators = TRUE,
                 include_docvars = TRUE,
                 remove_punct = T)
tok_fr <- tokens(corp_fr,
                 remove_separators = TRUE,
                 include_docvars = TRUE,
                 remove_punct = T)
tok_it <- tokens(corp_it,
                 remove_separators = TRUE,
                 include_docvars = TRUE,
                 remove_punct = T)
tok_de %>% head #looks like compound words are written together or separated via hyphens
tok_fr %>% head #compound words with ' connected, grammatical
tok_it %>% head
# hm. how to define compound words..? See example, Europäische Union, think about it
# maybe with regex: Europäische + *, Schweizerisch? + * und so ähnlich
#multiword <- c("Prime Minister", "European Union") #glue those words together
#toks %>% tokens_compound(pattern = phrase(multiword)) %>% .[[6]]

# cool research idea: identify countries in corpus (scrape names from somewhere), then 
# calculate salience of countries in texts

#textstat_readability(corp, measure="Flesch.Kincaid") #only in english?! SMOG.de
readability_de <- textstat_readability(corp_de, measure = "SMOG.de")

kwic(tok_de, "Covid", 4) %>% head(20)
# hm. not really useful...


# ATTENTION: create compound words first! iterative!
# german
tok_de <- tok_de %>% 
  tokens_tolower %>%  
  tokens_remove(stopwords(kind = "german")) %>% #it does change docs, but it doesn't eliminate really all terms I WOULD consider stopwords
  tokens_wordstem(language = "german") %>%
  tokens_remove("")
# check topwords, eliminate iteratively
tok_de <- tokens_remove(tok_de, c("dass", "00", "sei", "seit", "imm", "ab", "beim", "+", "frank"))

dfm_de <- dfm(tok_de)

top_words_de <-
  topfeatures(dfm_de, 50) %>% 
  data.frame(word = names(.),
             freq = .,
             row.names = c())

freq_tokens_de <-
  ggplot(top_words_de, 
         aes(x=reorder(word, freq), y=freq)) +
  geom_col() + 
  coord_flip() +
  theme_minimal() +
  ggtitle("Most frequent tokens") 
freq_tokens_de

wcloud_de <- textplot_wordcloud(dfm_de, 
                   min_count = 2800, 
                   random_order = FALSE, #assures reproducability
                   rotation = .3,
                   color = RColorBrewer::brewer.pal(8, "Dark2"))

# french
tok_fr <- tok_fr %>% 
  tokens_tolower %>% 
  tokens_remove(stopwords(kind = "french"), padding = TRUE) %>%
  tokens_wordstem(language = "french") %>%
  tokens_remove("")
# check topwords, eliminate iteratively
tok_fr <- tokens_remove(tok_fr, c("a", "d'un", "c'est", "comm", "deux", "auss", "si", "qu'il", "être", "n'est", "selon", "autr", "où", "s'est", "alor", "déjà", "dont"))
  
dfm_fr <- dfm(tok_fr)

top_words_fr <-
  topfeatures(dfm_fr, 50) %>% 
  data.frame(word = names(.),
             freq = .,
             row.names = c())

freq_tokens_fr <-
  ggplot(top_words_fr, 
         aes(x=reorder(word, freq), y=freq)) +
  geom_col() + 
  coord_flip() +
  theme_minimal() +
  ggtitle("Most frequent tokens") 
freq_tokens_fr

wcloud_fr <- textplot_wordcloud(dfm_fr, 
                   min_count = 300, 
                   random_order = FALSE, #assures reproducability
                   rotation = .3,
                   color = RColorBrewer::brewer.pal(8, "Dark2"))

# italian
tok_it <- tok_it %>% 
  tokens_tolower %>% 
  tokens_remove(stopwords(kind = "italian"), padding = TRUE) %>%
  tokens_wordstem(language = "italian") %>%
  tokens_remove("")
# check topwords, eliminate iteratively
tok_it <- tokens_remove(tok_it, c("due", "sol", "second", "dett", "far", "alcun"))

dfm_it <- dfm(tok_it)

top_words_it <-
  topfeatures(dfm_it, 50) %>% 
  data.frame(word = names(.),
             freq = .,
             row.names = c())

freq_tokens_it <-
  ggplot(top_words_it, 
         aes(x=reorder(word, freq), y=freq)) +
  geom_col() + 
  coord_flip() +
  theme_minimal() +
  ggtitle("Most frequent tokens") 
freq_tokens_it

wcloud_it <- textplot_wordcloud(dfm_it,
                   min_count = 15, 
                   random_order = FALSE, #assures reproducability
                   rotation = .3,
                   color = RColorBrewer::brewer.pal(8, "Dark2"))

# research idea: do subjects change over time? topwords salience changes over months



# similarities? test if this produces something substantive
?textstat_simil
?textstat_dist #1 - similarity


# SENTIMENT ANALYSIS

# these are unigrams, bigrams (no trigrams found at first sight)
# these are wordstems. de-stem words?

# can that be scraped..? refer to source!
de_neg<- read.csv("~/Desktop/Computational Social Science/Datasets/sentiment-lexicons/negative_words_de.txt", sep="", header = F)
de_pos<- read.csv("~/Desktop/Computational Social Science/Datasets/sentiment-lexicons/positive_words_de.txt", sep="", header = F)
de_sent <- dictionary(list(pos = unlist(de_pos), neg = unlist(de_neg)))
#lengths(de_sent)
fr_neg<- read.csv("~/Desktop/Computational Social Science/Datasets/sentiment-lexicons/negative_words_fr.txt", sep="", header = F)
fr_pos<- read.csv("~/Desktop/Computational Social Science/Datasets/sentiment-lexicons/positive_words_fr.txt", sep="", header = F)
fr_sent <- dictionary(list(pos = unlist(fr_pos), neg = unlist(fr_neg)))
#lengths(fr_sent)
it_neg<- read.csv("~/Desktop/Computational Social Science/Datasets/sentiment-lexicons/negative_words_it.txt", sep="", header = F)
it_pos<- read.csv("~/Desktop/Computational Social Science/Datasets/sentiment-lexicons/positive_words_it.txt", sep="", header = F)
it_sent <- dictionary(list(pos = unlist(it_pos), neg = unlist(it_neg)))
#lengths(it_sent)

toks_de_sent <- tokens_lookup(tok_de, dictionary =  de_sent)
mydfm_de_sent <- dfm(toks_de_sent)
mydfm_de_sent.dfm <- dfm(toks_de_sent)
mydfm_de_sent.dfm.dv <- docvars(corp_de) %>%
  mutate(doc_id = paste("text", rownames(.), sep = ""))
mydfm_de_sent.dfm %<>% # attention! this is the "assign and update" pipe!
  convert(to = "data.frame") %>%
  left_join(mydfm_de_sent.dfm.dv)
#indicator for emotionalizement
mydfm_de_sent.dfm$emo <- (mydfm_de_sent.dfm$pos + mydfm_de_sent.dfm$neg) / mydfm_de_sent.dfm$char_count
# does that correlate with covid salience?




mydfm_de_sent.dfm %<>%
  mutate(month = lubridate::floor_date(datum, "month"))

plot(mydfm_de_sent.dfm$month, mydfm_de_sent.dfm$emo)
ggplot(mydfm_de_sent.dfm, aes(x=month, group = month)) +
  geom_boxplot(aes(y=emo))

# group by month: medium_type_group
sent_de_mtype_group <- mydfm_de_sent.dfm %>%
  select(pos, neg, char_count, month, medium_type_group) %>%
  group_by(month, medium_type_group) %>% 
  summarize_all(sum) %>%
  mutate(bal = pos - neg) %>%
  mutate(pos_to_neg = (pos - neg) / (pos + neg)) %>%
  mutate(emo = (pos + neg) / char_count) %>%
  mutate(emo_pos = pos / char_count) %>%
  mutate(emo_neg = neg / char_count)
  
ggplot(sent_de_mtype_group, aes(x=month)) +
  geom_line(aes(y=pos_to_neg)) +
  facet_wrap(~medium_type_group)
ggplot(sent_de_mtype_group, aes(x=month)) +
  geom_line(aes(y=emo)) +
  facet_wrap(~medium_type_group)
ggplot(sent_de_mtype_group, aes(x=month)) +
  geom_line(aes(y=emo_pos)) +
  facet_wrap(~medium_type_group)
ggplot(sent_de_mtype_group, aes(x=month)) +
  geom_line(aes(y=emo_neg)) +
  facet_wrap(~medium_type_group)

# group by month: medium_type
sent_de_mtype <- mydfm_de_sent.dfm %>%
  select(pos, neg, char_count, month, medium_type) %>%
  group_by(month, medium_type) %>% 
  summarize_all(sum) %>%
  mutate(bal = pos - neg) %>%
  mutate(pos_to_neg = (pos - neg) / (pos + neg)) %>%
  mutate(emo = (pos + neg) / char_count) %>%
  mutate(emo_pos = pos / char_count) %>%
  mutate(emo_neg = neg / char_count)

ggplot(sent_de_mtype, aes(x=month)) +
  geom_line(aes(y=pos_to_neg)) +
  facet_wrap(~medium_type)
ggplot(sent_de_mtype, aes(x=month)) +
  geom_line(aes(y=emo, col = medium_type)) +
  scale_y_continuous(labels=scales::percent) +
  theme_minimal() + theme(legend.position = "none", 
                          plot.title = element_text(hjust = 0.5),
                          axis.text.x = element_text(angle=90, size=6)) +
  labs(x="", y="", title = "Emotionalization of media types for german articles ") +
  facet_wrap(~medium_type, ncol = 5)
ggplot(sent_de_mtype, aes(x=month)) +
  geom_line(aes(y=emo_pos)) +
  facet_wrap(~medium_type)
ggplot(sent_de_mtype, aes(x=month)) +
  geom_line(aes(y=emo_neg)) +
  facet_wrap(~medium_type)

# hypothesis: boulevard is more emotionalized
# calculate mean pos_to_neg and deviance by media_type_group

# think about:



## USER INTERACTION
#WATSON

url <- "https://www.watson.ch/Coronavirus"
url2 <- "https://www.watson.ch/Coronavirus/?page="

page <- page <- read_html(url)
#teasers <- page %>% html_nodes("a.teaserlink") %>% html_text() 
#hm. dirty, clean of tags, just headers, is this needed..?

#links <- page %>% html_nodes("a.teaserlink") %>% html_attr("href")

pages <- page %>% html_nodes(".widget.pagination") %>% html_text()
npages <- max(as.numeric(unlist(regmatches(pages, gregexpr("[[:digit:]]+", pages)))))

links <- list()
temp <- list()
for (i in 1:npages) { #15:13 - 15:15, 83*73: 6059links
  ifelse(i==1,
         temp <- list(read_html(url) %>% html_nodes("a.teaserlink") %>% html_attr("href")),
         temp <- list(read_html(paste(url2, i, sep="")) %>% html_nodes("a.teaserlink") %>% html_attr("href")))
  links <- rbind(links, temp)
}

links <- unlist(links)

articles <- list()
part <- list()
for (i in 1:length(links)) { #15:16 - 19:00
  temp <- read_html(links[i]) %>%
    html_nodes("p") %>% html_text()
  temp <- temp[-length(temp)] # clean from last entry, is from another article
  # bind parts together
  for (j in 1:length(temp)) {
    part <- paste(part, temp[j])
  }
  articles[[i]] <- part
  part <- NULL
  #print(i)
} 
# eliminate #CoronaInfoCHZahlen

dates <- list()
for (i in 1:length(links)) { #19:02 - 22:40
  temp <- read_html(links[i]) %>%
    html_nodes(".mb-8") %>% html_text()
  # date either in paragraph 1 or 2, if not then ignore
  try({ 
    x <- ifelse(gregexpr("[0-9][0-9].[0-9][0-9].[0-9][0-9][0-9][0-9]", temp[1])[[1]][1] != "-1",
              substr(temp[1],
                     (gregexpr("[0-9][0-9].[0-9][0-9].[0-9][0-9][0-9][0-9]", temp[1])[[1]][1]),
                     (gregexpr("[0-9][0-9].[0-9][0-9].[0-9][0-9][0-9][0-9]", temp[1])[[1]][1]+9)),
              substr(temp[2],
                     (gregexpr("[0-9][0-9].[0-9][0-9].[0-9][0-9][0-9][0-9]", temp[2])[[1]][1]),
                     (gregexpr("[0-9][0-9].[0-9][0-9].[0-9][0-9][0-9][0-9]", temp[2])[[1]][1]+9)))
  dates[[i]] <- as.Date(paste(substr(x, nchar(x)-3, nchar(x)),
                substr(x, nchar(x)-6, nchar(x)-5),
                substr(x, nchar(x)-9, nchar(x)-8),
                sep="-"))
  #print(i)
  })
}

comments <- list() # 06:42 - vorauss. 09:30
for (i in 1:length(links)) {
  temp <- read_html(links[i]) %>%
    html_nodes(".mb-2.watson-snippet__text") %>% html_text()
  comments[[i]] <- temp
  print(i)
}
# only "most popular comments", 0 for not commentable

reactions.love <- list()
for (i in 1:length(links)) {
  temp <- read_html(links[i]) %>%
    html_nodes(".vote_love") %>% html_text()
  reactions.love[[i]] <- temp
  print(i)
}
# refer to effective comments

reactions.hate <- list()
for (i in 1:length(links)) {
  temp <- read_html(links[i]) %>%
    html_nodes(".vote_hate") %>% html_text()
  reactions.hate[[i]] <- temp
  print(i)
}
# refer to effective comments

# compose data.frame
watson <- data.frame(matrix(nrow = length(articles)))
watson$article <- unlist(articles)
watson$date <- as.Date(as.numeric(matrix(unlist(purrr::map_depth(dates, 1, ~ifelse(is.null(.x), NA, .x) )
))), origin = "1970-01-01")

for (i in 1:length(articles)) {
  watson$comment1[i] <- unlist(comments[[i]])[1]
  watson$comment2[i] <- unlist(comments[[i]])[2]
  watson$comment3[i] <- unlist(comments[[i]])[3]
  watson$love1[i] <- as.numeric(unlist(reactions.love[[i]])[1])
  watson$love2[i] <- as.numeric(unlist(reactions.love[[i]])[2])
  watson$love3[i] <- as.numeric(unlist(reactions.love[[i]])[3])
  watson$hate1[i] <- as.numeric(unlist(reactions.hate[[i]])[1])
  watson$hate2[i] <- as.numeric(unlist(reactions.hate[[i]])[2])
  watson$hate3[i] <- as.numeric(unlist(reactions.hate[[i]])[3])
}

watson$love_to_hate1 <- ifelse(is.na(watson$hate1) == TRUE, 0, (watson$love1-watson$hate1)/(watson$hate1+watson$love1))
watson$love_to_hate2 <- ifelse(is.na(watson$hate2) == TRUE, 0, (watson$love2-watson$hate2)/(watson$hate2+watson$love2))
watson$love_to_hate3 <- ifelse(is.na(watson$hate3) == TRUE, 0, (watson$love3-watson$hate3)/(watson$hate3+watson$love3))

watson$char <- nchar(watson$article)

#cleaning
watson <- watson[!is.na(watson$date),]
watson <- watson[watson$date > "2020-01-01",]


# for analytic purposes: nchar comments, average.
# cannot be applied to loves/hates because they might internaly be different, pos or neg
# article: pos, neg



watson <- readRDS(file = "watson.rds")

ggplot(watson, aes(x=date, y=love3)) +
  geom_col()

ggplot(watson, aes(x=date)) +
  geom_col(aes(y=love1), fill="darkgreen") +
  geom_col(aes(y=-hate1), fill="red")
ggplot(watson, aes(x=date)) +
  geom_col(aes(y=love_to_hate1))

ggplot(watson, aes(x=date)) +
  geom_col(aes(y=love2), fill="darkgreen") +
  geom_col(aes(y=-hate2), fill="red")
ggplot(watson, aes(x=date)) +
  geom_col(aes(y=love_to_hate2))

ggplot(watson, aes(x=date)) +
  geom_col(aes(y=love3), fill="darkgreen") +
  geom_col(aes(y=-hate3), fill="red")
ggplot(watson, aes(x=date)) +
  geom_col(aes(y=love_to_hate3))

corp_watson <- corpus(watson, text_field = "article")

tok_watson <- tokens(corp_watson, 
                 remove_separators = TRUE,
                 include_docvars = TRUE,
                 remove_punct = T)

tok_watson <- tokens_remove(tok_watson, c("dass", "00", "sei", "seit", "imm", "ab", "beim", "+", "frank"))

tok_watson <- tok_watson %>% 
  tokens_tolower %>%  
  tokens_remove(stopwords(kind = "german")) %>% #it does change docs, but it doesn't eliminate really all terms I WOULD consider stopwords
  tokens_wordstem(language = "german") %>%
  tokens_remove("")

dfm_watson <- dfm(tok_watson)

top_words_watson <-
  topfeatures(dfm_watson, 50) %>% 
  data.frame(word = names(.),
             freq = .,
             row.names = c())
top_words_watson %>% head()

freq_tokens_watson <-
  ggplot(top_words_watson, 
         aes(x=reorder(word, freq), y=freq)) +
  geom_col() + 
  coord_flip() +
  theme_minimal() +
  ggtitle("Most frequent tokens") 
freq_tokens_watson

wcloud_watson <- textplot_wordcloud(dfm_watson, 
                                min_count = 2000, 
                                random_order = FALSE, #assures reproducability
                                rotation = .3,
                                color = RColorBrewer::brewer.pal(8, "Dark2"))
toks_watson_sent <- 
  tokens_lookup(tok_watson, 
                dictionary =  de_sent) # we only look for negative and positive
head(toks_watson_sent, 20)

mydfm_watson_sent <- dfm(toks_watson_sent)
mydfm_watson_sent.dfm <- dfm(toks_watson_sent)
mydfm_watson_sent.dfm.dv <- docvars(corp_watson) %>%
  mutate(doc_id = paste("text", rownames(.), sep = ""))

mydfm_watson_sent.dfm %<>% # attention! this is the "assign and update" pipe!
  convert(to = "data.frame") %>%
  left_join(mydfm_watson_sent.dfm.dv)

mydfm_watson_sent.dfm %<>%
  mutate(month = lubridate::floor_date(date, "month")) %>%
  mutate(bal = pos - neg)

ggplot(mydfm_watson_sent.dfm, aes(x=date)) +
  geom_col(aes(y=bal))

ggplot(mydfm_watson_sent.dfm, aes(x=date)) +
  geom_col(aes(y=pos), fill="darkgreen") +
  geom_col(aes(y=-neg), fill="red")

# group by month
sent_watson <- mydfm_watson_sent.dfm %>%
  select(pos, neg, month) %>%
  group_by(month) %>% 
  summarize_all(sum) %>%
  mutate(bal = pos - neg) %>%
  mutate(pos_to_neg = (pos - neg) / (pos + neg))

ggplot(sent_watson, aes(x=month)) +
  geom_col(aes(y=pos_to_neg)) #hm that's not weighted, so monthly totals are not intercomparable

ggplot(sent_watson, aes(x=month)) +
  geom_col(aes(y=bal)) 

ggplot(sent_watson, aes(x=month)) +
  geom_col(aes(y=pos), fill="darkgreen") +
  geom_col(aes(y=-neg), fill="red")

# maybe better to group by week?

# now do sentiment analysis on comments
corp_watson_comment1 <- corpus(watson, text_field = "comment1")

tok_watson_comment1 <- tokens(corp_watson_comment1, 
                     remove_separators = TRUE,
                     include_docvars = TRUE,
                     remove_punct = T)

tok_watson_comment1 <- tokens_remove(tok_watson_comment1, c("dass", "00", "sei", "seit", "imm", "ab", "beim", "+", "frank"))

tok_watson_comment1 <- tok_watson_comment1 %>% 
  tokens_tolower %>%  
  tokens_remove(stopwords(kind = "german")) %>% #it does change docs, but it doesn't eliminate really all terms I WOULD consider stopwords
  tokens_wordstem(language = "german") %>%
  tokens_remove("")

dfm_watson_comment1 <- dfm(tok_watson_comment1)

top_words_watson.comment1 <-
  topfeatures(dfm_watson_comment1, 50) %>% 
  data.frame(word = names(.),
             freq = .,
             row.names = c())
top_words_watson.comment1 %>% head()

freq_tokens_watson_comment1 <-
  ggplot(top_words_watson.comment1, 
         aes(x=reorder(word, freq), y=freq)) +
  geom_col() + 
  coord_flip() +
  theme_minimal() +
  ggtitle("Most frequent tokens") 
freq_tokens_watson_comment1

wcloud_watson_comment1 <- textplot_wordcloud(dfm_watson_comment1, 
                                    min_count = 100, 
                                    random_order = FALSE, #assures reproducability
                                    rotation = .3,
                                    color = RColorBrewer::brewer.pal(8, "Dark2"))
toks_watson_sent_comment1 <- 
  tokens_lookup(tok_watson_comment1, 
                dictionary =  de_sent) # we only look for negative and positive
head(toks_watson_sent_comment1, 20)

mydfm_watson_sent_comment1 <- dfm(toks_watson_sent_comment1)
mydfm_watson_sent_comment1.dfm <- dfm(toks_watson_sent_comment1)
mydfm_watson_sent_comment1.dfm.dv <- docvars(corp_watson_comment1) %>%
  mutate(doc_id = paste("text", rownames(.), sep = ""))

#join to main watson df, THINK IF THIS IS RIGHT!!
mydfm_watson_sent_comment1.dfm %<>% # attention! this is the "assign and update" pipe!
  convert(to = "data.frame") %>%
  left_join(mydfm_watson_sent.dfm.dv)

mydfm_watson_sent_comment1.dfm %<>%
  mutate(month = lubridate::floor_date(date, "month")) %>%
  mutate(bal = pos - neg)

ggplot(mydfm_watson_sent_comment1.dfm, aes(x=date)) +
  geom_col(aes(y=bal))

ggplot(mydfm_watson_sent_comment1.dfm, aes(x=date)) +
  geom_col(aes(y=pos), fill="darkgreen") +
  geom_col(aes(y=-neg), fill="red")

# group by month THINK ABOUT THIS AGAIN...
sent_watson_comment1 <- mydfm_watson_sent_comment1.dfm %>%
  select(pos, neg, month) %>%
  group_by(month) %>% 
  summarize_all(sum) %>%
  mutate(bal = pos - neg) %>%
  mutate(pos_to_neg = (pos - neg) / (pos + neg))

ggplot(sent_watson_comment1, aes(x=month)) +
  geom_col(aes(y=pos_to_neg)) #hm that's not weighted, so monthly totals are not intercomparable

ggplot(sent_watson_comment1, aes(x=month)) +
  geom_col(aes(y=bal)) 

ggplot(sent_watson_comment1, aes(x=month)) +
  geom_col(aes(y=pos), fill="darkgreen") +
  geom_col(aes(y=-neg), fill="red")

#map to main watson df: mydfm_watson_sent.dfm
df_temp <- mydfm_watson_sent_comment1.dfm %>%
  select(doc_id, pos, neg)

colnames(df_temp) <- c("doc_id", "pos_c1", "neg_c1")

watson_c1 <- mydfm_watson_sent.dfm %>% 
  left_join(df_temp)

watson_c1$char_c1 <- nchar(watson_c1$comment1)

hist(watson_c1$pos_to_neg)
hist(watson_c1$pos_to_neg_c1)
# hm. quite a difference in pos_to_neg between articles and comment1...


# INTERACTION 1st stage: a pos comment on a pos article is reinforcing, a neg comment on a pos article is contradicting
watson_c1 %<>% mutate(interact1 = ((pos - neg) / (pos + neg)) * ((pos_c1 - neg_c1) / (pos_c1 + neg_c1)))

ggplot(watson_c1) +
  geom_boxplot(aes(y = interact1))
summary(watson_c1$interact1) #hm, slightly pos mean
hist(watson_c1$interact1, breaks = seq(-1, 1, length.out=30)) #looks like comments are slightly reinforcing articles

watson_c1 %<>% 
  mutate(pos_to_neg = (pos - neg) / (pos + neg)) %<>%
  mutate(pos_to_neg_c1 = (pos_c1 - neg_c1) / (pos_c1 + neg_c1)) %<>%
  mutate(love_to_hate_c1 = (love1 - hate1) / (love1 + hate1))

hist(watson_c1$love_to_hate_c1)
hist(watson_c1$love1 - watson_c1$hate1)

ggplot(watson_c1, aes(x=pos_to_neg, y=pos_to_neg_c1)) +
  geom_jitter(aes(alpha = 0.1), size = 1, width = 0.05, height = 0.05) +
  geom_smooth()

# hm. maybe calculate a correlation?
summary(lm(data =  watson_c1, pos_to_neg ~ pos_to_neg_c1)) #at least a significant coeff, although small and low R^2
summary(lm(data =  watson_c1, pos_to_neg ~ pos_to_neg_c1 + as.factor(month))) #hm, that's interesting. all months are somehow significant

# INTERACTION 2nd stage: love reactions on a pos comment is reinforcing, hate reactions on a pos comment is contradicting
ggplot(watson_c1, aes(x=pos_to_neg_c1, y=love_to_hate_c1)) +
  geom_jitter(aes(alpha = 0.1), size = 1, width = 0.05, height = 0.05) +
  geom_smooth()

summary(lm(data = watson_c1, pos_to_neg_c1 ~ I(love_to_hate1_c1^2)))
summary(lm(data = watson_c1, pos_to_neg_c1 ~ love_to_hate1_c1 + I(love_to_hate1_c1^2)))

summary(lm(data = watson_c1, love_to_hate1_c1 ~ I(pos_to_neg_c1^2)))
summary(lm(data = watson_c1, love_to_hate1_c1 ~ pos_to_neg_c1 + I(pos_to_neg_c1^2)))

plot(lm(data = watson_c1, love_to_hate1_c1 ~ pos_to_neg_c1 + I(pos_to_neg_c1^2)))
# hm. logit needed?

summary(glm(data = watson_c1, love_to_hate1_c1 ~ pos_to_neg_c1 + I(pos_to_neg_c1^2), family = quasibinomial(link = "logit")))
plot(glm(data = watson_c1, love_to_hate1_c1 ~ pos_to_neg_c1 + I(pos_to_neg_c1^2), family = quasibinomial(link = "logit")))
# logit doesn't make qq plot better...
#https://stats.stackexchange.com/questions/481413/how-to-interpret-this-shape-of-qq-plot-of-standardized-residuals

summary(lm(data = watson_c1, love_to_hate1_c1 ~ pos_to_neg_c1 + + I(pos_to_neg_c1^2) + as.factor(month)))

#maybe do some regressions on absolute values of sentiments, comments and love/hates? or balances?
#but that is not scaled...




# hm. how can this be aggregated...?
sent_watson_c1 <- watson_c1 %>%
  select(pos, neg, pos_c1, neg_c1, char, char_c1, month) %>%
  group_by(month) %>% 
  summarize_all(sum, na.rm = T) %>%
  mutate(bal = pos - neg) %>%
  mutate(pos_to_neg = (pos - neg) / (pos + neg)) %>%
  mutate(pos_to_neg_c1 = (pos_c1 - neg_c1) / (pos_c1 + neg_c1)) %>%
  mutate(emo = (pos + neg) / char) %>%
  mutate(emo_c1 = (pos_c1 + neg_c1) / char_c1) %>%
  mutate(emo_pos = pos / char) %>%
  mutate(emo_pos_c1 = pos_c1 / char_c1) %>%
  mutate(emo_neg = neg / char) %>%
  mutate(emo_neg_c1 = neg_c1 / char_c1)










# TOPIC MODELS: check functionality, workings etc.
# latent dirichet analysis
# structural topic models

# OTHER METHODS?
# pca
# dendrograms
# clustering


# IN THE END: A SHINY TOOL WOULD BE VERY COOOOOOOL....

