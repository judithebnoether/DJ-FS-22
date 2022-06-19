###R Abschlussprojekt DJ FS 22

##Preparations of packages

packages <- c("stringr","readr","tidyverse","stm","quanteda",
              "data.table", "tm", "SnowballC", "rvest", "wordcloud",
              "Rtsne", "rsvd", "geometry", "devtools", "lubridate")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))



##Import data

subject_businesses <- read_csv("Studium/Master/S2/Datenjournalismus/chparl_full/chparl_full/subject_businesses.csv/subject_businesses.csv")
transcripts_Legislatur_46 <- read_csv("Studium/Master/S2/Datenjournalismus/chparl_full/chparl_full/transcripts_Legislatur-46.csv/transcripts_Legislatur-46.csv")
transcripts_Legislatur_47 <- read_csv("Studium/Master/S2/Datenjournalismus/chparl_full/chparl_full/transcripts_Legislatur-47.csv/transcripts_Legislatur-47.csv")
transcripts_Legislatur_48 <- read_csv("Studium/Master/S2/Datenjournalismus/chparl_full/chparl_full/transcripts_Legislatur-48.csv/transcripts_Legislatur-48.csv")
transcripts_Legislatur_49 <- read_csv("Studium/Master/S2/Datenjournalismus/chparl_full/chparl_full/transcripts_Legislatur-49.csv/transcripts_Legislatur-49.csv")
transcripts_Legislatur_50 <- read_csv("Studium/Master/S2/Datenjournalismus/chparl_full/chparl_full/transcripts_Legislatur-50.csv/transcripts_Legislatur-50.csv")
transcripts_Legislatur_51 <- read_csv("Studium/Master/S2/Datenjournalismus/chparl_full/chparl_full/transcripts_Legislatur-51.csv/transcripts_Legislatur-51.csv")
df_list <- list(transcripts_Legislatur_46, transcripts_Legislatur_47, transcripts_Legislatur_48,
                transcripts_Legislatur_49, transcripts_Legislatur_50, transcripts_Legislatur_51)


all.transcripts <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list)


##Preprocessing

#Filter relevant Rows

all.transcripts.small <- subset(all.transcripts, select= c(Language, Text, MeetingDate, SpeakerFunction, ParlGroupName,
                                   ParlGroupAbbreviation, LanguageOfText)) 

#HTML Text entfernen von Text

cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

all.transcripts.small$Text <-cleanFun(all.transcripts.small$Text)



###Filtern der inhaltlich relevanten Texte mit eigenem dictionary

flucht <- all.transcripts.small %>% 
  filter(str_detect(Text,"Flucht*| Flüchtling*|Flüchtende*
                    |Asylsuchender*|Geflüchteter*|
                    Geflüchtete*|Asylsuchende*|Flüchtlinge*|flüchten*|
                    geflüchtet*|Flüchtender*|flüchtet*|
                    Geflüchteten*|Flüchtende*|fliehen*|geflohen*|
                    Geflohener*|Geflohene*|flieht*|asylsuchend*|
                    Asylant*|Asylanten*|flüchtend*|fliehend*|
                    Vertriebene*|Vertriebener*|Asyl*|Migrant*|
                    Migrantin*|Migranten*"))  #braucht noch Ergänzungen, gendern?


flucht$FraktionenFactor <- recode(as.factor(flucht$ParlGroupName), 'Christlichdemokratische Fraktion' = "Mitte", 'CVP-Fraktion' = "Mitte",
                            'Die Mitte-Fraktion. Die Mitte. EVP.' = "Mitte", 'Evangelische und Unabhängige Fraktion' = "Rechts",
                            'EVP/EDU Fraktion' = "Rechts", 'FDP-Liberale Fraktion' = "Liberal",
                            'Fraktion CVP-EVP' = "Mitte", 'Fraktion CVP/EVP/glp' = "Mitte", 'Fraktion der Schweizerischen Volkspartei' = "Rechts",
                            'Freisinnig-demokratische Fraktion' = "Liberal", 'Grüne Fraktion' = "Links", 
                            'Grünliberale Fraktion' = "Mitte", 'Sozialdemokratische Fraktion' = "Links", .default = NA_character_)
flucht$FraktionenFactor <- factor(flucht$FraktionenFactor, levels= c('Links', 'Mitte', 'Liberal', 'Rechts')) 
flucht <- flucht %>%
  drop_na(FraktionenFactor)


flucht$Date <- as.Date(as.character(flucht$MeetingDate), format = "%Y%m%d")
flucht$Datenum <- year(flucht$Date)



#erstellen von Corpus
corp_docs <- corpus(flucht, text_field = "Text")
summary(corp_docs, 5)


###Structural topic model

##preprocessing

processed <- textProcessor(flucht$Text, metadata = flucht,
                           lowercase = T, removestopwords = T, removenumbers = T,
                           removepunctuation = T, stem = T, wordLengths = c(3, Inf),
                           sparselevel = 1, language = 'de', verbose = T,
                           onlycharacter = T, striphtml = F, 
                           customstopwords = c("dass", "amp", "che", "per", "non", "les"))
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta

out <- prepDocuments(processed$documents, processed$vocab,
                         processed$meta, lower.thresh = 2)


poliblogPrevFit <- stm(documents = out$documents, vocab = out$vocab,
                        K = 30,
                        data = out$meta,
                        init.type = "Spectral")


#Metadaten mit einbeziehen, neues Model


#plot(stm_select2, type = "perspectives", topics = 16)
prep <- estimateEffect(1:30 ~ FraktionenFactor + Datenum, poliblogPrevFit,
                       meta = out$meta, uncertainty = "Global")


stm_10_covar <- stm(processed$documents, processed$vocab, K = 10,
                    data=processed$meta, prevalence =~ FraktionenFactor + Datenum,
                    content =~ FraktionenFactor)


stm_10_covar_est <- estimateEffect(1:10 ~ FraktionenFactor + Datenum, stm_10_covar,
                                   metadata = processed$meta)



#Modelle speichern und mit stminsights visualisieren
save.image('stm_projekt.RData')

stminsights::run_stminsights()
