#### D2: First pilot cleaning ###############
# Function: To clean and merge the main 1651 responses 
# Author: Dr Peter King (p.king1@leeds.ac.uk)
# Last Edited: 31/10/2024
# Change/s:
# - Incomplete



# **********************************************************************************
#### Section 0: Setting up ####
## NOTES: This is just importing packages.
# **********************************************************************************


## Packages to install
# install.packages("haven")

## Any dcchoice problems run this first to rule out:
# BiocManager::install("Icens")


## Packages to library
library(haven)
library(here)
library(magrittr)
library(data.table)
library(tidyverse)
library(readxl)
library(DCchoice)
library(janitor)
library(tidytext)
library(tidyr)
library(wordcloud)

library(dplyr)
library(tidyr)
library(stringr)
# **********************************************************************************
#### Section 1: Import Data  ####
# **********************************************************************************


Data_Covariates <- here("Data/Main",
                        "Data_Covariates_Step3.csv") %>% fread() %>% data.frame()



# **********************************************************************************
#### Section 3A: Insect qual data Q9  ####
# **********************************************************************************



QualData_Q9InsectWords_clean_tidied <- Data_Covariates %>%
  dplyr::select(Q9Insect_Word1, Q9Insect_Word2, Q9Insect_Word3) %>%
  pivot_longer(cols = everything(), values_to = "word") %>%
  mutate(word = tolower(word)) %>%
  mutate(
    word = dplyr::recode(
      word,
      "bugs" = "bug",
      "a bug" = "ant",
      "bed bug" = "bedbug",
      "ants" = "ant",
      "bees" = "bee",
      "beed" = "bee",
      "bumble bee" = "bumblebee",
      "honey bee" = "honeybee",
      "spoder" =  "spider",
      "spiders" =  "spider",
      "spide" =  "spider",
      "spidefr" =  "spider" ,
      "pests" =  "pest",
      "critter" =  "critters",
      "cock roach" =  "cockroach",
      "cockroah" =  "cockroach",
      "cocorats" =  "cockroach",
      "cocoroch" =  "cockroach",
      "cocrach" =  "cockroach",
      "crocaoch" =  "cockroach",
      "laybird" =  "ladybird",
      "ladybirf" =  "ladybird",
      "laddybug" =  "ladybird",
      "lady bird" =  "ladybird",
      "lady bug" =  "ladybird",
      "ladybird bird" =  "ladybird",
      "ladybugs" =  "ladybird",
      "laybird" =  "ladybird" ,
      
      "creapy" =  "creepy",
      
      "creeping crawlies" =  "creepy crawly",
      "creepy and crealy" =  "creepy crawly",
      "creepy crawler" =  "creepy crawly",
      "creepy crawlers" =  "creepy crawly",
      "creey crawl" =  "creepy crawly",
      "creepy crawlie" =  "creepy crawly",
      "creepy-crawlies" =  "creepy crawly",
      "creepy crawlies" =  "creepy crawly",
      "creepy crawley" =  "creepy crawly",
      "creepy-crawlie" =  "creepy crawly",
      "creepycrawly" =  "creepy crawly",
      "creepcrawly" =  "creepy crawly",
      "dung beetle" =  "dungbeetle",
      "beetles" =  "beetle",
      "smalll" =  "small",
      "smal" =  "small",
      "wosp" =  "wasp",
      "slumy" =  "slimey",
      "slmey" =  "slimey",
      "slime" =  "slimey",
      "scaey" =  "scary",
      "scarey" =  "scary",
      "scaredd" =  "scary",
      "mosquitoes" =  "mosquito",
      "moscitous" =  "mosquito",
      "moscito" =  "mosquito",
      "moshino" =  "mosquito",
      "mosiquto" =  "mosquito",
      "mosqiuito" =  "mosquito",
      "mosquitoe" =  "mosquito",
      "misquote" =  "mosquito",
      "milipid" =  "milipede",
      "millipedes" =  "milipede",
      "lizzard" =  "lizards",
      "interested" =  "interesting",
      "interest" =  "interesting",
      "intresting" =  "interesting",
      "interesteing" =  "interesting",
      "intrested" =  "interesting",
      "any" =  "ant",
      "and" =  "ant",
      "honeybee" =  "bee",
      "bumblebee" =  "bee",
      "beer" =  "bee",
      "love beas" = "bee",
      "bee - nervous" =  "bee",
      "bee is kinda creepy" =  "bee",
      "bee sting" = "bee",
      "bees make me happy" = "bee",
      "bushy harney bee" = "bee",
      "beatle" = "beetle",
      "beatles" = "beetle",
      "blue beatle" = "beetle",
      "dungbeetle" = "beetle",
      "bites" = "bite",
      "biting" = "bite",
      "bit" = "bite",
      "biter" = "bite",
      "it will bite me" = "bite",
      "midgie bites" = "bite",
      "buzz" = "buzzing",
      "buzzy" = "buzzing",
      "buttelfly" = "butterfly",
      "butterfly are lovely" = "butterfly",
      "butterfly for garden" = "butterfly",
      "butterfly is beautif" = "butterfly",
      "scared" = "scary",
      "crawling" = "crawl",
      "crawly" = "crawl",
      "crawls" = "crawl",
      "crawls" = "crawl",
      "crawley" = "creepy crawly",
      "crawlies" = "creepy crawly",
      "crawler" = "crawl",
      "crawlers" = "crawl",
      "crawleing" = "crawl",
      "crawlie" = "creepy crawly",
      "crawling insect" = "crawl",
      "crawlly" = "crawl",
      "they can crawl" = "crawl",
      "oest" = "pest",
      "spder" = "spider",
      "weevle" = "weevil",
      
      "amazed" = "amazing",
      "animal" = "animals",
      "annoing" = "annoying",
      "annoy" = "annoying",
      "annoyance" = "annoying",
      "annoyed" = "annoying",
      "annoying" = "annoying",
      "annoyo" = "annoying",
      "anoyed" = "annoying",
      "anoying"  = "annoying",
      "apprehension" = "apprehensive",
      "awcward" = "awkward",
      "bestz" = "best",
      "birds" = "bird",
      "bothered" = "bother",
      "bothersome" = "bother",
      "buh" = "bug",
      "cakm" = "calm",
      "caring" = "care",
      "catapillers" = "caterpillar",
      "caterpillars" = "caterpillar",
      "centepedes" = "centipede",
      "colorful" = "colourful",
      "creature" = "creatures",
      "creeped" = "creepy",
      "creeped out" = "creepy",
      "creeping" = "creepy",
      "creeply" = "creepy",
      "creeps" = "creepy",
      "cricjet" = "cricket",
      "crickets" = "cricket",
      "curious" = "curiousity",
      "daddy long legs" = "daddy longlegs",  
      "dangerous" = "danger",
      "decrease" = "decreasing",
      "decline" = "declining",
      "different kinds of i" = "different",
      "dirt" = "dirty",
      "discusting" = "disgust",
      "disgusted" = "disgust",
      "disgusting" = "disgust",
      "diseases" = "disease",
      "dragon fly" = "dragonfly",
      "environmental" = "environment",
      "fast moving" = "fast",
      "fearfully" = "fear",
      "flea" = "fleas",
      "flower" = "flowers",
      "flyig" = "flying",
      "flys" = "fly",
      "frightened" = "frighten",
      "frightening" = "frighten",
      "grubby" = "grub",
      "infected" = "infect",
      "infection" = "infect", 
      "infections" = "infect",
      "infectious" = "infect",
      "infestation" = "infest",
      "infested" = "infest",
      "inject" = "infest",
      
      "wood lice" = "woodlouse",
      "wood louse" = "woodlouse",
      "woodlice" = "woodlouse",
      "waps" = "wasp",
      "wasps" = "wasp",
      "shivering" = "shiver",
      "shivers" = "shiver",
      "shivery" = "shiver",
      "polinating" = "pollination",
      "polination" = "pollination",
      "polinator" = "pollination",
      "pollen" = "pollination",
      "pollenation" = "pollination",
      "pollinate" = "pollination",
      "pollinating" = "pollination",
      "pollinator" = "pollination",
      "pollinators" = "pollination",
      "killer" = "kill",
      
      "soider" = "spider",
      "snails" = "snail",
      "snakes" = "snake",
      "snall" = "small",
      "scratchy" = "scratch",
      "sickness" = "sick",
      "noisey" = "noise",
      "midges" = "midge",
      "nector" = "nectar",
      "needed" = "need",
      "nervus" = "nervous",
      "insecictivw" = "insecticide",
      "irrirating" = "irritating",
      "irritant" = "irritate",
      "irritated" = "irritate",
      "irritating" = "irritate",
      "irritation" = "irritate",
      "icthy" = "itch",
      "itcy" = "itch",
      "itchy" = "itch",
      "itching" = "itch",
      "itching y" = "itch",
      "itchy" = "itch",
      "itck" = "itch",
      )
  )


# Remove punctuation and special characters
QualData_Q9InsectWords_clean_tidied <- QualData_Q9InsectWords_clean_tidied %>%
  mutate(word = str_remove_all(word, "[^[:alnum:] ]"))

# Remove stop words
stop_words <- c("the", "a", "is", "are", "and", "of", "to", "in", "it", "that", "they", "can", "be", "for", "on", "with", "as", "by", "not")

QualData_Q9InsectWords_clean_tidied <- QualData_Q9InsectWords_clean_tidied %>%
  filter(!word %in% stop_words)


QualData_Q9InsectWords_clean_tidied %>%
  group_by(word) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  View()



# **********************************************************************************
#### Section 3A: Insect qual data Q9  ####
# **********************************************************************************


QualData_Q10InsectWords_clean_tidied <- Data_Covariates %>%
  dplyr::select(Q10Insect_Species1, Q10Insect_Species2, Q10Insect_Species3) %>%
  pivot_longer(cols = everything(), values_to = "word") %>%
  mutate(word = tolower(word)) %>%
  mutate(
    word = dplyr::recode(
      word,
      
      "bugs" = "bug",
      "ants" = "ant",
      "ants" = "ant",
      "anr" = "ant",
      "aant" = "ant",
      "aunt" = "ant",
      "soldier ant" = "ant",
      "army ant" = "ant",
      "any" = "ant",
      "and" = "ant",
      "cant" = "ant",
      "tarmeant" = "ant",
      "bees" = "bee",
      "beed" = "bee",
      "bea" = "bee",
      "beetles" = "beetle",
      "beatle" = "beetle",
      "bettle" = "beetle",
      "catapilla" = "caterpillar",
      "catapiller" = "caterpillar",
      "caterpillars" = "caterpillar",
      "caterpiller" = "caterpillar",
      "cattepillar" = "caterpillar",
      "catterpillar" = "caterpillar",
      "catterpiller" = "caterpillar",
      "catterpillerb" = "caterpillar",
      
      "butterflies" = "butterfly",
      "mosquitoes" = "mosquito",
      "wasps" = "wasp",
      "wusp" = "wasp",
      "wosp" = "wasp",
      "wap" = "wasp",
      "waap" = "wasp",
      "coachroach" = "cockroach",
      "coackroch" = "cockroach",
      "cock croach"             = "cockroach",
      "cockriach"               = "cockroach",
      "cockroachs"               = "cockroach",
      "cockroack"                = "cockroach",
      "cockroch"                 = "cockroach",
      "cocorach"                 = "cockroach",
      "cocrach"                  = "cockroach",
      "cocraoch"                 = "cockroach",
      "cocroched"                = "cockroach",
      "cocroches"                = "cockroach",
      "crocaoch"                 = "cockroach",
      
      "bumble bee" = "bumblebee",
      "honey bee" = "honeybee",
      "spoder" =  "spider",
      "spiders" =  "spider",
      "spired" =  "spider",
      "spiser" =  "spider",
      "spide" =  "spider",
      "spidefr" =  "spider" ,
      "pests" =  "pest",
      "centapead" =  "centipede",
      "centeped" =  "centipede",
      "centepede" =  "centipede",
      "critter" =  "critters",
      "coackroaches" =  "cockroach",
      "cocroach" =  "cockroach",
      "cock roach" =  "cockroach",
      "cockroah" =  "cockroach",
      "cocorats" =  "cockroach",
      "cocoroch" =  "cockroach",
      "cocrach" =  "cockroach",
      "crocaoch" =  "cockroach",
      "laybird" =  "ladybird",
      "ladybirf" =  "ladybird",
      "laddybug" =  "ladybird",
      "lady bird" =  "ladybird",
      "lady bug" =  "ladybird",
      "ladybird bird" =  "ladybird",
      "ladybugs" =  "ladybird",
      "laybird" =  "ladybird" ,
      "ladybug" =  "ladybird" ,
      "creeping crawlies" =  "creepy crawly",
      "creepy and crealy" =  "creepy crawly",
      "creepy crawler" =  "creepy crawly",
      "creepy crawlers" =  "creepy crawly",
      "creey crawl" =  "creepy crawly",
      "dung beetle" =  "dungbeetle",
      "beetles" =  "beetle",
      "beatles" =  "beetle",
      "smalll" =  "small",
      "smal" =  "small",
      "wosp" =  "wasp",
      "slumy" =  "slimey",
      "slmey" =  "slimey",
      "slime" =  "slimey",
      "scaey" =  "scary",
      "scarey" =  "scary",
      "scaredd" =  "scary",
      "mosquitoes" =  "mosquito",
      "moscitous" =  "mosquito",
      "moscito" =  "mosquito",
      "moshino" =  "mosquito",
      "mosiquto" =  "mosquito",
      "mosqiuito" =  "mosquito",
      "mosquitoe" =  "mosquito",
      "misquote" =  "mosquito",
      "milipid" =  "milipede",
      "millipedes" =  "milipede",
      "lizzard" =  "lizards",
      "interested" =  "interesting",
      "interest" =  "interesting",
      "intresting" =  "interesting",
      "interesteing" =  "interesting",
      "intrested" =  "interesting",
      "flys" =  "fly",
      "worms" =  "worm",
      "woodworm" =  "worm",
      "earthworm" =  "worm",
      "we will" =  "weevil",
      "weavle" =  "weevil",
      "wood lice" =  "woodlouse",
      "wood louse" =  "woodlouse",
      "woodlce" =  "woodlouse",
      "woodlice" =  "woodlouse",
      "woodlouce" =  "woodlouse",
      "tse fly" =  "fly",
      "tse tse fly" =  "fly",
      "tse-tse fly" =  "fly",
      "tsetse fly" =  "fly",
      "mantis" =  "praying mantis",
      "religious mantis" =  "praying mantis",
      "grass hopper" = "grasshopper",
      "graasshopper" = "grasshopper",
      "milipede" = "millipede",
      "millioed" = "millipede",
      "midgies" = "midges",
      "moths" = "moth",
      "mopth" = "moth",
      "stag beetle" = "beetle",
      "dungbeetle" = "beetle",
      "furniture beetles" = "beetle",
      "ladybird beetle" = "beetle",
      "long-horned beetle" = "beetle",
      "rhino beetle" = "beetle",
      "stag bettle" = "beetle",
      "butterflys" = "butterfly",
      "buttefly" = "butterfly",
      "a fly" =  "fly",
      "flies" =  "fly",
      "flie" =  "fly",
      "dragon fly" =  "dragonfly",
      "dragofly" =  "dragonfly",
      "dragon flies" =  "dragonfly",
      "dragonflies" =  "dragonfly",
      "flas" = "flea",
      "fleas" = "flea",
      "fle" = "fly",
      "flie" = "fly",
      "flu" = "fly",
      "flyn" = "fly",
      "house fly" = "housefly",
      "green fly" = "greenfly",
      "amt" = "ant",
      "been" = "bee",
      "ber" = "bee",
      "bed bug" = "bedbug",
      "bed bugs" = "bedbug",
      "bedbugs" = "bedbug",
      "blubott" = "bluebottle",
      "blue bottle" = "bluebottle",
      "blue bottles" = "bluebottle",
      "cockroaches" = "cockroach",
      "crain fly" = "cranefly",
      "crane fly" = "cranefly",
      "na" = "none",
      "no" = "none",
      "nil" = "none",
      "creepy crawly" = "creepy crawlies",
      "creepy-crawlies" = "creepy crawlies",
      "mosquitos" = "mosquito",
      "moscitos" = "mosquito",
      "moscuito" = "mosquito",
      "mozzy" = "mosquito",
      "maggots" = "maggot",
      "midges" = "midge",
      
      "amazing" = "amaze",
      "amazed" = "amaze",
      "an animal" = "animal",
      "apis mellifera" = "bee",
      "asian hornets" = "asian hornet",
      "beetal" = "beetle",
      "beteal" = "beetle",
      "bitterly" = "butterfly",
      "bugd" = "bug",
      "cacroch" = "cockroach",
      "captiplliar" = "caterpillar",
      "daddy long legs" = "daddy longlegs",
      "daddylonglegs" = "daddy longlegs",
      "fky" = "fly",
      "firefly" = "fly",
      "fruit fly" = "fly",
      "garden spider" = "spider",
      "grasshoper" = "grasshopper",
      
      "ladibird" = "ladybird",
      "lady bugs" = "ladybird",
      "ladybird" = "ladybird",
      "ladybirds" = "ladybird",
      "larry bird" = "ladybird",
      "milipede" = "millipede",
      "moskitos" = "mosquito",
      "stuck insect" = "stick insect",
      "stick bug" = "stick insect",
      "stickbug" = "stick insect",
      "stickinsect" = "stick insect",
      "stickman" = "stick insect",
      "stuck insect" = "stick insect",
      )
  )

QualData_Q10InsectWords_clean_tidied %>%
  group_by(word) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  View()

# **********************************************************************************
#### Section X: Sentiment analysis  ####
# **********************************************************************************


TestData <- QualData_Q9InsectWords_clean_tidied %>%
  unnest_tokens(output = word, input = word)


## Run library(tidyr) for this to work
Sentiments_positive <- get_sentiments("bing") %>%
  filter(sentiment == "positive")
Sentiments_negative <- get_sentiments("bing") %>%
  filter(sentiment == "negative")


TestData %>%
  semi_join(Sentiments_positive) %>%
  count(word, sort = TRUE)


TestData %>%
  semi_join(Sentiments_negative) %>%
  count(word, sort = TRUE)


## Define here
bing <- get_sentiments("bing")


TestData %>%
  inner_join(bing, relationship = "many-to-many") %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(sentiment), scales = "free_y") +
  labs(x = "Contribution to sentiment", y = NULL)



TestData %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  reshape2::acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "green"),
                   max.words = 100)


# **********************************************************************************
#### Section Y: Dummy coding variables  ####
# **********************************************************************************




# Data_Covariates$Q9_Negative <- ifelse(
#   QualData_Q9InsectWords %in% c(
#     ant
#     fly
#     spiderr
#     bee
#     bug
#     wasp
#     grasshopper
#     beetle
#     cockroach
#     butterfly
#     mosquito
#     ladybird
#     ladybug
#     moth
#     worm
#     caterpillar
#     grasshopper
#   ),
#   1, 
#   0 
# )

# Data_Covariates$Q9_Negative <- ifelse(
#   QualData_Q9InsectWords %in% c(
#     creepy
#     yuck
#     annoying
#     not good
#     shiver
#     dirty
#     pest
#     nuisance
#     scary
#     awful
#     gross
#     unsettled
#     squeamish
#     scared
#     sad
#     fear
#     yuk
#     nasty
#     wasp - scared
#     negative
#     disgusting
#     eew
#     sick
#     parasites
#     ugly
#     it will bite me
#     annoyance
#     horrified
#     irritation
#   ),
#   1, 
#   0 
# )



# Data_Covariates$Q9_Positive <- ifelse(
#   QualData_Q9InsectWords %in% c(
#     beautiful
#     happy
#     brave
#     caring
#     bees make me happy
#     cool
#     good
#     interesting
#     can we please have
#     calm
#     relaxed
#     pretty
#     pleasant
#     beauty
#     important
#     ant makes me feel ok
#   ),
#   1, 
#   0 
# )



# Data_Covariates$Q9_Small <- ifelse(
#   QualData_Q9InsectWords %in% c(
#     little
#     small
#     tiny
#     they are tiny animal
#   ),
#   1, 
#   0 
# )

# Data_Covariates$Q9_Nonsense <- ifelse(
#   QualData_Q9InsectWords %in% c(
#     zilch
#     yyt
#     yut
#     yes
#     yac
#     xuxucu
#     xkkxkx
#     xkxkdkxk
#     vjfjfufjfjd
#     xicici
#     ttry
#     tret
#     tgvd
#     revv
#     out
#     oooo
#     onl
#     nxxnxn
#     nxxj
#     nik
#     nigga
#     kjsksb
#     jskvd
#     jdsj
#     huiii
#     f
#     ffycggdyd
#     gbb
#     gbbhh
#     gbbhh
#     gcvvv
#     gggccc
#     djdjdj
#     dkfkkdkd
#     dododo
#     
#     
#   ),
#   1, 
#   0 
# )
