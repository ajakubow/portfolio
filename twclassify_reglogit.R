################################################################################
# File:          twclassify_reglogit.R
#
# Description:    Uses regularized logistic regression classifier developed
#                 from testing against fb/cd data to predict classes of twitter
#                 corpus
# First version:  181207
# This verison:   181207
# Last executed:  181207    
# Last change by: Alex
# requires:       cdfb_analysis.Rdata
# provides:       
################################################################################


### Priors ---------------------------------------------------------------------
rm(list = ls())

## Packages ##
library(dplyr)
library(stringr)
library(data.table)
library(parallel)
library(quanteda)
library(pbapply)
library(qdapRegex)
library(preText)
library(caret)
library(glmnet)
library(stargazer)

## Load Functions ##
source("functions/functions.R")


## Create Dataset --------------------------------------------------------------
load("data_out/cdfb_analysis.Rdata")
load("data_out/tw_analysis.Rdata")

## Prep Twitter Data ##
tw2 <- tw[[1]] %>%
  filter(numFlagWords > 0) %>%
  mutate(cat_ai = ifelse(numFlagWords > 1, 1, 0), 
         cat_po = ifelse(numFlagWords > 1, 1, 0), 
         cat_ch = ifelse(numFlagWords > 1, 1, 0), 
         cat_pi = ifelse(numFlagWords > 1, 1, 0), 
         cat_un = ifelse(numFlagWords > 1, 1, 0), 
         cat_pich = ifelse(numFlagWords > 1, 1, 0)) %>%
  select(text, starts_with("cat_"))
  set.seed(8675309)
# twSample <- sample_n(tw2, 4000)
# fwrite(twSample, file = "data_out/twSample.csv")
  
# tw3 <- tw[[2]] %>%
#   filter(numFlagWords > 0) %>%
#   filter(!(i_trump == 1 & numFlagWords == 1)) %>%
#   select(text)
# twSample2 <- sample_n(tw3, 4000)
# fwrite(twSample2, file = "data_out/twSample_noTrump.csv")


## Prep Coded Data ##
cd <- cdfb[[3]] %>%
  select(-`ARTICLE TITLE`) 
name1 <- names(cd)
fb <- cdfb[[4]] %>%
  select(-shares, -likes, -`group who posted`, -`readng score`, -postType)
name2 <- names(fb)
coded <- setDF(rbind(cd, fb)) %>%
  select(text, everything()) %>%
  select( -`Anti-Black`, -`Anti-Inclusive`, -Political, 
          -Challenge, -`Pro-Inclusivity`,
          -`Aggressive/Hostile`, -Unrelated, -twLength, -read)

## Modify ##
coded <- coded %>%
  mutate(vars(starts_with("cat_")), 
         funs(as.numeric)) %>%
  mutate(cat_un = ifelse(cat == "", 1, cat_un),
         cat_pich = ifelse(cat_pi == 1 | cat_ch == 1, 1, 0)) %>%
  select(text, starts_with("cat_")) %>%
  select(-cat_ab, -cat_ag)

## Merge ##
master <- rbind(coded, tw2)
master$text <- gsub('@[0-9_A-Za-z]+', '@', master$text)

### Create Corpus and DFM ------------------------------------------------------
## Corpus ##
mycorp <- corpus(master$text)

## Preprocess ##
# work <- work %>%
#   filter(twLength == 1)
#first,lower case and remove links
toks <- tokens(char_tolower(master$text),
               remove_url = TRUE,
               remove_punct = TRUE)
#then, remove stopwords and other link sources
toks <- tokens_remove(toks, 
                      c(stopwords("english"), 
                       "t.co", "https", "rt", "amp", "http", "t.c",  "can"))
toks <- tokens_remove(toks, "[[:punct:]]", valuetype = "regex")
#then stem words
toks <- tokens_wordstem(toks)
#then, create n-grams
toks <- tokens_ngrams(toks, 1:2)
#featnames(dfm(toks, verbose = FALSE))


## Create DFM ##
myDFM <- dfm(toks)
myDFM <- dfm_trim(myDFM, min_docfreq = 2)  #must appear in at least 2 posts
myDFM <- dfm_trim(myDFM, max_docfreq = 0.9, docfreq_type = "quantile")


### Regularized Regression -----------------------------------------------------
## Models ##
start <- Sys.time()
ai <- rregTW(dat = master, corp = mycorp, DFM = myDFM, text_var = "text", 
            class_var = "cat_ai")
po <- rregTW(dat = master, corp = mycorp, DFM = myDFM, text_var = "text", 
           class_var = "cat_po")
ch <- rregTW(dat = master, corp = mycorp, DFM = myDFM, text_var = "text", 
           class_var = "cat_ch")
pi <- rregTW(dat = master, corp = mycorp, DFM = myDFM, text_var = "text", 
           class_var = "cat_pi")
pich <- rregTW(dat = master, corp = mycorp, DFM = myDFM, text_var = "text", 
             class_var = "cat_pich")
un <- rregTW(dat = master, corp = mycorp, DFM = myDFM, text_var = "text", 
           class_var = "cat_un")
stop <- Sys.time()
stop - start

## Extract Assignments ##
assignments <- data.frame(ai, po, ch, pi, pich, un)
names(assignments) <- c("cat_ai", "cat_po", "cat_ch", "cat_pi", "cat_pich", "
                        cat_un")

### Tidy and Save --------------------------------------------------------------
twOut <- tw[[1]] %>%
  filter(numFlagWords > 0)
twOut <- bind_cols(twOut, assignments)
save(twOut, file = "data_out/tw_classified.Rdata")

