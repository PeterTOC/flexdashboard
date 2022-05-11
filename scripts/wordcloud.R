# script to create word cloud

# Author: Peter Boshe
# Version: 2022-05-09

# Packages
library(tidyverse)
require(wordcloud2)
require(tm)

# Parameters
  #input file
file_raw <- here::here("data/table.rds")
  #output file
file_out <- here::here("data/wordcloud.html")

# ============================================================================

# Code

df <- read_rds(file_raw)

# create corpus function
corpus_tm <- function(x){
  corpus_tm <- Corpus(VectorSource(x))
}
#create corpus
df |>
  pull(job_description) |>
  unlist() |> # might need to remove
  corpus_tm() ->corpus_descriptions

#inspect corpus
# summary(corpus_descriptions)

corpus_descriptions |>
  tm_map(removePunctuation) |>
  tm_map(stripWhitespace) |>
  tm_map(content_transformer(function(x) iconv(x, to='UTF-8', sub='byte'))) |>
  tm_map(removeNumbers) |>
  tm_map(removeWords, stopwords("en")) |>
  tm_map(content_transformer(tolower)) |>
  tm_map(removeWords, c("etc","ie","eg", stopwords("english"))) -> clean_corpus_descriptions

# inspect content

#clean_corpus_descriptions[[1]]$content

# create termdocumentmatrix to attain frequent terms

find_freq_terms_fun <- function(corpus_in){
  doc_term_mat <- TermDocumentMatrix(corpus_in)
  freq_terms <- findFreqTerms(doc_term_mat)[1:max(doc_term_mat$nrow)]
  terms_grouped <- doc_term_mat[freq_terms,] %>%
    as.matrix() %>%
    rowSums() %>%
    data.frame(Term=freq_terms, Frequency = .) %>%
    arrange(desc(Frequency)) %>%
    mutate(prop_term_to_total_terms=Frequency/nrow(.))
  return(data.frame(terms_grouped))
}

description_freq_terms <- data.frame(find_freq_terms_fun(clean_corpus_descriptions))

# description_freq_terms

htmlwidgets::saveWidget(wordcloud2::wordcloud2(description_freq_terms[,1:2], shape="circle",
                       size=1.6, color='random-light', backgroundColor="#7D1854"),  #ED581F
                       file = file_out,
                       selfcontained = FALSE)



