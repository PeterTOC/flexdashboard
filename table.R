# script to extract to webscrape indeed for entry level data scientist

# Author: Peter Boshe
# Version: 2022-05-07

# Packages
library(tidyverse)
library(rvest)
library(httr)
library(broom)
require(wordcloud2)
require(tm)

# Parameters
url <- "https://www.indeed.com/jobs?q=entry%20level%20data%20scientist&l=Remote&from=searchOnHP&vjk=e4dd563aa0c5df59"
domain <- "https://www.indeed.com"
file_out <- here::here("data/table.rds")
file_out2 <- here::here("data/wordcloud.html")
report <- here::here("index.Rmd")
html_output <- here:: here("docs/")
log_output <- here::here("data/logs/")

# ============================================================================

# Code




html <- read_html(url)


job_title <- html |>
  html_nodes("span[title]") |>
  html_text()


company <- html |>
  html_nodes("span.companyName") |>
  html_text()

location <- html |>
  html_nodes("div.companyLocation") |>
  html_text()


links <- html |>
  html_nodes(xpath = "/html/body//tbody//div[1]/h2/a") |>
  html_attr("href")

max_length <- length(job_title)
df <- data.frame(job_title,
                 company,
                 location,
                 domain,
                 links) |>
  mutate(url_link = str_c(domain,"",links)) |>
  select(job_title, company, location, url_link)


# second iteration through the links --------------------------------------

# test with one link

# x <- "https://www.indeed.com/rc/clk?jk=655e1551430353b4&fccid=11619ce0d3c2c733&vjs=3"

extract_description <- function(x) {

  Sys.sleep(2) # to pause between requests

  cat(".") # stone age progress bar

  html2 <- read_html(x)

  job_description <- html2 |>
    html_nodes(xpath = '//*[@id="jobDescriptionText"]') |>
    html_text() |>
    str_squish()

  count_r <- job_description |>
    str_count('[./ ,]R{1}[./ ,]')

  r_present <- job_description |>
    str_detect('[./ ,]R{1}[./ ,]')


  data.frame(job_description = job_description,
         r_present = r_present,
         count_r = count_r)    #important to name the variables to avoid script failure

}


# functional programming for the win! -------------------------------------


listed_df <- df |>
  mutate(description = map(url_link, safely(~ extract_description(.x), otherwise = NA_character_)))


# Our new data set --------------------------------------------------------

indeed_df <- listed_df |>
  unnest(description) |>
  unnest(description) |>
  arrange(desc(count_r))

# Write out table

write_rds(indeed_df,file_out)


# word cloud --------------------------------------------------------------



df <- indeed_df

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

# save out wordcloud

htmlwidgets::saveWidget(wordcloud2::wordcloud2(description_freq_terms[,1:2], shape="circle",
                                               size=1.6, color='random-light', backgroundColor="#7D1854"),  #ED581F
                        file = file_out2,
                        selfcontained = FALSE)

# knit report

rmarkdown::render(report, output_dir = html_output)

# clean environment
rm(list = ls())




