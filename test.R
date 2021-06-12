# load libraries
library(shiny)
library(tidyverse)
library(tidytext)
library(glue)
library(plotly)
#read data
covid_speeches_words <- read_rds("covid-speeches-words.rds")

# Transform Data
csw <- covid_speeches_words %>%
  filter(origin == "Scotland") %>%
  anti_join(stop_words) 
