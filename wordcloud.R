library(wordcloud)
library(tidyverse)
library(scales)
library(lubridate)

dataset <- read.csv("output.csv")

text_p <- dataset$review_text

wordcloud(US_tags$tags, US_tags$frequency,
          scale = c(8, .2), min.freq = 30000,
          max.words = Inf, random.order = FALSE, rot.per = .15, colors = brewer.pal(8, "Set1")
)
