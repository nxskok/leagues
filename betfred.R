#' see if I can read from betfred's website
#' 
#' 

library(tidyverse)

read_odds <- function(fname, new_colnames) {
  readLines(fname) %>% 
    enframe(name = NULL) %>% 
    mutate(is_time = str_detect(value, ":")) %>% 
    mutate(is_time.1 = lead(is_time)) %>% 
    mutate(is_time.2 = lead(is_time.1)) %>% 
    filter(is_time.1 | is_time.2 | str_detect(value, "/")) %>% 
    select(value) %>% 
    mutate(col = rep(new_colnames, length.out = nrow(.))) %>% 
    mutate(row = rep(1:1000, each = 5, length.out = nrow(.))) %>% 
    pivot_wider(names_from = "col", values_from = "value") 
}

o2p <- function(odds_string) {
  str_split_1(odds_string, pattern = "/") %>% 
    as.numeric() -> p
  p[2]/(p[1]+p[2])
}

odds_to_probs <- function(d) {
  d %>% 
    rowwise() %>% 
    mutate(across(starts_with("odds"), \(x) o2p(x))) %>% 
    mutate(sum = odds_1 + odds_x + odds_2) %>% 
    mutate(across(starts_with("odds"), \(x) x/sum)) %>% 
    select(-sum)
}

new_colnames <- c("t1", "t2", "odds_1", "odds_x", "odds_2")
d1 <- read_odds("betfred_thing.txt", new_colnames)
d1

#' turn these odds into probabilities with overround

d2 <- odds_to_probs(d1)
d2

#' then turn these into poisson means
#' then link with ptf


#' this doesn't work

library(rvest)

my_url <- "https://www.betfred.com/sports/football"
html <- read_html(my_url)
html %>% html_nodes(css = "div._yynop3:nth-child(1)")
