# Load the required packages
library(dplyr)

# Function to get the tables from the Wikipedia page
get_tables <- function(url) {
  page <- rvest::read_html(url)
  # Extract all the tables from the page
  tables <- page %>%
    rvest::html_nodes("table") %>%
    rvest::html_table(fill = TRUE)
  return(tables)
}

# URL for the list of Presidents
url_presidents <- "https://en.wikipedia.org/wiki/List_of_presidents_of_the_United_States"

# Get tables from the URL
tables <- get_tables(url_presidents)

# Process the Presidents table
presidents <- tables[[1]] %>%
  select(Name = 3, Term = 4, Party = 6) %>%
  mutate(
    # Process the columns for Name, Term, Party, and add additional empty columns
    Name = stringr::str_replace(Name, "\\s*\\(.*", ""),
    Term = stringr::str_replace_all(stringr::str_replace(Term, "–.*", ""), "[\\[].*", "") %>%
      as.Date(format = "%B %d, %Y"),
    Party = stringr::str_replace_all(Party, "[\\[|/].*", ""),
    Link = paste0("https://en.wikipedia.org/wiki/", stringr::str_replace_all(Name, " ", "_")),
    Word.Count = NA_real_,
    Rating = NA_real_,
    Score.Anger = NA_real_,
    Score.Fear = NA_real_,
    Score.Joy = NA_real_,
    Score.Sadness = NA_real_,
    Score.Trust = NA_real_,
    Score.Negative = NA_real_,
    Score.Positive = NA_real_
  )

# Loop to analyze each President's page
for (i in 1:nrow(presidents)) {
  url <- presidents$Link[i]
  webpage <- rvest::read_html(url)
  text <- webpage %>%
    rvest::html_nodes("p") %>%
    rvest::html_text(trim = TRUE) %>%
    stringr::str_replace_all("[\\[\\{].*?[\\]\\}]", "") %>%
    paste(collapse = " ")
  Word.Count <- stringr::str_count(text, stringr::boundary("word")) %>% as.numeric()
  sentiment_scores <- syuzhet::get_nrc_sentiment(text)
  
  # Store sentiment scores
  scores <- c(Word.Count,
              sentiment_scores$anger / Word.Count,
              sentiment_scores$fear / Word.Count,
              sentiment_scores$joy / Word.Count,
              sentiment_scores$sadness / Word.Count,
              sentiment_scores$trust / Word.Count,
              sentiment_scores$negative / Word.Count,
              sentiment_scores$positive / Word.Count)
  
  # Assign values to the respective columns
  presidents$Word.Count[i] <- scores[1]
  presidents$Score.Anger[i] <- scores[2]
  presidents$Score.Fear[i] <- scores[3]
  presidents$Score.Joy[i] <- scores[4]
  presidents$Score.Sadness[i] <- scores[5]
  presidents$Score.Trust[i] <- scores[6]
  presidents$Score.Negative[i] <- scores[7]
  presidents$Score.Positive[i] <- scores[8]
  presidents$Rating[i] <- (scores[8] - scores[7]) * 100
}

# URL for the list of Presidents by home state
url_presidents_states <- "https://en.wikipedia.org/wiki/List_of_presidents_of_the_United_States_by_home_state"

# Get tables from the URL
tables_states <- get_tables(url_presidents_states)

# Process the Presidents by home state table
presidents_states <- tables_states[[1]] %>%
  select(2,4) %>%
  rename(president = 1, name = 2) %>%
  mutate(name = stringr::str_replace_all(name, "[^a-zA-Z0-9 ]", "")) %>%
  group_by(name) %>%
  summarize(total = n()) %>%
  arrange(desc(total))

# Get the map data
states <- albersusa::usa_sf("laea") %>% left_join(presidents_states, by = "name")

states$total[is.na(states$total)] <- 0