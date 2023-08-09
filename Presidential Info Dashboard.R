# Load the packages
library(albersusa)
library(ggplot2)
library(shiny)
library(DT)
library(dplyr)
library(scales)
library(shinythemes)
library(stringr)
library(rvest)
library(magrittr)
library(syuzhet)

# Function to get the tables from the Wikipedia page
get_tables <- function(url) {
  page <- read_html(url)
  tables <- page %>% html_nodes("table") %>% html_table(fill = TRUE)
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
    Name = str_replace(Name, "\\s*\\(.*", ""),
    Term = str_replace_all(str_replace(Term, "–.*", ""), "[\\[].*", "") %>%
      as.Date(format = "%B %d, %Y"),
    Party = str_replace_all(Party, "[\\[|/].*", ""),
    Link = paste0("https://en.wikipedia.org/wiki/", str_replace_all(Name, " ", "_")),
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
  webpage <- read_html(url)
  text <- webpage %>% html_nodes("p") %>% html_text(trim = TRUE) %>%
    str_replace_all("[\\[\\{].*?[\\]\\}]", "") %>% paste(collapse = " ")
  Word.Count <- str_count(text, boundary("word")) %>% as.numeric() 
  sentiment_scores <- get_nrc_sentiment(text)
  
  presidents$Word.Count[i] <- Word.Count
  presidents$Score.Anger[i] <- (sentiment_scores$anger %>% as.numeric()) / Word.Count
  presidents$Score.Fear[i] <- sentiment_scores$fear %>% as.numeric() / Word.Count
  presidents$Score.Joy[i] <- sentiment_scores$joy %>% as.numeric() / Word.Count
  presidents$Score.Sadness[i] <- sentiment_scores$sadness %>% as.numeric() / Word.Count
  presidents$Score.Trust[i] <- sentiment_scores$trust %>% as.numeric() / Word.Count
  presidents$Score.Negative[i] <- sentiment_scores$negative %>% as.numeric() / Word.Count
  presidents$Score.Positive[i] <- sentiment_scores$positive %>% as.numeric() / Word.Count
  presidents$Rating[i] <- (presidents$Score.Positive[i] - presidents$Score.Negative[i]) * 100
  
}

# URL for the list of Presidents
url_presidents.states <- "https://en.wikipedia.org/wiki/List_of_presidents_of_the_United_States_by_home_state"

# Get tables from the URL
tables.states <- get_tables(url_presidents.states)

# Process the Presidents table
presidents.states <- tables.states[[1]] %>%
  select(2,4) %>%
  rename(president = 1, name = 2) %>%
  mutate(name = str_replace_all(name, "[^a-zA-Z0-9 ]", "")) %>%
  group_by(name) %>%
  summarize(total = n()) %>%
  arrange(desc(total))

# Get the map data
states <- usa_sf("laea") %>% left_join(presidents.states, by = "name")

states$total[is.na(states$total)] <- 0

ui <- fluidPage(
  theme = shinytheme("flatly"),
  tabsetPanel(
    tabPanel("Presidential Sentiments", # Name of the first tab
             sidebarLayout(
               sidebarPanel(
                 selectInput("nameFilter", "Select Name", choices = c("ALL", unique(presidents$Name)), selected = "ALL", multiple = TRUE),
                 selectInput("partyFilter", "Select Party", choices = c("ALL", unique(presidents$Party)), selected = "ALL", multiple = TRUE),
                 dateRangeInput("dateFilter", 
                                "Select Term Range",
                                start = min(presidents$Term),
                                end = max(presidents$Term),
                                format = "yyyy",
                                separator = " - "),
                 checkboxGroupInput("columns", "Select Columns to Display", 
                                    choices = setdiff(names(presidents)[-1], "Link"), 
                                    selected = c("Name", "Word.Count", "Rating"))
               ),
               mainPanel(
                 h3("Presidential Data Table"), # Title for the first table
                 DTOutput("presidentsTable"),
                 hr(),
                 h3("Summary Statistics"), # Title for the summary table
                 tableOutput("summaryTable")
               )
             )),
    tabPanel("Presidential Birthplaces", # Name of the second tab
             tags$h3("Count of Presidents by State", style = "text-align:center;"),
             plotOutput("presidentsMap")
    )
  )
)

server <- function(input, output, session) {
  output$presidentsTable <- renderDT({
    filtered_presidents <- presidents
    
    if (!"ALL" %in% input$nameFilter) {
      filtered_presidents <- filtered_presidents %>%
        filter(Name %in% input$nameFilter)
    }
    
    if (!"ALL" %in% input$partyFilter) {
      filtered_presidents <- filtered_presidents %>%
        filter(Party %in% input$partyFilter)
    }
    
    filtered_presidents <- filtered_presidents %>%
      filter(Term >= input$dateFilter[1] & Term <= input$dateFilter[2]) %>%
      select(c("Name", input$columns))
    
    if ("Word.Count" %in% colnames(filtered_presidents)) {
      
      filtered_presidents <- filtered_presidents %>%
        mutate(Word.Count = comma(Word.Count, accuracy = 1))
      
    }
    
    if ("Rating" %in% colnames(filtered_presidents)) {
      
      filtered_presidents <- filtered_presidents %>%
        mutate(Rating = comma(Rating, accuracy = 0.00001))
      
    }
    
    filtered_presidents %>%
      mutate(across(starts_with("Score"), ~ scales::percent(.x, accuracy = 0.01))) %>%
      datatable(options = list(pageLength = 10, 
                               columnDefs = list(list(className = 'dt-center', targets = '_all'))))
  })
  
  output$summaryTable <- renderTable({
    filtered_presidents <- presidents
    
    if (!"ALL" %in% input$nameFilter) {
      filtered_presidents <- filtered_presidents %>%
        filter(Name %in% input$nameFilter)
    }
    
    if (!"ALL" %in% input$partyFilter) {
      filtered_presidents <- filtered_presidents %>%
        filter(Party %in% input$partyFilter)
    }
    
    filtered_presidents <- filtered_presidents %>%
      filter(Term >= input$dateFilter[1] & Term <= input$dateFilter[2]) %>%
      select(c("Name", "Party", "Term", input$columns)) %>%
      select(-Name, -Party, -Term)
    
    if (ncol(filtered_presidents) > 0) { 
      
      filtered_presidents <- filtered_presidents %>%
        select_if(is.numeric) %>%
        summarise(across(everything(), mean))
      
      if ("Word.Count" %in% colnames(filtered_presidents)) {
        
        filtered_presidents <- filtered_presidents %>%
          mutate(Word.Count = comma(Word.Count, accuracy = 1))
        
      }
      
      if ("Rating" %in% colnames(filtered_presidents)) {
        
        filtered_presidents <- filtered_presidents %>%
          mutate(Rating = comma(Rating, accuracy = 0.00001))
        
      }
      
      # Convert numeric values to percentages except for Word.Count
      filtered_presidents <- filtered_presidents %>%
        mutate(across(starts_with("Score"), ~ scales::percent(.x, accuracy = 0.01)))
      
      colnames(filtered_presidents) <- paste0("Average ", colnames(filtered_presidents))
      
      filtered_presidents
      
    }
  })
  
  output$presidentsMap <- renderPlot({
    
    ggplot(data = states) +
      geom_sf(aes(fill = total), color = "black") +
      scale_fill_gradient(low = "white", high = "blue", name = "Total") +
      theme_void() +
      theme(legend.position = "bottom")
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
