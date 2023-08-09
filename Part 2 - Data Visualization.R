# Load the necessary packages
library(dplyr)
library(shiny)
library(DT)

# Define the UI of the Shiny App
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  tabsetPanel(
    tabPanel("Presidential Sentiments",
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
                 h3("Presidential Data Table"),
                 DTOutput("presidentsTable"),
                 hr(),
                 h3("Summary Statistics"),
                 tableOutput("summaryTable")
               )
             )),
    tabPanel("Presidential Birthplaces",
             tags$h3("Count of Presidents by State", style = "text-align:center;"),
             plotOutput("presidentsMap")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Render the President Data Table based on filters
  output$presidentsTable <- renderDT({
    
    # Filter the data based on input conditions
    filtered_presidents <- presidents %>%
      filter(("ALL" %in% input$nameFilter | Name %in% input$nameFilter) &
               ("ALL" %in% input$partyFilter | Party %in% input$partyFilter) &
               (Term >= input$dateFilter[1] & Term <= input$dateFilter[2])) %>%
      select(c("Name", input$columns))
    
    # Format the columns if present
    if ("Word.Count" %in% colnames(filtered_presidents)) {
      filtered_presidents$Word.Count <- scales::comma(filtered_presidents$Word.Count, accuracy = 1)
    }
    
    if ("Rating" %in% colnames(filtered_presidents)) {
      filtered_presidents$Rating <- scales::comma(filtered_presidents$Rating, accuracy = 0.00001)
    }
    
    # Convert numeric columns starting with "Score" to percentages
    datatable(filtered_presidents %>% mutate(across(starts_with("Score"), ~ scales::percent(.x, accuracy = 0.01))), 
              options = list(pageLength = 10, columnDefs = list(list(className = 'dt-center', targets = '_all'))))
  })
  
  # Render the Summary Table
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
          mutate(Word.Count = scales::comma(Word.Count, accuracy = 1))
        
      }
      
      if ("Rating" %in% colnames(filtered_presidents)) {
        
        filtered_presidents <- filtered_presidents %>%
          mutate(Rating = scales::comma(Rating, accuracy = 0.00001))
        
      }
      
      # Convert numeric values to percentages except for Word.Count
      filtered_presidents <- filtered_presidents %>%
        mutate(across(starts_with("Score"), ~ scales::percent(.x, accuracy = 0.01)))
      
      colnames(filtered_presidents) <- paste0("Average ", colnames(filtered_presidents))
      
      filtered_presidents
      
    }
  })
  
  # Render the map of presidents' birthplaces
  output$presidentsMap <- renderPlot({
    
    ggplot2::ggplot(data = states) +
      ggplot2::geom_sf(ggplot2::aes(fill = total), color = "black") +
      ggplot2::scale_fill_gradient(low = "white", high = "blue", name = "Total") +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "bottom")
    
  })
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

