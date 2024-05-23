library(shiny)
library(igraph)

# Function to parse the grammar and create a graph
parse_grammar <- function(grammar) {
  # Split input into lines
  lines <- strsplit(grammar, "\n")[[1]]
  
  # Create an empty directed graph
  g <- make_empty_graph(directed = TRUE)
  
  # Set to keep track of unique vertices
  vertices <- character(0)
  
  # Add the final state vertex
  final_state <- "Z"
  g <- add_vertices(g, 1, color = "red", name = final_state)
  vertices <- c(vertices, final_state)
  
  initial_state <- "S"
  g <- add_vertices(g, 1, color = "green", name = initial_state)
  vertices <- c(vertices, initial_state)
  
  # Loop over lines to add edges
  for (line in lines) {
    # Match the pattern "NonTerminal -> Terminal NonTerminal" or "NonTerminal -> Terminal"
    matches <- regmatches(line, regexec("([A-Z])\\s*->\\s*([a-z])([A-Z]?)", line))[[1]]
    if (length(matches) >= 3) {
      from <- matches[2]
      terminal <- matches[3]
      to <- ifelse(nchar(matches[4]) > 0, matches[4], final_state)
      
      # Add vertices if they don't exist
      if (!(from %in% vertices)) {
        g <- add_vertices(g, 1, name = from)
        vertices <- c(vertices, from)
      }
      if (!(to %in% vertices)) {
        g <- add_vertices(g, 1, name = to)
        vertices <- c(vertices, to)
      }
      
      # Add edges to the graph
      g <- add_edges(g, c(from, to), label = terminal)
    }
  }
  
  return(g)
}

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Regular Grammar to Automaton"),
  
  # Sidebar with a text area input for grammar
  sidebarLayout(
    sidebarPanel(
      textAreaInput("grammar_input", 
                    "Write your grammar here:", 
                    value = "S -> aA\nS -> bA\nA -> bB\nA -> c\nB -> c", 
                    rows = 20)
    ),
    
    # Show the text output and plot output
    mainPanel(
      plotOutput("automatonPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$automatonPlot <- renderPlot({
    # Parse the grammar and create the graph
    grammar <- input$grammar_input
    g <- parse_grammar(grammar)
    
    # Plot the graph
    plot(g, vertex.label = V(g)$name, edge.label = E(g)$label,
         vertex.size = 30, 
         edge.arrow.size = 0.5)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
