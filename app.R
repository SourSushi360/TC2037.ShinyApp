library(shiny)
library(igraph)

parse_grammar <- function(grammar) {
  lines <- strsplit(grammar, "\n")[[1]]
  g <- make_empty_graph(directed = TRUE)
  vertices <- character(0)
  
  final_state <- "Z"
  g <- add_vertices(g, 1, color = "red", name = final_state)
  vertices <- c(vertices, final_state)
  
  initial_state <- "S"
  g <- add_vertices(g, 1, color = "green", name = initial_state)
  vertices <- c(vertices, initial_state)
  
  for (line in lines) {
    matches <- regmatches(line, regexec("([A-Z])\\s*->\\s*([a-z])([A-Z]?)", line))[[1]]
    if (length(matches) >= 3) {
      from <- matches[2]
      terminal <- matches[3]
      to <- ifelse(nchar(matches[4]) > 0, matches[4], final_state)
      
      if (!(from %in% vertices)) {
        g <- add_vertices(g, 1, name = from)
        vertices <- c(vertices, from)
      }
      if (!(to %in% vertices)) {
        g <- add_vertices(g, 1, name = to)
        vertices <- c(vertices, to)
      }
      
      g <- add_edges(g, c(from, to), label = terminal)
    }
  }
  
  return(g)
}

ui <- fluidPage(
  titlePanel("Regular Grammar to Automaton"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput("grammar_input", "Write your grammar here:", 
                    value = "S -> aA\nS -> bA\nA -> bB\nA -> c\nB -> c", 
                    rows = 20)
    ),
    mainPanel(
      plotOutput("automatonPlot")
    )
  )
)

server <- function(input, output) {
  output$automatonPlot <- renderPlot({
    grammar <- input$grammar_input
    g <- parse_grammar(grammar)
    
    plot(g, vertex.label = V(g)$name, edge.label = E(g)$label,
         vertex.size = 30,
         layout = layout_with_kk,
         edge.arrow.size = 0.5)
  })
}

shinyApp(ui = ui, server = server)
