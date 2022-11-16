#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(fluidPage(
  titlePanel("Discourse visualizer"),

  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        id = "panel_selection",
        tabPanel(
          value = "document",
          title = "Document:",
          selectizeInput("description_selection",
                         "Select document",
                         arenas,
                         selected = "ne_2011-11-07"),
          numericInput("row", "Select remark #:", 75, step = 1),
          numericInput("interactions", "No. interactions:", 5, step = 1),
          actionButton(inputId = "update_convo", label = "Update"),
          tags$br(),
          tags$hr(),
          tags$em(
            "The graph on the right visualizes the exchange in the transcript ",
            "on the right. When the bar for one topic dominates the graph ",
            "for a remark, the remark likely falls into this topic. When ",
            "there are two or three tall bars, the remark likely straddles ",
            "these topics. When the bar graphs for two remarks visually ",
            "look different, two actors likely talk about different areas ",
            "of the discourse, e.g., lawmaking process vs. constituents' ",
            "interests."
          )
        ),
        tabPanel(
          value = "search",
          title = "Search",
          selectizeInput("token_selection",
                         "Find token:",
                         choices = NULL),
          tags$hr(),
          tags$em("Use the search to learn what terms are part of the formal ",
                  "topic model. Terms that appear in a lot of ",
                  "remarks--i.e., show up in more than 15% of remarks--are ",
                  "not suitable for modeling the difference between remarks ",
                  "and achieving a good separation. Rare tokens that show up",
                  "in less than 1% of remarks ",
                  "also been disregarded. The table on the right shows what ",
                  "topics a token is associated with ",
                  "and how strong the association is.")
        ),
        tabPanel(
          value = "distance",
          title = "Calculate distance",
          h3("Remark 1"),
          selectizeInput("distance_doc_1",
                         "Select document",
                         arenas,
                         selected = "ne_2011-11-07"),
          numericInput("distance_row_1", "Select row:", 75, step = 1),
          h3("Remark 2"),
          selectizeInput("distance_doc_2",
                         "Select document",
                         arenas,
                         selected = "ne_2011-11-07"),
          numericInput("distance_row_2", "Select row:", 76, step = 1)
        )
      )
    ),

    mainPanel(
      conditionalPanel(
        "input.panel_selection == 'document'",
        h3("Transcript"),
        fluidRow(column(6, tableOutput("convoTable")),
                 column(
                   6, plotOutput("convoPlot",
                                 height = "600px")
                 ))
      ),
      conditionalPanel(
        "input.panel_selection == 'search'",
        fluidRow(column(
          8,
          h3("Locations of token"),
          tableOutput("tokenTable")
        ),
        column(
          4,
          h3("Loadings for token"),
          tableOutput("tokenBetas")
        ))
      ),
      conditionalPanel(
        "input.panel_selection == 'distance'",
        h2(textOutput("quote_distance")),
        tags$em(
          "Calculated as Euclidean distance",
          "between vectors for remarks 1 & 2"
        ),
        fluidRow(
          column(
            6,
            h3("Remark 1"),
            plotOutput("quote_gammas_1",
                       height = "100px"),
            tags$blockquote(textOutput("quote_1"),
                            style = "font-size:100%;"),
            h4("Tokens:"),
            textOutput("quote_tokens_1")
          ),
          column(
            6,
            h3("Remark 2"),
            plotOutput("quote_gammas_2",
                       height = "100px"),
            tags$blockquote(textOutput("quote_2"),
                            style = "font-size:100%;"),
            h4("Tokens:"),
            textOutput("quote_tokens_2")
          )

        )
      )
    )
  )
))
