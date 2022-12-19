# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  updateSelectizeInput(session, 'token_selection', server = TRUE,
                       choices = unique_tokens, selected = "emin domain")

  convo_ls <- reactive({
    kxl_data %>%
      keep(~.x$title == input$description_selection) %>%
      flatten() %>%
      modify_at(
        c("document", "preprocessing", "tokens", "token_counts", "gammas"),
        ~ filter(.x, order %in% input$row:(input$row + input$interactions)))
  }) %>%
    bindEvent(input$update_convo, ignoreNULL = FALSE)

  remark_1 <- reactive({
    kxl_data %>%
      keep(~.x$title == input$distance_doc_1) %>%
      flatten() %>%
      modify_at(
        c("document", "preprocessing", "tokens", "token_counts", "gammas"),
        ~ filter(.x, order == input$distance_row_1))
  }) %>%
    bindEvent(input$distance_doc_1, input$distance_row_1)

  remark_2 <- reactive({
    kxl_data %>%
      keep(~.x$title == input$distance_doc_2) %>%
      flatten() %>%
      modify_at(
        c("document", "preprocessing", "tokens", "token_counts", "gammas"),
        ~ filter(.x, order == input$distance_row_2))
  }) %>%
    bindEvent(input$distance_doc_2, input$distance_row_2)

  output$convoTable <- renderTable({
    convo_ls() %>%
      pluck("document") %>%
      table_styling()
  })

  output$downloadTable <- downloadHandler(
    filename = function() {paste("exchange.csv")},
    content = function(file) {
      convo_ls() %>%
        pluck("document") %>%
        table_styling() %>%
        write_csv(file)
    }
  )

  convo_plot <- reactive({
    convo_ls() %>%
      plot_topic_bars(na_to_zero = T, legend_cols = input$legend_cols) +
      facet_wrap(~ order, ncol = 1, scale = "free_x",
                 strip.position = "left") +
      labs(title = "Topic loadings by remark")
  }) %>%
    bindEvent(input$update_convo, ignoreNULL = FALSE)

  output$convoPlot <- renderPlot({
    convo_plot()
  })

  output$downloadGraph <- downloadHandler(
    filename = function(){"convo.png"},
    content = function(file) {
      ggsave(file, plot = convo_plot(), device = "png",
             width = input$width, height = input$height)
    }
  )

  output$tokenTable <- renderTable({
    tokens %>%
      filter(token == input$token_selection) %>%
      table_styling()
  }) %>%
    bindEvent(input$token_selection)

  output$tokenBetas <- renderTable({
    betas %>%
      filter(term == input$token_selection) %>%
      select(! term) %>%
      table_styling()
  }) %>%
    bindEvent(input$token_selection)

  output$quote_distance <- renderText({
    get_gammas <- function(entry){
      entry %>%
        pluck("gammas") %>%
        arrange(topic) %>%
        {.$gamma}
    }
    sqrt(sum((get_gammas(remark_1()) - get_gammas(remark_2()))^2)) %>%
      round(digits = 4) %>%
      {glue("Distance: {.}")}
  })

  output$quote_1 <- renderText({
    remark_1() %>%
      pluck("document") %>%
      {.$content}
  })

  output$quote_2 <- renderText({
    remark_2() %>%
      pluck("document") %>%
      {.$content}
  })

  output$quote_gammas_1 <- renderPlot({
    remark_1() %>%
      plot_topic_bars() +
      guides(fill = "none") +
      labs(subtitle = "Topic Loadings Remark 1 (vector)")
  })

  output$quote_gammas_2 <- renderPlot({
    remark_2() %>%
      plot_topic_bars() +
      guides(fill = "none") +
      labs(subtitle = "Topic Loadings Remark 1 (vector)")
  })

  output$quote_tokens_1 <- renderText({
    remark_1() %>%
      pluck("tokens") %>%
      {str_c(.$token, collapse = "; ")}
  })

  output$quote_tokens_2 <- renderText({
    remark_2() %>%
      pluck("tokens") %>%
      {str_c(.$token, collapse = "; ")}
  })

})
