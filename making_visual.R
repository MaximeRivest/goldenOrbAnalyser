#------------------------------------------------------------------------------
# Making the visual
#------------------------------------------------------------------------------
making_visual <- function(WOS_table, index_cr_list, community, keyword_freq_df, suit_freq_tables,
                          most_cited_df, university_freq_df, country_freq_df) 
{
  #------------------------------------------------------------------------------
  # Values to show
  #------------------------------------------------------------------------------
  total_cr_quant <- length(unlist(strsplit(WOS_table$CR, split="; ")))
  replicated_cr_quant <- length(unlist(indexed_cr_list))
  communities_quant <- length(community)
  communities_sizes <- as.data.frame(t(as.matrix(sizes(community))), row.names = "Number of records")
  commun_kw_across_com <- reshape::merge_all(keyword_freq_df)
  quant_com <- max(WOS_table$record_community, na.rm = T) + 1
  descri_num <- 1:quant_com
  descriminant_keyword_vector <- find_descriminant_keyword(keyword_freq_df)
  names(descri_num) <- descriminant_keyword_vector
  library(shiny)
  library(ggplot2)
  library(DT)
  #------------------------------------------------------------------------------
  ui <- fluidPage(
    titlePanel(tags$h1("WOS tool")),
    hr(),
    fluidRow(
      column(12,
             tags$h2("Summary"),
             tags$h3("Records : ", nrow(WOS_table)),
             tags$h3("Cited references : ", total_cr_quant),
             tags$h3("Cited references found more than once : ", replicated_cr_quant),
             tags$h3("Communities detected : ", communities_quant))),
    hr(),
    fluidRow(
      column(2, tags$h3("Communities sizes : ")),
      column(4, DT::dataTableOutput("Csizes"))),
    br(),
    br(),
    fluidRow(
      column(2, h3("Community : ")),
      column(10, radioButtons("num",
                              label = NULL,
                              choices = descri_num,
                              inline = T,
                              selected = quant_com))),
    br(),
    br(),
    fluidRow(
      column(4, DT::dataTableOutput(outputId = "keyword")),
      column(8, DT::dataTableOutput(outputId = "cr"))),
    fluidRow(
      column(6, DT::dataTableOutput(outputId = "author")),
      column(6, DT::dataTableOutput(outputId = "SO"))),
    fluidRow(
      column(4, DT::dataTableOutput(outputId = "WC")),
      column(4, DT::dataTableOutput(outputId = "journal")),
      column(4, DT::dataTableOutput(outputId = "cit_auth"))),
    fluidRow(
      column(3, DT::dataTableOutput(outputId = "Uni")),
      column(3, DT::dataTableOutput(outputId = "country")),
      column(6, plotOutput(outputId = "PY"))),
    fluidRow(
      column(12, dataTableOutput(outputId = "ref_df")))
  )
  #------------------------------------------------------------------------------
  server <- function(input,output) {
    
    output$keyword <- DT::renderDataTable(
      keyword_freq_df[[as.integer(input$num)]],
      extensions = "Scroller",
      options = list(
        deferRender = TRUE,
        dom = "frtiS",
        scrollY = 400,
        scrollCollapse = TRUE,
        searching = FALSE)
    )
    output$cr <- DT::renderDataTable(
      suit_freq_tables$CR[[as.integer(input$num)]],
      extensions = "Scroller",
      options = list(
        deferRender = TRUE,
        dom = "frtiS",
        scrollY = 400,
        scrollCollapse = TRUE,
        searching = FALSE)
    )
    output$journal <- DT::renderDataTable(
      most_cited_df$journal,
      extensions = "Scroller",
      options = list(
        deferRender = TRUE,
        dom = "frtiS",
        scrollY = 350,
        scrollCollapse = TRUE,
        searching = FALSE)
    )
    output$cit_auth <- DT::renderDataTable(
      most_cited_df$author,
      extensions = "Scroller",
      options = list(
        deferRender = TRUE,
        dom = "frtiS",
        scrollY = 350,
        scrollCollapse = TRUE,
        searching = FALSE)
    )
    output$author <- DT::renderDataTable(
      suit_freq_tables$AU[[as.integer(input$num)]],
      extensions = "Scroller",
      options = list(
        deferRender = TRUE,
        dom = "frtiS",
        scrollY = 350,
        scrollCollapse = TRUE,
        searching = FALSE)
    )
    output$SO <- DT::renderDataTable(
      suit_freq_tables$SO[[as.integer(input$num)]],
      extensions = "Scroller",
      options = list(
        deferRender = TRUE,
        dom = "frtiS",
        scrollY = 350,
        scrollCollapse = TRUE,
        searching = FALSE)
    )
    output$WC <- DT::renderDataTable(
      suit_freq_tables$WC[[as.integer(input$num)]],
      extensions = "Scroller",
      options = list(
        deferRender = TRUE,
        dom = "frtiS",
        scrollY = 350,
        scrollCollapse = TRUE,
        searching = FALSE)
    )
    output$Uni <- DT::renderDataTable(
      university_freq_df[[as.integer(input$num)]],
      extensions = "Scroller",
      options = list(
        deferRender = TRUE,
        dom = "frtiS",
        scrollY = 350,
        scrollCollapse = TRUE,
        searching = FALSE)
    )
    output$country <- DT::renderDataTable(
      country_freq_df[[as.integer(input$num)]],
      extensions = "Scroller",
      options = list(
        deferRender = TRUE,
        dom = "frtiS",
        scrollY = 350,
        scrollCollapse = TRUE,
        searching = FALSE)
    )
    output$PY <- renderPlot({
      if (input$num != quant_com){
        temp_df <- subset(WOS_table, record_community == input$num, PY)
        ggplot(temp_df, aes(x = PY)) + geom_bar() + scale_x_continuous(limits = c(1950,2016))
      } else {
        ggplot(WOS_table, aes(x = PY)) + geom_bar() + scale_x_continuous(limits = c(1950,2016))
      }
    })
    output$Csizes <- DT::renderDataTable(
      communities_sizes,
      options = list(
        paging = FALSE,
        ordering = FALSE,
        info = FALSE,
        searching = FALSE)
    )
    # output$ref_df <- renderDataTable(
    #   cited_reference_df,
    #   extensions = "Scroller",
    #   escape = F,
    #   options = list(
    #     deferRender = TRUE,
    #     dom = "frtiS",
    #     scrollY = 350,
    #     scrollCollapse = TRUE,
    #     searching = FALSE)
    # )
  }
  #------------------------------------------------------------------------------
  #------------------------------------------------------------------------------
  return(shinyApp(ui = ui , server = server))
  #------------------------------------------------------------------------------
  #------------------------------------------------------------------------------
}
