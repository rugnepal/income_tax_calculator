
# Define server logic

function(input, output, session) {
  observe({
    iv <- InputValidator$new()
    iv$add_rule("income", sv_between(0, Inf))
    iv$add_rule("cit", sv_between(0, max_cit(input$income)))
    iv$add_rule("ssf", sv_between(0, max_ssf(input$cit)))
    iv$add_rule("life", sv_between(0, max_life(input$year)))
    iv$add_rule("medical", sv_between(0, 20000))
    iv$add_rule("mexpense", sv_between(0, 5000))

    iv$enable()
  })

  observeEvent(input$submit, {
    data_in <- reactive({
      req(input$income != 0)
      income_tax(
        input$disab,
        input$sex,
        input$mstatus,
        input$income,
        input$year,
        input$cit,
        input$ssf,
        input$life,
        input$medical,
        input$mexpense
      )
    })

    data_in2 <- reactive({
      req(input$income != 0)

      total_taxable(
        input$disab,
        input$income,
        input$year,
        input$cit,
        input$ssf,
        input$life,
        input$medical
      )
    })

    output$table2 <- renderDataTable({
      req(c(nrow(data_in2()) != 0, input$submit))
      datatable(data_in2() ,
        colnames = "Calculation Details",
        # filter = "top",
        extensions = c("Scroller", "Responsive"),
        options = list(
          dom = "t",
          deferRender = TRUE,
          scrollY = 150,
          scroller = TRUE
        ), rownames = FALSE
      ) |> formatRound("val", digits=0)
    })

    output$table <- renderDataTable({
      req(c(nrow(data_in()) != 0, input$submit))
      datatable(data_in() ,
        colnames = c("Taxable Income", "Rates (%)", "Tax Liability"),
        # filter = "top",
        extensions = c("Scroller", "Responsive"),
        options = list(
          dom = "t",
          deferRender = TRUE,
          scrollY = 300,
          scroller = TRUE
        ), rownames = FALSE
      ) |> formatRound("tax_liability", digits=0)
    })
  })
}