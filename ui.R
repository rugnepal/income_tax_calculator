
# Define UI for application
fluidPage(

  # Application title
  h2("Personal Income Tax - Nepal"),
  h4(""),
  br(),
  br(),
  meta() %>%
    meta_social(
      title = "Personal Income Tax - Nepal",
      description = "Personal Income Tax calculator helps you know how much tax you pay and how much you save.",
      url = "https://bjung.shinyapps.io/",
      image = "https://bsmedia.business-standard.com/_media/bs/img/about-page/1562037516.jpg",
      image_alt = "Personal Income Tax - Nepal",
      twitter_creator = "@bjungbogati",
      twitter_card_type = "summary",
      twitter_site = "@bjungbogati"
    ),

  # sidebar ui
  sidebarPanel(
    fluidRow(
      column(
        6,
        checkboxInput("sex", "Female?", value = FALSE, width = NULL),
      ),
      column(
        6,
        checkboxInput("disab", "Disabled?", value = FALSE, width = NULL),
      ),
      column(
        6,
        checkboxInput("mstatus", "Couple?", value = FALSE, width = NULL),
      ),
      column(
        6,
        checkboxInput("flancer", "Freelancer?", value = FALSE, width = NULL),
      ),
      column(
        6,
        numericInput("income", "Total Annual Income*", value = 0, min = 0, max = Inf)
      ),
      column(
        6,
        selectizeInput("year", "Fiscal Year", c("2078/79", "2079/80")),
      ),
      column(
        6,
        numericInput("cit", "CIT/EPF Contribution",
          value = 0, min = 0,
          max = 300000
        )
      ),
      column(
        6,
        numericInput("ssf", "SSF Contribution",
          value = 0, min = 0,
          max = 500000
        )
      ),
      column(
        6,
        numericInput("life", "Life Contribution",
          value = 0, min = 0,
          max = 25000
        )
      ),
      column(
        6,
        numericInput("medical", "Medical Contribution",
          value = 0, min = 0,
          max = 20000
        )
      ),
      column(
        12,
        numericInput("mexpense", "Annual Medical Expense",
          value = 0, min = 0,
          max = 20000
        )
      ),
      column(
        6,
        actionButton("submit", "Calculate")
      )
    ),
    br(),
    p("* Including Bonus, Allowances or Any special benefits"),
    p("Disclaimer: Only general cases for Income tax calculation are included, 
        so exact information may differ"),
    br(), br(), br(),
    tags$a(
      href = "https://github.com/rugnepal/income_tax_calculator",
      "Source code: GitHub"
    )
  ),


  # main table
  mainPanel(
    dataTableOutput("table2"),
    dataTableOutput("table"),
    hr(),
    # tags$body(style = "overflow-y:hidden;height:100%;"),
    img(
      src = "https://bsmedia.business-standard.com/_media/bs/img/about-page/1562037516.jpg",
      align = "left", style = "position: absolute; opacity: 0.4; width:85%;"
    )
  )
)
