# BSD_2_clause

#############################################################################
# Define the header and sidebar (disabled)
header <- dashboardHeader(disable = TRUE)
sidebar <- dashboardSidebar(disable = TRUE)

#############################################################################
# Define the page(s) with dashboardBody
body <- dashboardBody(fluidPage(
  theme = shinythemes::shinytheme("paper"),
  div(class = "outer",
    shinyjs::useShinyjs(),
    # tags$style(appCSS),
    tags$head(
      HTML("<link href='https://fonts.googleapis.com/css?family=Open+Sans:300,400'
           rel='stylesheet' type='text/css'>"),
      HTML('<link rel="icon" type="image/png" href="favicon-32x32.png" sizes="32x32" />'),
      tags$style(HTML(readLines("www/custom-style.css")))
    ),

    br(),
    fluidRow(
      column(1,
        tags$a(href="https://esadocs.cci-dev.org/esadocs-edit/",
               img(src = "ESAdocs_edit.svg",
                   height = "80px")
             )
      ),
      column(10,
        fluidRow(
          column(2),
          column(4,
            textInput(
              "doc_id",
              label = "Document ID",
              width = "110%",
              placeholder = "From ESAdocs Search"
            )
          ),
          column(6,
            hidden(
              p(class = "warnbadge", id = "nohit", "No Hits")
            ),
            shinyBS::bsAlert("success_note")
          )
        ),
        fluidRow(hr(class="style-four")),
        # fluidRow(),
        fluidRow(
          br(),
          column(4,
            textInput(
              inputId = "in_title",
              label = textOutput("cur_title"),
              width = "110%",
              value = NA,
              placeholder = "new (text)"
            )
          ),
          column(4,
            textInput(
              inputId = "in_date",
              label = textOutput("cur_date"),
              width = "110%",
              value = NA,
              placeholder = "new (YYYY-MM-DD)"
            )
          ),
          column(4,
            textInput(
              inputId = "in_npages",
              label = textOutput("cur_npages"),
              width = "110%",
              value = NA,
              placeholder = "new (numeric)"
            )
          )
        ),
        fluidRow(
          br(),
          column(4,
            textInput(
              inputId = "in_frpage",
              label = textOutput("cur_frpage"),
              width = "110%",
              value = NA,
              placeholder = "new (text, FR citation)"
            )
          ),
          column(4,
            textInput(
              inputId = "in_fed",
              label = textOutput("cur_fed"),
              width = "110%",
              value = NA,
              placeholder = "new (text, semicolon sep. list)"
            )
          ),
          column(4,
            textInput(
              inputId = "in_actcode",
              label = textOutput("cur_actcode"),
              width = "110%",
              placeholder = "new (text, alpha-numeric)"
            )
          )
        ),
        fluidRow(
          br(),
          column(4,
            textInput(
              inputId = "in_chstatus",
              label = textOutput("cur_chstatus"),
              width = "110%",
              placeholder = "new (text)"
            )
          ),
          column(4,
            textInput(
              inputId = "in_misc_doctype",
              label = textOutput("cur_misc_type"),
              width = "110%",
              placeholder = "new (text)"
            )
          ),
          column(4,
            textInput(
              inputId = "in_species",
              label = textOutput("cur_species"),
              width = "110%",
              value = NA,
              placeholder = "new (text, semicolon sep. list)"
            )
          )
        ),
        fluidRow(
          br(),
          column(4,
            textInput(
              inputId = "in_geo",
              label = textOutput("cur_geo"),
              width = "110%",
              value = NA,
              placeholder = "new (text, semicolon sep. list)"
            )
          ),
          column(4,
            textInput(
              inputId = "in_tags",
              label = textOutput("cur_tags"),
              width = "110%",
              value = NA,
              placeholder = "new (text, semicolon sep. list)"
            )
          ),
          column(4,
            textInput(
              inputId = "key_code",
              label = "Current key",
              width = "110%",
              value = NA,
              placeholder = "alpha-numeric"
            )
          ),
          column(10),
          column(1,
            br(), br(),
            actionButton(
              "cancel",
              label = "Cancel",
              style = "background-color: #F44336; color: white"
            )
          ),
          column(1,
            br(), br(),
            actionButton(
              "submit",
              label = "Submit",
              style = "background-color: #304FFE; color: white"
            )
          )
        ),
        fluidRow(
          br(), br()
        )
      ),

      # This is still at the top of the page, just far-right...
      column(1,
        br(),
        actionButton("help",
                     "Help",
                     icon = icon("question-circle"))
      )
    ),
    fluidRow(
      column(1),
      column(10,
        div(
          style = "text-align:center",
          hr(),
          HTML('<footer>
              <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">
              <img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a>
              <br />
              This <span xmlns:dct="http://purl.org/dc/terms/" href="http://purl.org/dc/dcmitype/InteractiveResource" rel="dct:type">work</span>
              by <a xmlns:cc="http://creativecommons.org/ns" href="http://defenders.org" property="cc:attributionName" rel="cc:attributionURL">Defenders of Wildlife</a>
              is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>.
            </footer>'),
          hr(),
          br()
        )
      ),
      column(1)
    )
  )
))

body
