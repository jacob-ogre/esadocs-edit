# BSD_2_clause

shinyServer(function(input, output, session) {

  cur_doc <- reactive({
    input$doc_id
  })

  inp_title <- reactive({
    input$in_title
  })

  got_doc <- reactive({
    if(nchar(cur_doc()) > 15 & nchar(cur_doc()) < 25) {
      res <- try(
        docs_get(
          "esadocs",
          type = "_all",
          id = cur_doc()
        )
      )
      if(class(res) != "try-error") {
        hide("nohit")
        return(res)
      } else {
        show("nohit")
        return(NULL)
      }
    } else {
      return(NULL)
    }
  })

  got_dat <- reactive({
    got_doc()$`_source`
  })

  output$cur_example <- renderText({
    paste0("Example variable (current: 'current value, may be empty')")
  })

  output$cur_title <- renderText({
    paste0("Title (current: ",
           ifelse(is.null(got_dat()$title),
                  "NA",
                  got_dat()$title),
           ")")
  })

  output$cur_date <- renderText({
    paste0("Date (current: ",
           ifelse(is.null(got_dat()$date),
                  "NA",
                  got_dat()$date),
           ")")
  })

  output$cur_npages <- renderText({
    paste0("# pages (current: ",
           ifelse(is.null(got_dat()$n_pages),
                  "NA",
                  got_dat()$n_pages),
           ")")
  })

  output$cur_frpage <- renderText({
    paste0("FR citation (current: ",
           ifelse(is.null(got_dat()$fr_citation_page),
                  "NA",
                  got_dat()$fr_citation_page),
           ")")
  })

  output$cur_fed <- renderText({
    paste0("Federal agency(ies) (current: ",
           ifelse(is.null(got_dat()$federal_agency),
                  "NA",
                  got_dat()$federal_agency),
           ")")
  })

  output$cur_actcode <- renderText({
    paste0("Activity code (current: ",
           ifelse(is.null(got_dat()$activity_code),
                  "NA",
                  got_dat()$activity_code),
           ")")
  })

  output$cur_chstatus <- renderText({
    paste0("Crit. hab. status (current: ",
           ifelse(is.null(got_dat()$ch_status),
                  "NA",
                  got_dat()$ch_status),
           ")")
  })

  output$cur_misc_type <- renderText({
    paste0("Misc. doc. type (current: ",
           ifelse(is.null(got_dat()$doc_type),
                  "NA",
                  got_dat()$doc_type),
           ")")
  })

  output$cur_species <- renderText({
    res <- paste0("Species (current: ",
           ifelse(!is.null(got_dat()$species) & got_dat()$species != "",
                  paste(unlist(got_dat()$species),
                        collapse = "; "),
                  "NA"),
           ")")
    return(unique(res))
  })

  output$cur_geo <- renderText({
    res <- paste0("Geo-tags (current: ",
           ifelse(is.null(got_dat()$geo),
                  "NA",
                  paste(unlist(got_dat()$geo),
                        collapse = "; ")),
           ")")
    return(unique(res))
  })

  output$cur_tags <- renderText({
    res <- paste0("Tags (current: ",
           ifelse(is.null(got_dat()$tags),
                  "NA",
                  paste(unlist(got_dat()$tags),
                        collapse = "; ")),
           ")")
    return(unique(res))
  })

  get_value <- function(inpt, var) {
    if(inpt != "") return(inpt)
    if(inpt == "" & !is.null(got_dat()[[var]])) return(got_dat()[[var]])
    if(inpt == "" & is.null(got_dat()[[var]])) {
      return("")
    }
    observe(print(c("in_getval", inpt, var)))
  }

  submit_changes <- function() {
    prep_species <- ifelse(input$in_species != "",
                          strsplit(input$in_species, split = "; "),
                          NA)
    prep_geo <- ifelse(input$in_geo != "",
                       strsplit(input$in_geo, split = "; "),
                       NA)
    prep_tags <- ifelse(input$in_tags != "",
                        strsplit(input$in_tags, split = "; "),
                        NA)
    # observe(print(got_dat()$date))
    result <- docs_update(
      index = "esadocs",
      type = got_dat()$type,
      id = cur_doc(),
      body = list(
        doc = list(
          title = get_value(input$in_title, "title"),
          n_pages = ifelse(input$in_npages != "" | is.null(got_dat()$n_pages),
                           as.numeric(input$in_npages),
                           as.numeric(got_dat()$n_pages)),
          fr_citation_page = ifelse(input$in_frpage != "" |
                                      is.null(got_dat()$fr_citation_page),
                                    input$in_frpage,
                                    got_dat()$fr_citation_page),
          federal_agency = ifelse(input$in_fed != "" |
                                    is.null(got_dat()$federal_agency),
                                  input$in_fed,
                                  got_dat()$federal_agency),
          activity_code = ifelse(input$in_actcode != "" |
                                   is.null(got_dat()$activity_code),
                                 input$in_actcode,
                                 got_dat()$federal_agency),
          ch_status = ifelse(!is.na(input$in_chstatus) |
                               is.null(got_dat()$ch_status),
                             input$in_chstatus,
                             got_dat()$ch_status),
          doc_type = ifelse(input$in_misc_doctype != "" |
                              is.null(got_dat()$doc_type),
                            input$in_misc_doctype,
                            got_dat()$doc_type),
          species = ifelse(!is.na(prep_species) | is.null(got_dat()$species),
                           prep_species,
                           got_dat()$species),
          geo = ifelse(!is.na(prep_geo) | is.null(got_dat()$geo),
                       prep_geo,
                       c("")),
          tags = ifelse(!is.na(prep_tags) | is.null(got_dat()$tags),
                        prep_tags,
                        got_dat()$tags)
        )
      )
    )
    dater <- NA
    if(input$in_date != "") {
      if(input$in_date == "NA") {
        to_add_date <- NA
      } else {
        to_add_date <- input$in_date
      }
      dater <- docs_update(
        index = "esadocs",
        type = got_dat()$type,
        id = cur_doc(),
        body = list( doc = list( date = to_add_date ) )
      )$result
    }
    return(list(main_res = result$result, date_res = dater))
  }

  # Submit modal
  observeEvent(input$submit, {
    showModal(modalDialog(
      title = HTML("<h3>Submit</h3>"),
      HTML("<h4>Submit these changes?</h4>"),
      HTML(
        paste(
          c("<div style='font-size:large; padding-left:15px'>",
            paste("<b>Title</b>:", input$in_title),
            paste("<b>Date</b>:", input$in_date),
            paste("<b># pages</b>:", input$in_npages),
            paste("<b>FR citation</b>:", input$in_frpage),
            paste("<b>Federal agency</b>:", input$in_fed),
            paste("<b>Activity code</b>:", input$in_actcode),
            paste("<b>CH status</b>:", input$in_chstatus),
            paste("<b>Misc. doc type</b>:", input$in_misc_doctype),
            paste("<b>Species</b>:", input$in_species),
            paste("<b>Geo-tags</b>:", input$in_geo),
            paste("<b>Tags</b>:", input$in_tags),
            "</div>"
          ),
          collapse = "<br>")
      ),
      size = "m",
      footer = tagList(
        actionButton(
          "cancel_submit",
          label = "No",
          style = "background-color: #F44336; color: white"),
        actionButton(
          "real_submit",
          label = "Yes",
          style = "background-color: #304FFE; color: white")
      )
    ))
  })

  observeEvent(input$cancel_submit, {
    removeModal()
  })

  # Submit for real and remove modal
  observeEvent(input$real_submit, {
    changes <- submit_changes()
    observe(print(changes))
    updateTextInput(session,
                    "doc_id",
                    value = "")
    updateTextInput(session,
                    "in_title",
                    value = "")
    updateTextInput(session,
                    "in_date",
                    value = "")
    removeModal()
    OKS <- c("noop", "updated")
    if(changes$main_res %in% OKS | is.na(changes$main_res)) {
      observe(print("OK!"))
    } else {
      obeserve(print("WTF?!"))
    }
  })


  # Cancel modal
  observeEvent(input$cancel, {
    showModal(modalDialog(
      title = "Cancel",
      "Cancel edits of this record?",
      size = "s",
      footer = tagList(
        actionButton(
          "cancel_cancel",
          label = "No",
          style = "background-color: #F44336; color: white"
        ),
        actionButton(
          "real_cancel",
          label = "Yes",
          style = "background-color: #304FFE; color: white"
        )
      )
    ))
  })

  # Cancel for real and remove cancel modal
  observeEvent(input$real_cancel, {
    updateTextInput(session,
                    "doc_id",
                    value = "")
    removeModal()
  })

  # Cancel the cancel
  observeEvent(input$cancel_cancel, {
    removeModal()
  })

  # Help modal
  observeEvent(input$help, {
    showModal(modalDialog(
      title = "Help",
      HTML("
        <div class='help_dialog'>
          <h3>Edit ESAdocs data</h3>
          <hr>
          <h4>Caution: With great power comes great responsibility</h4>
          <span style='font-size:larger'>
            <p>This app is a convenience tool to add or update data in the
               ESAdocs database. For now, there is no round of review...once
               an edit is submitted, the elasticsearch database is changed!
               There's no need to worry about this, but be aware.</p>
          <hr>
          <p>Use <a href='https://esadocs.cci-dev.org' target='_blank'>ESAdocs Search</a>
            to find documents that need to (or can have) their data updated. Each
             document will hit will include the `Document ID` at the bottom of
             the hit. Copy that ID and paste it in the `Document ID` input on
             this page. The app will find all editable data fields and update
             the labels to show the current data. Enter any changes in each
             field's text box. Once done, hit `Submit` to have the changes made
             to the database.</p>
          </span>
        </div>
      "),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

})
