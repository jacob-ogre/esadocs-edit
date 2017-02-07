# BSD_2_clause

shinyServer(function(input, output, session) {

  cur_doc <- reactive({
    input$doc_id
  })

  inp_title <- reactive({
    input$in_title
  })

  got_doc <- reactive({
    if(nchar(cur_doc()) > 15 & nchar(cur_doc()) < 35) {
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
           ifelse(!is.null(got_dat()$species),
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

  output$cur_link <- renderText({
    paste0("Original URL (current: ",
           ifelse(is.null(got_dat()$link),
                  "NA",
                  got_dat()$link),
           ")")
  })

  get_value <- function(inpt, var) {
    if(inpt != "") return(inpt)
    if(inpt == "" & !is.null(got_dat()[[var]])) {
      return(got_dat()[[var]])
    }
    if(inpt == "" & is.null(got_dat()[[var]])) {
      return("")
    }
  }

  submit_changes <- function() {
    prep_agency <- ifelse(input$in_fed != "",
                          strsplit(input$in_fed, split = "; "),
                          NA)
    prep_species <- ifelse(input$in_species != "",
                          strsplit(input$in_species, split = "; "),
                          NA)
    prep_geo <- ifelse(!is.na(input$in_geo) & input$in_geo != "",
                       strsplit(input$in_geo, split = "; "),
                       NA)
    prep_tags <- ifelse(input$in_tags != "",
                        strsplit(input$in_tags, split = "; "),
                        NA)

    result <- docs_update(
      index = "esadocs",
      type = got_doc()$`_type`,
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
                                  prep_agency,
                                  got_dat()$federal_agency),
          activity_code = ifelse(input$in_actcode != "" |
                                   is.null(got_dat()$activity_code),
                                 input$in_actcode,
                                 got_dat()$activity_code),
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
                       got_dat()$geo),
          tags = ifelse(!is.na(prep_tags) | is.null(got_dat()$tags),
                        prep_tags,
                        got_dat()$tags),
          link = ifelse(input$in_orig_url == "" |
                          is.null(got_dat()$link),
                        input$in_orig_url,
                        got_dat()$link)
        )
      )
    )
    dater <- NA
    if(input$in_date != "") {
      if(input$in_date == "NA") {
        to_add_date <- NA
      } else {
        isdate <- try(as.Date(input$in_date))
        if(class(isdate) != "try-error") {
          to_add_date <- input$in_date
        } else {
          stop(paste(input$in_date, "is not a date."))
        }
      }
      dater <- docs_update(
        index = "esadocs",
        type = got_doc()$`_type`,
        id = cur_doc(),
        body = list( doc = list( date = to_add_date ) )
      )$result
    }
    return(list(main_res = result$result, date_res = dater))
  }

  validate_data <- function() {
    if(input$in_date != "") {
      is_date <- try(as.Date(input$in_date))
      if(class(is_date) != "try-error") return(TRUE)
      return(FALSE)
    }
    return(TRUE)
  }

  # Submit modal
  observeEvent(input$submit, {
    if(validate_data()) {
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
    } else {
      showModal(modalDialog(
        title = HTML("<h3>Error!</h3>"),
        HTML("<p style='font-size:large'>The date is not properly formatted.
              Please use the international standard: <b>YYYY-MM-DD</b>.</p>"),
        HTML("<p style='font-size:larger'>For example, July 1st, 2013 would
             be 2013-07-01.</p>"),
        size = "m",
        footer = actionButton(
          "cancel_submit",
          label = "OK",
          style = "background-color: #F44336; color: white"
        )
      ))
    }
  })

  observeEvent(input$cancel_submit, {
    removeModal()
  })

  log_changes <- function() {
    origins <- c(got_dat()$title, got_dat()$date, got_dat()$n_pages,
                 got_dat()$federal_agency, got_dat()$activity_code,
                 got_dat()$fr_citation_page, got_dat()$ch_status,
                 got_dat()$doc_type,
                 paste(unlist(got_dat()$spp_tmp), collapse = "; "),
                 paste(unlist(got_dat()$geo), collapse = "; "),
                 paste(unlist(got_dat()$tags), collapse = "; "))
    changes <- c(input$in_title, input$in_date, input$in_npages,
                 input$in_fed, input$in_actcode,
                 input$in_frpage, input$in_chstatus,
                 input$in_misc_doctype, input$in_species,
                 input$in_geo, input$in_tags)
    to_write <- str_c(cur_doc(),
                      Sys.time(),
                      str_c(origins, collapse = "\t"),
                      str_c(changes, collapse = "\t"),
                      sep = "\t")
    cmd <- str_c("echo '", to_write, "' >> ",
                 "/home/jacobmalcom/Data/ESAdocs/changes.tsv",
                 # "/Users/jacobmalcom/Work/Data/esadocs/changes.tsv",
                 sep = "")
    system(cmd, intern = FALSE)
  }

  # Submit for real and remove modal
  observeEvent(input$real_submit, {
    if(input$key_code == Sys.getenv("ESADOC_KEY")) {
      changes <- submit_changes()
      fields <- c("doc_id", "in_title", "in_date", "in_npages",
                  "in_fed", "in_actcode", "in_frpage", "in_chstatus",
                  "in_misc_doctype", "in_species", "in_geo", "in_tags")
      res <- lapply(fields, updateTextInput, session = session, value = "")

      removeModal()
      removeClass(id = "key_code", "attention")
      OKS <- c("noop", "updated")
      if(changes$main_res %in% OKS | is.na(changes$main_res)) {
        shinyBS::createAlert(
          session,
          anchorId = "success_note",
          content = paste("Record for", cur_doc(), "updated!"),
          style = "success",
          append = FALSE
        )
        log_changes()
      } else {
        shinyBS::createAlert(
          session,
          anchorId = "success_note",
          content = paste("Record update for", cur_doc(), "failed!"),
          style = "error",
          append = FALSE
        )
      }
    } else {
      showModal(modalDialog(
        title = HTML("<h3>Key required</h3>"),
        HTML("<p style='font-size:large'>Enter the current key found in the
             shared GDrive folder.</p>"),
        size = "m",
        footer = actionButton(
          "cancel_submit",
          label = "OK",
          style = "background-color: #F44336; color: white"
        )
      ))
      addClass(id = "key_code", "attention")
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
      title = "Edit ESAdocs data",
      HTML("
        <div class='help_dialog'>
          <hr>
          <h4>Caution: With great power comes great responsibility</h4>
          <span style='font-size:larger'>
            <p>This app is a convenience tool to add or update data in the
               ESAdocs database. For now, there is no round of review...once
               an edit is submitted, the elasticsearch database is changed!
               There's no need to worry about this, but be aware.</p>
          <hr>
          <h4>Get a document ID</h4>
          <ol>
            <li>Use <a href='https://esadocs.cci-dev.org' target='_blank'>ESAdocs Search</a>
            to find documents that need to (or can have) their data updated.</li>
            <li>Each document will hit will include the `Document ID` at the
            bottom of the hit. Copy that ID.</li>
          </ol>
          <img src='search_and_edit.png' width='100%'>
          <hr>
          <h4>Edit the data</h4>
          <ol>
            <li> Paste the copied ID string in the `Document ID` input at the
            top of this page.</li>
            <li>The app will find all editable data fields and update the labels
            to show the current data.</li>
            <li>Enter any changes in each field's text box, such as the activity
            code for a section 7 consultation.</li>
            <li>Make sure you enter the current key.</li>
            <li>Once done, click `Submit` to have the changes made to the database.</li>
            <ul>
              <li>You may also cancel the changes; all fields will be cleared.</li>
            </ul>
          </ol>
          <img src='edit_example.png' width='100%'>
          <hr>
          <h4>Submit the changes</h4>
          <ol>
            <li>Carefully check over the proposed changes.</li>
            <li>If a field needs more editing, click 'No'.</li>
            <li>If everything is good, click 'Yes'.</li>
          </ol>
          <img src='changes_submit.png' width='50%'>
          </span>
        </div>
      "),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

})
