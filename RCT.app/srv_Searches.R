# srv_Searches.R
# v1.0 - TomW - 10/18/16

# A note on chunking
#
# Every time a new filter, including no filters at all, is requested, begin by filtering the allHits table
#    into filteredHits, a vector of Rids. Then chunk filteredHits in a list, where each chunk has the Rids
#    for one page. Next, using the vector of Rids for the page we're on, get theseHits from allHits. Then
#    display all hits and set up the pagination.


#############
# Inits

rv$render_SearchNew <<- 0         # inc to re-render
gl$init_SearchNew <<- TRUE        # TRUE uses blanks in inputs, FALSE uses existing contents
rv$render_SearchList <<- 0        # inc to re-render
gl$init_SearchEdit <<- TRUE       # TRUE gets inputs from Sid, FALSE uses existing contents
gl$edit_this_Sid <<- "0"          # "0" for List view; an actual Sid gives editing view

#############
# Functions

   # builds standard sourceInfo table
buildS <- function(nrecs=1) {
   Sids = Riddle(nrecs)
   return(tibble(Sid = Sids, upSid="", status="", db="", hitsL1="", hitsL2="0",
                 SidName="", added_date=as.POSIXct(NA), processed_date=as.POSIXct(NA),
                 from_date=as.POSIXct(NA), to_date=as.POSIXct(NA), filename="", download_date=as.POSIXct(NA),
                 update_date=as.POSIXct(NA), terms="", query="", citeLevel="", comment=""))
}

   # formats a display table of a single search
formatSearch = function(Sid, i) {
   if(input$main_menu!="Searches") { return() }
   s = prj$sourceInfo[prj$sourceInfo$Sid==Sid,]
   pubmed_terms = if(s$db=="PubMed-Live") {
      paste0("<tr><td><b>Terms:</b> ", s$terms, "</td></tr>")
   }

   webofk_levels = if(s$db=="Web of Knowledge-CIW") {
      paste0("<tr><td><b>Keep citations at level:</b> ", s$citeLevel, "</td></tr>")
   }

   sel_file = if(s$db!="PubMed-Live") {
      paste0("<tr><td><b>Selected file:</b> ", s$filename, "</td></tr>",
             "<tr><td><b>File modification date</b> <i>(likely date obtained from database):</i> ", s$download_date, "</td></tr>")
   }

   buttons = switch(s$status,
                  "processed" = {
                     paste0("<tr><td><b>Date entered:</b> ", s$added_date, "</td></tr>",
                      "<tr><td><b>Date processed:</b> ", s$processed_date, "</td></tr>",
                      "<tr><td>",
                     actionButton(paste0("search_update", i), "Update Search", class="btn-warning btn-sm", style="display:inline-block;"),
                     "</td></tr>")
                   },
                  "updated" = {
                     paste0("<tr><td><b>Date entered:</b> ", s$added_date, "</td></tr>",
                      "<tr><td><b>Date processed:</b> ", s$processed_date, "</td></tr>",
                      "<tr><td><b>Date updated:</b> ", s$update_date, " <i>by Search ID ", s$upSid, "</i></td></tr>")
                  },
                  { # otherwise
                     paste0("<tr><td>",
                     actionButton(paste0("search_edit", i), "Edit Search", class="btn-warning btn-sm", style="display:inline-block;"),
                     actionButton(paste0("search_process", i), "Process", class="btn-primary btn-sm", style="display:inline-block; margin-left:5px;"),
                      "</td></tr>")
                  }
            )

   return(paste0("<tr><td><table>
           <tr><td><b>Search ID:</b> ", Sid, "</td></tr>
           <tr><td><b>Database:</b> ", s$db, "</td></tr>
           <tr><td><b>Search name:</b> ", s$SidName, "</td></tr>
           <tr><td><b>Date range of search:</b> ", s$from_date, " to ", s$to_date, "</td></tr>",
           pubmed_terms,
          "<tr><td><b>Query:</b> ", s$query, "</td></tr>",
           webofk_levels,
          "<tr><td><b>Hits :</b> ", as.numeric(s$hitsL1)+as.numeric(s$hitsL2), "</td></tr>",
          "<tr><td><b>Comment:</b> ", s$comment, "</td></tr>",
           sel_file,
           buttons,
          "</table><hr/></td></tr>"))
}

   # formats the exterior table of searches based on a vector of Sids
tableSearches = function(Sid_vec) {
   interior = ""
   for(i in seq_along(Sid_vec)) {
      interior = paste0(interior, formatSearch(Sid_vec[i], i))
   }
   return(paste0("<table>", interior, "</table>"))
}

#############
# Outputs

   # This is the output for the New Search tab and panel.
   #    The embedded outputs are reactive expressions/renders found below.

output$tab_SearchNew <- renderUI({
   x=rv$render_SearchNew             # reactivity
   if(gl$init_SearchNew) {           # set to TRUE at initialization and after a save
      if(is.null(input$new_search_db)) {
         gl$new_search_db <<- "PubMed-Live"
      } else {
         gl$new_search_db <<- input$new_search_db
      }
      gl$new_search_name <<- ""
      gl$new_search_from_date <<- "0001-01-01"
      gl$new_search_to_date <<- Sys.Date()
      gl$new_search_terms <<- ""
      gl$new_search_query <<- ""
      gl$new_search_citelevel <<- "Level 1 only"
      gl$new_search_filename <<- ""
      gl$new_pubmed_query <<- ""
      gl$new_search_comment <<- ""
      gl$error_msgs <<- ""
   } else {                           # in this case, it's a forced re-render
      isolate({                       # prevent additional re-renders
         gl$new_search_db <<- input$new_search_db
         gl$new_search_name <<- input$new_search_name
         gl$new_search_from_date <<- input$new_search_date[1]
         gl$new_search_to_date <<- input$new_search_date[2]
         gl$new_search_terms <<- input$new_search_terms
         gl$new_search_query <<- input$new_search_query
         gl$new_search_citeLevel <<- input$new_search_citeLevel
         # gl$new_search_filename <<- itself; set elsewhere
         # gl$new_pubmed_query <<-           ""
         gl$new_search_comment <<- input$new_search_comment
      })
   }
   error_msgs = {
      if(any(gl$error_msgs!="")) {
         errors=""
         for(error in gl$error_msgs) {
               errors = paste0(errors, '<h5 style="color:red">', error, '</h5>')
         }
         HTML(errors)
      }
   }
   search_qt = {
      if(gl$new_search_db=="PubMed-Live") {
         textareaInput(inputId="new_search_terms", label="Search terms:",
                       value=gl$new_search_terms, rows=3, width="100%")
      } else {
         textareaInput(inputId="new_search_query", label="Search query:",
                       value=gl$new_search_query, rows=3, width="100%")
      }
   }
   citeLevelRadios = {
      if(gl$new_search_db == "Web of Knowledge-CIW") {
         radioButtons("new_search_citeLevel", label="Citations to include:",
                      choices=list("Level 1 only", "Level 2 only", "Both"),
                      selected=gl$new_search_citeLevel, inline=TRUE)
      }
   }
   pubmed_response = {
      if(gl$new_search_db == "PubMed-Live" && gl$new_pubmed_query!="") {
         HTML("<p><b>Query translation:</b> ", gl$new_pubmed_query, "</p>",
              "<p><b>Hits:</b> ", gl$new_pubmed_hits, "</p>")
      }
   }
   search_buttons = {
      switch(gl$new_search_db,
         "PubMed-Live" = {
            div(
               actionButton("pubmed_test_new_search", "Test Search", class="btn-warning btn-sm", style="display:inline-block;"),
               actionButton("pubmed_save_new_search", "Save Search", class="btn-primary btn-sm", style="display:inline-block; margin-left:5px;")
            )
         },
         {  if(gl$new_search_filename=="") {
                btn_text = "Select File"
                file_text = ""
            } else {
                btn_text = "Edit File"
                file_text = HTML("<p><b>Selected file:</b> ", gl$new_search_filename, "</p>")
            }
            tagList(
               file_text,
               div(
                  actionButton("new_upload_file", btn_text, class="btn-warning btn-sm", style="display:inline-block;"),
                  actionButton("save_new_search", "Save Search", class="btn-primary btn-sm", style="display:inline-block; margin-left:5px;")
               )
            )
         }
      )
   }
   tagList(
      fluidRow(style="margin: 3px 0 0 0",
         column(12,
            styledPanel(
               panelTitle="New Search:",
               outputType="input",
               panelColor="blue",
                  fluidRow(style="margin: 3px 0 0 0",
                     column(2,
                        radioButtons(inputId="new_search_db", label="Database:", width="100%",
                           choices = c("PubMed-Live", "Cochrane-TXT", "Web of Knowledge-CIW",
                                       "Embase-CSV", "Ovid-RIS", "PMIDs-TXT"),
                           selected = gl$new_search_db)
                     ),
                     column(10,
                        error_msgs,
                        textInput(inputId="new_search_name", label="Name for this search:", value=gl$new_search_name),
                        dateRangeInput("new_search_date", label="Date range of search:",
                                       start=gl$new_search_from_date, end=gl$new_search_to_date),
                        search_qt,
                        citeLevelRadios,
                        textareaInput(inputId="new_search_comment", label="Comments:", value=gl$new_search_comment,
                                      rows=3, width="100%"),
                        pubmed_response,
                        search_buttons
                     )
                  )
            )
         )
      )
   )
})

## This is the code for listing the existing searches on one (paginated) page
re$search_list_body <<- reactive({
   x = rv$render_SearchList                           # needed to re-render after an edit
   if(as.numeric(gl$edit_this_Sid)>0) { return() }    # but skip it when starting an edit
   if(is.list(re$chunked_ids())) {                    # if empty, it's not a list
      return(HTML(tableSearches(re$chunked_ids()[[rv$activeChunk]])))  # here are the primary reactives
   } else {
      return('<h5 style="color:red">No searches yet...</h5>')
   }
})

output$search_list_filters = renderUI("")
output$search_list_pagination_top = renderUI(re$paginTop())    # <-- how to render a reactive()
output$search_list_buttons = renderUI("")
output$search_list_body = renderUI(re$search_list_body())
output$search_list_pagination_btm = renderUI(re$paginBtm())

   # This is the main output for the Search List tab, Search List panel
output$tab_SearchList <- renderUI({
   x=rv$render_SearchList                   # reactivity
   if(as.numeric(gl$edit_this_Sid)==0) {
#print("Render SearchList tab as list.")
      tagList(
         fluidRow(style="margin: 3px 0 0 0",
            column(12,
               styledPanel(
                  panelTitle="Search List:",
                  outputType="input",
                  panelColor="blue",
                     uiOutput("search_list_filters"),
                     uiOutput("search_list_pagination_top"),
                     uiOutput("search_list_buttons"),
                     uiOutput("search_list_body"),
                     uiOutput("search_list_pagination_btm")
               )
            )
         )
      )
   } else {
         # This is the output for editing a search.
      if(gl$init_SearchEdit) {                                     # set to TRUE at initialization and elsewhere
         s = prj$sourceInfo[prj$sourceInfo$Sid==gl$edit_this_Sid,] #    TRUE loads data from Sid to be edited
         gl$edit_search_db <<- s$db
         gl$edit_search_name <<- s$SidName
         gl$edit_search_from_date <<- s$from_date
         gl$edit_search_to_date <<- s$to_date
         gl$edit_search_terms <<- s$terms
         gl$edit_search_query <<- s$query
         gl$edit_search_citeLevel <<- s$citeLevel
         gl$edit_search_filename <<- s$filename
         gl$edit_search_comment <<- s$comment
         gl$edit_pubmed_query <<- s$query
         gl$edit_pubmed_hits <<- s$hitsL1
         gl$error_msgs <<- ""
      } else {                                                     #    FALSE for re-rendering with current data
         isolate({
            # gl$edit_search_db <<- input$edit_search_db  # (in this case, this input doesn't exist)
            gl$edit_search_name <<- input$edit_search_name
            gl$edit_search_from_date <<- input$edit_search_date[1]
            gl$edit_search_to_date <<- input$edit_search_date[2]
            gl$edit_search_terms <<- input$edit_search_terms
            gl$edit_search_query <<- input$edit_search_query
            gl$edit_search_citeLevel <<- input$edit_search_citeLevel
            # gl$edit_search_filename <<- itself; set elsewhere
            # gl$edit_pubmed_hits <<-         "
            gl$edit_search_comment <<- input$edit_search_comment
         })
      }
      error_msgs = {
         if(any(gl$error_msgs!="")) {
            errors=""
            for(error in gl$error_msgs) {
                  errors = paste0(errors, '<h5 style="color:red">', error, '</h5>')
            }
            HTML(errors)
         }
      }
      search_qt= {
         if(gl$edit_search_db=="PubMed-Live") {
            textareaInput(inputId="edit_search_terms", label="Search terms:", value=gl$edit_search_terms, rows=3, width="100%")
         } else {
            textareaInput(inputId="edit_search_query", label="Search query:", value=gl$edit_search_query, rows=3, width="100%")
         }
      }
      citeLevelRadios = {
         if(gl$edit_search_db == "Web of Knowledge-CIW") {
            radioButtons("edit_search_citeLevel", label="Citations to include:",
                         choices=list("Level 1 only", "Level 2 only", "Both"),
                         selected=gl$edit_search_citeLevel, inline=TRUE)
         }
      }
      pubmed_response = {
         if(gl$edit_search_db == "PubMed-Live" && gl$edit_pubmed_query!="") {
            HTML("<p><b>Query translation:</b> ", gl$edit_pubmed_query, "</p>",
                 "<p><b>Hits:</b> ", gl$edit_pubmed_hits, "</p>")
         }
      }
      search_buttons = {
         switch(gl$edit_search_db,
            "PubMed-Live" = {
               div(
                  actionButton("pubmed_test_edit_search", "Test Search", class="btn-warning btn-sm", style="display:inline-block;"),
                  actionButton("pubmed_save_edit_search", "Save Search", class="btn-primary btn-sm", style="display:inline-block; margin-left:5px;"),
                  actionButton("delete_edit_search", "Delete", class="btn-danger btn-sm", style="display:inline-block; margin-left:5px;")
               )
            },
            {  if(gl$edit_search_filename=="") {
                   btn_text = "Select File"
                   file_text = ""
               } else {
                   btn_text = "Edit File"
                   file_text = HTML("<p><b>Selected file:</b> ", gl$edit_search_filename, "</p>")
               }
               tagList(
                  file_text,
                  div(
                     actionButton("edit_upload_file", btn_text, class="btn-warning btn-sm", style="display:inline-block;"),
                     actionButton("save_edit_search", "Save Search", class="btn-primary btn-sm", style="display:inline-block; margin-left:5px;"),
                     actionButton("delete_edit_search", "Delete", class="btn-danger btn-sm", style="display:inline-block; margin-left:5px;")
                  )
               )
            }
         )
      }
      tagList(
         fluidRow(style="margin: 3px 0 0 0",
            column(12,
               styledPanel(
                  panelTitle="Edit Search:",
                  outputType="input",
                  panelColor="blue",
                     fluidRow(style="margin: 3px 0 0 0",
                        column(12,
                           error_msgs,
                           HTML("<p><b>Database:</b> ", gl$edit_search_db, "</p>"),
                           textInput(inputId="edit_search_name", label="Name for this search:", value=gl$edit_search_name),
                           dateRangeInput("edit_search_date", label="Date range of search:",
                                          start=gl$edit_search_from_date, end=gl$edit_search_to_date),
                           search_qt,
                           citeLevelRadios,
                           textareaInput(inputId="edit_search_comment", label="Comments:", value=gl$edit_search_comment,
                                         rows=3, width="100%"),
                           pubmed_response,
                           search_buttons
                        )
                     )
               )
            )
         )
      )
   }
})

# Search Analysis tab
output$tab_SearchAn <- renderUI(
   fluidRow(style="margin: 3px 0 0 0",
      column(12,
         styledPanel(outputId="",
            panelTitle="Search Analysis:",
            outputType="html",
            panelColor="blue",
               HTML("Coming soon...")
         )
      )
   )
)

#############
# Reactives

# Note: these reactives are used to build both search and citation lists with pagination

re$filtered_ids = reactive({                                 # pick the appropriate dataframe and filter
   x = input$save_new_search                                 # force reaction
   x = input$pubmed_save_new_search
   x = input$delete_edit_search
   if(input$main_menu=="Searches") {
      if(!is.data.frame(prj$sourceInfo)) { return("0") }
      Keepers = as.numeric(prj$sourceInfo$Sid)>0             # temp placeholder for more elaborate filters
      return(prj$sourceInfo$Sid[Keepers])
   }
   if(input$main_menu=="Stage 1 Review") {
      if(!is.data.frame(prj$hits)) { return(character(0)) }  # character(0) because this is what a filter that finds nothing returns
      Keepers = re$S() & re$D() & re$A() & re$J() & re$Y() & re$V() & re$N() & re$P() & re$W() & re$R() # an elaborate filter!
      return(prj$hits$Rid[Keepers])
   }
})

re$chunked_ids = reactive({
   if(length(re$filtered_ids())==0) {                        # happens at init and when filters result in nothing
      rv$activeChunk <<- 1                                   # this can't be 0, causes problems with [[0]]
      return("0")                                            # character rather than list signals no ids
   } else {
      chunks = chunker(re$filtered_ids(), as.numeric(rv$show_per_page))
      rv$activeChunk <<- min(length(chunks),rv$activeChunk)  # if a filter reduces the number of chunks
      return(chunks)                                         #    keep from running off right end
   }
})

re$show_per_page_options = reactive({
   if(input$main_menu=="Searches") { return(c("1", "5", "10"))}         # don't increase max without increasing
   if(input$main_menu=="Stage 1 Review") { return(c("10", "20", "40", "80"))}     #  observeEvents for selector button!!!
})

paginLinks = function(z) {                     # z is zero for top links and 7 for bottom links
   nChunks = length(re$chunked_ids())          # number of vectors in chunked list
   pchunks = ifelse(nChunks>5, 5, nChunks)     # number of vectors for pagination
   p = max(1, rv$activeChunk-2)                # p is the new pagination start, can't be less than 1
   p = min(p , max(1, nChunks-4))              #   but can't push the right end over the number of chunks, either
   gl$pageLeftChunk <<- p                      # used in observers to calculate a new rv$activeChunk
   middle_links=""
   for(i in 1:pchunks) {
      if(p==rv$activeChunk) {
         class = ' class="active"'
      } else {
         class = ''
      }
      middle_links = paste0(middle_links, paste0('<li', class, '><a id="page_link_', i+z, '" href="#" class="action-button">', p, '</a></li>\n'))
      p=p+1
   }
   return(middle_links)
}

re$show_per_page = reactive({
   sppo = ""
   for(i in re$show_per_page_options()) {
      if(i == rv$show_per_page) { s = ' selected' } else { s='' }
      sppo = paste0(sppo, '<option value="', i, '"', s, '>Show ', i, ' per page</option>\n')
   }
   return(sppo)
})

re$paginTop = reactive({
   tagList(fluidRow(
      column(5,
         HTML('<ul class="pagination">',
              '<li><a id="page_link_0" href="#" class="action-button">&laquo;</a></li>',
               paginLinks(0),
              '<li><a id="page_link_6" href="#" class="action-button">&raquo;</a></li>',
              '</ul>')),
      column(4, offset=3,
         HTML('<div class="form-group shiny-input-container" style="width: 70%; padding-top:2em">
                  <select class="form-control" id="show_per_page_top">',
                  re$show_per_page(),
                 '<script type="application/json" data-for="show_per_page" data-nonempty="">{}</script>
               </div>'))
   ))
})
# style="background-color: yellow;",
re$paginBtm = reactive({
   tagList(fluidRow(
      column(5,
         HTML('<ul class="pagination">',
              '<li><a id="page_link_7" href="#" class="action-button">&laquo;</a></li>',
               paginLinks(7),
              '<li><a id="page_link_13" href="#" class="action-button">&raquo;</a></li>',
              '</ul>')),
      column(4, offset=3,
         HTML('<div class="form-group shiny-input-container" style="width: 70%; padding-top:2em">
                  <select class="form-control" id="show_per_page_btm">',
                  re$show_per_page(),
                 '<script type="application/json" data-for="show_per_page" data-nonempty="">{}</script>
               </div>'))
   ))
})

#############
# Observers

   # function used by three test/save observers below; builds list of errors to display
new_search_error_check <- function() {
   errors=""
   if(s$name=="") { errors = c(errors, "Please provide a search name.")}
   if(is.na(input$search_date[1])) { errors = c(errors, "Please provide a search from date.")}
   if(is.na(input$search_date[2])) { errors = c(errors, "Please provide a search to date.")}
   return(errors)
}


##### Observers for New Search tab

   # database radio buttons
   observeEvent(input$new_search_db, {                  # database switch; save previously entered info
      rv$render_SearchNew <<- rv$render_SearchNew +1    # force re-render
      gl$init_SearchNew <<- FALSE                       # keep inputs
   })

   get_filename = function(previous_filename) {
      gl$error_msgs <<- ""                              # clear previous errors
      filename = choose.files(paste0(RCT_path, "Citations/citations"))
      if(length(filename)==0) {                         # trap a click on the choose.files cancel button (character(0))
         return(previous_filename)
      } else {
         return(filename)
      }
   }
   # file upload button
   observeEvent(input$new_upload_file, {
      gl$new_search_filename <<- get_filename(gl$new_search_filename)
      rv$render_SearchNew <<- rv$render_SearchNew +1    # force re-render
      gl$init_SearchNew <<- FALSE                       # keep existing inputs
   })
   observeEvent(input$edit_upload_file, {
      gl$edit_search_filename <<- get_filename(gl$edit_search_filename)
      rv$render_SearchList <<- rv$render_SearchList+1   # re-render the edit
      gl$init_SearchEdit <<- FALSE                      # keep existing inputs
   })

   # special function used during tests and saves
   error_check_on_save = function(status, kind) {
      errors=""
      if(status=="new") {
         if(input$new_search_name=="") { errors = c(errors, "Please provide a search name.")}
         if(is.na(input$new_search_date[1])) { errors = c(errors, "Please provide a search from date.")}
         if(is.na(input$new_search_date[2])) { errors = c(errors, "Please provide a search to date.")}
         if(kind=="pubmed") {
            if(input$new_search_terms=="") { errors = c(errors, "Please provide the search query.")}
         } else {
            if(input$new_search_query=="") { errors = c(errors, "Please provide the search query.")}
         }
      } else {
         if(input$edit_search_name=="") { errors = c(errors, "Please provide a search name.")}
         if(is.na(input$edit_search_date[1])) { errors = c(errors, "Please provide a search from date.")}
         if(is.na(input$edit_search_date[2])) { errors = c(errors, "Please provide a search to date.")}
         if(kind=="pubmed") {
            if(input$edit_search_terms=="") { errors = c(errors, "Please provide the search query.")}
         } else {
            if(input$edit_search_query=="") { errors = c(errors, "Please provide the search query.")}
         }
      }
      return(errors)
   }
   # special function used during saves
   save_search = function(status, kind) { # status is "new" or other; kind is "pubmed" or other
      errors = error_check_on_save(status, kind)
      if(kind!="pubmed") {
         if(status=="new") {
            if(gl$new_search_filename=="") { errors = c(errors, "Please select a file to upload.")}
         } else {
            if(gl$edit_search_filename=="") { errors = c(errors, "Please select a file to upload.")}
         }
      }
      if(any(errors!="")) {
         gl$error_msgs <<- errors
      } else {
         gl$error_msgs <<- ""
         s = buildS()
         if(kind=="pubmed") {
            if(status=="new") {
               terms = input$new_search_terms
               fromDate = input$new_search_date[1]
               toDate = input$new_search_date[2]
            } else {
               terms = input$edit_search_terms
               fromDate = input$edit_search_date[1]
               toDate = input$edit_search_date[2]
            }
            sr = pubmed_search(terms, max=1, from_date=fromDate, to_date=toDate )  # max=1; can't throw error
         } else {
            if(status=="new") {
               filename = gl$new_search_filename
            } else {
               filename = gl$edit_search_filename
            }
         }
         if(status=="new") {
            s$db =        gl$new_search_db
            s$SidName =   input$new_search_name
            s$from_date = input$new_search_date[1]
            s$to_date =   input$new_search_date[2]
            s$terms =     paste0("", input$new_search_terms)      # paste0 deals with possibly null input
            s$query =     paste0("", input$new_search_query)
            s$citeLevel = paste0("", input$new_search_citeLevel)
            s$comment =   input$new_search_comment
         } else {
            s$db =        gl$edit_search_db
            s$SidName =   input$edit_search_name
            s$from_date = input$edit_search_date[1]
            s$to_date =   input$edit_search_date[2]
            s$terms =     paste0("", input$edit_search_terms)
            s$query =     paste0("", input$edit_search_query)
            s$citeLevel = paste0("", input$edit_search_citeLevel)
            s$comment =   input$edit_search_comment
         }
         if(kind=="pubmed") {
            s$query =  sr$query
            s$hitsL1 = sr$search_hits
         } else {
            s$filename = filename
            s$download_date = file.info(filename)$mtime
         }
         s$added_date = Sys.time()
         s$status = status
         if(status=="new") {
            if(!is.data.frame(prj$sourceInfo)) {
               prj$sourceInfo <<- s
            } else {
               prj$sourceInfo <<- rbind(prj$sourceInfo, s)
            }
         } else {
            s$Sid = gl$edit_this_Sid                                     # editing, so restore the Sid...
            prj$sourceInfo[prj$sourceInfo$Sid==gl$edit_this_Sid,] <<- s
         }
         check_for_duplicate_filename(s$Sid)
         save_prj()
      }
   }

   test_search = function(status) {
      errors = error_check_on_save(status, "pubmed")
      if(any(errors!="")) {
         gl$error_msgs <<- errors
      } else {
         gl$error_msgs <<- ""
         if(status=="new") {
            terms = input$new_search_terms
            fromDate = input$new_search_date[1]
            toDate = input$new_search_date[2]
         } else {
            terms = input$edit_search_terms
            fromDate = input$edit_search_date[1]
            toDate = input$edit_search_date[2]
         }
         sr = pubmed_search(terms, max=1, from_date=fromDate, to_date=toDate )  # max=1, can't throw error
         if(status=="new") {
            gl$new_pubmed_query <<- sr$query
            gl$new_pubmed_hits <<- sr$search_hits
         } else {
            gl$edit_pubmed_query <<- sr$query
            gl$edit_pubmed_hits <<- sr$search_hits
         }
      }
   }

   # save a new search button
   observeEvent(input$save_new_search, {
      save_search("new", "standard")
      if(all(gl$error_msgs=="")) {                         # if no errors
         rv$render_SearchNew <<- rv$render_SearchNew+1     # re-render
         gl$init_SearchNew <<- TRUE                        # with blank inputs
      } else {
         rv$render_SearchNew <<- rv$render_SearchNew+1     # otherwise re-render
         gl$init_SearchNew <<- FALSE                       # with existing inputs (and gl$error_msgs!)
      }
   })
   # pubmed save a new search button
   observeEvent(input$pubmed_save_new_search, {
      save_search("new", "pubmed")
      if(all(gl$error_msgs=="")) {                         # if no errors
         rv$render_SearchNew <<- rv$render_SearchNew+1     # re-render
         gl$init_SearchNew <<- TRUE                        # with blank inputs
      } else {
         rv$render_SearchNew <<- rv$render_SearchNew+1     # otherwise re-render
         gl$init_SearchNew <<- FALSE                       # with existing inputs (and gl$error_msgs!)
      }
   })
   # pubmed test a new search button
   observeEvent(input$pubmed_test_new_search, {
      test_search("new")                                   # whether or not there are errors
      rv$render_SearchNew <<- rv$render_SearchNew+1        # re-render
      gl$init_SearchNew <<- FALSE                          # with existing inputs (and gl$error_msgs!)
   })


### Observers for Editing Searches
   # save an edited search button
   observeEvent(input$save_edit_search, {
      save_search("edited", "standard")
      if(all(gl$error_msgs=="")) {                         # if no errors
         rv$render_SearchList <<- rv$render_SearchList+1   # re-render
         gl$edit_this_Sid <<- "0"                          #    the list
         gl$init_SearchEdit <<- TRUE                       #    prep for next time
      } else {
         rv$render_SearchList <<- rv$render_SearchList+1   # otherwise re-render the edit
         gl$init_SearchEdit <<- FALSE                      # with existing inputs (and gl$error_msgs!)
      }
   })
   # pubmed save an edited search button
   observeEvent(input$pubmed_save_edit_search, {
      save_search("edited", "pubmed")
      if(all(gl$error_msgs=="")) {                         # if no errors
         rv$render_SearchList <<- rv$render_SearchList+1   # re-render
         gl$edit_this_Sid <<- "0"                          #    the list
         gl$init_SearchEdit <<- TRUE                       #    prep for next time
      } else {
         rv$render_SearchList <<- rv$render_SearchList+1   # otherwise re-render the edit
         gl$init_SearchEdit <<- FALSE                      # with existing inputs (and gl$error_msgs!)
      }
   })
   # pubmed test an edited search button
   observeEvent(input$pubmed_test_edit_search, {
      test_search("edited")                                # whether or not there are errors
      rv$render_SearchList <<- rv$render_SearchList+1      # re-render the edit
      gl$init_SearchEdit <<- FALSE                         # with existing inputs (and gl$error_msgs!)
   })
   # delete a search - part 1
   observeEvent(input$delete_edit_search, {
      i = which(prj$sourceInfo$upSid==gl$edit_this_Sid)    # is this an update of a search
      if(length(i)>0) {
         modal_text <<- "update"                # yes
      } else {
         modal_text <<- "search"                # nope, just an unprocessed search
      }
      showModal(modalDialog(
                title = paste0("Delete ", modal_text, "?"),
                paste0("Do you really want to delete this ", modal_text, "?"),
                footer = tagList(
                   modalButton("NO!"),
                   actionButton("ok_to_delete", "Yes")
                )
      ))
   })
   # delete a search - part 2
   observeEvent(input$ok_to_delete, {
      removeModal()
      i = which(prj$sourceInfo$upSid==gl$edit_this_Sid)    # if update, repair original Sid
      if(length(i)>0) {
         prj$sourceInfo[[i,"status"]] <<- "processed"
         prj$sourceInfo[[i,"update_date"]] <<- as.POSIXct(NA)
         prj$sourceInfo[[i,"upSid"]] <<- ""
      }
      prj$sourceInfo <<- prj$sourceInfo[-which(prj$sourceInfo$Sid==gl$edit_this_Sid),]
      save_prj()
      rv$render_SearchList <<- rv$render_SearchList+1      # re-render
      gl$edit_this_Sid <<- "0"                             #    the list
      gl$init_SearchEdit <<- TRUE                          #    prep for next time
   })

##### Observers for Search List tab

   # update (new time period) an existing search - 10 buttons
updateSearch = function(button_num) {
   Sid = re$chunked_ids()[[rv$activeChunk]][button_num]
   su=buildS()
   i = which(prj$sourceInfo$Sid==Sid)
   prj$sourceInfo[[i,"status"]] = "updated"
   prj$sourceInfo[[i,"update_date"]] = Sys.time()
   prj$sourceInfo[[i,"upSid"]] = su$Sid
   su$status = "new"
   su$db = prj$sourceInfo[[i,"db"]]
   su$SidName = prj$sourceInfo[[i,"SidName"]]
   su$from_date = prj$sourceInfo[[i,"to_date"]]
   su$to_date = Sys.Date()
   su$terms = prj$sourceInfo[[i,"terms"]]
   su$query = prj$sourceInfo[[i,"query"]]
   prj$sourceInfo <<- rbind(prj$sourceInfo, su)
   save_prj()
   gl$edit_this_Sid <<- su$Sid
   gl$init_SearchEdit <<- TRUE
   rv$render_SearchList <<- rv$render_SearchList+1
}
   observeEvent(input$search_update1, { updateSearch(1) })
   observeEvent(input$search_update2, { updateSearch(2) })
   observeEvent(input$search_update3, { updateSearch(3) })
   observeEvent(input$search_update4, { updateSearch(4) })
   observeEvent(input$search_update5, { updateSearch(5) })
   observeEvent(input$search_update6, { updateSearch(6) })
   observeEvent(input$search_update7, { updateSearch(7) })
   observeEvent(input$search_update8, { updateSearch(8) })
   observeEvent(input$search_update9, { updateSearch(9) })
   observeEvent(input$search_update10, { updateSearch(10) })

   # edit an exising search - 10 buttons
editSearch = function(button_num) {
   gl$edit_this_Sid <<- re$chunked_ids()[[rv$activeChunk]][button_num]
   rv$render_SearchList <<- rv$render_SearchList+1
}
   observeEvent(input$search_edit1, { editSearch(1) })
   observeEvent(input$search_edit2, { editSearch(2) })
   observeEvent(input$search_edit3, { editSearch(3) })
   observeEvent(input$search_edit4, { editSearch(4) })
   observeEvent(input$search_edit5, { editSearch(5) })
   observeEvent(input$search_edit6, { editSearch(6) })
   observeEvent(input$search_edit7, { editSearch(7) })
   observeEvent(input$search_edit8, { editSearch(8) })
   observeEvent(input$search_edit9, { editSearch(9) })
   observeEvent(input$search_edit10, { editSearch(10) })

   # process an existing search - 10 buttons
begin_processing = function(button_num) {
   gl$process_Sid <<- re$chunked_ids()[[rv$activeChunk]][button_num]  # send Sid to an observer in srv_Uploads.R
   rv$process_Sid <<- rv$process_Sid+1
   rv$render_SearchList <<- rv$render_SearchList+1      # re-render
}
   observeEvent(input$search_process1, { begin_processing(1) })
   observeEvent(input$search_process2, { begin_processing(2) })
   observeEvent(input$search_process3, { begin_processing(3) })
   observeEvent(input$search_process4, { begin_processing(4) })
   observeEvent(input$search_process5, { begin_processing(5) })
   observeEvent(input$search_process6, { begin_processing(6) })
   observeEvent(input$search_process7, { begin_processing(7) })
   observeEvent(input$search_process8, { begin_processing(8) })
   observeEvent(input$search_process9, { begin_processing(9) })
   observeEvent(input$search_process10, { begin_processing(10) })


##### These observers are related to pagination

   # page buttons (14)
   observeEvent(input$page_link_0, { rv$activeChunk <<- 1 })
   observeEvent(input$page_link_1, { rv$activeChunk <<- gl$pageLeftChunk })
   observeEvent(input$page_link_2, { rv$activeChunk <<- gl$pageLeftChunk + 1 })
   observeEvent(input$page_link_3, { rv$activeChunk <<- gl$pageLeftChunk + 2 })
   observeEvent(input$page_link_4, { rv$activeChunk <<- gl$pageLeftChunk + 3 })
   observeEvent(input$page_link_5, { rv$activeChunk <<- gl$pageLeftChunk + 4 })
   observeEvent(input$page_link_6, { rv$activeChunk <<- length(re$chunked_ids()) })
   observeEvent(input$page_link_7, { rv$activeChunk <<- 1 })
   observeEvent(input$page_link_8, { rv$activeChunk <<- gl$pageLeftChunk })
   observeEvent(input$page_link_9, { rv$activeChunk <<- gl$pageLeftChunk + 1 })
   observeEvent(input$page_link_10, { rv$activeChunk <<- gl$pageLeftChunk + 2 })
   observeEvent(input$page_link_11, { rv$activeChunk <<- gl$pageLeftChunk + 3 })
   observeEvent(input$page_link_12, { rv$activeChunk <<- gl$pageLeftChunk + 4 })
   observeEvent(input$page_link_13, { rv$activeChunk <<- length(re$chunked_ids()) })

   # DEBUGGING
   observeEvent(rv$activeChunk, {print(paste0("rv$activeChunk=",rv$activeChunk))})

   # show per page selectors (2)
   observeEvent(input$show_per_page_top, {
      rv$show_per_page <<- input$show_per_page_top
      if(input$main_menu=="Searches") {gl$search_spp <<- rv$show_per_page}      # reset in observer looking for
      if(input$main_menu=="Stage 1 Review") {gl$cites_spp <<- rv$show_per_page} #   menu tab switches
      rv$activeChunk <<- 1                       # restarts at chunk 1; seems complicated to improve
   })
   observeEvent(input$show_per_page_btm, {
      rv$show_per_page <<- input$show_per_page_btm
      if(input$main_menu=="Searches") {gl$search_spp <<- rv$show_per_page}
      if(input$main_menu=="Stage 1 Review") {gl$cites_spp <<- rv$show_per_page}
      rv$activeChunk <<- 1                       # restarts at chunk 1; seems complicated to improve
   })

# for debugging
# observeEvent(rv$activeChunk,
#    print(paste0("rv$activeChunk just changed to: ", rv$activeChunk))
# )
