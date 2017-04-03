# srv_S1Review.R
# v1.0 - TomW - 10/31/16

# This file handles the "Stage 1 Review" tab

#############
# Inits

rv$render_HitList <<- 0    # to force re-render when reviewHit doesn't change
rv$reviewRid <<- 0         # 0 gives list; >0 (the location in the active chunk), gives review

#############
# Functions

### Functions for making the single citation view...

   # This formats the data for a single citation
#  First check if this Rid is a confirmed duplicate, if so, skip.
#     This step also fills in $dupOk on duplicates of this citation.
#   If pmidOK show PubMed Data for review
#   If PMID exists and is unconfirmed, show both original and Pubmed Data, ask for confirmation before review
#       If confirmed and no reviews, go to PMID exists and is confirmed.
#       If confirmed and reviews exist, go to next with dialog (and DupOf()).
#       If wrong, make it blank and go to PMID is blank
#   If PMID is blank
#       Allow PMID entry; then goto PMID exists and is unconfirmed
#       If confirmed no PMID, check for duplicates among other no PMID citations.

showCite = function(Rid) {
   r <- prj$hits[prj$hits$Rid==Rid,]   # get data for this Rid
   if(r$dupOf!="" || dupOf(r$Rid, r$pmid)) {       # if it's a duplicate, move along...
      gl$direction()                               #    ...nothing to see here
      return("")
   }
   if(r$pmid=="") {       # Used where there's no pmid
      if(r$pmidOK) {      # If we have confirmation that PMID doesn't exist, skip the input and buttons
         buttons = ""
      } else {
         buttons = HTML('<div style="margin:0 auto; width:270px;">
                           <div style="display:inline-block; align:center">
                              <p style="text-align:center;"><input id="pmidNew" type="text" class="input-small" style="display:inline; width:8em" value=""/></p>
                              <button class="btn action-button btn-success btn-sm" id="pmidSubmit" type="button">Submit PMID</button>
                              &nbsp
                              <button class="btn action-button btn-danger btn-sm" id="pmidFail" type="button">No PMID Exists</button>
                        </div></div>')
      }
      return(tagList(
         fluidRow(
            column(12,
               buttons
            )
         ),
         fluidRow(style="margin: 1em 0 0 0",
            column(12,
               completeCite(r)
            )
         )
      ))
   } else {                      # We have a PMID...
      if(r$pmid!=gl$r$pmid) {    # check to see if we already have PubMed results cached
         gl$r <<- pmid2r(r)      # if not, get and cache them (prevents reloading when showing/hiding reviews)
      }
      if(r$pmidOK) {             # used when the pmid is known to be correct
         return(completeCite(gl$r))
      } else {
         return(tagList(         # used when we have a pmid but it hasn't been confirmed yet.
            fluidRow(
               column(6,
                  HTML('<p style="text-align:center;"><b><i>Original Citation</i></b></p>')
               ),
               column(6,
                  HTML('<div style="margin:0 auto; width:270px;">
                           <div style="display:inline-block; align:center">
                              <p style="text-align:center;"><b><i>Suggested Match</i></b></p>
                              <button class="btn action-button btn-success btn-sm" id="pmidCorrect" type="button">Confirm Match</button>
                              &nbsp
                              <button class="btn action-button btn-danger btn-sm" id="pmidWrong" type="button">Not a Match</button>
                        </div></div>')
               )
            ),
            fluidRow(
               column(6,
                  completeCite(r)
               ),
               column(6,
                  completeCite(gl$r)
               )
            )
         ))
      }
   }
}

completeCite = function(r) {
   if(r$ptype!="j") { type = paste0(" Type: ", r$ptype) } else { type="" }
   return(tagList(
      HTML(paste0("<h4>", r$title, "</h4>")),    # NOTE: Each htmlOutput has its own <div>, so formatting
      HTML("<hr/>"),                #    has to be done when building the output
      HTML(paste0("<b>", r$journal, "</b> ", r$Y, " ", r$V, ":", r$N, " ", r$P, type, "<br>")),
      HTML(paste0(
      '<b>PMID:</b> <a href="https://www.ncbi.nlm.nih.gov/pubmed/', r$pmid, '">', r$pmid, '</a>',
      ' <b>PMCID:</b> ',
           if(is.na(r$pmcid) || r$pmcid=="") {
              "NA "
           } else {
              paste0('<a href="https://www.ncbi.nlm.nih.gov/pmc/articles/', r$pmcid, '">', r$pmcid, '</a>')
           },
      ' <b>DOI:</b> <a href="http://dx.doi.org/', r$doi, '">', r$doi, '</a><br>')),
      HTML("<b>Authors:</b> ", r$authors),
      HTML("<hr/>"),
      HTML("<b>Abstract:</b><br>", r$abstract)
      )
   )
}

showComment = function(Rid) {
   return(tagList(
      textareaInput("article_comment", "Comment (on article):", value=prj$hits$comments[prj$hits$Rid==rv$reviewRid], rows=3, width="1000px")
   ))
}
   # checks if this is a skippable duplicate (return(TRUE)) and marks $dupOf on skippable duplicates
dupOf = function(Rid, pmid) {
   if(pmid=="") {                                              # need special handling if PMID is blank
      d = prj$hits$dupOf[prj$hits$Rid==Rid]                    #    in this case, it's all about dupOf
      if(nchar(d)>0) {
         return(TRUE)         # duplicate
      } else {
         return(FALSE)        # not a duplicate
      }
   } else {                                                    # Otherwise, use PMID to find duplicates
      dup_i <- which(prj$hits$pmidOK & prj$hits$pmid==pmid)    #    index vector of duplicates of this pmid
      if(length(dup_i)>1) {                                    #    if there are 2 or more
         prj$hits$dupOf[dup_i[-1]] <<- prj$hits$Rid[dup_i[1]]  #    mark all but the first with Rid of the first
         save_prj()
         if(Rid %in% dup_i && Rid!=prj$hits$Rid[dup_i[1]]) {   # if this one is in list and isn't the first,
            return(TRUE)                                       #    let caller know it's a duplicate
         }                                                     # (it could be the same PMID with pmidOK=FALSE)
      }
      return(FALSE)                                            # otherwise, let caller know it's not a duplicate
   }
}

previousReviews = function(Rid) {
   if(is.data.frame(prj$reviews)) {
      Keepers <- prj$reviews$Rid==Rid
      n_rev <- sum(Keepers)
   } else {
      n_rev=0
   }
   if(n_rev==0) {               # if there's no review there's nothing to return
      msg="No Previous Review."
      r=""
   } else {                                              # not blank...
      msg = "Previous Reviews:"
      names = prj$reviews$reviewer[Keepers]
      dates = as_date(prj$reviews$time[Keepers])
      r = rep_along(n_rev, "")
      for(i in 1:n_rev) {
         r[i] = paste0(names[i], "; ", dates[i], "<br/>")
      }
   }
   return(tagList(
      HTML("<hr/>"),
      HTML("ID: ", Rid, "<br/>"),
      HTML("<p><b>", msg, "</b></p>"),
      HTML(r)
   ))
}

### Functions for making the list view...

formatCite = function(Rid, i) {
   statuses = c("Not Reviewed", "Stage 1 Fail", "Stage 1 Pass")
   r = prj$hits[prj$hits$Rid==Rid,]
   if(dupOf(Rid, r$pmid)) {
      button = paste0('<b><i>This is a duplicate of ID ', r$dupOf, "</i></b>")
   } else {
      button = paste0('<button id="cite', i, '" type="button" class="btn btn-primary btn-xs action-button">Review</button>')
   }
   paste0("<tr><td><table>
           <tr><td><b>Stage 1 reviews:</b> ", as.character(r$nrev), " <b>Status:</b> ", statuses[as.numeric(r$rev)], "</td></tr>
           <tr><td><b>Year:</b> ", r$Y, " <b>Author:</b> ", r$authors, " <b>Journal:</b> ", r$journal, "</td></tr>
           <tr><td><i>", r$title, "</i></td></tr>
           <tr><td><b>ID:</b> ", r$Rid, " <b>PMID:</b> ", r$pmid, " <b>PMC:</b> ", r$pmcid, " <b>DOI:</b> ", r$doi, "</td></tr>
           <tr><td>", button, "
           </td></tr></table><hr/></td></tr>")
}

tableCites = function(Rid_vec) {
   req(Rid_vec)
   interior = ""
   for(i in seq_along(Rid_vec)) {
      interior = paste0(interior, formatCite(Rid_vec[i], i))
   }
   return(paste0("<table>", interior, "</table>"))
}

re$cite_list_body = reactive({
   x=rv$render_HitList
   if(input$main_menu != "Stage 1 Review") {return()}
   if(is.list(re$chunked_ids())) {
      return(HTML(tableCites(re$chunked_ids()[[rv$activeChunk]])))
   } else {                      # happens before there are citations and with filters that leave nothing
      return(HTML('<h5 style="color:red">No citations found...</h5>'))
   }
})

#############
# Reactives
#
#   these reactives are for filtering the list of reviews

#    x=rv$render_HitList is to force recalculation when it's possible that prj changed
#       For example, PMID status can change in prj without the filter changing.

### PMID status
re$S = reactive({   # returns a vector of characters, 1, 2, or 3
   x=rv$render_HitList   # prj$hits$pmidOK could change
   if(is.null(input$S_filter) || length(input$S_filter)==3) { return(rep(TRUE, length(prj$hits$Rid))) }
   Keepers = rep(FALSE, length(prj$hits$Rid))
   if(any(input$S_filter=="1")) {Keepers = Keepers | prj$hits$pmidOK}
   if(any(input$S_filter=="2")) {Keepers = Keepers | (!prj$hits$pmidOK & prj$hits$pmid!="")}
   if(any(input$S_filter=="3")) {Keepers = Keepers | prj$hits$pmid==""}
   return(Keepers)
})

### Hide Duplicates
re$D = reactive({
   x=rv$render_HitList   # prj$hits$dupOf could change
   if(!is.null(input$D_filter) && input$D_filter) {  # ie, input$D_filter==TRUE, which means hide
      return(prj$hits$dupOf=="")
   } else {
      return(rep(TRUE, length(prj$hits$Rid)))
   }
})

### by Author
re$A = reactive({
   if(!is.null(input$A_filter) && nchar(input$A_filter)>0) {
      return(str_detect(str_to_lower(prj$hits$authors), str_to_lower(input$A_filter)))
   } else {
      return(rep(TRUE, length(prj$hits$Rid)))
   }
})

### by Journal
re$J = reactive({
   if(!is.null(input$J_filter) && nchar(input$J_filter)>0) {
      return(str_detect(str_to_lower(prj$hits$journal), str_to_lower(input$J_filter)))
   } else {
      return(rep(TRUE, length(prj$hits$Rid)))
   }
})

### by Year
re$Y = reactive({
   if(!is.null(input$Y_filter) && nchar(input$Y_filter)>0) {
      return(str_detect(prj$hits$Y, input$Y_filter))
   } else {
      return(rep(TRUE, length(prj$hits$Rid)))
   }
})

### by Volume
re$V = reactive({
   if(!is.null(input$V_filter) && nchar(input$V_filter)>0) {
      return(str_detect(prj$hits$V, input$V_filter))
   } else {
      return(rep(TRUE, length(prj$hits$Rid)))
   }
})

### by Number
re$N = reactive({
   if(!is.null(input$N_filter) && nchar(input$N_filter)>0) {
      return(str_detect(prj$hits$N, input$N_filter))
   } else {
      return(rep(TRUE, length(prj$hits$Rid)))
   }
})

### by Page
re$P = reactive({
   if(!is.null(input$P_filter) && nchar(input$P_filter)>0) {
      return(str_detect(prj$hits$P, input$P_filter))
   } else {
      return(rep(TRUE, length(prj$hits$Rid)))
   }
})

### by Words (in title OR abstract
re$W = reactive({
   if(!is.null(input$W_filter) && nchar(input$W_filter)>0) {
      return(str_detect(str_to_lower(prj$hits$title), str_to_lower(input$W_filter)) | str_detect(str_to_lower(prj$hits$abstract), str_to_lower(input$W_filter)))
   } else {
      return(rep(TRUE, length(prj$hits$Rid)))
   }
})

### by Review
re$R = reactive({
   x=rv$render_HitList   # prj$hits$rev could change
   if(is.null(input$R_filter) || length(input$R_filter)==3) { return(rep(TRUE, length(prj$hits$Rid))) }
   Keepers = rep(FALSE, length(prj$hits$Rid))
   if(any(input$R_filter=="1")) {Keepers = Keepers | prj$hits$rev=="1"}
   if(any(input$R_filter=="2")) {Keepers = Keepers | prj$hits$rev=="2"}
   if(any(input$R_filter=="3")) {Keepers = Keepers | prj$hits$rev=="3"}
   return(Keepers)
})

#############
# Outputs
#

output$cite_list_filters = renderUI({
   tagList(
      fluidRow(
         column(3,
            checkboxGroupInput("S_filter", label="PMID status",
               choices=list("Verifed"=1, "Unverified"=2, "No PMID"=3),
               selected=c(1,2,3)),
            HTML('<div style="display:block"><span><b>Hide Duplicates</b>
                     <input id="D_filter" type="checkbox" checked="checked" style="display:inline"/>
                  </span></div>')
         ),
         column(6, style="margin: 0 0 0 0;",
            fluidRow(
               column(12,
                  tagList(
                     HTML('<div style="display:block"> <p><b>Citation details:</b></p>'),
                     HTML('<span>
                               Author
                               <input id="A_filter" type="text" class="input-small" style="display:inline; width:8em" value=""/>
                               &nbsp&nbspJournal
                               <input id="J_filter" type="text" class="input-small" style="display:inline; width:8em" value=""/>
                           </span></div>
                           <div style="display:block; margin:1em 0"><span>
                               Year
                               <input id="Y_filter" type="text" class="input-small" style="display:inline; width:3em" value=""/>
                               &nbsp&nbsp Vol
                               <input id="V_filter" type="text" class="input-small" style="display:inline; width:3em" value=""/>
                               &nbsp&nbsp Num
                               <input id="N_filter" type="text" class="input-small" style="display:inline; width:3em" value=""/>
                               &nbsp&nbsp Page
                               <input id="P_filter" type="text" class="input-small" style="display:inline; width:3em" value=""/>
                          </span></div>')
                  )
               )
            ),
            fluidRow(
               column(12,
                  tagList(
                     HTML('<div style="display:block"> <p><b>Phrase in title or abstract:</b></p>'),
                     HTML('<span>
                             <input id="W_filter" type="text" class="input-small" style="display:inline; width:25em" value=""/>
                          </span></div>')
                  )
               )
            )
         ),
         column(3,
            checkboxGroupInput("R_filter", label="Review status",
               choices=list("Not reviewed"=1, "Stage 1 Fail"=2, "Stage 1 Pass"=3),
               selected=c(1,2,3))
         )
      )
#      HTML(paste0())
   )
})

output$cite_list_pagination_top = renderUI(re$paginTop())
output$cite_list_body = renderUI(re$cite_list_body())
output$cite_list_pagination_btm = renderUI(re$paginBtm())

output$tab_S1R <- renderUI({
   x=rv$render_HitList
   if(rv$reviewRid == 0) {
      tagList(
         fluidRow(style="margin: 3px 0 0 0",
            column(12,
               styledPanel(outputId="cite_list",
                  panelTitle="Citation List:",
                  outputType="input",
                  panelColor="blue",
                     uiOutput("cite_list_filters"),
                     uiOutput("cite_list_pagination_top"),
                     uiOutput("cite_list_body"),
                     uiOutput("cite_list_pagination_btm")
               )
            )
         )
      )
   } else {
      tagList(
         fluidRow(style="margin: 3px 0 0 0",
            column(12,
               if(reviewer=="") {
                  styledPanel(
                     panelTitle="Reviewer Login:",
                     outputType="table",
                     panelColor="blue",
                     ttextInput(inputId="reviewer_name", label="Reviewer", value="", style="width: 20%;", autofocus=TRUE),
                     actionButton("reviewer_new", "Login", class="btn-primary btn-sm")
                  )
               } else {
                  styledPanel(
                     panelTitle="Review - Stage 1:",
                     outputType="table",
                     panelColor="blue",
                     tagList(fluidRow(
                        column(3,
                           HTML("<b>Reviewer:</b> ", reviewer, "<br/>"),
                           actionButton("reviewer_clear", "Change Reviewer", class="btn-danger btn-xs"),
                           previousReviews(rv$reviewRid)
                        ),
                        column(3,
                           radioButtons("stage_1_review", "Verdict:", choices = prj$options$stage1, selected = "Not reviewed", width="100%")
                        ),
                        column(5,
                           textareaInput("review_comment", "Comment (on review):", value="", rows=3, width="1000px"),
                           div(style="float:right;",
                               actionButton("review_prev", "<- Prev", class="btn-primary btn-sm"),
                               HTML("&nbsp"),
                               actionButton("review_next", "Next ->", class="btn-primary btn-sm")
                           ),
                           div(style="float:right; clear:right; margin: 3px 0 0 0;",
                              actionButton("review_cancel", "Cancel", class="btn-info btn-sm", style="float:right")
                           )
                        )
                     ),
                     fluidRow(
                        column(12,
                           HTML("<hr/>"),
                           showCite(rv$reviewRid)
                        )
                     ),
                     fluidRow(
                        column(12,
                           HTML("<hr/>"),
                           showComment(rv$reviewRid)
                        )
                     ),
                     fluidRow(
                        column(12,
                           HTML("<hr/>"),
                           showReviews()
                        )
                     )
                     )
                  )
               }
            )
         )
      )
   }
})

showReviews = function() {
   if(gl$showReviews) {
      if(is.data.frame(prj$reviews)) {  # skip if no reviews yet
         r = prj$reviews[prj$reviews$Rid==rv$reviewRid, ]
         if(nrow(r)>0) {
            txt=""
            for(i in 1:nrow(r)) {
               txt = paste0(txt, '<tr><td style="padding:3px;">', r$time[i],
                                '</td><td style="padding:3px;">', r$reviewer[i],
                                '</td><td style="padding:3px;">', r$decision[i],
                                '</td><td style="padding:3px;">', r$comment[i], '</td></tr>\n')
            }
            tagList(
               HTML('<button class="btn action-button btn-primary btn-sm" id="showReviews" type="button">Hide Reviews</button><br/><br/>'),
               HTML('<table>'),
               HTML('<tr><th style="padding:3px; width:11em">When</th>
                         <th style="padding:3px; width:5em">Reviewer</th>
                         <th style="padding:3px; width:9em">Decision</th>
                         <th style="padding:3px;">Comment</th></tr>'),
               HTML(txt),
               HTML('</table>')
            )
         } else {  # when there are no reviews for this citation, only show Hide button
            HTML('<button class="btn action-button btn-primary btn-sm" id="showReviews" type="button">Hide Reviews</button>')
         }
      }
   } else {
      if(is.data.frame(prj$reviews)) {  # skip if no reviews yet
         HTML('<button class="btn action-button btn-primary btn-sm" id="showReviews" type="button">Show Reviews</button>')
      }
   }
}

#############
# Reactives
#

# See srv_Searches.R for reactives used to build lists with pagination

#############
# Observers
#

observeEvent(input$pmidCorrect, {
   i = which(prj$hits$Rid==rv$reviewRid)
   prj$hits$pmidOK[i] <<- TRUE
   save_prj()
   if(dupOf(prj$hits$Rid[i], prj$hits$pmid[i])) {
      gl$modal_title <<- "Duplicate PMID."
      gl$modal_text <<- "We already have this one...moving on."
      rv$modal_warning <<- rv$modal_warning+1
      gl$direction()
   }
   rv$render_HitList <<- rv$render_HitList+1
})

observeEvent(input$pmidWrong, {
   prj$hits$pmid[prj$hits$Rid==rv$reviewRid] <<- ""
   save_prj()
   rv$render_HitList <<- rv$render_HitList+1
})

observeEvent(input$pmidSubmit, {
   i = which(prj$hits$Rid==rv$reviewRid)
   prj$hits$pmid[i] <<- input$pmidNew
   prj$hits$pmidOK[i] <<- TRUE
   save_prj()
   if(dupOf(prj$hits$Rid[i], prj$hits$pmid[i])) {
      gl$modal_title <<- "Duplicate PMID."
      gl$modal_text <<- "We already have this one...moving on."
      rv$modal_warning <<- rv$modal_warning+1
      gl$direction()
   }
   rv$render_HitList <<- rv$render_HitList+1
})

observeEvent(input$pmidFail, {
   prj$hits$pmidOK[prj$hits$Rid==rv$reviewRid] <<- TRUE
   save_prj()
   rv$render_HitList <<- rv$render_HitList+1
})

observeEvent(input$showReviews, {   # on button click, reverse setting of flag for showing reviews
   if(gl$showReviews){              #   at bottom of single citation page
      gl$showReviews <<- FALSE
      rv$render_HitList <<- rv$render_HitList+1                    # force a new render
   } else {
      gl$showReviews <<- TRUE
      rv$render_HitList <<- rv$render_HitList+1                    # force a new render
   }
})

saveReview = function() {
print(paste0("current rv$reviewRid=", rv$reviewRid))
   prj$hits$comments[prj$hits$Rid==rv$reviewRid] <<- input$article_comment # save comment in any case
   if(input$stage_1_review!="Not reviewed") { # don't modify list of reviews if this one is still "Not reviewed"
      prj$hits$nrev[prj$hits$Rid==rv$reviewRid] <<- prj$hits$nrev[prj$hits$Rid==rv$reviewRid]+1 # inc counter
      if(input$stage_1_review=="Stage 1 pass") {
         prj$hits$rev[prj$hits$Rid==rv$reviewRid] <<- "3"
      } else {
         if(prj$hits$rev[prj$hits$Rid==rv$reviewRid]=="1") { # don't overwrite a "3" with a "2"!
            prj$hits$rev[prj$hits$Rid==rv$reviewRid] <<- "2"
         }
      }
      this_review = tibble(Rid=rv$reviewRid,
                           time=now(),
                           decision=input$stage_1_review,
                           comment=input$review_comment,
                           reviewer=reviewer)
      if(is.data.frame(prj$reviews)){
         prj$reviews <<- rbind(prj$reviews, this_review)     # add this review to the others
      } else {
         prj$reviews <<- this_review                         # special handling for first row
      }
      save_prj()
   }
}
observeEvent(input$review_prev, {
   saveReview()
   gl$direction <<- gotoPrev
   gl$direction()
})

observeEvent(input$review_next, {
   saveReview()
   gl$direction <<- gotoNext
   gl$direction()
})

gotoPrev = function() {
   theseIds = re$filtered_ids()                   # determine what to do next:
   n = which(theseIds==rv$reviewRid)              # where are we in the filtered list of Rids?
   if(length(n)==0) {                             # check length in case filter dropped our Rid
      n = gl$RidPosition                          # where we were before the Rid was filtered out
   }
   if(n<=1) {                                     # if we're at or beyond the first one
      rv$reviewRid <<- 0                          #    return to list
   } else {
      gl$RidPosition <<- n-1                      # go back one and save where we are now
      rv$reviewRid <<- theseIds[gl$RidPosition]
   }
}

gotoNext = function() {
   theseIds = re$filtered_ids()                   # determine what to do next:
   n = which(theseIds==rv$reviewRid)              # where are we in the filtered list of Rids?
   if(length(n)==0) {                             # check length in case filter dropped our Rid
      n = gl$RidPosition-1                        # where we were before the Rid was filtered out
   }
   if(n>=length(theseIds)) {                      # if we're at or beyond the last one
      rv$reviewRid <<- 0                          #    return to list
   } else {
      gl$RidPosition <<- n+1                      # go forward one and save where we are now
      rv$reviewRid <<- theseIds[gl$RidPosition]
   }
}

observeEvent(input$review_cancel, {
   rv$reviewRid <<- 0                             # return to list
})

observeEvent(input$reviewer_new, {
   reviewer <<- input$reviewer_name
   rv$render_HitList <<- rv$render_HitList+1      # force a new render
})

observeEvent(input$reviewer_clear, {
   reviewer <<- ""
   rv$render_HitList <<- rv$render_HitList+1                 # force a new render
})

# We need the Rid position in filtered Rids in case a prj$ change drops the Rid from the filtered Rids
reviewThis = function(button) {
   rv$reviewRid <<- re$chunked_ids()[[rv$activeChunk]][button]   # Rid to review
   gl$RidPosition <<- which(re$filtered_ids()==rv$reviewRid)     #    and its position in filtered_ids
   gl$direction <<- gotoNext                                     #    assume we'll go forward after PMID ok
}                                                                #       if this one is a duplicate
   # These react to clicks on the Review button...
observeEvent(input$cite1, reviewThis(1))
observeEvent(input$cite2, reviewThis(2))
observeEvent(input$cite3, reviewThis(3))
observeEvent(input$cite4, reviewThis(4))
observeEvent(input$cite5, reviewThis(5))
observeEvent(input$cite6, reviewThis(6))
observeEvent(input$cite7, reviewThis(7))
observeEvent(input$cite8, reviewThis(8))
observeEvent(input$cite9, reviewThis(9))
observeEvent(input$cite10, reviewThis(10))
observeEvent(input$cite11, reviewThis(11))
observeEvent(input$cite12, reviewThis(12))
observeEvent(input$cite13, reviewThis(13))
observeEvent(input$cite14, reviewThis(14))
observeEvent(input$cite15, reviewThis(15))
observeEvent(input$cite16, reviewThis(16))
observeEvent(input$cite17, reviewThis(17))
observeEvent(input$cite18, reviewThis(18))
observeEvent(input$cite19, reviewThis(19))
observeEvent(input$cite20, reviewThis(20))
observeEvent(input$cite21, reviewThis(21))
observeEvent(input$cite22, reviewThis(22))
observeEvent(input$cite23, reviewThis(23))
observeEvent(input$cite24, reviewThis(24))
observeEvent(input$cite25, reviewThis(25))
observeEvent(input$cite26, reviewThis(26))
observeEvent(input$cite27, reviewThis(27))
observeEvent(input$cite28, reviewThis(28))
observeEvent(input$cite29, reviewThis(29))
observeEvent(input$cite30, reviewThis(30))
observeEvent(input$cite31, reviewThis(31))
observeEvent(input$cite32, reviewThis(32))
observeEvent(input$cite33, reviewThis(33))
observeEvent(input$cite34, reviewThis(34))
observeEvent(input$cite35, reviewThis(35))
observeEvent(input$cite36, reviewThis(36))
observeEvent(input$cite37, reviewThis(37))
observeEvent(input$cite38, reviewThis(38))
observeEvent(input$cite39, reviewThis(39))
observeEvent(input$cite40, reviewThis(40))
observeEvent(input$cite41, reviewThis(41))
observeEvent(input$cite42, reviewThis(42))
observeEvent(input$cite43, reviewThis(43))
observeEvent(input$cite44, reviewThis(44))
observeEvent(input$cite45, reviewThis(45))
observeEvent(input$cite46, reviewThis(46))
observeEvent(input$cite47, reviewThis(47))
observeEvent(input$cite48, reviewThis(48))
observeEvent(input$cite49, reviewThis(49))
observeEvent(input$cite50, reviewThis(50))
observeEvent(input$cite51, reviewThis(51))
observeEvent(input$cite52, reviewThis(52))
observeEvent(input$cite53, reviewThis(53))
observeEvent(input$cite54, reviewThis(54))
observeEvent(input$cite55, reviewThis(55))
observeEvent(input$cite56, reviewThis(56))
observeEvent(input$cite57, reviewThis(57))
observeEvent(input$cite58, reviewThis(58))
observeEvent(input$cite59, reviewThis(59))
observeEvent(input$cite60, reviewThis(60))
observeEvent(input$cite61, reviewThis(61))
observeEvent(input$cite62, reviewThis(62))
observeEvent(input$cite63, reviewThis(63))
observeEvent(input$cite64, reviewThis(64))
observeEvent(input$cite65, reviewThis(65))
observeEvent(input$cite66, reviewThis(66))
observeEvent(input$cite67, reviewThis(67))
observeEvent(input$cite68, reviewThis(68))
observeEvent(input$cite69, reviewThis(69))
observeEvent(input$cite70, reviewThis(70))
observeEvent(input$cite71, reviewThis(71))
observeEvent(input$cite72, reviewThis(72))
observeEvent(input$cite73, reviewThis(73))
observeEvent(input$cite74, reviewThis(74))
observeEvent(input$cite75, reviewThis(75))
observeEvent(input$cite76, reviewThis(76))
observeEvent(input$cite77, reviewThis(77))
observeEvent(input$cite78, reviewThis(78))
observeEvent(input$cite79, reviewThis(79))
observeEvent(input$cite80, reviewThis(80))
observeEvent(input$cite81, reviewThis(81))
observeEvent(input$cite82, reviewThis(82))
observeEvent(input$cite83, reviewThis(83))
observeEvent(input$cite84, reviewThis(84))
observeEvent(input$cite85, reviewThis(85))
observeEvent(input$cite86, reviewThis(86))
observeEvent(input$cite87, reviewThis(87))
observeEvent(input$cite88, reviewThis(88))
observeEvent(input$cite89, reviewThis(89))
observeEvent(input$cite90, reviewThis(90))
observeEvent(input$cite91, reviewThis(91))
observeEvent(input$cite92, reviewThis(92))
observeEvent(input$cite93, reviewThis(93))
observeEvent(input$cite94, reviewThis(94))
observeEvent(input$cite95, reviewThis(95))
observeEvent(input$cite96, reviewThis(96))
observeEvent(input$cite97, reviewThis(97))
observeEvent(input$cite98, reviewThis(98))
observeEvent(input$cite99, reviewThis(99))
observeEvent(input$cite100, reviewThis(100))

   # This writes the above code
# for(i in 1:100) {
#    cat(str_c("observeEvent(input$cite", i, ", reviewThis(", i, "))\n"))
# }
