# RCT App
#    R-enabled categorization and meta-analysis
#    v1.2 - TomW - 11/17/16
#

# Project overview
# In the big picture, this app collects data about scientific studies from user input and the internet,
#    saves the data in a format optimized for meta-analysis, and provides features for performing a
#    variety of meta-analyses with the data as well as repoting results with standard graphics.

# All of the data related to a project is saved in an R list() called "prj". This list is saved and reloaded
#    all at once. There is a much smaller data file, RCT, that allows the user to have multiple projects
#    ongoing at the same time and switch between them.

# This is the first file in the app. In addition to this overall documentation, it loads libraries and specialized
#    functions; establishes global variables; loads the user interface file ui_All.R and sets up the basic
#    tabbed naviation system; and loads the server files.

# There is more information at the top of each of these files about what they do and how they work.

#  prj data structure overview:

#  prj$name                  a string with the name of the project

#  prj$options               a list holding project options, most of which can be edited in the Settings
#  prj$options$lastTab          last tab opened (for returning to at next startup)
#  prj$options$stage1           a vector of stage1 review choices (editable)
#  prj$options$stage1_selected  the stage1 option that was last selected (refers only to the Settings)
#  prj$options$maxhits          maximum allowed hits on a PubMed non-test query (editable)
#  prj$options$note             string holding textarea notes about the project (editable)

#  prj$sourceInfo             a tibble holding information about searches
#     for details see the function buildS(), which creates new rows for this tibble
#     each row has a "Sid", or sourceInfo id.

#  prj$hits                   a tibble of summary data on all hits; used to create Hits tab listing
#     for details see the function buildR(), which creates new rows for this tibble
#     each row has a "Rid", or "our-id".
#

#  prj$reviews                a tibble of data about each review (there can be multiple reviews per Rid)
#

#  prj$trials                 a tibble of data about each trial (there can be multiple citations per trial)
#

#  prj$arms                   a tibble of data about each review (there can be multiple arm per citation)
#

#  prj$lastRid                the last number used to create an id
#

#######################################################
# Shiny app initialization
#


### Startup
   # RCT_path can be set up previously; this runs only if it doesn't already exist
if (!exists("RCT_path")) {RCT_path = paste0(project_path, "/RCT.app/")}

   # PeekOn should be TRUE for development and FALSE for production
   #   It displays information useful for debugging
PeekOn = FALSE

   # Libraries
library(foreign)
library(stringr)
library(dplyr)
library(tibble)
library(purrr)
library(magrittr)
library(lubridate)
library(rmarkdown)
library(ggplot2)
library(shiny)
library(shinythemes)
library(shinyBS)
library(xml2)
library(beepr)
library(xtable)
# library(rhandsontable)


   # Options
options(big.mark = ",")

### FUNCTIONS

Riddle <- function(n) {                                    # This creates a character vector of new ids
   Rid <- prj$lastRid+1
   prj$lastRid <<- Rid+n-1
   return(as.character(Rid:(Rid+n-1)))
}
buildR <- function(nrecs=1) {                              # standard tibble for hits table
#   "" = rep("", nrecs)
   Rids = Riddle(nrecs)
   return(tibble(Rid = Rids, Sid="", L="", pmid="", pmidOK=FALSE, dupOf="", ptype="", journal="", Y="", V="", N="", P="",
            title="", authors="", doi="", pmcid = "", comments="", abstract="", nrev=0, rev="1"))
}
buildT <- function(nrecs=1) {                              # standard tibble for trials table
   Tids = Riddle(nrecs)
   return(tibble(Tid = Tids, comments=""))
}

   # Function used to space calls to PubMed and other web resources
   #   Remembers when it was last called and doesn't return until at least 'n' milliseconds has passed.
pauseR = function(n) {
   while(Sys.time()-lastTime<(n/1000)) {}
#   print(Sys.time()-lastTime, digits=6)      # confirms this is working right
   lastTime <<- Sys.time()
}
   # testcode for pauseR
# pauseR(300)
# pauseR(300)
# pauseR(3000)
# pauseR(3000)

   # chunk a long vector into a list
chunker = function(vec, chunksize) {
#   print("In Chunker...")
#   print(paste0("Chunksize=", chunksize))
#   print(paste0("vec=", vec))
   if(length(vec)==0) { return(vec) }                             # happens when filters leave nothing
   r = list()
   chunks = length(vec)%/%chunksize                               #    number of complete chunks
   lastchunk = length(vec)%%chunksize                             #    size of partial chunk
   if(length(vec)>=chunksize) {                                   # skip this if there is just a partial chunk
      for(i in 1:chunks) {                                        #   "
         r[[i]] = vec[c(1+((i-1)*chunksize)):(i*chunksize)]       #   "
      }                                                           #   "
   }
   if(lastchunk>0) {                                              # skip this if size of last chunk is zero
      r[[chunks+1]] = vec[c(((chunks*chunksize)+1):length(vec))]  #   "
   }                                                              #   "
   return(r)
}

  # special function for this because it's needed multiple times and it's complicated.
search_results_table = function(t) {
   return(renderText("Function needs to be updated."))
   colnames(t) = c("Search Date", "From Date", "Status", "Hits", "New Hits")
   row.names(t)=NULL
   return(renderTable(t, display=c("d", "s", "s", "s", "d", "d"))) # see xtable(); +1 for rownames
}

   # Bootswatch styledPanel; eg., see http://bootswatch.com/readable/#containers
   #
   # You can use this to get a styled Bootswatch panel. If the panel will contain just
   #    one shiny output, it can also take the place of some Shiny outputXX() functions.
   #    Change the Bootswatch theme below (search for "shinytheme"), if desired.
   #
   # Note that uiOutput() is still needed at the outermost level - in this case in the
   #    tabPanels below.
   #
   # For uiOutput() or htmlOutput(), use "html" for the outputType parameter.
   # For textOutput(), use "text" for the outputType parameter.
   # For verbatimTextOutput(), use "pre" for the outputType parameter.
   # For tableOutput(), use "table" (or "html") for the outputType parameter.
   # For datatableOutput(), use "DT" for the outputType parameter.
   # For input-only or input-output or multiple-output panels, use "input" for the outputType parameter.
   #
   # If the data is coming from output$idxxx, then just make sure the ids match.
   # If you need multiple inputs inside the panel, put them inside a tagList().
   #
   # If a panel has both output and inputs, revert back to the xxxOutput() functions. If you
   #    treat it as an ouput panel, the output will overwrite the input. You also should revert
   #    back if you need to put two or more outputs in one panel.
   #
   # For imageOuput() and plotOutput(), use the shiny versions.
   #
   # In function, ... parameter comes first, which means all parameters that aren't named will be passed along.

styledPanel = function(..., outputId="", panelTitle="", outputType=NULL, panelColor="blue") {

   panelTitle =  HTML(paste0('<h3 class="panel-title">', panelTitle, '</h3>'))  # paste0() to supress extra spaces HTML
                                                                                #    itself will add (it's like paste().)
   switch(outputType,
          html = {outputClass="panel-body shiny-html-output"},      # uiOutput, htmlOutput, tableOutput
          text = {outputClass="panel-body shiny-text-output"},      # textOutput
           pre = {outputClass="panel-body shiny-text-output"},      # verbatimTextOutput (<pre>)
         table = {outputClass="panel-body shiny-html-output"},      # tableOutput (same as html, but just to be clear)
            DT = {outputClass="panel-body shiny-datatable-output"}, # dataTableOutput
         input = {outputClass="panel-body"; outputId = ""}          # for input-only, input-output, or multiple-output panels
   )                                                                #    (in those cases, revert to Shiny's xxxOutput() functions)

   switch(panelColor,
          blue = {panelClass="panel panel-primary"},
           red = {panelClass="panel panel-danger"},
        yellow = {panelClass="panel panel-warning"},
         green = {panelClass="panel panel-success"},
          cyan = {panelClass="panel panel-info"}
   )

   div(class=panelClass,
      div(class="panel-heading", panelTitle),
      if(outputType=="pre") {
         if(outputId=="") {
            pre(class=outputClass, ...)
         } else {
            pre(id=outputId, class=outputClass, ...)
         }
      } else {
         if(outputId=="") {
            div(class=outputClass, ...)
         } else {
            div(id=outputId, class=outputClass, ...)
         }
      }
   )
}

# sample code for calling styledPanel
#    styledPanel(outputId="xx",  # must match output$xx; will be missing for input-only panels
#       panelTitle="",           # actual text for the panel's header
#       outputType="",           # html, text, pre, table, DT (datatable), or input
#       panelColor="",           # blue, red, yellow, green, or cyan
#       ...                      # the content for input-type panels
#    )

textareaInput <- function(inputId, label, value="", rows=20, width="300px"){
   class="form-control shiny-input-container"
   style = paste0("width:", width, ";")
   div(class="form-group",
      tags$label('for'=inputId, label),
      tags$textarea(id=inputId, label=label, value, class=class, rows=rows, style=style)
   )}

   # Tom's textInput allows for autofocus and size options
ttextInput <- function(inputId, label, value="", style="width: 20%;", size="", autofocus=FALSE){
   switch(size,
          Small = class <- "form-control input-sm shiny-input-container",
          Large = class <- "form-control input-lg shiny-input-container",
          class <- "form-control shiny-input-container"
          )
   af = if(autofocus) {"autofocus"} else {""}
   div(class="form-group",
      tags$label('for'=inputId, class="control-label", label),
      HTML(paste0('<input id="', inputId, '" label="', label, '" value="', value, '" class="', class,
           '" style="', style, '" type="text" ', af, '/>'))
   )}

   # Search the journals data frame
   #    "style" is most usefully MedAbbr", but could be the name of any column in journals.
   #    Note: MedAbbr and ISOAbbr are very similar, but MedAbbr has 1,430 more missing and
   #       ISOAbbr has over 2,000 with embedded periods, MedAbbr has zero. Not counting those
   #       differences, only 211 (.66%) are different. MedAbbr is the recommended style for this
   #       app (it will return the full title if MedAbbr is missing).
journalSearch = function(jName, exact=FALSE, style="MedAbbr") {
   if(is.null(jName) || is.na(jName) || length(jName)==0) { return(list(found=0, jName="")) }
   jName = oName = str_trim(jName)
   jName = str_to_upper(str_replace(jName, "\\.", ""))     # also removes periods
   jName = str_replace_all(jName, " \\& ", " and ")        # standardize " & " to " and "
      # Fix errors in journals list
      if(jName=="QJM: AN INTERNATIONAL JOURNAL OF MEDICINE") {jName = "QJM"}
   t = which(journals$TITLE %in% jName)                    # %in% finds exact matches
   m = which(journals$MEDABBR %in% jName)
   i = which(journals$ISOABBR %in% jName)
   p = which(journals$ISSNp %in% jName)                    # include search by ISSN
   o = which(journals$ISSNo %in% jName)
   x = unique(c(t,m,i,p,o))                                # remove duplicates and nulls

   if(length(x)==0) {
      x = which(journals$TITLE %in% paste0("THE ", jName)) # try adding "THE " to the title
   }

   if(!exact & length(x)==0) {                       # if partial is ok and there are no exact, use partial
      t = which(str_detect(journals$TITLE, jName))         # str_detect finds partial matches
      m = which(str_detect(journals$MEDABBR, jName))
      i = which(str_detect(journals$ISOABBR, jName))
      x = unique(c(t,m,i))
   }
   found=length(x)
   jName = oName                                           # if nothing found, return original
   if(found>0) {
      jName = journals[[style]][x]                         # if we found multiple, return a vector
   }
   return(list(found=found, jName=jName))
}

   # journalSearch test code
# journalSearch("Database of Abstracts of Reviews of Effects")
# journalSearch("Journal of Clinical Endocrinology and Metabolism")
# journalSearch("Journal of Clinical Endocrinology and Metabolism", TRUE)
# journalSearch("American Journal of Clinical Nutrition", TRUE, exact=FALSE)
# journalSearch("American Journal of Clinical Nutrition", exact=TRUE)  # journal name begins with "The"
# journalSearch("Pediatric Research ")


####################
# convertAuthors()
#
#   Inside the app, we store authors' full names in a single string (lastname, forename; lastname, forename),
#      but we often need the names in other formats for printing or searching. This function does the conversions.
#
#   The function assumes each author name arrives inside a string, last name first: "Weishaar Tom"
#      There can be multiple first names in the string: "Weishaar Joseph Thomas George"
#      There can be multiple authors in the string, separated by incoming_separators[1]: "Weishaar; Rajan"
#      Or you can pass in multiple author strings using a vector: c("Weishaar Tom", "Rajan Sonali")
#      incoming_separators[2] is what's between lastname and firstname; sometimes "," but sometimes " "
#         eg: "Weishaar, Joseph Thomas; Rajan, Sonali" using incoming_separators ; and ,
#
#   Outgoing names are available as lastname only (default), lastname & initials ("init") or ("full") full name
#      In addition, outgoing names can either be in a vector: c("Weishaar, Tom" "Rajan, Sonali")
#         or multiple names in a single string: "Weishaar, Joseph Thomas George; Rajan, Sonali"
#      There is currently no choice for outgoing separators, they are ; and ,
#
#   return_string=TRUE for string (default), FALSE for vector
#   type="last" for lastname only, type="init" for lastname & initials, type="full" for everything (default)
#   incoming_separator[1] is between people; incoming_separator[2] is between last and other names; default is , " "

convertAuthors = function(authors, return_string=TRUE, type="full", incoming_separators=c(";", ",")) {
      # check class - incoming data must be in a string vector; could be all in [1], however.
   if(class(authors)[1]!="character") { stop(paste0("convertAuthors() expects a character vector, 'authors' is class: ", class(authors)[1])) }
      # if incoming data is a single string, make it a vector of author names
   if(length(authors)==1) {
      authors = str_trim(unlist(str_split(authors, incoming_separators[1])))  # if all in one string, vectorize
   }
      # str_split makes a list, exploding each author name into a vector of words to pass to cAfun above
   awords = str_split(authors, incoming_separators[2])
   authdf = t(sapply(awords, cAfun)) # sapply returns a dataframe; names in rows, last-initial-fullfirst in cols

   r = authdf[,1]                                                            # lastname only
   if(type=="init") { r = str_trim(paste0(authdf[,1], ", ", authdf[,2])) }    # last & initital
   if(type=="full") { r = str_trim(paste0(authdf[,1], ", ", authdf[,3])) }    # last & full

      # return the requested format
   if(return_string) { return(paste0(r, collapse="; ")) }
   return(r)
}
   # function used internally only by convertAuthors() for identifing name parts author-by-author
cAfun = function(x) {
   first_name = ""                        # x is a single author with name-words exploded into a vector
   initials = ""
   if(length(x)>2) {                                  # author name has more than two words
      for(i in 2:(length(x))) {                       #    for all chunks from 2 on
         first_name = paste0(first_name, x[i], " ")   #    paste this name with the others in a single string
         init = str_sub(x[i], 1, 1)                   #    get the first letter of this name
         if(init == str_to_upper(init)) {             #    if it's in lower case, skip it
            initials = paste0(initials, init)         #    otherwise paste this initial with the others
         }
      }
      initials = str_sub(initials,1,2)                #    no more than two initials, though
   }
   if(length(x)==2) {                                 # for an author with exactly two words
      first_name=x[2]                                 #
      initials = str_to_upper(str_sub(x[2],1,1))      #    assume first letter of second word is the only initial
      if(nchar(x[2]==2) & x[2]==str_to_upper(x[2])) { #    but if there are exactly 2 upper case chars, it's both
         initials=x[2]
      }
   }
   return(c(str_trim(x[1]), str_trim(initials), str_trim(first_name))) # return a vector with all three
}

   # testcode for convertAuthors
# type = "last"
# rstring = TRUE
# convertAuthors(c("Weishaar Joseph Thomas George"), rstring, type)
# convertAuthors(c("Weishaar, Joseph Thomas George"), rstring, type)
# convertAuthors(c("Weishaar Joseph, Thomas George"), rstring, type)
# convertAuthors(c("Weishaar Joseph Thomas, George"), rstring, type)
# convertAuthors(c("Weishaar Joseph Thomas George"), rstring, type)
# convertAuthors(c("Weishaar Joseph Thomas George"), rstring, type)
# convertAuthors(c("Weishaar JT"), rstring, type)
# convertAuthors(c("Weishaar Li"), rstring, type)
# convertAuthors(c("Weishaar"), rstring, type)
# convertAuthors(c("Weishaar", "Rajan"), rstring, type)
# convertAuthors(c("Weishaar Tom", "Rajan Sonali"), rstring, type)
# convertAuthors(c("Weishaar Joseph Thomas", "Rajan Sonali S"), rstring, type)
# convertAuthors(c("Weishaar", "Rajan"), rstring, type)

   # functions for initializing global variables:
initialize_RCT = function(path=RCT_path) {
   rct <<- list(names="My first project", selected=1)
   save_RCT()
}
initialize_prj = function(path=RCT_path, prj_name="My first project"){
   prj <<- list(name=prj_name,
                options=list(lastMenu="Searches",
                             lastTab="New Search",
                             stage1 = c("Not reviewed", "Not an RCT",
                                        "No valid subjects", "No valid treatment", "No valid outcome",
                                        "Other exclusion", "Stage 1 pass"),
                             stage1_selected = "Not reviewed",
                             maxhits = 5000,
                             note = ""
                ),
                sourceInfo="empty",
                hits="empty",
                reviews="empty",
                trials="empty",
                arms="empty",
                lastRid=0)
   save_prj()
}

   # functions for loading global variables:
load_RCT = function(path=RCT_path){ rct <<- readRDS(file=paste0(path, "data - RCT.R")) }
load_prj = function(path=RCT_path){ prj <<- readRDS(file=paste0(path, "data - ", rct$names[[rct$selected]], ".R")) }

   # functions for saving global variables
save_RCT = function(path=RCT_path){ saveRDS(rct, file=paste0(path, "data - RCT.R")) }
save_prj = function(path=RCT_path){ saveRDS(prj, file=paste0(path, "data - ", prj$name, ".R")) }

#############
# Other Globals

journals = readRDS(file=paste0(path=RCT_path, "lib_journals.R"))  # tibble of journal names, abbreviations, and ISSNs
lastTime = Sys.time()
reviewer <- ""

re <- list()             # for reactive expressions

rv <- reactiveValues()   # for reactive values
rv$process_Sid = 0
rv$modal_warning = 0     # used in an observer below to bring up a modal warning dialog
rv$show_per_page = "10"

gl <- list()             # global list for non-reactive values/expressions
gl$search_spp = "10"     # show_per_page
gl$cites_spp = "10"
gl$showReviews = FALSE   # initialize whether to show existing reviews at bottom of citation review page

#######################################################
# Shiny UI
#
   # Determine whether data exists or this is a new installation
if (!file.exists(paste0(RCT_path, "data - RCT.R"))) {
   initialize_RCT()
   initialize_prj()
}
   # In any case, load the newly-initialized or the pre-existing data
rct = load_RCT()
prj = load_prj()

gl$r = buildR()          # initialize cache

   # Main Shiney UI
ui = fluidPage(title="RCT.app", theme=shinytheme("readable"),
         tags$head(
            # These tags control the look of the progress bar
            tags$style(HTML('
               .shiny-notification {
               width: 200%;
               height: 100px;
               margin-left: -400px;
               }
               .shiny-progress-notification .progress {
               margin-top: 20px;
               height: 25px;
               width: 90%;
               }'))
         ),
#        fluidRow(style="margin: 0 1em 0 1em", HTML("<H3>RCT.app</H3>")),
        fluidRow(style="margin: 0 1em 0 1em", span(id="tab", class="shiny-text-output")),
        fluidRow(style="margin: 0 1em 0 1em",
#           tabsetPanel(id="main_menu", class="navbar-inverse",
           tabsetPanel(id="main_menu",
              tabPanel("Searches",
                 {
                 fluidRow(
                    tabsetPanel(id="search_menu",
                       tabPanel("New Search", uiOutput("tab_SearchNew")),
                       tabPanel("Search List", uiOutput("tab_SearchList")),
                       tabPanel("Search Analysis", uiOutput("tab_SearchAn"))
                    )
                 )
                 }),
              tabPanel("Stage 1 Review",
                 {
                 fluidRow(
                    tabsetPanel(id="s1r_menu",
                       tabPanel("Citation List", uiOutput("tab_S1R")),
                       tabPanel("Stage 1 Analysis", uiOutput("tab_S1RAn"))
                    )
                 )
                 }),
              tabPanel("Trials", uiOutput("tab_Trials")),
              tabPanel("Arms", uiOutput("tab_Arms")),
              tabPanel("Results",
                 {
                 fluidRow(
                    tabsetPanel(id="results_menu",
                       tabPanel("Forest Plot", uiOutput("tab_Forest")),
                       tabPanel("Funnel Plot", uiOutput("tab_Funnel"))
                    )
                 )
                 }),
              tabPanel("Settings", uiOutput("tab_Settings"))
           )
        )
     )

#######################################################
# Shiny Server
#
server <- function(input, output, session) {

#################
# Server for Tabs

   # When the user switches tabs with clicks or we switch tabs with updateTabsetPanel(), we arrive here and use
   #    re-render the tab and to remember what tab we're on.

rememberTab = function(main, tab="") {
   prj$options$lastMenu <<- main
   prj$options$lastTab <<- tab
   save_prj()
}
observeEvent(input$main_menu, {
   switch(input$main_menu,
      "Searches" = {
         rv$activeChunk = 1                       # when main_menu changes, initialize rv$activeChunk
         rv$show_per_page <<- gl$search_spp
         rememberTab(input$main_menu, input$search_menu)
#print(paste0("Tab change: ", prj$options$lastMenu ,"/", prj$options$lastTab))
      },
      "Stage 1 Review" = {
         rv$activeChunk = 1                       # when main_menu changes, initialize rv$activeChunk
         rv$show_per_page <<- gl$cites_spp
         rememberTab(input$main_menu, input$s1r_menu)
#print(paste0("Tab change: ", prj$options$lastMenu ,"/", prj$options$lastTab))
      },
      "Results" = {
         rememberTab(input$main_menu, input$results_menu)
#print(paste0("Tab change: ", prj$options$lastMenu ,"/", prj$options$lastTab))
      },
      {  rememberTab(input$main_menu)                # for menus without a submenu
#print(paste0("Tab change: ", prj$options$lastMenu ,"/", prj$options$lastTab))
      }
   )
})

   # These are called when the user switches submenus without switching the main_menu.
observeEvent(input$search_menu, {
   rememberTab(input$main_menu, input$search_menu)
print(paste0("Tab change: ", prj$options$lastMenu ,"/", prj$options$lastTab))
})

observeEvent(input$s1r_menu, {
   rememberTab(input$main_menu, input$s1r_menu)
print(paste0("Tab change: ", prj$options$lastMenu ,"/", prj$options$lastTab))
})

observeEvent(input$results_menu, {
   rememberTab(input$main_menu, input$results_menu)
print(paste0("Tab change: ", prj$options$lastMenu ,"/", prj$options$lastTab))
})

   # a generic for a modal warning
   # to call:
   #    gl$modal_title <<- ""
   #    gl$modal_text <<- ""
   #    rv$modal_warning <<- rv$modal_warning+1
observeEvent(rv$modal_warning, {
   if(rv$modal_warning>0) {         # skip initialization
      showModal(modalDialog(
         title = gl$modal_title,
         gl$modal_text,
         footer = modalButton("Ok")
      ))
   }
})
   #####################################################
   # Individual observeEvents are in page-specific files
   #    source files must have "local=TRUE" option
   source(paste0(project_path, "/RCT.app/srv_Settings.R"), local=TRUE)
   source(paste0(project_path, "/RCT.app/srv_Searches.R"), local=TRUE)
   source(paste0(project_path, "/RCT.app/srv_Uploads.R"), local=TRUE)
   source(paste0(project_path, "/RCT.app/srv_S1Review.R"), local=TRUE)

      # This loads the remaining outputs for tabs I haven't worked on yet.
   source(paste0(project_path, "/RCT.app/ui_All.R"), local=TRUE)

   # At startup, go to tab in use at end of last session
print(paste0("prj$options for lastMenu/lastTab: ", prj$options$lastMenu, "/", prj$options$lastTab))
#updateTabsetPanel(session, prj$options$lastMenu, prj$options$lastTab)
}


#shinyApp(ui = ui, server = server)


