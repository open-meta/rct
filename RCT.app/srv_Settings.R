# srv_Settings.R
# v1.3 - TomW - 11/21/16

#############
# Inits

rv$render_Settings <<- 0         # inc to re-render

#############
# Settings tab
#
output$tab_Settings <- renderUI({
   x=rv$render_Settings
#print(paste0("Rendering Settings; rv$render_Settings = ", rv$render_Settings))
   tagList(
   fluidRow(style="margin: 3px 0 0 0",
      column(6,
         styledPanel(
            panelTitle="Select Project:",
            outputType="input",
            panelColor="blue",
               radioButtons("project_selected", "", choices=rct$names, selected=rct$names[[rct$selected]], width="100%")
         )
      ),
      column(6,
         styledPanel(
            panelTitle="Name Selected Project:",
            outputType="input",
            panelColor="blue",
               textInput("project_name", "New project name", width="100%"),
               uiOutput("dash_pname_msg"),
               actionButton("project_rename", "Rename Selected Project", class="btn-primary btn-sm"),
               actionButton("project_new", "Add New Project", class="btn-warning btn-sm")
         )
      )
   ),
   fluidRow(style="margin: 3px 0 0 0",
      column(12,
         styledPanel(
            panelTitle="Options for Selected Project:",
            outputType="input",
            panelColor="cyan",
            tagList(fluidRow(
               column(3,
                  radioButtons("options_stage1", "Stage 1 Options:", choices=prj$options$stage1, selected=prj$options$stage1_selected, width="100%")
               ),
               column(5,
                  textInput("stage1_name", "New option name:", width="100%"),
                  actionButton("stage1_rename", "Rename", class="btn-primary btn-sm"),
                  actionButton("stage1_add", "Add", class="btn-warning btn-sm"),
                  actionButton("stage1_delete", "Delete", class="btn-danger btn-sm"),
                  div(id="dash_opts_msg", class="shiny-html-output", style="margin: 1em 0 0 0;")
               ),
               column(4,
                  textInput("option_maxhits", "Maximum PubMed Hits Allowed:", value=prj$options$maxhits, width="100%"),
                  actionButton("option_maxhits_chg", "Change Maximum", class="btn-primary btn-sm")
               )
            ),
            HTML("<hr/>"),
            fluidRow(
               column(2,
                  actionButton("option_note_save", "Save Notes", class="btn-primary btn-sm")
               ),
               column(10,
                  textareaInput("option_note", "Notes about this project:", value=prj$options$note, rows=8, width="100%")
               )
            ))
         )
      )
   )
)})

############
# Functions

   # function to determine whether a Stage 1 Option Name can be renamed or deleted
option_in_use = function(option_name) {
   if(!is.data.frame(prj$reviews)) { return(FALSE) }            # it's ok, there are no reviews yet
   return(option_name %in% prj$reviews$decision)                # only ok if no reviews using this option
}

############
# Reactives

output$dash_pname_msg <- renderText({
   if(is.null(rv$dash_pname_msg)) {
      HTML("")
   } else {
      HTML("<p><i>", rv$dash_pname_msg, "</i></p")
   }
})
output$dash_opts_msg <- renderText({
   if(is.null(rv$dash_opts_msg)) {
      HTML("")
   } else {
      HTML("<p><i>", rv$dash_opts_msg, "</i></p")
   }
})

############
# Observers

      # Select a different project
   observeEvent(input$project_selected, {
      if(input$project_selected!="") {  # skip this if project_selected == "" (happens sometimes during initialization)
         save_prj()                                                                  # save current prj
         rct$selected <<- which(rct$names %in% input$project_selected)               # update rct
         save_RCT()                                                                  # save rct
         load_prj()                                                                  # load new prj
         rv$render_Settings <<-rv$render_Settings+1         # initializing screen trips this observer
      }
   })

      # Rename a project
   observeEvent(input$project_rename, {
      rv$dash_pname_msg=""
      if(input$project_name=="") {rv$dash_pname_msg="Please enter a name."}          # name is blank
      if(input$project_name %in% rct$names) {rv$dash_pname_msg="Duplicate name."}    # name is a duplicate
      if(rv$dash_pname_msg=="") {                                                    # otherwise...
         old_name = rct$names[[rct$selected]]                                        # get old_name
         new_name = input$project_name                                               # get new_name
         rct$names[[rct$selected]] <<- new_name                                      # update rct
         save_RCT()                                                                  # save rct
         prj$name <<- new_name                                                       # change name in prj
         save_prj()                                                                  # save prj
         from_name = paste0(project_path, "/Site/RCT.app/data - ", old_name, ".R")   # change file name
         to_name = paste0(project_path, "/Site/RCT.app/data - ", new_name, ".R")     #    "
         file.rename(from_name, to_name)                                             #    "
      }
      rv$render_Settings <<-rv$render_Settings+1
   })

      # Add a project
   observeEvent(input$project_new, {
      rv$dash_pname_msg=""
      if(input$project_name=="") {rv$dash_pname_msg="Please enter a name."}          # name is blank
      if(input$project_name %in% rct$names) {rv$dash_pname_msg="Duplicate name."}    # name is a duplicate
      if(rv$dash_pname_msg=="") {                                                    # otherwise...
         new_name = input$project_name                                               # get new_name
         rct$names <<- c(rct$names, new_name)                                        # add name to rct
         rct$selected <<- length(rct$names)                                          # select it
         save_RCT()                                                                  # save rct
         save_prj()                                                                  # save current prj
         initialize_prj(prj_name=new_name)                                           # new, empty prj (init saves it)
      }
      rv$render_Settings <<-rv$render_Settings+1
   })

      # Add a Stage 1 option
   observeEvent(input$stage1_add, {
      rv$dash_opts_msg=""
      if(input$stage1_name=="") { rv$dash_opts_msg="Please enter a name." }
      if(any(prj$options$stage1 %in% input$stage1_name)) { rv$dash_opts_msg="That name is a duplicate." }
      if(rv$dash_opts_msg=="") {
         option_num <- which(prj$options$stage1 %in% input$options_stage1)
         if(option_num==0) {
            rv$dash_opts_msg = paste0("No option selected.")
         } else {
            for(i in (length(prj$options$stage1)):(option_num+1)) {     # Note this never overwrites
               prj$options$stage1[i+1] <<- prj$options$stage1[i]        #    option 1, "Not Reviewed"
            }
            prj$options$stage1[option_num+1] <<- input$stage1_name
            prj$options$stage1_selected <<- input$stage1_name
            save_prj()
         }
      }
      rv$render_Settings <<-rv$render_Settings+1
   })

      # Rename a Stage 1 option
   observeEvent(input$stage1_rename, {
      rv$dash_opts_msg=""
      if(option_in_use(input$options_stage1)) {
         rv$dash_opts_msg = paste0("<b>", input$options_stage1, "</b> can't be changed because it's in use.")
      }
      if(input$stage1_name=="") {
         rv$dash_opts_msg="Please enter a name."
      }
      if(input$options_stage1=="Not reviewed" | input$options_stage1=="Stage 1 Pass") {
         rv$dash_opts_msg = paste0("Can't rename <b>", input$options_stage1, "</b>")
      }
      if(rv$dash_opts_msg=="") {
         option_num <- which(prj$options$stage1 %in% input$options_stage1)
         if(option_num==0) {
            rv$dash_opts_msg = paste0("No option selected.")
         } else {
            prj$options$stage1[option_num] <<- input$stage1_name
            prj$options$stage1_selected <<- input$stage1_name
            save_prj()
         }
      }
      rv$render_Settings <<-rv$render_Settings+1
   })

      # Delete a Stage 1 option
   observeEvent(input$stage1_delete, {
      rv$dash_opts_msg = ""
      if(option_in_use(input$options_stage1)) {
         rv$dash_opts_msg = paste0("<b>", input$options_stage1, "</b> can't be deleted because it's in use.")
      }
      if(input$options_stage1=="Not reviewed" | input$options_stage1=="Stage 1 pass") {
         rv$dash_opts_msg = paste0("Can't delete <b>", input$options_stage1, ".</b>")
      }
      if(rv$dash_opts_msg=="") {
         option_num <- which(prj$options$stage1 %in% input$options_stage1)
         if(option_num==0) {
            rv$dash_opts_msg = paste0("No option selected.")
         } else {
            prj$options$stage1 <<- prj$options$stage1[-option_num]
            save_prj()
         }
      }
      prj$options$stage1_selected <<- "Not reviewed"
      rv$render_Settings <<-rv$render_Settings+1
   })

      # Change PubMed Hits Maximum
   observeEvent(input$option_maxhits_chg, {
      prj$options$maxhits <<- input$option_maxhits
      save_prj()
      rv$render_Settings <<-rv$render_Settings+1
   })

      # update notes as typing occurs
   observeEvent(input$option_note, {
      if(input$option_note!="") { prj$options$note <<- input$option_note }    # to avoid blanking problem at init
   })

  observeEvent(input$option_note_save, {
      save_prj()
      rv$render_Settings <<-rv$render_Settings+1
   })

