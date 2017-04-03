# ui_All.R
# v1.0 - TomW - 9/5/2016

# This file sets up the inital user interface elements for the RCT project
#   and stores them in a list called "rendered"

# Turns out this was a dumb idea and slowly fading it out...


#############
# Stage 1 Analysis tab
#
output$tab_S1RAn <- renderUI(
   fluidRow(style="margin: 3px 0 0 0",
      column(12,
         styledPanel(outputId="",
            panelTitle="Stage 1 Analysis:",
            outputType="html",
            panelColor="blue",
               HTML("Coming soon...")
         )
      )
   )
)

# some useful code for this tab

# table(prj$hits$dupOf=="", prj$hits$Sid)
#
#   #         2   3   4   5
#   # FALSE   0   0  38  28
#   # TRUE   53  58 114 103
#
# x = merge(prj$hits[,c("Rid", "Sid")], prj$reviews[,c("Rid", "decision")])
# table(x$decision,x$Sid)
#
#   #                      2  3  4  5
#   # All analogues        5  4  4  0
#   # All genetic          0  0 16 10
#   # All observational    2  0 15 51
#   # Not a meta-review    1  0  3  7
#   # Only 25(OH)D status  0  0  4  1
#   # Other exclusion      0  1  2  1
#   # Some pass           15 14 18  9
#   # Stage 1 pass        14 10 41 18
#   # Treatment not D     16 29 11  6
#
# prj$reviews$comment[which(prj$reviews$decision=="Other exclusion")]
# # [1] "Withdrawn"                             "Abstract not available."
# # [3] "Observational and genetic"             "Not in English/No Abstract Available."

# counter = function(df) {
#    Sids = unique(df$Sid)
#    for(i in Sids) {                               # for each Sid
#       x = df$Sid == i                             # T/F vector; T=in this Sid
#       nx = df$Sid != i                            # T/F vector; T=not in this Sid
#       dups = sum(df$pmid[x] %in% df$pmid[nx])     # how many in this Sid are in other Sids?
#       print(paste0("Sid ", i, " has ", sum(x), " citations, of which ", dups, " (", round(dups/sum(x)*100,0), "%) were found by other searches."))
#    }
# }
#
# counter(prj$hits)
# # now do this just for Stage 1 pass
# df = merge(prj$hits[,c("Rid", "Sid", "pmid")], prj$reviews[prj$reviews$decision=="Stage 1 pass",c("Rid", "decision")], all=TRUE)
# for(i in 1:nrow(df)) {                                 # look at all rows
#    if(df$decision[i] == "Stage 1 pass")                # if this row is a Stage 1 pass
#    df$decision[df$pmid==df$pmid[i]] = "Stage 1 pass"   # set all matching PMIDs to Stage 1 pass
# }
# counter(df)
#
# x = merge(prj$hits[,c("Rid", "Sid", "dupOf")], prj$reviews[,c("Rid", "decision")], all=TRUE, sort=FALSE)
# blanks = rep(1,nrow(x))
# y = data.frame(copies=blanks)
# x = cbind(x, y)
# #for(i in 1:nrow(x)) { x[i,as.numeric(x$Sid[i])+3] = TRUE }
# for(i in 1:nrow(x)) {
#    if(x$dupOf[i] !="") {
#       this_Rid = x$Rid[i]
#       org_Rid = x$dupOf[i]
#       x$copies[x$Rid==org_Rid] = x$copies[x$Rid==org_Rid]+1
#       x$copies[x$Rid==this_Rid] = x$copies[x$Rid==this_Rid]+1
#       x$decision[x$Rid==this_Rid] = x$decision[x$Rid==org_Rid]
#    }
# }
# View(x)
# table(x$decision, x$Sid)
# table(x$copies, x$Sid)

# Filters:  by Search   by Reviewer
# Total citations
# Duplicates
# Not reviewed   | blue      pie chart these on right
# Stage 1 fail   | red
# Stage 1 maybe  | yellow
# Stage 1 Pass   | green
#
# Stage 1 failure reason:
#   (give fractional points if multiple reviews for one Rid)
#
# Duplicate analysis by Sid

#
# k = prj$hits$comments != ""
# r = prj$hits[k, c("Rid", "authors", "Y", "comments")]
# r$comments = str_replace_all(r$comments, '"', '##')
# r$comments = str_replace_all(r$comments, '\n', '$$')
# for(i in 1:nrow(r)) {
#    r$authors[i] = convertAuthors(r$authors[i], return_string=FALSE, type="last")[1]
# }
# fname = paste0(project_path, "/comments.csv")
# write.table(r, file=fname, sep=",")

# IN EXCEL, replace $$ with alt-0010 and ## with "

#############
# Trials tab
#
output$tab_Trials <- renderUI(
   fluidRow(style="margin: 3px 0 0 0",
      column(12,
         styledPanel(outputId="",
            panelTitle="Trials:",
            outputType="html",
            panelColor="blue",
               HTML("Coming soon...")
         )
      )
   )
)

#############
# Arms tab
#
output$tab_Arms <- renderUI(
   fluidRow(style="margin: 3px 0 0 0",
      column(12,
         styledPanel(outputId="",
            panelTitle="Arms:",
            outputType="html",
            panelColor="blue",
               HTML("Coming soon...")
         )
      )
   )
)

#############
# Forest tab
#
output$tab_Forest <- renderUI(
   fluidRow(style="margin: 3px 0 0 0",
      column(12,
         styledPanel(outputId="",
            panelTitle="Forest Plot:",
            outputType="html",
            panelColor="blue",
               HTML("Coming soon...")
         )
      )
   )
)

#############
# Funnel tab
#
output$tab_Funnel <- renderUI(
   fluidRow(style="margin: 3px 0 0 0",
      column(12,
         styledPanel(outputId="",
            panelTitle="Funnel Plot:",
            outputType="html",
            panelColor="blue",
               HTML("Coming soon...")
         )
      )
   )
)



