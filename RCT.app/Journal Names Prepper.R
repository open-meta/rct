# Capture data on medical journal names and abbreviations
# v1.0 - TomW - 9/22/16
# v1.1 - TomW - 10/4/16: added TITLE, MEDABBR, ISOABBR

# This downloads a text file of journal names and abbreviations available from the Library of Medicine,
#    puts it in a tibble named "journals", and saves the tibble in the RCT.app folder.

# This takes about 30 seconds; all but ~1 second is downloading the file.
# The R file loads in ~.15 seconds

# On 9/22/16 there were 31,779 journals in the file
# On 10/13/16 there were 31,828

startTime = Sys.time()

library(stringr)
library(tibble)
library(purrr)

url = "ftp://ftp.ncbi.nih.gov/pubmed/J_Entrez.txt"   # PubMed and NCBI Molecular Biology Database Journals
recs = readLines(url, warn=FALSE)

headers = which(str_sub(recs, 1L, 2L)=="--")         # find rows with headers
n_jour = length(headers)                             # length of headers is number of journals
blanks = rep_along(headers, "")                      # prep results tibble
journals = tibble(Title=blanks, MedAbbr=blanks, ISOAbbr=blanks, ISSNp=blanks, ISSNo=blanks, NlmId=blanks,
                  TITLE=blanks, MEDABBR=blanks, ISOABBR=blanks)

journals$Title = str_sub(recs[which(str_sub(recs, 1L, 12L)=="JournalTitle")], 15L, -1L)
journals$Title = str_replace_all(journals$Title, " \\& ", " and ")    # standardize " & " to " and "
journals$MedAbbr = str_sub(recs[which(str_sub(recs, 1L, 7L)=="MedAbbr")], 10L, -1L)
journals$ISOAbbr = str_sub(recs[which(str_sub(recs, 1L, 7L)=="IsoAbbr")], 10L, -1L)
journals$ISSNp = str_sub(recs[which(str_sub(recs, 1L, 12L)=="ISSN (Print)")], 15L, -1L)
journals$ISSNo = str_sub(recs[which(str_sub(recs, 1L, 13L)=="ISSN (Online)")], 16L, -1L)
journals$NlmId = str_sub(recs[which(str_sub(recs, 1L, 5L)=="NlmId")], 8L, -1L)
journals$TITLE = str_to_upper(journals$Title)
journals$MEDABBR = str_to_upper(journals$MedAbbr)
journals$ISOABBR = str_to_upper(str_replace_all(journals$ISOAbbr, "\\.", ""))    # also removes periods

journals = as.data.frame(journals) # prevents some problems with paste(..., collapse=) that tibbles are prone to

# if MedAbbr is blank, replace it with the full title
journals$MedAbbr[journals$MedAbbr==""] = journals$Title[journals$MedAbbr==""]
path="C:/Users/Tom/Documents/SugarSync-Tom/Vitamin D Trials/Site/RCT.app/"
saveRDS(journals, file=paste0(path, "lib_journals.R"))

Sys.time() - startTime

write.table(journals, file="C:/Users/Tom/Documents/SugarSync-Tom/Vitamin D Trials/journals.csv", sep=",", row.names=FALSE)

startTime = Sys.time()
path="C:/Users/Tom/Documents/SugarSync-Tom/Vitamin D Trials/Site/RCT.app/"             ### Here's how to load
journals = readRDS(file=paste0(path, "lib_journals.R"))                             ###    the file....
Sys.time() - startTime



####################################################################################




   # fewer ISOAbbr's are missing (10) thatn MedAbbr's (1440)
sum(journals$Title=="")      # None Missing
sum(journals$MedAbbr=="")
sum(journals$ISOAbbr=="")
sum(journals$ISSNp=="")
sum(journals$ISSNo=="")
sum(journals$NlmId=="")      # None Missing

   # not counting where MedAbbr is blank, there are 2,318 (7.3%) cases where MEDABBR and ISOABBR are different
t = subset(journals, MedAbbr!="", select=c("MedAbbr", "ISOAbbr"))
t = t[t$MedAbbr != t$ISOAbbr,]
nrow(t)
nrow(t)/ nrow(journals)

   # not counting where MedAbbr is blank, capitalization, and removing periods in ISOAbbr,
   #    there are 211 (.66%) cases where MEDABBR and ISOABBR are different
t = subset(journals, MEDABBR!="", select=c("MEDABBR", "ISOABBR"))
t = t[t$MEDABBR != t$ISOABBR,]
nrow(t)
nrow(t)/ nrow(journals)


   # Search the journals data frame
   #    "style" is most usefully MedAbbr", but could be the name of any column in journals.
   #    If you return a string (return_string=TRUE) and need to turn it back into a vector, use:
   #       str_split(string, ",# ")
   #    Note: MedAbbr and ISOAbbr are very similar, but MedAbbr has 1,430 more missing and
   #       ISOAbbr has over 2,000 with embedded periods, MedAbbr has zero. Not counting those
   #       differences, only 211 (.66%) are different. MedAbbr is the recommended style (it
   #       will return the full title if MedAbbr is missing).
journalSearch = function(jName, return_string=FALSE, exact=FALSE, style="MedAbbr") {
   jName = str_to_upper(str_replace(jName, "\\.", ""))   # also removes periods
   t = which(journals$TITLE %in% jName)                  # %in% finds exact matches
   m = which(journals$MEDABBR %in% jName)
   i = which(journals$ISOABBR %in% jName)
   x = unique(c(t,m,i))                                  # remove duplicates

   if(!exact & length(x)==0) {                       # if partial is ok and there are no exact, use partial
      t = which(str_detect(journals$TITLE, jName))       # str_detect finds partial matches
      m = which(str_detect(journals$MEDABBR, jName))     #   needed because "American journal..."
      i = which(str_detect(journals$ISOABBR, jName))     #   won't find "The American journal..."
      x = unique(c(t,m,i))
   }
   if(return_string){
      return(paste0(journals[x,style], collapse=",# "))  # return a string with ",# " separating matches
   } else {
      return(journals[x,style])                          # return a vector
   }
}
journalSearch("Clinical Nutrition", "MedAbbr", TRUE, TRUE)
journalSearch("Clinical Nutrition", "MedAbbr", TRUE, FALSE)
journalSearch("Clinical Nutrition", "MedAbbr", FALSE, TRUE)
journalSearch("Clinical Nutrition", "MedAbbr", FALSE, FALSE)

remove(list=objects())
