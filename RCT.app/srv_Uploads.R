# srv_Uploads.R
# v1.2 - TomW - 11/14/16

# This page has code that processes citations from PubMed and other data bases.

# prj$hits is a table with one row for every hit we've ever found. Any data in that row comes from
#    the original data source, with the following exceptions: Rid, Sid, pmid, pmidOK, and dupOf.

# buildR(nrec) is a function that builds a consistent version of this table usable with rbind() to
#    add new rows to the main table.

############
# Functions

# srv_PubMed.R
# v1.0 - TomW - 9/3/16
# v1.1 - TomW - 11/14/16

##### PubMed-related functions

   # function to search PubMed using E-Utilities
   #    begin 'terms' with "&term="); returns a vector of PMIDs
doEsearch = function(terms) {
   Esearch = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?tool=rct.app&email=jtw2117@columbia.edu"
   pauseR(300)                                                  # force 300 milliseconds between calls
   url = paste0(Esearch, str_replace_all(terms, " ", "+"))      # form url, replace " " with "+"
   raw = read_xml(url)
   return(list(
      count = xml_text(xml_find_first(raw, "/eSearchResult/Count")),
      pmids = xml_text(xml_find_all(raw, "/eSearchResult/IdList/Id")),
      query = xml_text(xml_find_first(raw, "/eSearchResult/QueryTranslation"))
   ))
}
   # testcode for doEsearch
# doEsearch("&term=Weishaar Tom[Author]")
# doEsearch("&term=Tom Weishaar")
# doEsearch("&term=10.1002/14651858.CD007470.pub3[All Fields]")

   # function to search PubMed using the human interface
PubMedhtml2pmid = function(terms) {
   pauseR(300)                                                  # force 300 milliseconds between calls
   terms = str_replace_all(terms, "\\[", "%5B")                 # replace brackets with entities
   terms = str_replace_all(terms, "\\]", "%5D")
   url = paste0("https://www.ncbi.nlm.nih.gov/pubmed/?term=", str_replace_all(terms, " ", "+")) # fix spaces
   raw = readLines(url, warn=FALSE)
   id = str_match(raw, "<dd><a href=\"/pubmed/(.*?)\"")          # this finds the PMID on some pages
   id = id[!is.na((id[,2])),2]                                   # this pulls it out of the str_match matrix
   if(length(id)==0) {
      id = str_match(raw, "<dt>PMID:</dt> <dd>(.*?)</dd>")       # this finds the PMID on some other pages
      id = id[!is.na((id[,2])),2]                                # this pulls it out of the str_match matrix
   }
   id = str_replace(id, '<span class=\"highlight\" style=\"background-color:\">', "")  # only a few result in this
   id = str_replace(id, '</span>', "")
   return(id)
}
   # function to search PubMed given terms, dates, and maxhits; returns a list
pubmed_search = function(terms, max=prj$options$maxhits, from_date="0001/01/01", to_date=Sys.time()){
   terms_in = terms                                              # save for output
      # update terms to include RetMax and search from date: yyyy/mm/dd as a string
   from_date=str_sub(from_date,1,10)
   to_date=str_sub(to_date,1,10)
   terms = paste0('&RetMax=', max, '&term=("', from_date, '"[MDAT]:"', to_date , '"[MDAT]) AND ', terms)
   r = doEsearch(terms)
   if(as.numeric(r$count)>as.numeric(max) & as.numeric(max)>1) {                  # make sure we didn't get more hits than we can handle
      gl$modal_title <<- "Processing Error!"  # tiny max means testing - just looking for number of hits
      gl$modal_text <<- paste0("Number of hits (", r$count, ") exceeds maximum allowed (", max, ") - see Settings.")
      rv$modal_warning <<- rv$modal_warning+1
      return(FALSE)
   }
   return(list(
         query = r$query,
         search_hits = r$count,
         pmids = r$pmids
   ))
}

   # given a vector of pmids, this returns a raw xml file of bibliographic data
pubmed.fetch = function(pmids){
   pmids = paste0(pmids, collapse=",")
   url = paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed",
                "&tool=rct.app&email=jtw2117@columbia.edu",
                "&retmode=xml",
                "&id=", pmids)
   pauseR(300)                                                    # force 300 milliseconds between calls
   raw = read_xml(url)  # result is an XML file
   return(raw)
}

   # given a hits table containing pmids, this function retrieves the bibliographic data from PubMed
   #   and places it in the table
pmid2r = function(r, chunksize=500) {
   n = nrow(r)
   if(n>0) {                                                            # on reload of same file we have them all already
      withProgress(value=1, max=n, message="Getting PubMed data: ", {
            # prep return table
         pmidList <- chunker(r$pmid, chunksize)
         for(chunk in (1:length(pmidList))) {
            raw = pubmed.fetch(pmidList[[chunk]])                           # get the xml for a group from pubmed
            nb = length(xml_find_all(raw, "PubmedBookArticle"))
            nj = length(xml_find_all(raw, "PubmedArticle"))
            if(nb>0) {                                                  # first do any books
               for(i in 1:nb) {
                  pathA = paste0("/PubmedArticleSet/PubmedBookArticle[", i, "]/BookDocument/")
                  pmid = xml_text(xml_find_first(raw, paste0(pathA, "PMID")))
                  rI = which(r$pmid==pmid)                              # index into r
                  r[rI,"ptype"] = "b"
                  r[rI,"journal"] = paste0(xml_text(xml_find_first(raw, paste0(pathA, "Book/Publisher/PublisherName"))),
                                           ", ", xml_text(xml_find_first(raw, paste0(pathA, "Book/Publisher/PublisherLocation"))))
                  r[rI,"Y"] = xml_text(xml_find_first(raw, paste0(pathA, "Book/PubDate/Year")))
                  r[rI,"title"] = xml_text(xml_find_first(raw, paste0(pathA, "ArticleTitle")))
                  lasts = xml_text(xml_find_all(raw, paste0(pathA, "AuthorList/Author/LastName")))
                  firsts = xml_text(xml_find_all(raw, paste0(pathA, "AuthorList/Author/ForeName")))
                  r[rI,"authors"] = convertAuthors(paste0(lasts, ", ", firsts))
                     # suck out the abstract
                  abstract=""
                  id_types = xml_attr(xml_find_all(raw, paste0(pathA, "Abstract/AbstractText")), "Label")
                  if(any(is.na(id_types))) {
                     abstract = xml_text(xml_find_all(raw, paste0(pathA, "Abstract/AbstractText")))
                  } else {
                     id_vec = vector(mode="character", length=length(id_types))
                     names(id_vec) = id_types
                     for(t in 1:(length(id_types))) {
                        id_vec[id_types[t]] = xml_text(xml_find_all(raw, paste0(pathA, "Abstract/AbstractText[", t, "]")))
                        abstract = paste0(abstract, "<p><b>", id_types[t], ":</b> ", id_vec[id_types[t]], "</p>")
                     }
                  }
                  r[rI,"abstract"] = abstract
                  setProgress(rI, detail=paste0(rI, " of ", n))
               }
            }
            if(nj>0) {
               for(i in 1:nj) {                                          # number of pmids in this group
                  pathA = paste0("/PubmedArticleSet/PubmedArticle[", i, "]/MedlineCitation/")  # these are paths within the xml
                  pathB = paste0("/PubmedArticleSet/PubmedArticle[", i, "]/PubmedData/")       #    called "xpaths"; i is the
                                                                                               #    article number in this group
                     # suck out the ids
                  id_types = xml_attr(xml_find_all(raw, paste0(pathB, "ArticleIdList/ArticleId")), "IdType")
                  id_vec = vector(mode="character", length=length(id_types))
                  names(id_vec) = id_types
                  for(t in 1:(length(id_types))) {
                     id_vec[id_types[t]] = xml_text(xml_find_all(raw, paste0(pathB, "ArticleIdList/ArticleId[", t, "]")))
                  }
                  rI = which(r$pmid==id_vec["pubmed"])                   # index into r
                  ptype = xml_text(xml_find_all(raw, paste0(pathA, "Article/PublicationTypeList/PublicationType")))
                  if("JOURNAL ARTICLE" %in% str_to_upper(ptype)) {ptype = "j"}
                  r[rI,"ptype"] = paste0(ptype, collapse=", ") # just in case it's a vector of types, none of which is JOURNAL ARTICLE
                  r[rI,"Y"] = xml_text(xml_find_first(raw, paste0(pathA, "Article/Journal/JournalIssue/PubDate/Year")))
                     if(is.na(r[rI,"Y"])){
                        r[rI,"Y"] = substr(xml_text(xml_find_first(raw, paste0(pathA, "Article/Journal/JournalIssue/PubDate/MedlineDate"))),1,4)
                     }
                  r[rI,"V"] = xml_text(xml_find_first(raw, paste0(pathA, "Article/Journal/JournalIssue/Volume")))
                  r[rI,"N"] = xml_text(xml_find_first(raw, paste0(pathA, "Article/Journal/JournalIssue/Issue")))
                  r[rI,"P"] = xml_text(xml_find_first(raw, paste0(pathA, "Article/Pagination/MedlinePgn")))
                  lasts = xml_text(xml_find_all(raw, paste0(pathA, "Article/AuthorList/Author/LastName")))
                  firsts = xml_text(xml_find_all(raw, paste0(pathA, "Article/AuthorList/Author/ForeName")))
                  r[rI,"authors"] = convertAuthors(paste0(lasts, ", ", firsts))
                  r[rI,"title"] = xml_text(xml_find_first(raw, paste0(pathA, "Article/ArticleTitle")))
                  r[rI,"journal"] = gsub("\\.", "", xml_text(xml_find_first(raw, paste0(pathA, "Article/Journal/ISOAbbreviation"))))
                  r[rI,"doi"] = id_vec["doi"]
                  r[rI,"pmcid"] = id_vec["pmc"]
                     # suck out the abstract
                  abstract=""
                  id_types = xml_attr(xml_find_all(raw, paste0(pathA, "Article/Abstract/AbstractText")), "Label")
                  if(any(is.na(id_types))) {
                     abstract = xml_text(xml_find_all(raw, paste0(pathA, "Article/Abstract/AbstractText")))
                  } else {
                     id_vec = vector(mode="character", length=length(id_types))
                     names(id_vec) = id_types
                     for(t in 1:(length(id_types))) {
                        id_vec[id_types[t]] = xml_text(xml_find_all(raw, paste0(pathA, "Article/Abstract/AbstractText[", t, "]")))
                        abstract = paste0(abstract, "<p><b>", id_types[t], ":</b> ", id_vec[id_types[t]], "</p>")
                     }
                  }
                  r[rI,"abstract"] = abstract
                  rI = rI+1
                  setProgress(rI, detail=paste0(rI, " of ", n))
               }
            }
         }
      }) # withProgress
      return(r)
   }
}

#r=pmid2r(prj$searches$pubmed$hits)
#r
   # some code for getting the url to look at the raw xml in a browser
#pmids=paste(prj$searches$pubmed$hits, collapse=",")
#pmids = 25646340
#paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed",
#    "&tool=rct.app&email=jtw2117@columbia.edu", "&retmode=xml", "&id=", pmids)

# raw = read_xml(paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed",
#      "&tool=rct.app&email=jtw2117@columbia.edu", "&retmode=xml", "&id=20301770,26957379,26868735,25646340,25420177,23415504"))
#
# raw = read_xml(paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed",
#      "&tool=rct.app&email=jtw2117@columbia.edu", "&retmode=xml", "&id=", paste0(prj$searches$pubmed$hits, collapse=",")))

   # Given a tibble of "standard" citation data, this returns the tibble with a pmid in
   #    the $Rid where possible.
getPMIDs = function(r) {
      # internal function;
      #    given the PMIDs of hits from a search,
      #    which one has a title starting like the one we're looking for
   confirmByTitle = function(pmids, title) {
      nids = length(pmids)                               # number of pmids
      tt = tibble(id=rep("", nids), title=rep("", nids)) # a title tibble
      raw = pubmed.fetch(pmids)                          # get the xml for these pmids from pubmed
      for(t in (1:nids)) {                               # cycle through ids building tt
         path = paste0("/PubmedArticleSet/PubmedArticle[", t, "]/MedlineCitation/")
         tt$id[t] = xml_text(xml_find_first(raw, paste0(path, "PMID")))
         tt$title[t] = str_sub(xml_text(xml_find_first(raw, paste0(path, "Article/ArticleTitle"))), 1, 30)
      }
         # search title tibble for our title; could get 0, 1, or multiple hits
      tids = tt$id[which(str_to_upper(tt$title) %in% str_to_upper(title))] # magic sauce
   }

   withProgress(value=1, max=nrow(r), message="Getting PMIDs: ", {
      for(i in 1:nrow(r)) {
         setProgress(i, detail=paste0(i, " of ", nrow(r)))
         # first see if we already have a pmid, if so, skip (possible with Embase/Cochrane)
         if(!is.na(r$pmid[i]) && nchar(r$pmid[i])>0) { r$pmidOK[i] <- TRUE; next }
         # next search by doi, if we have one...
         if(r$doi[i]!="") {
            pmids = doEsearch(paste0("&term=", r$doi[i],"[All Fields]"))[["pmids"]]
            if(length(pmids)==1) {
               r$pmid[i] <- pmids
               r$pmidOK[i] <- TRUE        # If we got the PMID from a doi, then mark as OK
               next
            }
         }
         # if doi didn't work, try journal-authors last names-year
         js = journalSearch(r$journal[i])
         J = "("                                               # works for journal not found
         if(js$found>0) {
            J = paste0("((", paste0(js$jName, collapse="[Journal]) OR ("), "[Journal])) AND (")
         }
         A = convertAuthors(r$authors[i], return_string=FALSE, type="last")
         A = paste0(A, collapse="[Author]) AND (")
         Y = paste0(A, "[Author]) AND (", r$Y[i], "[PDAT] : ", r$Y[i], "[PDAT])")
         JAY = paste0(J,Y)

         pmids = doEsearch(paste0("&term=", JAY))[["pmids"]]
         if(length(pmids)>0) {
            pmids = confirmByTitle(pmids, str_sub(r$title[i], 1, 30))
            if (length(pmids)==1) { r$pmid[i] <- pmids; next }              # try JAY
         }
         JAYids = pmids

         # if JAY didn't work, try journal-year-volume-number-page
         Y = r$Y[i]; V = r$V[i]; N = r$N[i]; P = r$P[i]          # JYVNP = year, volumne, number, page
         JYVNP = paste0(J, Y, "[Date - Publication] : ", Y, "[Date - Publication])")
         if(nchar(V)>0) {JYVNP = paste0(JYVNP, " AND (", V, "[Volume])")}
         if(nchar(N)>0) {JYVNP = paste0(JYVNP, " AND (", N, "[Issue])")}
         if(nchar(V)>0) {JYVNP = paste0(JYVNP, " AND (", P, "[Pagination])")}

         pmids = doEsearch(paste0("&term=", JYVNP))[["pmids"]]
         if(length(pmids)>0) {
            pmids = confirmByTitle(pmids, str_sub(r$title[i], 1, 30))
            if (length(pmids)==1) { r$pmid[i] <- pmids; next }              # try JAY
         }
         JYVNPids = pmids
         pmids = unique(c(JAYids, JYVNPids))
         if(length(pmids)>0) {
            pmids = confirmByTitle(pmids, str_sub(r$title[i], 1, 30))
            if (length(pmids)==1) { r$pmid[i] <- pmids; next }              # try JAY
         }

            # if not found yet, try the actual PubMed interface, which sometimes does a better query translation
            #    all of these have found something...
         pmids = PubMedhtml2pmid(JAY)
         if(length(pmids)>0) {
            pmids = confirmByTitle(pmids, str_sub(r$title[i], 1, 30))
            if (length(pmids)==1) { r$pmid[i] <- pmids; next }              # try JAY
         }
         pmids = PubMedhtml2pmid(JYVNP)
         if(length(pmids)>0) {
            pmids = confirmByTitle(pmids, str_sub(r$title[i], 1, 30))
            if (length(pmids)==1) { r$pmid[i] <- pmids; next }              # try JAY
         }
         title = str_replace(r$title[i], " \\(Provisional abstract\\)", "")
         title = str_replace(title, " \\(Structured abstract\\)", "")
         pmids = PubMedhtml2pmid(title)
         if(length(pmids)>0) {
            pmids = confirmByTitle(pmids, str_sub(r$title[i], 1, 30))
            if (length(pmids)==1) { r$pmid[i] <- pmids; next }              # try JAY
         }

            # if we get here, nothing was found and we retain Rid
      }
   }) # withProgress
   return(r)
}


cochrane_search = function(file_raw) {
      # number of records according to first line of file
   nrecs = as.numeric(str_split(file_raw[1], " ")[[1]][4])
   r = buildR(nrecs)                            # standard table for storage
      # go through file line by line and get the stuff we're interested in
   file_lines = str_split(file_raw, " ")        # explode file into vectors of individual words in a list of lines
   rec = 0                                      # index for output data frame
               # iterate
   withProgress(value=0, max=nrecs, message="Reading file: ", {
      for(ln in 1:(length(file_lines))) {
         setProgress(rec, detail=paste0(rec, " of ", nrecs))
         switch(str_sub(file_lines[[ln]][1],1,2),  # get line's first word, first 2 characters
            Re = {                                 # new record
                  rec = rec+1
                 },
            AU = {
                  thisAuthor = convertAuthors(paste0(file_lines[[ln]][-1], collapse=" "), incoming_separators = c(",", " "))
                  if(r[rec,"authors"]=="") {         # all authors
                     r[rec,"authors"] =  thisAuthor
                 } else {
                     r[rec,"authors"] =  paste0(r[rec,"authors"], "; ", thisAuthor)
                 }},
            TI = {
                 r[rec,"title"] = paste0(file_lines[[ln]][-1], collapse=" ")
                 },
            SO = {
                 r[rec,"journal"] = paste0(file_lines[[ln]][-1], collapse=" ")
                 js = journalSearch(r[rec,"journal"])
                 r[rec,"ptype"] = as.character(js$found)
                 if(js$found==1) {
                    r[rec,"ptype"] = "j"
                    r[rec,"journal"] = js$jName
                 }},
            YR = {
                 r[rec,"Y"] = paste0(file_lines[[ln]][-1], collapse=" ")
                 },
            VL = {                                                           # this means all the words in the line,
                 r[rec,"V"] = paste0(file_lines[[ln]][-1], collapse=" ")     #   except the first, pasted back together
                 },                                                          #   (first word is the RIS-like code)
            NO = {
                 r[rec,"N"] = paste0(file_lines[[ln]][-1], collapse=" ")
                 },
            PG = {
                 r[rec,"P"] = paste0(file_lines[[ln]][-1], collapse=" ")
                 },
            PM = {
                 r[rec,"pmid"] = paste0(file_lines[[ln]][c(-1,-2)], collapse=" ")  # c(-1,-2) to remove "PUBMED", too
                 },
#            XR = {  # This is the EMBASE id
#                 r[rec,???] = paste0(file_lines[[ln]][c(-1,-2)], collapse=" ")
#                 },
            DO = {
                 r[rec,"doi"] =  paste0(file_lines[[ln]][-1], collapse=" ")
                 },
            AB = {
                 r[rec,"abstract"] = paste0(file_lines[[ln]][-1], collapse=" ")
                 }
         )
      }
   })
   return(getPMIDs(r))
}

webofsci_search = function(file_raw) {
   if(file_raw[2]!="VR 1.0") {
      gl$modal_title <<- "Processing Error!"
      gl$modal_text <<- "Web of Science .ciw file format has changed."
      rv$modal_warning <<- rv$modal_warning+1
      rv$render_SearchList <<- rv$render_SearchList+1                    # re-render list
      return(FALSE)
   }
   file_raw = file_raw[-c(1:2)]                 # get rid of first two lines of the file
   nrecs = sum(str_sub(file_raw, 1,2)=="ER")
   r = buildR(nrecs)                            # standard table for storage
      # go through file line by line and get the stuff we're interested in
   file_lines = str_split(file_raw, " ")        # explode file into vectors of individual words in a list of lines
   rec = 0                                      # index for output data frame
   cites = ""                                   # vector for storing cited references
   code = ""                                    # initialize
               # iterate
   withProgress(value=0, max=nrecs, message="Reading file: ", {
      for(ln in 1:(length(file_lines))) {
         setProgress(rec, detail=paste0(rec, " of ", nrecs))
         switch(str_sub(file_lines[[ln]][1],1,2),  # get line's first word, first 2 characters
            PT = {                                 # new record
                  rec = rec+1
#                  r$ptype[rec] = str_to_lower(file_lines[[ln]][-1])   # see DT
                 },
            PM = {
                 pmid = paste0(file_lines[[ln]][-1], collapse=" ")
                 if(pmid!=""){
                    r$pmid[rec] = pmid
                 }
                 },
            DT = {
                 type = paste0(file_lines[[ln]][-1], collapse=" ")
                 if(type=="Article") {type = "j"}
                 r[rec,"ptype"] = type
                 },
            SO = {
                 r[rec,"journal"] = paste0(file_lines[[ln]][-1], collapse=" ")
                 js = journalSearch(r[rec,"journal"])
                 if(js$found==1) {
                    r[rec,"journal"] = js$jName
                 }},
            TI = {
                 code=""                                                               # end of authors
                 r$authors[rec] = authors
                 r$title[rec] = paste0(file_lines[[ln]][-1], collapse=" ")
                 },
            SO = {
                 js = journalSearch(paste0(file_lines[[ln]][-1], collapse=" "))          # journal
                 if(js$found==1) {
                    r$journal[rec] = js$jName
                 }},
            SN = {
                 if(js$found==0) {                                                       # ISSN
                    js = journalSearch(paste0(file_lines[[ln]][-1], collapse=" "))
                    if(js$found==1) {
                       r$journal[rec] = js$jName
                    }
                 }},
            PY = {
                 r$Y[rec] = paste0(file_lines[[ln]][-1], collapse=" ")                 # year
                 },
            VL = {                                                           # this means all the words in the line,
                 r$V[rec] = paste0(file_lines[[ln]][-1], collapse=" ")     #   except the first, pasted back together
                 },                                                          #   (first word is the RIS-like code)
            IS = {
                 r$N[rec] = paste0(file_lines[[ln]][-1], collapse=" ")                 # issue number
                 },
            BP = {
                 r$P[rec] = paste0(file_lines[[ln]][-1], collapse=" ")                 # beginning page
                 },
            EP = {
                 r$P[rec] = paste0(r$P[rec], "-", paste(file_lines[[ln]][-1], collapse=" ")) # end page
                 },
            DI = {
                 r$doi[rec] =  paste0(file_lines[[ln]][-1], collapse=" ")              # doi
                 },
            AB = {
                 code="AB"
                 r$abstract[rec] = paste0(file_lines[[ln]][-1], collapse=" ")          # abstract
                 },
            AF = {                                                                     # author full name
                 code="AF"
                 authors = paste0(file_lines[[ln]][-1], collapse=" ")
                 },
            CR = {
                 code="CR"
                 cites[rec] = paste0(file_lines[[ln]][-1], collapse=" ")
                 },
            NR = {
                 code=""                                                               # end of cited refs
                 },
         # now do lines with blank start or alternate codes
         {  line=paste0(file_lines[[ln]], collapse=" ")
            if(str_sub(line,1,2)=="  ") {  # if line doesn't start with 2 blanks, skip it
               if(code=="AB") {                                                     # more abstract
                    r$abstract[rec] = paste0(r$abstract[rec], "<br>", str_sub(line,4,-1))
               }
               if(code=="AF") {                                                     # more authors
                    authors = paste0(authors, "; ", str_sub(line,4,-1))
               }
               if(code=="CR") {                                                     # more cited refs
                    cites[rec] = paste0(cites[rec], ";", str_sub(line,4,-1))
               }
            }
         })
      }
   })
   r$L = "1"
   r = getPMIDs(r)
   return(list(r=r, cites=cites))
}

   # this processes the cited references embedded in Web of Science citations
   # r$ needs to have Sid and originating Rid. Also PMID lookup?
process_cites = function(cites) {
   big_cites = paste0(cites, collapse=";")             # pastes everything into one string
   just_cites = unlist(str_split(big_cites, ";"))      # explodes string into individual cites
   unique_cites = unique(just_cites)                   # deletes duplicates
   cite_items = str_split(unique_cites, ", ")          # explodes individual cites into items
   r = buildR(length(cite_items))
   for(i in 1:nrow(r)) {
      vol = page = doi = ""
      r$authors[i] = str_trim(cite_items[[i]][1])
      r$Y[i] =       str_trim(cite_items[[i]][2])
      r$journal[i] = str_trim(cite_items[[i]][3])
      item4 = str_trim(cite_items[[i]][4])
      switch(str_sub(item4, 1, 1),
         V = { vol = item4 },
         P = { page = item4 },
         D = { doi = item4 }
      )
      item5 = str_trim(cite_items[[i]][5])
      switch(str_sub(item5, 1, 1),
         P = { page = item5 },
         D = { doi = item5 }
      )
      item6 = str_trim(cite_items[[i]][6])
      switch(str_sub(item6, 1, 1),          # The trick here is that if item6 is "", doi won't get overwritten
         D = { doi = item6 }
      )
      r$V[i]   = str_sub(vol, 2, -1)
      r$P[i]   = str_sub(page, 2, -1)
      r$doi[i] = str_sub(doi, 5, -1)
   }
   r$L = "2"
   return(getPMIDs(r))
}

   # For Embase, there are two "export" buttons. Use the one in the "Results" row, not the "History" row.
   #   Then pick "CSV - Fields by Column" and "Full Record"
embase_search = function(r.csv) {
   r = buildR(nrow(r.csv))
   r$ptype = "u"
   source = str_split(r.csv$Source, "\\(")
   journal = unlist(lapply(source, function(x) x[1]))
   yvn = str_split(unlist(lapply(source, function(x) x[2])), "\\)")
   year = unlist(lapply(yvn, function(x) x[1]))
   vn =  str_split(unlist(lapply(yvn, function(x) x[2])), "\\:")
   vol = unlist(lapply(vn, function(x) x[1]))
   num = unlist(lapply(vn, function(x) x[2]))
   pages = str_split(unlist(lapply(source, function(x) x[3])), "\\)")
   r$Y = year
   r$V = str_trim(vol)
   r$N = str_trim(num)
   r$P = str_trim(unlist(lapply(pages, function(x) x[1])))
   r$title = r.csv[[1]]
   r$doi = str_sub(r.csv$Full.Text.Link, 19, -1)
   r$abstract = r.csv$Abstract
   r$pmid = str_trim(r.csv$Medline.PMID)
   r$pmidOK = ifelse(r$pmid=="", FALSE, TRUE)
   for(i in 1:nrow(r.csv)) {
      r$authors[i] = convertAuthors(r.csv$Author.Names[i], incoming_separators=c(",", " "))
      r$journal[i] = journalSearch(journal[i])$jName
   }
   return(getPMIDs(r))
}

ovid_search = function(file_raw) {
   if(file_raw[1]!="1. ") {
      gl$modal_title <<- "Processing Error!"
      gl$modal_text <<- "Ovid .ris file format has changed."
      rv$modal_warning <<- rv$modal_warning+1
      return(FALSE)
   }
   nrecs = sum(str_sub(file_raw, 1,2)=="ER")    # end of item marker
   r = buildR(nrecs)
   i = which(str_sub(file_raw, 1,2)=="ID")      # find ID (pmid) cells in file_raw vector
   if(length(i)==nrecs) {
      r$pmid = str_sub(file_raw[i], 7, -1)      # if there's PMID for each citation, put them in r
      r$pmidOK = TRUE                # Don't know whether a missing PMID means a missing ID or an ID with
   } else {     # a blank in Ovid's .ris protocol. Either case will be a problem for this minimalist code
      gl$modal_title <<- "Processing Error!"
      gl$modal_text <<- "In Ovid.ris there are missing PMIDs."
      rv$modal_warning <<- rv$modal_warning+1
      return(FALSE)
   }
   if(any(is.na(r$pmid)) || any(r$pmid=="")) {
      gl$modal_title <<- "Processing Error!"
      gl$modal_text <<- "In Ovid.ris there are missing PMIDs."
      rv$modal_warning <<- rv$modal_warning+1
      return(FALSE)
   }
   if(any(duplicated(pmids))) {
      gl$modal_title <<- "Processing Error!"
      gl$modal_text <<- "In Ovid.ris there are duplicate PMIDs."
      rv$modal_warning <<- rv$modal_warning+1
      rv$render_SearchList <<- rv$render_SearchList+1                    # re-render list
      return(FALSE)
   }
   return(pmid2r(r))
}

check_for_duplicate_filename = function(Sid) {
print(paste0("In check for duplicate filename, Sid=", Sid))
   dupfiles = duplicated(prj$sourceInfo$filename)
   if(dupfiles[which(prj$sourceInfo$Sid==Sid)]) {
      gl$modal_title <<- "Note."
      gl$modal_text <<- "This file is also in an earlier search."
      rv$modal_warning <<- rv$modal_warning+1
   }
}
   # rv$process_Sid is just a +1 that starts the process; gl$process_Sid is the actual SID
observeEvent(rv$process_Sid, {
   if(rv$process_Sid==0) { return() }                          # skip running at init
   thisSid_i = which(prj$sourceInfo$Sid==gl$process_Sid)
   tibSid = prj$sourceInfo[thisSid_i,]
   hitsL1 = hitsL2 = "0"
   switch(tibSid$db,
      "PubMed-Live"={
         sr = pubmed_search(tibSid$terms, from_date=tibSid$from_date, to_date=tibSid$to_date )
         if(is.list(sr)) {
            r = buildR(length(sr$pmids))
            r$pmid = sr$pmids
            r$pmidOK <- TRUE
            r = pmid2r(r)
            hitsL1 = as.character(nrow(r))
         } else {
            rv$render_SearchList <<- rv$render_SearchList+1    # error; handled in pubmed_search
            return()
         }
         },
      "Cochrane-TXT"={
         check_for_duplicate_filename(gl$process_Sid)
         r = cochrane_search(readLines(tibSid$filename))
         hitsL1 = as.character(nrow(r))
      },
      "Web of Knowledge-CIW"={
         check_for_duplicate_filename(gl$process_Sid)
         ws = webofsci_search(readLines(tibSid$filename))
         if(is.list(ws)) {                                        # No error
            r=ws$r                                                # Level 1 only
            hitsL1 = as.character(nrow(r))
            if(tibSid$citeLevel=="Level 2 only") {
               r = process_cites(ws$cites)
               hitsL2 = as.character(nrow(r))
            }
            if(tibSid$citeLevel=="Both") {
               r2 = process_cites(ws$cites)
               hitsL2 = as.character(nrow(r2))
               r = rbind(r, r2)
            }
         }                                                        # errors handled in webofsci_search()
      },
      "Embase-CSV"={
         check_for_duplicate_filename(gl$process_Sid)
         r = embase_search(read.csv(tibSid$filename, sep=",", stringsAsFactors = FALSE))
         hitsL1 = as.character(nrow(r))
      },
      "Ovid-RIS"={
         check_for_duplicate_filename(gl$process_Sid)
         r = ovid_search(readLines(tibSid$filename))
         if(is.data.frame(r)) {                                                 # check for errors
            hitsL1 = as.character(nrow(r))
         } else {
            rv$render_SearchList <<- rv$render_SearchList+1                    # on error, re-render list
            return()
         }
      },
      "PMIDs-TXT"={
         check_for_duplicate_filename(gl$process_Sid)
         pmids = readLines(tibSid$filename)   # expects a .txt file with a PMID on each line
         if(any(duplicated(pmids))) {
            gl$modal_title <<- "Processing Error!"
            gl$modal_text <<- "File contains duplicate PMIDs."
            rv$modal_warning <<- rv$modal_warning+1
            rv$render_SearchList <<- rv$render_SearchList+1                    # re-render list
            return()
         } else {
            r = buildR(length(pmids))            #   a character vector of PMIDs
            r$pmid <- pmids
            r$pmidOK <- TRUE
            r <- pmid2r(r)
            hitsL1 <- as.character(nrow(r))
         }
      }
   )
   r$Sid = tibSid$Sid
   if(!is.data.frame(prj$hits)) {
      prj$hits <<- r
   } else {
      prj$hits <<- rbind(prj$hits, r)
   }
   prj$sourceInfo[thisSid_i, "status"] <<- "processed"
   prj$sourceInfo[thisSid_i, "processed_date"] <<- Sys.time()
   prj$sourceInfo[thisSid_i, "hitsL1"] <<- hitsL1
   prj$sourceInfo[thisSid_i, "hitsL2"] <<- hitsL2
   save_prj()
   rv$render_SearchList <<- rv$render_SearchList+1                    # re-render list
})



