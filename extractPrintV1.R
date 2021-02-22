
# Scrape Citations from a document and provide a unique ordered reference list. 

#
# Create Functions 
#

findAuthors <- function(txt) {
  
  #Personsname = "([A-Z][a-z'’`-]+)"
  Personsname = "([a-zA-Z\\u0080-\\uFFFF-]+)"
  YearPattern = "(, *(19|20)[0-9][0-9]| ?\\( *(19|20)[0-9][0-9]\\))"
  etal = "( et al.?)"
  andconj = paste0(Personsname, " and ", Personsname)
  andconj1 = paste0(Personsname, ", and ", Personsname)
  andconj2 = paste0(Personsname, ", ", Personsname, ", and ", Personsname)
  andconj3 = paste0(Personsname, ", ", Personsname, ", ", Personsname, ", and ", Personsname)
  andconj4 = paste0(Personsname, " & ", Personsname)
  andconj5 = paste0(Personsname, ", & ", Personsname)
  andconj6 = paste0(Personsname, ", ", Personsname, ", & ", Personsname)
  andconj7 = paste0(Personsname, ", ", Personsname, ", ", Personsname, ", ", Personsname, ", and ", Personsname)
  andconj8 = paste0(Personsname, ", ", Personsname, ", ", Personsname, ", ", Personsname, ", & ", Personsname)
  andconj9 = paste0(Personsname, ", ", Personsname, ", ", Personsname, ", & ", Personsname)
  commaconj = paste0(Personsname, ", ", "(", Personsname, "|", "et al.?", ")")
  totcit = paste0(Personsname, "?", etal, "?", "(", andconj, "|", andconj1, "|", andconj2, "|", 
                  andconj3, "|", andconj4, "|", andconj5, "|", andconj6, "|", andconj7, "|", 
                  andconj8, "|", andconj9, "|", commaconj, ")*", etal, "?", YearPattern)
  
  authorList <- str_extract_all(txt, totcit)
  
  return(authorList)
}

extractDates = function(authorDate) {
  YearPattern <- "((19|20)[0-9][0-9]| ?\\((19|20)[0-9][0-9]\\))"
  allDates <- str_extract_all(authorDate, YearPattern)
  return(allDates)
}

extractAllDatesAuthors = function(txt) {
  
  allAuthors <- findAuthors(txt)
  allDatesList = list()
  
  for(i in 1:length(allAuthors[[1]])) {
    citeAll = allAuthors[[1]][i]
    citeDate = extractDates(allAuthors[[1]][i])
    name <- paste('item:',i,sep='')
    tmp <- list(date = citeDate, cite = citeAll)
    allDatesList[[name]] <- tmp
  }
  
  allCiteDate <- data.table::rbindlist(allDatesList, fill=TRUE)
  
  return(allCiteDate)
  
}

sortDatesUnique = function(df) {
  allCiteSort <- df %>% 
    mutate(dateCl = readr::parse_number(as.character(date))) %>% 
    select(-date) %>% 
    mutate(fixedName = ifelse(
      str_starts(word(cite), "[a-z]"),
      word(cite, 2, -1), 
      word(cite, 1, -1)), 
      fixedName2 = str_trim(gsub('[[:digit:](),]+', '', fixedName), 
               side = c("both", "left", "right")))  %>% 
    arrange(dateCl, fixedName2) %>%
    distinct(dateCl, fixedName2, .keep_all = TRUE) %>% 
    select(dateCl, fixedName) %>% 
    rename("PublishDate" = dateCl, "FullCitation" = fixedName)
  
  return(allCiteSort)
}

newFileName = function(fileName) {
  newName = gsub(" ", "",fileName)
  newFileName = paste0(substr(newName,1,nchar(newName)-5), "_CITATIONS.csv")
  return(newFileName)
}

#
#
# End Functions 
#
#

# Load Packages 
library(dplyr)
library(tidyr)
library(stringr)
library(readtext)

# Load Text File 

wd = "/working/directory/"
readFileName = "filename.docx"

setwd(wd)
fullText = readtext(file = readFileName)

# Extract Dates and Authors 
datesAuthors = sortDatesUnique(extractAllDatesAuthors(fullText$text))

# Save Outcome 
write.csv(datesAuthors, file = newFileName(readFileName), row.names = FALSE)
