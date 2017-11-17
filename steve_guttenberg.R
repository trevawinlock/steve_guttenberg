#### Group Project Text Mining ####

## Get Book's and Book Information from Gutenberg Library ##
## Create getbooks Function ##

## Final Outputs in the Global Enviornment is Books labled ID(Gutenberg Number) and a Data Frame with ## 
## the subjects of all the books pulled ##

## The input for the Function is a Number for amount of Books to Randomly Select ##

## For 100 Books, the function takes roughly 48 seconds to run ##

#Load Libraries
library(gutenbergr)
library(tidyverse)
library(purrr)


getbooks <- function(x){
        
#Set X to Numeric Value
y <- as.numeric(x)

#Get all Books and IDS from Gutenberg
data <- gutenberg_metadata

#Filter books with no text, non-english books, and remove presient speeches and other speeches 
#or congressional statements
data <- filter(data, has_text == "TRUE" )
data <- filter(data, language == "en" )
data <- filter(data, author != ("Jefferson, Thomas")) 
data <- filter(data, author != ("United States"))
data <- filter(data, author != ("Lincoln, Abraham"))
data <- filter(data, author != ("United States. Central Intelligence Agency"))
data <- filter(data, author != ("Kennedy, John F. (John Fitzgerald)"))
data <- filter(data, author != ("Henry, Patrick"))
data <- filter(data, author != ("United States. Bureau of the Census"))
data <- filter(data, author != ("Library of Congress"))
data <- filter(data, author != ("Unknown"))
data <- filter(data, author != ("Roosevelt, Franklin D. (Franklin Delano)"))
data <- filter(data, author != ("Beethoven, Ludwig van"))
data <- filter(data, author != ("Various"))
data <- filter(data, author != ("United States. Work Projects Administration"))
data <- filter(data, author != "Anonymous")


#Filter out Books that do not have a Category
data <- filter(data, gutenberg_bookshelf != "NA")

#Randomly Chose X Amount of Books
bookid <- data[sample(nrow(data), y), ]

#Create Null Variables for Loop
listofbooks <- NULL
book <- NULL

#Loop through the random IDS and pull the book text

#100 Books takes roughly 209 seconds to run
  for (i in 1:nrow(bookid)) {
    book <- gutenberg_download(bookid[i,1])
    listofbooks[[i]] <- book
  }

#Loop through list and extract each element as dataframe to enviornment and returns a dataframe
#of IDS of books
id <- NULL
ids <- NULL

#100 Books takes roughly 34 seconds to run and returned 19 books
  for (i in 1:length(listofbooks)) {
    
          #Removes Books that have more then 4000 observations or less then 500 observations
    if(nrow(data.frame(map(listofbooks[i], "text"))) > 7000 | nrow(data.frame(map(listofbooks[i], "text"))) < 500){
      next
    } else {
      
        #Prints the Book to the Enviornment with the title ID + gutenberg_id
        assign(paste0("ID", listofbooks[[i]]$gutenberg_id[1]), as.data.frame(listofbooks[[i]]), envir=globalenv())
      
        #Creates Dataframe of gutenberg_id used
        id <- listofbooks[[i]]$gutenberg_id[1]
        ids[[i]] <- id
        ids <- data.frame(matrix(unlist(ids), byrow=T),stringsAsFactors=FALSE)
        ids <- na.omit(unique(ids))
      
      
    }
  }

bookid$gutenberg_id <- as.numeric(bookid$gutenberg_id)
ids[,1] <- as.numeric(ids[,1])
colnames(ids) <- "gutenberg_id"

#Filter out Book IDS that we did not keep
bookid <- filter(bookid, gutenberg_id %in% ids$gutenberg_id)

#Loop through IDS and link the subject of the book to the book id
subject <- NULL
subjects <- NULL


for (i in 1:nrow(ids)) {
  
  
  #Selects the Gutenberg Bookshelf Classification for Each ID
  subject <- bookid$gutenberg_bookshelf[i]
  
  #Stores Subjects of IDS to List
  subjects[[i]] <- subject
}

subjects <- as.data.frame(subjects)

#Merge Subjects to ID
idinfo <<- cbind(ids, subjects)

}


