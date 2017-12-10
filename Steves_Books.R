
#### Group Project Text Mining ####

## Treva Winlock, Patrick Passafiume, Derek Kopecky, Ian O'Connor ##


### Data Extraction ###

#### Create getbooks Function ####

## Get Book and Book Information from Gutenberg Library ##

## Final Outputs in the Global Enviornment is Books labled ID(Gutenberg Number) and a Data Frame with ## 
## the subjects of all the books pulled ##

## The input for the Function is a Number for amount of Books to Randomly Select ##

#Load Libraries
library(gutenbergr)
library(tidyverse)
library(purrr)
library(e1071)
library(RMySQL)
library(readability)
library(RTextTools)
library(tm)
library(stringr)
library(topicmodels)

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
  books <- NULL
  
  #100 Books takes roughly 34 seconds to run and returned 19 books
  for (i in 1:length(listofbooks)) {
    
    #Removes Books that have more then 4000 observations or less then 500 observations
    if(nrow(data.frame(map(listofbooks[i], "text"))) > 7000 | nrow(data.frame(map(listofbooks[i], "text"))) < 500){
      next
    } else {
      
      #Prints the Book to the Enviornment with the title ID + gutenberg_id
      x <- as.data.frame(listofbooks[[i]])
      
      x$text <- paste(x$text, collapse =" ")
      
      x <- x[1,]
      
      books <- rbind(books,x)
      
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
  
  #Loop through titles and link the subject of the book to the book id
  title <- NULL
  titles <- NULL
  for (i in 1:nrow(ids)) {
    
    
    #Selects the Gutenberg Bookshelf Classification for Each ID
    title <- bookid$title[i]
    
    #Stores Subjects of IDS to List
    titles[[i]] <- title
  }
  
  titles <- as.data.frame(titles)
  
  #Loop through titles and link the subject of the book to the book id
  author <- NULL
  authors <- NULL
  for (i in 1:nrow(ids)) {
    
    
    #Selects the Gutenberg Bookshelf Classification for Each ID
    author <- bookid$author[i]
    
    #Stores Subjects of IDS to List
    authors[[i]] <- author
  }
  
  authors <- as.data.frame(authors)
  
  #Merge Subjects to ID
  idinfo <- cbind(ids, authors, titles, subjects)
  books <- books
  bookinfo <- cbind(books, authors, titles, subjects)
  
  ## Filters out Books with UTF-8 Encoding ##
  for (i in 1:nrow(bookinfo)) {
    if(Encoding(bookinfo[i,2]) == "UTF-8"){
      
      bookinfo <- bookinfo[-i,]
      
      books <- books[-i,]
      
      idinfo <- idinfo[-i,]
    } else {
      next
    }
  }
  
  for (i in 1:nrow(bookinfo)) {
    if(Encoding(bookinfo[i,2]) == "UTF-8"){
      
      bookinfo <- bookinfo[-i,]
      
      books <- books[-i,]
      
      idinfo <- idinfo[-i,]
    } else {
      next
    }
  }

  bookinfo <<- bookinfo
    
}

#### Data Preperation ####

## Condense subjects into more broad topcis for modeling process ##
# Improves model performance with having larger groups #
bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"War") == TRUE, "War", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"History") == TRUE, "History", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Science Fiction") == TRUE, "Science Fiction", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Animals") == TRUE | str_detect(bookinfo$subjects,"Zoology") == TRUE, "Animals", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"[C-c]hild") == TRUE, "Children", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Cookery") == TRUE, "Cookery", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Fantasy") == TRUE, "Fantasy", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Harvard Classics") == TRUE, "Harvard Classics", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Native America") == TRUE, "Native America", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Mythology") == TRUE, "Mythology", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Opera") == TRUE, "Music", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Christianity") == TRUE | str_detect(bookinfo$subjects,"Islam") == TRUE | str_detect(bookinfo$subjects,"Buddhism") == TRUE | str_detect(bookinfo$subjects,"Judaism") == TRUE | str_detect(bookinfo$subjects,"Latter Day Saints") == TRUE | str_detect(bookinfo$subjects,"Paganism") == TRUE | str_detect(bookinfo$subjects,"Bahá'í Faith") == TRUE | str_detect(bookinfo$subjects,"Hinduism") == TRUE| str_detect(bookinfo$subjects,"Atheism") == TRUE, "Religion", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Humor") == TRUE, "Humor", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Art") == TRUE | str_detect(bookinfo$subjects,"Photography") == TRUE | str_detect(bookinfo$subjects,"Masterpieces in Colour") == TRUE, "Art/Photography", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"African American Writers") == TRUE, "African American Writers", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Slavery") == TRUE | str_detect(bookinfo$subjects,"Racism") == TRUE, "Racism", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Architecture") == TRUE, "Architecture", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Bestsellers") == TRUE, "Bestsellers", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Technology") == TRUE, "Technology", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Australia") == TRUE | str_detect(bookinfo$subjects,"Egypt") == TRUE |str_detect(bookinfo$subjects,"Africa") == TRUE |str_detect(bookinfo$subjects,"United Kingdom") == TRUE |str_detect(bookinfo$subjects,"Germany") == TRUE |str_detect(bookinfo$subjects,"New Zealand") == TRUE |str_detect(bookinfo$subjects,"Italy") == TRUE |str_detect(bookinfo$subjects,"Czech") == TRUE |str_detect(bookinfo$subjects,"United States") == TRUE |str_detect(bookinfo$subjects,"India") == TRUE |str_detect(bookinfo$subjects,"France") == TRUE |str_detect(bookinfo$subjects,"Canada") == TRUE | str_detect(bookinfo$subjects,"Mexico") == TRUE | str_detect(bookinfo$subjects,"Iraq") == TRUE | str_detect(bookinfo$subjects,"Iran") == TRUE | str_detect(bookinfo$subjects,"Syria") == TRUE | str_detect(bookinfo$subjects,"China") == TRUE | str_detect(bookinfo$subjects,"Japan") == TRUE | str_detect(bookinfo$subjects,"Russia") == TRUE | str_detect(bookinfo$subjects,"USSR") == TRUE | str_detect(bookinfo$subjects,"Sweden") == TRUE | str_detect(bookinfo$subjects,"Switzerland") == TRUE, "Country", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Country") == TRUE, "Geography", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Movie Books") == TRUE, "Movie Books", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Horticulture") == TRUE, "Horticulture", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Sociology") == TRUE, "Sociology", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Classical Antiquity") == TRUE, "Classical Antiquity", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Horror") == TRUE |str_detect(bookinfo$subjects,"Witchcraft") == TRUE, "Horror", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Play") == TRUE, "Plays", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Education") == TRUE | str_detect(bookinfo$subjects,"School") == TRUE, "Education", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Law") == TRUE, "Law", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Reviews") == TRUE, "Reviews", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Fiction") == TRUE, "Fiction", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Nonfiction") == TRUE, "Nonfiction", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Political") == TRUE | str_detect(bookinfo$subjects,"Politics") == TRUE | str_detect(bookinfo$subjects,"Government") == TRUE | str_detect(bookinfo$subjects,"Anarchism") == TRUE, "Politics/Government", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Travel") == TRUE | str_detect(bookinfo$subjects,"Transportation") == TRUE, "Travel", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Mathematics") == TRUE | str_detect(bookinfo$subjects,"Biology") == TRUE | str_detect(bookinfo$subjects,"Microscopy") == TRUE | str_detect(bookinfo$subjects,"Physics") == TRUE | str_detect(bookinfo$subjects,"Chemistry") == TRUE | str_detect(bookinfo$subjects,"Astronomy") == TRUE | str_detect(bookinfo$subjects,"Microbiology") == TRUE | str_detect(bookinfo$subjects,"Geology") == TRUE, "Science/Math", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Botany") == TRUE | str_detect(bookinfo$subjects,"Forestry") == TRUE | str_detect(bookinfo$subjects,"Horticulture") == TRUE, "Botany/Horticulture", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Reference") == TRUE | str_detect(bookinfo$subjects,"Biblio") == TRUE, "Reference", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Classics") == TRUE | str_detect(bookinfo$subjects,"Best Books Ever") == TRUE, "Classics", as.character(bookinfo$subjects))

bookinfo$subjects <- ifelse(str_detect(bookinfo$subjects,"Anthropology") == TRUE | str_detect(bookinfo$subjects,"Psychology") == TRUE | str_detect(bookinfo$subjects,"Sociology") == TRUE | str_detect(bookinfo$subjects,"Economics") == TRUE, "Social Sciences", as.character(bookinfo$subjects))

bookinfo$Child_Adult <- ifelse(str_detect(bookinfo$subjects,"[C-c]hild") == TRUE, "Child", "Adult")


#### MODELING ####

## Use getbooks() function to grab amount of books needed (Output contains roughly 50% of number asked) ##
# Can use any number 1 - 7800 #
getbooks(7800)

#### Readability ####

# Create null value for loop 
bookinfo1 <- bookinfo
bookinfo2 <- NULL

# For loop to generate reading level
for (i in 1:nrow(bookinfo)) {
  bookinfo2[[i]] <- readability(bookinfo[i,2], NULL)
}

## Map the Reading Level to the Book ##
reading_level <- map(bookinfo2, "Coleman_Liau")

bookinfo1$reading_level <- reading_level

bookinfo1$reading_level <- as.numeric(bookinfo1$reading_level)

## Seperate out Children and Adult Books to Analyze Reading Levels ##
childrenbook <- filter(bookinfo1, Child_Adult == "Child")

adultbook <- filter(bookinfo1, Child_Adult == "Adult")

# Create Data Frame for Children Reading Levels for Analysis #
childread <- as.data.frame(as.numeric(round(childrenbook$reading_level, 0)))

# Summary of Reading Level Values #
summary(childread)

# Boxplot of Values

boxplot(childread)

## Label the Readabilty Levels to Corresponding Grade Levels ##
childrenbook$reading <- NA

childrenbook$reading <- ifelse(childread == 1, "1st Grade", childrenbook$reading)
childrenbook$reading <- ifelse(childread == 2, "2nd Grade", childrenbook$reading)
childrenbook$reading <- ifelse(childread == 3, "3rd Grade", childrenbook$reading)
childrenbook$reading <- ifelse(childread == 4, "4th Grade", childrenbook$reading)
childrenbook$reading <- ifelse(childread == 5, "5th Grade", childrenbook$reading)
childrenbook$reading <- ifelse(childread == 6, "6th Grade", childrenbook$reading)
childrenbook$reading <- ifelse(childread == 7, "7th Grade", childrenbook$reading)
childrenbook$reading <- ifelse(childread == 8, "8th Grade", childrenbook$reading)
childrenbook$reading <- ifelse(childread == 9, "9th Grade", childrenbook$reading)
childrenbook$reading <- ifelse(childread == 10, "10th Grade", childrenbook$reading)
childrenbook$reading <- ifelse(childread == 11, "11th Grade", childrenbook$reading)
childrenbook$reading <- ifelse(childread == 12, "12th Grade", childrenbook$reading)


# Create Data Frame for Adult Reading Levels for Analysis #
adultread <- as.data.frame(as.numeric(round(adultbook$reading_level)))

# Summary of Reading Level Values #
summary(adultread)

# Box Plot of Reading Level Values #
boxplot(adultread)

## Label the Readabilty Levels to Corresponding Grade Levels ##

adultbook$reading <- NA

adultbook$reading <- ifelse(adultread == 4, "4th Grade", adultbook$reading)
adultbook$reading <- ifelse(adultread == 5, "5th Grade", adultbook$reading)
adultbook$reading <- ifelse(adultread == 6, "6th Grade", adultbook$reading)
adultbook$reading <- ifelse(adultread == 7, "7th Grade", adultbook$reading)
adultbook$reading <- ifelse(adultread == 8, "8th Grade", adultbook$reading)
adultbook$reading <- ifelse(adultread == 9, "9th Grade", adultbook$reading)
adultbook$reading <- ifelse(adultread == 10, "10th Grade", adultbook$reading)
adultbook$reading <- ifelse(adultread == 11, "11th Grade", adultbook$reading)
adultbook$reading <- ifelse(adultread == 12, "12th Grade", adultbook$reading)
adultbook$reading <- ifelse(adultread == 13, "13th Grade", adultbook$reading)
adultbook$reading <- ifelse(adultread == 14, "14th Grade", adultbook$reading)
adultbook$reading <- ifelse(adultread == 15, "15th Grade", adultbook$reading)
adultbook$reading <- ifelse(adultread == 16, "16th Grade", adultbook$reading)
adultbook$reading <- ifelse(adultread == 17, "17th Grade", adultbook$reading)
adultbook$reading <- ifelse(adultread == 18, "18th Grade", adultbook$reading)

## Select Wanted Columns ##

childrenbook <- childrenbook[,c(1:4,6)]
adultbook <- adultbook[,c(1:4,6)]

# Merge Data Frames Together # 
# Reading Levels for Books #
classbookinfo <- rbind(childrenbook,adultbook)

## The readability model is chosen for business purposes. As an unsupervised model, it classifies books into reading levels ##

#### Maxent, SVM and RF Model's ####

#Build matrix
dtMatrix <- create_matrix(bookinfo$text, stemWords = TRUE, removePunctuation = TRUE, removeStopwords = TRUE, toLower = TRUE, stripWhitespace = TRUE, weighting = tm::weightTfIdf)

#Configure the training data
container <- create_container(dtMatrix, factor(bookinfo$subjects), trainSize=1:(nrow(bookinfo)*.7),testSize = ((nrow(bookinfo)*.7)+1):nrow(bookinfo), virgin=FALSE)
container2 <- create_container(dtMatrix, as.numeric(factor(bookinfo$subjects)), trainSize=1:(nrow(bookinfo)*.7),testSize = ((nrow(bookinfo)*.7)+1):nrow(bookinfo), virgin=FALSE)

### Maxent Model ###
# Train Maxent Model #
maxent_model <- train_model(container, "MAXENT")

# Test Maxent Model #
maxent_classify <- as.data.frame(classify_model(container, maxent_model))


### SVM Model ###
# Train SVM Model #
svm_model <- train_model(container,"SVM")

# Test SVM Model #
svm_classify <- classify_model(container, svm_model)


### RF Model ###
# Train RF Model #
rf_model <- train_model(container, "RF")

# Test RF Model #
rf_classify <- classify_model(container, rf_model)

## Above Model Evaluation and Performance ##

# Create analytics for evaluation #
analytics <- create_analytics(container2, cbind(svm_classify, maxent_classify, rf_classify))

# Evaluate Analytics #
eval_subjects <- as.data.frame(summary(analytics))

summary(analytics)


topic_summary <- analytics@label_summary
alg_summary <- analytics@algorithm_summary
ens_summary <- analytics@ensemble_summary
doc_summary <- analytics@document_summary

create_ensembleSummary(analytics@document_summary)
ensemble_summary <- as.data.frame(create_ensembleSummary(analytics@document_summary))

### From the Models Ran above, we chose Maxent Model for business purposes ###
#Using the create_analytics() function, it was determined that the Maximum Entropy was the best predictor of subjects. 
#The F-Score, recall, and precision were all higher for the Maximum Entropy model than for the other models. 
#The SVM model was the worst performing model. # 

maxent_summary <- as.data.frame(maxent_classify)
maxent_summary <- cbind(maxent_summary, bookinfo[((nrow(bookinfo)*.7)+1):(nrow(bookinfo)), c(1,4)])
colnames(maxent_summary) <- c("Subject", "Probability", "Gutenberg_ID", "Title")

# Final Output with the Subject Grouping and the Probabilties of the text matching that subject (choses highest probabilities for all subjects) #
maxent_summary <- select(maxent_summary, "Gutenberg_ID", "Title", "Subject", "Probability")


#### Naive Bayes Model ####

bookinfo3 <- bookinfo    

bookinfo3$subjects <- as.factor(bookinfo3$subjects)

# Split into Train and Test Set #
train <- bookinfo3[1:(nrow(bookinfo3)* .7),]
test <- bookinfo3[((nrow(bookinfo3)* .7) + 1):(nrow(bookinfo3)),]

## Create First Model with Train Set ##
# Remove ID from Train #
train1 <- select(train, -c(gutenberg_id, Book_Level))
model <- naiveBayes(subjects ~ text, data = train1)

## Predict Test Set ##
# Remove Subjects and ID from Data Frame #
test1 <- select(test, text)

# Make Prediciton #
prediction <- predict(model, newdata = test1, type = "class")

# Put Prediction with Actual Subject to see how well it did #

testpred <- data.frame(test$gutenberg_id, prediction, test$subjects)


summary(prediction)


### Naive Bayes model is not a sufficient model for classification ###
## Not using the Model for the Business Process ##
# The Naive Bayes model classifies each book with the same classification #
# this does not create valid results #
