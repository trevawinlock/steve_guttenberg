# steve_guttenberg

Group 1 – Text Mining Project (Derek K., Ian O., Patrick P., Treva W.)

Define a question & approach	Established book retailer launching online site, competitive advantage will be displaying books by reading level and inherent sub-categories. 

Business Understanding	
•	Use multiple APIs to analyze texts from online book libraries
  o	https://www.gutenberg.org
  o	https://developers.google.com/books/
  o	https://openlibrary.org/developers/api

Data Understanding
•	Information Retrieval: Import data into database
•	Import title, author, year, full text into full corpora

Data Preparation	•	Remove punctuation, convert to lower, remove sparse terms, strip whitespace, dimensionality reduction (remove stopwords), consider start words to filter false positives (using regular expressions)
•	Stem morphemes/ lemmatization
•	Text/ Document Classification
•	Evaluate and manage duplicates from using multiple APIs
•	Convert to TDM/ DTM
•	Evaluate tf-idf and need for log transformation(s)
•	Use lexical diversity
•	Store functions in separate libraries
•	Use syllable() and readability() packages to assign reading levels for modeling
•	MEET WITH CASEY

Modeling	•	(Store functions in libraries)
•	Supervised Learning: Naïve Bayes Classifier for genre
•	Unsupervised Learning: Probabilistic language model
•	MEET WITH CASEY

Evaluate Model Performance	
•	Use k-folds cross validation model to evaluate Bayesian Classifier
•	Use kmeans to measure probabilistic language model
Deployment	
•	Model to assign new texts to categories
•	Model to recommend texts to customers

Timeline of tasks: 
•	Nov 8 mtg – meet at Allen Hall, each member should have their API data in dataframe & Git established
•	Nov 13 mtg – each member has completed their data prep responsibility, assign models to pairs
•	Nov 20 mtg – assign model evaluation methods to pairs for completion
•	Dec 2 mtg – compile written report for 12/5 submission
•	Dec 5 – compile presentation
