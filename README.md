Libraries: gutenbergr, tidyverse, purrr, e1071, RMySQL, readability, RTextTools, tm, stringr, topicmodels

DATA EXTRACTION
Book info extracted from gutengberg API using user-defined getbooks(x) function that takes a numeric value and returns a (randomly selected) dataframe with gutenberg_id, text, subjects
- Books without text, non-english books, speeches, congressional statesments, and no subject are removed  
- x = 200 takes about one minute to run
- very long and very short books are removed 
- filter out books with UTF-8 encoding issues

DATA PREPARATION
Condense detailed subjects into broader topics to improve model performance

MODELING
- Readability - classifies books into reading levels
  - unsupervised
  - method = Coleman Liau
  - Child/ adult
  - Grades 1 - 18
  - includes boxplot
- Maximum Entrophy, Support Vector Machines, and Random Forest
  - stem words, remove punctuation, remove stopwords, convert case to lower, strip whitespacetf-idf weightingh
  - train/ test sets
- Naive Bayes Classification
  - train/ test sets for predictive modeling
  - NBC does not generate any valuable results

MODEL EVALUATION AND PERFORMANCE
- Interpreting results
  - create dataframe summaries
  - evaluate f-score, recall, and precision
- Best predictor: Maxent
- Worst predictor: SVM
- final output includes subject grouping and probabilities of text matching (each subject)
