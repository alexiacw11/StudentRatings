# Set working directory
setwd("insert/your/working/directory/here")

# Load libraries 
library(tidymodels)
library(tidyverse)
library(readxl)  
library(tidytext) 
library(stopwords) # Removes stop words
library(textclean) # Cleans contractions 
library(purrr) # Reduces
library(forcats) # Visualizations
library(caret) # Removes nearzerovar

# Function to read all the sheets 
read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

# Read the sheets
all_sheets <- read_excel_allsheets("Aims&CommentsCMPS.xlsx")

# Here is where you can specify the aim you are interested in. The options are: 
# Spiritually strengthening,Intellectually enlarging, Character building, Leading to lifelong learning and service
target_aim <- "Intellectually enlarging"

# Filter out no responses + written NAs and select only necessary columns
data_sheets <- lapply(all_sheets, function(df) {
  df |> 
    dplyr::filter(Question == target_aim & !Response_1 %in% c("No Response", "N/A", "n/a")) |> 
    dplyr::select(-College, -Department, -Section, -"Total Enrollment", 
                  -"Total Responses", -"Student Random ID", -"Comment Question")
})

# Combine all sheets into singular data frame  
full_data <- data_sheets |> 
  reduce(dplyr::full_join)

# Rename full data columns + Create new column to identify Semester,Faculty, Course as a document
full_data <- full_data |> 
  rename(Faculty_Member = "Faculty Member") |> 
  mutate(document_id = paste(Semester, Faculty_Member, Course, sep = "_"))

# Combining the reviews and getting the mean rating for each document_id
full_data <- full_data %>%
  dplyr::select(document_id, Response, Response_1) |> 
  group_by(document_id) %>%
  summarize(Combined_Reviews = str_c(Response_1, collapse = " "), 
            ratings = mean(Response)) |> 
  as.data.frame()

# Tokenize words and leave out the aim
review_words <- full_data |> 
  unnest_tokens(word, Combined_Reviews) |> 
  count(document_id, word, sort = TRUE)

# Clean up words with replacing contractions and stopwords
# Stopwords are commonly occurring words that do not add meaning to the context, such as articles, prepo-
# sitions, punctuations, hyphens, numbers and symbols otherwise they will also show up in the data matrix.
review_words$word <- replace_contraction(review_words$word, contraction.key = lexicon::key_contractions)
review_words <- review_words |> 
  anti_join(stop_words) |> 
  dplyr::arrange(document_id)

# Calculate total words per all reviews and group by course,prof, and semester  
total_words <- review_words %>% 
  group_by(document_id) %>% 
  summarize(total = sum(n))

# Create frame with all information
full_frame <- left_join(review_words, total_words) |> 
  dplyr::arrange(document_id)

reviews_tf_idf <- full_frame |>
  bind_tf_idf(term = word, document = document_id, n = n)

# Transformed to wide format
df_wide <- reviews_tf_idf %>%
  dplyr::select(document_id, word, tf_idf) %>%  # Replace `name_column` with the name of the column with document identifiers
  pivot_wider(names_from = word, values_from = tf_idf, values_fill = 0) |> 
  dplyr::arrange(document_id)

# For Spiritually strengthening + Character building
# One of the words from a review was, "ratings", which is the name of our column.
# So, need to remove it from df_wide, then join
df_wide <- df_wide |> 
  dplyr::select(-ratings) |>                       
  left_join(full_data |> dplyr::select(document_id, ratings), by = "document_id") |> 
  arrange(document_id)

# For Intellectually enlarging + Leading to lifelong learning and service
df_wide <- df_wide |> 
  left_join(full_data |> dplyr::select(document_id, ratings), by = "document_id") |> 
  arrange(document_id)

# This shows which columns with near zv will be removed
# nearZeroVar(df_wide, saveMetrics = TRUE)
simplified_frame <- df_wide[,-nearZeroVar(df_wide)]


#Getting rid of unnecessary words/phrases for each aim -----------------------
# Spiritually strengthening, 86 columns
simplified_frame <- simplified_frame |> 
  dplyr::select(-c(dr, `I am`, chemistry, stuff, `it is`, class, students, 
                   `was not`, `I have`, `do not`, math, professors))

# Intellectually enlarging, 54 columns
simplified_frame <- simplified_frame |> 
  dplyr::select(-c(dr, `it is`, `I have`, math, professor, class, `do not`, 
                   `I am`, ochem, chem, chemistry, organic))

# Character building, 49 columns
simplified_frame <- simplified_frame |> 
  dplyr::select(-c(dr, professor, `I have`, `it is`, `do not`, `I am`))

# Leading to lifelong learning and service, 53 columns
simplified_frame <- simplified_frame |> 
  dplyr::select(-c(dr, professor, `it is`, `do not`, `I am`, math)) |> 
  rename(ratings = ratings.y)

# Here are the columns/words left for the analysis 
colnames(simplified_frame)

# Write each aim dataset to a spreadsheet 
vroom::vroom_write(x=simplified_frame, file="StudentRatings.csv", delim=",")