########################## IMPORTS: R LIBRARIES & DATA SELECTION ##########################

# Load necessary libraries
library(dplyr)     # For data manipulation and plotting
library(stringr)   # For text cleaning
library(readr)     # For reading CSV data files
library(wordcloud) # For creating the word cloud
library(RColorBrewer) # For creating the word cloud
library(png)       # For saving the word cloud as a png
library(syuzhet)   # For sentiment analysis
library(openxlsx)  # For excel exporting
library(topicmodels) # For LDA topic modeling (Latent Dirichlet Allocation)
library(tm) # For text mining - required for LDA
library(boot) # For Hierarchical Bootstrapped Regression

# When importing the CSV file, ensure to locate the pop-up box to select the document where it is saved on your computer - a more portable approach than using a specified file path
# Ensure the imported data file was previously converted to CSV-UTF8
# Select CSV data file - select via file explorer dialogue box (it will pop up on your desktop, exit fullscreen if not visible)
csv_file_path <- file.choose()

################################### GLOBAL VARIABLES ###################################

# Read CSV data
df <- read_csv(csv_file_path, col_types = cols())

# Define consolidated terms for mapping - defined by researcher using terminology from the course content, including synonyms, related terms, and differential spelling.
consolidated_terms <- list(
  "acquiring grit" = c("acquiring grit", "grit", "courage", "drive", "driven"),
  "help-seeking" = c("asking for help", "help seeking", "seeking help"),
  "backwards scheduling" = c("backwards scheduling", "schedules", "schedule", "scheduling", "time management"),
  "burn out" = c("burn out", "burnt out", "exhausted", "exhaustion", "exhausting", "fatigue", "fatigued", "tired", "tiring", "overworked", "overworking", "depleting", "depleted"),
  "coping with setbacks" = c("setback", "setbacks", "failed", "failing", "failure", "academic setbacks", "growth", "growth mindset", "resilience", "resilient"),
  "cornell note-taking" = c("cornell note", "cornell note-taking", "cornell notes"),
  "curbing procrastination" = c("curb procrastination", "curbing procrastination", "procrastination", "procrastinating", "procrastinate"),
  "goal-setting" = c("goal setting", "setting goals", "goals", "making goals", "creating goals"),
  "learning" = c("learning", "learned", "understand", "understanding", "comprehend", "comprehending", "comprehension", "deep learning", "retention", "memory"),
  "motivation" = c("motivation", "motivate", "motivates", "motivated", "motivating", "carrots", "carrot", "motivational factors", "rewards", "reward", "rewarding"),
  "sleep hygiene" = c("sleep hygiene", "sleep", "sleeping"),
  "stress management" = c("stress", "stress management", "stress as load", "as load", "and load", "course load", "overwhelmed", "overwhelm", "stress as worry", "as worry", "worried", "worrying", "and worry", "anxious", "anxiety", "negative thinking", "negative thoughts", "rumination", "ruminate", "ruminative thinking", "spiral", "spiralling", "catastrophize", "catastrophizing", "relaxation breathing", "breathing", "mindfulness", "fact-checking", "fact checking"),
  "study habits and skills" = c("study", "studying", "study strategies", "study skills", "pomodoro technique", "pomodoros", "breaks", "taking breaks", "spaced repetition", "interleaved practice", "deliberate practice", "active recall", "cue cards", "flashcards", "flash cards", "que cards"),
  "to-do lists" = c("to-do lists", "to do list", "to-do list", "to do lists", "todo list", "todo lists"),
  "well-being" = c("well-being", "well being", "happy", "happiness", "perma", "PERMA", "perma theory", "positive emotions", "engagement", "relationships", "meaning", "achievement", "mental health", "self-esteem", "depression", "depressed", "mental illness", "mental health condition"),
  "course satisfaction" = c("satisfaction", "satisfied", "satisfying", "enjoyed", "enjoy", "positive", "liked", "appreciated", "appreciate", "positive experience"),
  "course helpfulness" = c("helpful", "helped", "helps", "useful", "benefitted", "benefit", "beneficial", "valued", "value", "valuable", "practical", "practicality", "applicable", "application", "effective", "works", "worked", "impactful", "impact", "support", "supportive", "supporting")
)

##################### DEFINING FUNCTIONS: NLP ANALYSES OF CLEANED TEXT #####################

#### TEXT CLEANING ####
# Function to clean the text
Clean_String <- function(string) {
  if (is.na(string) || string == "") {
    print("text is not cleaned. returned empty")
    return("")
  }
  temp <- stringr::str_replace_all(string, "[^a-zA-Z\\s'.-]", " ")
  temp <- stringr::str_replace_all(temp, "[\\s]+", " ")
  temp <- tolower(temp)
  return(temp)
}

#### WORD COUNT EXTRACTION ####
# Function to calculate the number of words (verbosity) in text
Get_Word_Count <- function(text) {
  if (is.na(text) || text == "" || grepl("^\\s*$", text)) {
    return(0)
  }
  # Counts sequences of alphanumeric characters
  return(stringr::str_count(text, "\\w+"))
}

#### FREQUENCY ANALYSIS ####
# Function to perform frequency analysis of target strings (consolidated terms)
Count_Targets <- function(cleaned_text, consolidated_terms) {
  target_count <- numeric(length(consolidated_terms))
  for (i in seq_along(consolidated_terms)) {
    consolidated_target <- consolidated_terms[[i]]
    
    if (is.character(consolidated_target)) {
      # Single term mapping
      target_count[i] <- sum(stringr::str_count(cleaned_text, regex(paste0("\\b", consolidated_target, "\\b"), ignore_case = TRUE)))
    } else {
      # Multiple terms mapping
      target_count[i] <- sum(sapply(consolidated_target, function(t) sum(stringr::str_count(cleaned_text, regex(paste0("\\b", t, "\\b"), ignore_case = TRUE)))))
    }
  }
  
  return(data.frame(Target_String = names(consolidated_terms), Present_Count = target_count))
}

#### SENTIMENT ANALYSIS (NRC LEXICON) ####
# Function to perform sentiment analysis
Get_Sentiment <- function(cleaned_text) {
  sentiment <- get_nrc_sentiment(cleaned_text)
  return(sentiment)
}

#### LATENT DIRICHLET ALLOCATION (LDA) TOPIC MODELING ####
## Function to perform LDA topic modeling with automated stop-word removal
Perform_LDA <- function(text_corpus, num_topics = 5, top_terms = 8) { # Lowered default topics to 6
  corpus <- Corpus(VectorSource(text_corpus))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  
  # Standard English & French stop words
  standard_stops <- c(stopwords("en"), stopwords("fr"))
  
  # Custom question-level stop words to clear structural noise in topics due to question format/domain
  custom_stops <- c("course", "class", "classes", "semester", 
                    "university", "student", "students",
                    "enjoyed", "enjoy", "liked", "like", "most", "why",
                    "helped", "helps", "help", "helpful", "improve", "improved", "other", "kinds", "better",
                    "one", "two", "three", "four", "five", "thing", "things", "skill", "skills", "important", 
                    "use", "used", "know", "did", "not","can", "cant", "could", "would", "made", "much", 
                    "lot", "found", "got", "get", "just", "however", "really", "also", "something", "anything"
                    )
  
  corpus <- tm_map(corpus, removeWords, c(standard_stops, custom_stops))
  corpus <- tm_map(corpus, stripWhitespace)
  
  # Create document-term matrix (DTM)
  dtm <- DocumentTermMatrix(corpus, control = list(wordLengths = c(3, 15)))
  
  # Remove empty rows (documents with no terms)
  rowTotals <- apply(dtm, 1, sum)  
  dtm <- dtm[rowTotals > 0, ]      
  
  # Check if the DTM is empty after filtering
  if (nrow(dtm) == 0) {
    warning("No valid documents for LDA; All documents were empty after preprocessing.")
    return(NULL)
  }
  
  # To perform LDA topic modeling
  lda_model <- LDA(dtm, k = num_topics, control = list(seed = 1234))
  
  # Retrieve the top terms for each topic
  lda_terms <- terms(lda_model, top_terms)
  
  # Print the top terms for each topic
  print("Top terms for each topic:")
  print(lda_terms)
  
  return(lda_terms)
}

#### WORD CLOUDS ####
# Helper function to generate word clouds with custom dimensions
save_wordcloud <- function(data_vector, filename, folder) {
  # High resolution PNG with large dimensions for readability
  png(file.path(folder, filename), width = 1800, height = 1200, res = 200)
  
  # Standard margins
  par(mar = c(0, 0, 0, 0))
  
  # Create cloud with custom scaling
  tryCatch({
    wordcloud(words = names(data_vector), 
              freq = as.vector(data_vector), 
              min.freq = 1, 
              random.order = TRUE,
              colors = brewer.pal(8, "Dark2"),
              scale = c(5.0, 1.2),
              max.words = 100)
  }, error = function(e) {
    message(paste("Failed to plot:", filename))
  })
  
  dev.off()
}

############################# MAIN CODE: DATA MAPPING & STORAGE #############################

## Anonymize respondents
# Assign anonymous IDs to each participant in place of student email & IP addresses
df <- df %>% mutate(anonymous_id = paste0("ID_", 1:n()))

## Defining column mappings for qualitative & quantitative responses
# Define column mappings for qualitative responses (open-ended)
question_columns <- list(
  P1 = 5:9,                  # Part 1, Combining/Collapsing subsections 1, 2, 3, 4, 5
  P2_1 = 11,                 # Part 2, Subsection 1
  P2_2 = 12,                 # Part 2, Subsection 2
  P3 = c(13, 16, 19, 22, 25) # Part 3, Combining skills 1 to 5 into one text corpus
)

# Define column mappings for quantitative ratings (Likert-scale 1-10) - P3 ratings are merged/summarized based on data-type (ordinal) in statistics section
quantitative_columns <- list(
  P1_R1 = 2,     # Part 1 - satisfaction 1: course enjoyment
  P1_R2 = 3,     # Part 1 - satisfaction 2: course meaning 
  P1_R3 = 4,     # Part 1 - satisfaction 3: course engagement
  
  P2_R = 10,     # Part 2 - general helpfulness: course helpfulness
  
  P3_1_R1 = 14,  # Part 3 - skill 1 helpfulness
  P3_1_R2 = 15,  # Part 3 - skill 1 usage
  P3_2_R1 = 17,  # Part 3 - skill 2 helpfulness
  P3_2_R2 = 18,  # Part 3 - skill 2 usage
  P3_3_R1 = 20,  # Part 3 - skill 3 helpfulness
  P3_3_R2 = 21,  # Part 3 - skill 3 usage
  P3_4_R1 = 23,  # Part 3 - skill 4 helpfulness
  P3_4_R2 = 24,  # Part 3 - skill 4 usage
  P3_5_R1 = 26,  # Part 3 - skill 5 helpfulness
  P3_5_R2 = 27   # Part 3 - skill 5 usage
)

## Initialize data frames & lists to store results
# Extract names of parts 1 to 3 from the question columns in the imported CSV data file
parts_list <- names(question_columns)

# Dynamically generate empty data frames and lists for part 1-3 for word count, frequency, sentiment, and LDA analyses
word_count_results <- setNames(lapply(parts_list, function(x) data.frame()), parts_list)
count_tables_list  <- setNames(lapply(parts_list, function(x) list()), parts_list)
sentiment_results  <- setNames(lapply(parts_list, function(x) data.frame()), parts_list)
lda_corpus         <- setNames(lapply(parts_list, function(x) character()), parts_list)

# Store final corpus-level LDA matrices (18 topics x 10 terms)
lda_results <- list()

############# DATA PROCESSING #############

#### Loop through each participant by row to process data ####
for (i in 1:nrow(df)) {
  
  ### Assign the participant's anonymous ID
  participant_id <- df$anonymous_id[i]
  
  ### Process Parts P1, P2_1, P2_2, P3
  for (part_name in names(question_columns)) {
    part_columns <- question_columns[[part_name]]
    
    # Extract and combine text from the mapped columns
    cell_text <- paste(df[i, part_columns], collapse = " ")
    
    # Handle empty/missing responses
    if (is.na(cell_text) || cell_text == "" || grepl("^\\s*$", cell_text) || cell_text == "NA NA NA NA NA") {
      # Store empty results to ensure every participant is retained in the dataset
      word_count_results[[part_name]] <- rbind(word_count_results[[part_name]], data.frame(anonymous_id = participant_id, Word_Count = 0))
      count_tables_list[[part_name]][[participant_id]] <- data.frame(anonymous_id = participant_id, Target_String = NA, Present_Count = 0)
      sentiment_results[[part_name]] <- rbind(sentiment_results[[part_name]], data.frame(anonymous_id = participant_id, word_count = 0, anger = 0, anticipation = 0, disgust = 0, fear = 0, joy = 0, sadness = 0, surprise = 0, trust = 0, negative = 0, positive = 0))
      next
    }
    
    # Clean the text and perform word count, frequency and sentiment analyses - LDA is performed directly after the loop due to its different mathematical logic
    cleaned_text <- Clean_String(cell_text)
    word_count   <- Get_Word_Count(cleaned_text)
    count_table  <- Count_Targets(cleaned_text, consolidated_terms)
    sentiment    <- Get_Sentiment(cleaned_text)
    
    ## Storing results dynamically for each part
    # Store cleaned text for corpus-level LDA later
    lda_corpus[[part_name]] <- c(lda_corpus[[part_name]], cleaned_text)
    # Store Word Count
    word_count_results[[part_name]] <- rbind(word_count_results[[part_name]], data.frame(anonymous_id = participant_id, Word_Count = word_count))
    #Store Frequency Counts
    count_table <- count_table %>% mutate(anonymous_id = participant_id)
    count_tables_list[[part_name]][[participant_id]] <- count_table
    #Store Sentiment Results
    sentiment_results[[part_name]] <- rbind(sentiment_results[[part_name]], cbind(data.frame(anonymous_id = participant_id), sentiment))
  }
}

# Loop confirmation message in console
cat("Data processing complete. All parts have been analyzed.\n")

#### CORPUS-LEVEL LDA ANALYSIS ####
## Run one LDA model per prompt using all participant responses
for (part_name in names(lda_corpus)) {
  
  cat("\nRunning LDA for:", part_name, "...\n")
  
  # Retrieve the corpus text for this part
  current_corpus <- lda_corpus[[part_name]]
  
  # Filter any possible lingering empty strings
  current_corpus <- current_corpus[current_corpus != "" & !is.na(current_corpus)]
  
  # Check there's enough content in the corpus/document to run the algorithm
  if (length(current_corpus) > 0) {
    
    lda_results[[part_name]] <- Perform_LDA(
      text_corpus = current_corpus,
      num_topics = 5,
      top_terms = 8
    )
    
  } else {
    warning(paste("Skipping LDA for", part_name, "- No valid documents found."))
    lda_results[[part_name]] <- NULL
  }
}

# LDA confirmation message in console
cat("\nLDA Analysis complete. Topic models stored in 'lda_results'.\n")

####################### STATISTICS: DESCRIPTIVE, SPEARMAN'S, REGRESSION #######################

#### QUANTIATIVE AGGREGATION ####
## Extract and aggregate frequency & sentiment metrics per participant
flat_frequency <- list()
flat_sentiment <- list()

for (part in names(count_tables_list)) {
  # Stacking individual participant matrices into unified dataframes
  flat_frequency[[part]] <- bind_rows(count_tables_list[[part]]) %>%
    group_by(anonymous_id) %>%
    summarise(Frequency = sum(Present_Count, na.rm = TRUE), .groups = "drop")
  
  # Stack sentiment vectors and summarize emotional volume
  flat_sentiment[[part]] <- sentiment_results[[part]] %>%
    group_by(anonymous_id) %>%
    # Excluding 'word_count' column from syuzhet (already have token/word counter)
    summarise(Sentiment = sum(rowSums(across(c(anger, anticipation, disgust, fear, joy, sadness, surprise, trust, negative, positive)), na.rm = TRUE)), .groups = "drop")
}

## Aggregate ordinal ratings for P3 using Mean Index
# Create renaming mapping
rename_mapping <- setNames(names(df)[unlist(quantitative_columns)], names(quantitative_columns))

# Rename columns and compute composites
df_processed <- df %>%
  rename(all_of(rename_mapping)) %>%
  rowwise() %>%
  mutate(
    P3_Mean_Helpfulness = mean(c(P3_1_R1, P3_2_R1, P3_3_R1, P3_4_R1, P3_5_R1), na.rm = TRUE),
    P3_Mean_Usage       = mean(c(P3_1_R2, P3_2_R2, P3_3_R2, P3_4_R2, P3_5_R2), na.rm = TRUE)
  ) %>%
  ungroup()

## Initialize unified data frames for P1 - P3
# Combining Likert ratings, word count, frequency metrics, and sentiment metrics (composites)
analysis_frames <- list()

# Part 1 Master Frame
analysis_frames$P1 <- df_processed %>%
  select(anonymous_id, P1_R1, P1_R2, P1_R3) %>%
  left_join(word_count_results$P1, by = "anonymous_id") %>%
  left_join(flat_frequency$P1, by = "anonymous_id") %>%
  left_join(flat_sentiment$P1, by = "anonymous_id")

# Part 2 (Subsection 1) Master Frame
analysis_frames$P2_1 <- df_processed %>%
  select(anonymous_id, P2_R) %>%
  left_join(word_count_results$P2_1, by = "anonymous_id") %>%
  left_join(flat_frequency$P2_1, by = "anonymous_id") %>%
  left_join(flat_sentiment$P2_1, by = "anonymous_id")

# Part 2 (Subsection 2) Master Frame
analysis_frames$P2_2 <- df_processed %>%
  select(anonymous_id, P2_R) %>%
  left_join(word_count_results$P2_2, by = "anonymous_id") %>%
  left_join(flat_frequency$P2_2, by = "anonymous_id") %>%
  left_join(flat_sentiment$P2_2, by = "anonymous_id")

# Part 3 Master Frame
analysis_frames$P3 <- df_processed %>%
  select(anonymous_id, P3_Mean_Helpfulness, P3_Mean_Usage) %>%
  left_join(word_count_results$P3, by = "anonymous_id") %>%
  left_join(flat_frequency$P3, by = "anonymous_id") %>%
  left_join(flat_sentiment$P3, by = "anonymous_id")

#### DESCRIPTIVE STATISTICS ####
# Calculate descriptive statistics
calculate_descriptives <- function(dataframe, part_label) {
  numeric_cols <- setdiff(names(dataframe), "anonymous_id")
  
  results <- lapply(numeric_cols, function(col_name) {
    vec <- dataframe[[col_name]]
    vec_clean <- vec[!is.na(vec)]
    
    data.frame(
      Prompt   = part_label,
      Variable = col_name,
      N        = length(vec_clean),
      Mean     = round(mean(vec_clean), 2),
      SD       = round(sd(vec_clean), 2),
      Median   = round(median(vec_clean), 2),
      IQR      = round(IQR(vec_clean), 2),
      Min      = round(min(vec_clean), 2),
      Max      = round(max(vec_clean), 2)
    )
  })
  return(bind_rows(results))
}

# Compile and print descriptive stats table
descriptive_table <- bind_rows(
  calculate_descriptives(analysis_frames$P1, "Part 1 (General Course Evaluation)"),
  calculate_descriptives(analysis_frames$P2_1, "Part 2.1 (Behavioral Adaptations)"),
  calculate_descriptives(analysis_frames$P2_2, "Part 2.2 (Attitudinal Adaptations)"),
  calculate_descriptives(analysis_frames$P3, "Part 3 (Top 5 Selected Skills)")
)

# Descriptive stats table print & confirmation message in console
cat("\n--- DESCRIPTIVE STATISTICS TABLE ---\n")
print(descriptive_table, row.names = FALSE)


#### NON-PARAMTRIC SPEARMAN'S RANK-ORDER CORRELATION TEST ####
## Run Spearman correlations
run_spearman_matrix <- function(dataframe, target_ratings, part_label) {
  cor_results <- list()
  
  # Model 1: Likert Ratings vs. NLP Metrics
  for (rating in target_ratings) {
    # Likert Score vs. Frequency Counts
    test_freq <- cor.test(dataframe[[rating]], dataframe$Frequency, method = "spearman", exact = FALSE)
    cor_results[[length(cor_results) + 1]] <- data.frame(
      Prompt      = part_label,
      Variable_A  = rating,
      Variable_B  = "Frequency",
      Spearman_Rho= round(test_freq$estimate, 3),
      p_value     = format.pval(test_freq$p.value, digits = 3, eps = 0.001)
    )
    
    # Likert Score vs. Sentiment Counts
    test_sent <- cor.test(dataframe[[rating]], dataframe$Sentiment, method = "spearman", exact = FALSE)
    cor_results[[length(cor_results) + 1]] <- data.frame(
      Prompt      = part_label,
      Variable_A  = rating,
      Variable_B  = "Sentiment",
      Spearman_Rho= round(test_sent$estimate, 3),
      p_value     = format.pval(test_sent$p.value, digits = 3, eps = 0.001)
    )
  }
  
  # Model 2: Frequency Counts vs. Sentiment Counts
  test_nlp_intercor <- cor.test(dataframe$Frequency, dataframe$Sentiment, method = "spearman", exact = FALSE)
  cor_results[[length(cor_results) + 1]] <- data.frame(
    Prompt      = part_label,
    Variable_A  = "Frequency",
    Variable_B  = "Sentiment",
    Spearman_Rho= round(test_nlp_intercor$estimate, 3),
    p_value     = format.pval(test_nlp_intercor$p.value, digits = 3, eps = 0.001)
  )
  
  return(bind_rows(cor_results))
}

# Execute mapping across all domains
spearman_table <- bind_rows(
  run_spearman_matrix(analysis_frames$P1, c("P1_R1", "P1_R2", "P1_R3"), "Part 1"),
  run_spearman_matrix(analysis_frames$P2_1, "P2_R", "Part 2.1"),
  run_spearman_matrix(analysis_frames$P2_2, "P2_R", "Part 2.2"),
  run_spearman_matrix(analysis_frames$P3, c("P3_Mean_Helpfulness", "P3_Mean_Usage"), "Part 3")
)

# Spearman's correlations table print & confirmation message in console
cat("\n--- SPEARMAN RANK CORRELATION RESULTS ---\n")
print(spearman_table, row.names = FALSE)

#### HIERARCHICAL BOOTSTRAPPED LINEAR REGRESSION ####
# Mapping DVs to corresponding dataframes
regression_map <- list(
  P1_Enjoyment    = list(frame = "P1",   dv = "P1_R1"),
  P1_Meaning      = list(frame = "P1",   dv = "P1_R2"),
  P1_Engagement   = list(frame = "P1",   dv = "P1_R3"),
  P2_1_Helpful    = list(frame = "P2_1", dv = "P2_R"),
  P2_2_Helpful    = list(frame = "P2_2", dv = "P2_R"),
  P3_Composite_Helpful = list(frame = "P3", dv = "P3_Mean_Helpfulness"),
  P3_Composite_Usage   = list(frame = "P3", dv = "P3_Mean_Usage")
)

# Extractor function pulling the full 4-coefficient vector for bootstrapping Block 3
boot_regression_extractor <- function(data, indices, formula) {
  d <- data[indices, ]
  fit <- lm(formula, data = d)
  return(coef(fit))
}

final_regression_results <- list()
set.seed(1234) # For reproducibility

for (model_name in names(regression_map)) {
  # Extract target dataframe & DV variable column for current model (Block 1, 2, or 3)
  target_frame <- analysis_frames[[regression_map[[model_name]]$frame]]
  dv_col       <- regression_map[[model_name]]$dv
  
  # Build clean matrix from raw counts
  reg_data <- data.frame(
    DV         = target_frame[[dv_col]],
    Word_Count = target_frame$Word_Count,
    Frequency  = target_frame$Frequency,
    Sentiment  = target_frame$Sentiment
  ) %>% na.omit()
  
  # Define formulas to isolate sentiment
  formula_block1 <- DV ~ Word_Count
  formula_block2 <- DV ~ Word_Count + Frequency
  formula_block3 <- DV ~ Word_Count + Frequency + Sentiment # Sentiment is entered LAST
  
  #Fit linear models for each hierarchical block (1-3)
  model_b1 <- lm(formula_block1, data = reg_data)
  model_b2 <- lm(formula_block2, data = reg_data)
  model_b3 <- lm(formula_block3, data = reg_data)
  
  # Calculate Delta R2 explicitly from Block 2 to Block 3 (Isolating Sentiment)
  r2_b2 <- summary(model_b2)$r.squared
  r2_b3 <- summary(model_b3)$r.squared
  delta_r2_sentiment <- r2_b3 - r2_b2
  
  # Extract parametric statistics from the final complete model (Block 3)
  summary_b3 <- summary(model_b3)$coefficients
  
  # Run non-parametric bootstrapping on final model (5,000 resamples)
  boot_execution <- boot(
    data      = reg_data, 
    statistic = boot_regression_extractor, 
    R         = 5000, 
    formula   = formula_block3
  )
  
  # Define predictors
  predictors <- c("Word_Count", "Frequency", "Sentiment")
  
  # Extract values and format into clean predictor-specific dataframe row
  model_rows <- lapply(seq_along(predictors), function(idx) {
    pred_name <- predictors[idx]
    
    # Extract empirical Percentile CIs - idx + 1 avoids the intercept slot
    boot_ci  <- boot.ci(boot_execution, type = "perc", index = idx + 1)
    ci_lower <- boot_ci$percent[4]
    ci_upper <- boot_ci$percent[5]
    
    data.frame(
      Model_Target       = model_name,
      Predictor          = pred_name,
      Unstandard_B       = round(summary_b3[pred_name, "Estimate"], 4),
      Std_Error          = round(summary_b3[pred_name, "Std. Error"], 4),
      t_stat             = round(summary_b3[pred_name, "t value"], 3),
      p_value            = format.pval(summary_b3[pred_name, "Pr(>|t|)"], digits = 3, eps = 0.001),
      Boot_CI_Low        = round(ci_lower, 4),
      Boot_CI_High       = round(ci_upper, 4),
      Sentiment_Delta_R2 = if(pred_name == "Sentiment") round(delta_r2_sentiment, 4) else NA
    )
  })
  
  # Bind rows for this model & store into master list
  final_regression_results[[model_name]] <- bind_rows(model_rows)
}
# Combine results from all models into one master table
hierarchical_regression_table <- bind_rows(final_regression_results)

# Hierarchical linear regression table print & confirmation message in console
cat("\n--- HIERARCHICAL LINEAR BOOTSTRAPPED REGRESSION TABLE ---\n")
print(hierarchical_regression_table, row.names = FALSE)

#################################### EXPORTS: PNGs & EXCEL ####################################

#### EXPORT FOLDER MANAGEMENT ####
## Like CSV data import, a dialogue box will open to select the save location (ie., file path) of all exports within the master folder
cat("Please select the location where you want to save your 'master folder'...\n")
parent_dir <- choose.dir(caption = "Select chosen folder to save your exports")

# Ensure save location is selected
if (is.na(parent_dir)) {
  stop("Export cancelled: No save location selected.")
}

## Define folder paths within chosen save location
master_folder <- file.path(parent_dir, "MAY23_EXPORTS") # Includes all exports
freq_dir      <- file.path(master_folder, "WC_FA")      # Includes Frequency Analysis word clouds (raw)
lda_dir       <- file.path(master_folder, "WC_LDA")     # Includes LDA word clouds (weighted)
stats_dir     <- file.path(master_folder, "QUANT_STATS")      # Includes quantitative & statistical findings

# Create all folders at once using the full path
dir.create(master_folder, showWarnings = FALSE)
dir.create(freq_dir, showWarnings = FALSE)
dir.create(lda_dir, showWarnings = FALSE)
dir.create(stats_dir, showWarnings = FALSE)

#### WORD CLOUD GENERATION & EXPORT ####
## Generate word clouds for Frequency Analysis
for (part_name in names(count_tables_list)) {
  all_data <- do.call(rbind, count_tables_list[[part_name]])
  agg_freq <- tapply(all_data$Present_Count, all_data$Target_String, sum)
  
  # Save word clouds as PNG to respective folder in master folder
  save_wordcloud(agg_freq, paste0("WordCloud_", part_name, ".png"), freq_dir)
}

## Generate weighted word clouds for LDA
for (part_name in names(lda_results)) {
  if(!is.null(lda_results[[part_name]])) {
    mat <- lda_results[[part_name]] # 5x8 matrix
    
    # Create a weighted matrix for proportional ranking algorithm representative of LDA terms per topic in each part (Rank 1 = 8 points, Rank 8 = 1 point)
    term_scores <- setNames(numeric(), character())
    
    # Loop through matrix and calculate weighted scores
    for (col in 1:ncol(mat)) {
      for (row in 1:nrow(mat)) {
        term <- mat[row, col]
        weight <- (nrow(mat) - row + 1) # Rank 1 = 8, Rank 8 = 1
        
        # Add weight to the term in tally
        if (term %in% names(term_scores)) {
          term_scores[term] <- term_scores[term] + weight
        } else {
          term_scores[term] <- weight
        }
      }
    }
    
    # Save word clouds as PNG to respective folder in master folder
    save_wordcloud(term_scores, paste0("WordCloud_LDA_", part_name, ".png"), lda_dir)
  }
}

cat("\nWordcloud exporting complete. All word clouds saved to:", master_folder, "\n")

#### EXCEL WORKBOOK GENERATION & EXPORT ####
### Create Excel workbook
wb <- createWorkbook()

## Add statistical sheets
# Descriptive stats
addWorksheet(wb, "Descriptive_Stats")
writeData(wb, "Descriptive_Stats", descriptive_table)

# Spearman's rank order correlation test
addWorksheet(wb, "Spearman_Results")
writeData(wb, "Spearman_Results", spearman_table)

# Hierarchical bootstrapped linear regression (with verbosity control)
addWorksheet(wb, "Regression_Results")
writeData(wb, "Regression_Results", hierarchical_regression_table)

## Add dynamic sheets -- Frequency Analysis, Sentiment Analysis, and LDA Topic Modeling for each part
for (part in parts_list) {
  # LDA Tables
  if (!is.null(lda_results[[part]])) {
    sheet_name <- paste0("LDA_", part)
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, as.data.frame(lda_results[[part]]))
  }
  
  # Frequency Tables
  sheet_name_f <- paste0("Freq_", part)
  addWorksheet(wb, sheet_name_f)
  # Combine the list into one DF for the sheet
  writeData(wb, sheet_name_f, bind_rows(count_tables_list[[part]]))
  
  # Sentiment Tables
  sheet_name_s <- paste0("Sent_", part)
  addWorksheet(wb, sheet_name_s)
  writeData(wb, sheet_name_s, sentiment_results[[part]])
}

# 5. Save the workbook
saveWorkbook(wb, file.path(stats_dir, "MAY23_QUANT_STATS.xlsx"), overwrite = TRUE)

cat("\nExcel exporting complete. Workbook saved to:", stats_dir, "\n")


