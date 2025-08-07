# Choose a file using file dialog
file_path <- file.choose()

# Read the file
lines <- readLines(file_path, warn = FALSE)

# Print contents
cat(lines, sep = "\n")


# Combine lines into paragraphs (assuming blank lines separate paragraphs)
paragraphs <- unlist(strsplit(paste(lines, collapse = "\n"), split = "\n\\s*\n"))

# Function to compute stats for a paragraph
paragraph_stats <- function(paragraph) {
  words <- unlist(strsplit(paragraph, "\\s+"))  # split into words
  words <- gsub("[[:punct:]]", "", words)       # remove punctuation
  words <- words[words != ""]                   # remove empty entries
  num_words <- length(words)
  avg_word_length <- if (num_words > 0) mean(nchar(words)) else 0
  return(list(num_words = num_words, avg_word_length = avg_word_length))
}

# Apply function to all paragraphs
results <- lapply(paragraphs, paragraph_stats)

# Convert to a data frame
results_df <- data.frame(
  Paragraph = seq_along(results),
  Num_Words = sapply(results, function(x) x$num_words),
  Avg_Word_Length = round(sapply(results, function(x) x$avg_word_length), 2)
)

# Print results
print(results_df)

