# Caesar cipher function (encrypt or decrypt)
caesar_cipher <- function(text, shift, mode = "encrypt") {
  # Normalize shift to range 0-25
  shift <- shift %% 26
  
  # For decryption, just reverse the shift
  if (mode == "decrypt") {
    shift <- -shift
  }
  
  chars <- strsplit(text, "")[[1]]   # Split into characters
  result <- character(length(chars)) # Pre-allocate vector
  
  for (i in seq_along(chars)) {
    char <- chars[i]
    
    if (grepl("[A-Z]", char)) {
      base <- as.integer(charToRaw("A"))
      result[i] <- rawToChar(as.raw((as.integer(charToRaw(char)) - base + shift + 26) %% 26 + base))
    } else if (grepl("[a-z]", char)) {
      base <- as.integer(charToRaw("a"))
      result[i] <- rawToChar(as.raw((as.integer(charToRaw(char)) - base + shift + 26) %% 26 + base))
    } else {
      result[i] <- char  # Keep non-alphabet characters unchanged
    }
  }
  
  return(paste0(result, collapse = ""))
}

# ---------------- Main Program ----------------
sentence <- readline(prompt = "Enter a sentence: ")
shift <- as.integer(readline(prompt = "Enter shift value: "))
mode <- readline(prompt = "Choose mode (encrypt/decrypt): ")

output <- caesar_cipher(sentence, shift, mode)
cat("Result:", output, "\n")

