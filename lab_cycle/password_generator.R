
# -------- Password Generator --------
generate_password <- function(length) {
  # Character sets
  uppercase <- LETTERS                # A-Z
  lowercase <- letters                # a-z
  digits <- as.character(0:9)         # 0-9
  special <- strsplit("!@#$%^&*()-_=+[]{};:,.<>?/~", "")[[1]]
  
  # Combine all characters
  all_chars <- c(uppercase, lowercase, digits, special)
  
  # Ensure password includes at least 1 of each type
  password <- c(
    sample(uppercase, 1),
    sample(lowercase, 1),
    sample(digits, 1),
    sample(special, 1)
  )
  
  # Fill remaining characters randomly
  if (length > 4) {
    remaining <- sample(all_chars, length - 4, replace = TRUE)
    password <- c(password, remaining)
  }
  
  # Shuffle characters so first 4 are not always predictable
  password <- sample(password)
  
  # Return as a single string
  return(paste0(password, collapse = ""))
}

# -------- Main Program --------
len <- as.integer(readline(prompt = "Enter desired password length: "))

if (is.na(len) || len < 4) {
  cat("❌ Password length must be at least 4.\n")
} else {
  pwd <- generate_password(len)
  cat("✅ Generated password:", pwd, "\n")
}

