# Function to validate and read student records
read_students <- function() {
  students <- data.frame(Name=character(),
                         Age=integer(),
                         Grade=character(),
                         stringsAsFactors=FALSE)
  
  repeat {
    name <- readline(prompt = "Enter student name (or 'done' to finish): ")
    if (tolower(name) == "done") break
    
    age <- as.integer(readline(prompt = "Enter age: "))
    grade <- toupper(readline(prompt = "Enter grade (A/B/C/D/F): "))
    
    # Validation checks
    if (is.na(age) || age <= 0) {
      cat("❌ Invalid age. Must be a positive integer.\n")
      next
    }
    if (!(grade %in% c("A", "B", "C", "D", "F"))) {
      cat("❌ Invalid grade. Must be A, B, C, D, or F.\n")
      next
    }
    
    # If valid, add to data frame
    students <- rbind(students, data.frame(Name=name, Age=age, Grade=grade, stringsAsFactors=FALSE))
    cat("✅ Student record added.\n")
  }
  
  return(students)
}

# -------- Main Program --------
cat("Enter student records. Type 'done' as name to finish.\n")
student_records <- read_students()

if (nrow(student_records) > 0) {
  avg_age <- mean(student_records$Age)
  cat("\nValid student records:\n")
  print(student_records)
  cat("\nAverage age of valid students:", avg_age, "\n")
} else {
  cat("\nNo valid student records entered.\n")
}


