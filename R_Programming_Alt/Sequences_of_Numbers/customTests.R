# Returns TRUE if e$expr matches any of the expressions given
# (as characters) in the argument.
ANY_of_exprs <- function(...){
  e <- get("e", parent.frame())
  any(sapply(c(...), function(expr)omnitest(expr)))
}

notify <- function() {
  e <- get("e", parent.frame())
  if(e$val == "No") return(TRUE)
  message("What is your MAT 331 ID number? \n")
  ID <- readinteger()
  
  # Get course and lesson names
  course_name <- attr(e$les, "course_name")
  lesson_name <- attr(e$les, "lesson_name")
  seed <- attr(e$les,"author")
  subject <- paste(course_name, "-", lesson_name)
  
  if(seed%%2 == 0)
  { code = seed + 2*ID}
  else { code = seed - 3*ID}
  hrule()  
  message("Your code for ",subject)
  message("is: ", code)
  hrule()
  
  # Return TRUE to satisfy swirl and return to course menu
  TRUE
}

readline_clean <- function(prompt = "") {
  wrapped <- strwrap(prompt, width = getOption("width") - 2)
  mes <- stringr::str_c("| ", wrapped, collapse = "\n")
  message(mes)
  readline()
}

readinteger <- function()
{ 
  n <- readline(prompt="Enter your ID: ")
  return(as.integer(n))
}

hrule <- function() {
  message("\n", paste0(rep("#", getOption("width") - 2), collapse = ""), "\n")
}