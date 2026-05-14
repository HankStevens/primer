# List all data files
data_files <- list.files("data", pattern = "\\.(rda|RData)$", full.names = TRUE)

# Check each one for namespace references
for (f in data_files) {
  env <- new.env()
  load(f, envir = env)
  for (obj_name in ls(env)) {
    obj <- get(obj_name, envir = env)
    # Print the object's class and any S4/R6 references
    cat(f, ":", obj_name, "- class:", class(obj), "\n")
  }
}

for (f in data_files) {
  cat(f, ":\n")
  con <- gzfile(f, "rb")
  raw_bytes <- readBin(con, "raw", n = file.info(f)$size * 2)
  close(con)

  raw_text <- rawToChar(raw_bytes[raw_bytes > 0])
  matches <- regmatches(raw_text, gregexpr("[a-zA-Z][a-zA-Z0-9.]*", raw_text))[[1]]

  # Look for known package names among the strings
  installed <- rownames(installed.packages())
  found <- unique(matches[matches %in% installed])
  cat("  Possible namespace references:", paste(found, collapse = ", "), "\n\n")
}

tools::checkRdaFiles("data")
roxygen2::roxygenise()
devtools::check()
gitcreds::gitcreds_set()
