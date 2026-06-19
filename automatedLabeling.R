library(rstudioapi)
library(stringr)

library(tokenizers)
library(dplyr)
library(stringdist)
library(knitr)

##########################################################################
##########################################################################

script_path <- rstudioapi::getActiveDocumentContext()$path
if (!is.null(script_path) || script_path != "") {
  directory <- dirname(script_path)
  inputdirectory <- str_c(directory, "/_input")
} else {
  stop("script path null")
}

##########################################################################
##########################################################################

#extention, singlelinecommentoperator, multilinecommentstarter, multilinecommentcloser, programminglanguage
commentrules <- list(
  c("c","//","/\\*","\\*/","C"),
  c("cpp","//","/\\*","\\*/","C++"),
  c("cs","//","/\\*","\\*/","C#"),
  c("java","//","/\\*","\\*/","JAVA"),
  c("m","%","%{","}%","MATLAB"),
  c("py","#","\"\"\"","\"\"\"","PYTHON"),
  c("r","#","NULL","NULL","R")
)

##########################################################################
##########################################################################

get_clean_comment_nospace <- function(comment_line) {
  if (length(comment_line) == 0 || is.na(comment_line)) return("")
  parts <- strsplit(comment_line, ";;", fixed = TRUE)[[1]]
  if (length(parts) < 2) return("")
  tag <- sub(".*::", "", parts[1])
  if (!(tag %in% c("COMC", "COMO"))) return("")
  comment <- parts[2]
  comment <- gsub("^[#/%\\s]+", "", comment)
  comment <- tolower(comment)
  comment <- gsub("[^a-z]", "", comment)    
  if (nchar(comment) < 3) return("")        
  comment
}

get_clean_comment <- function(comment_line) {
  if (length(comment_line) == 0 || is.na(comment_line)) return("")
  parts <- strsplit(comment_line, ";;", fixed = TRUE)[[1]]
  if (length(parts) < 2) return("")
  tag <- sub(".*::", "", parts[1])
  if (!(tag %in% c("COMC", "COMO"))) return("")
  comment <- parts[2]
  comment <- gsub("^[#/%\\s]+", "", comment)
  comment <- tolower(comment)
  comment <- gsub("[^a-z0-9 ]+", " ", comment)
  comment <- trimws(gsub("\\s+", " ", comment))
  if (nchar(comment) < 3) return("")
  comment
}

##########################################################################
##########################################################################

pl_selector = which(sapply(commentrules, function(x) "py" %in% x))
pl_extension = commentrules[[pl_selector]][1]
pl_name = commentrules[[pl_selector]][5]
pl_single = commentrules[[pl_selector]][2]
pl_multi_first = commentrules[[pl_selector]][3]
pl_multi_end = commentrules[[pl_selector]][4]
multicomment <- FALSE

##########################################################################
##########################################################################

file_paths <- list.files(inputdirectory, pattern = paste0("\\.", pl_extension, "$"), full.names = TRUE, recursive = TRUE)

##########################################################################
##########################################################################

process_file <- function(file) {
  file_content <- readLines(file, warn = FALSE, encoding = "UTF-8")
  code_lines <- c()
  comment_lines <- c()
  tagged_lines <- c()
  line_number <- 1
  
  #label as CODE, COMO, COMC, or COMM
  for (line in file_content) {
    line <- str_squish(line)
    if (str_trim(line) == "") next
    # multiline comment detection
    if (multicomment) {
      if (str_detect(line, pl_multi_end))
        multicomment = FALSE
      else {
        comment_lines <- c(comment_lines, paste0(line_number, "::", "COMM;;", line))
        tagged_lines <- c(tagged_lines, paste0(line_number, "::", "COMM;;",  line))
      }
    } else {
      if (str_detect(line, pl_multi_first)){
        multicomment = TRUE
      } else {
        #single line comment detection
        if (str_detect(line, pl_single)) {
          #comment only
          if (str_detect(line, paste0("^", pl_single))) {
            comment_lines <- c(comment_lines, paste0(line_number, "::", "COMO;;", line))
            tagged_lines <- c(tagged_lines, paste0(line_number, "::", "COMO;;",  line))
          } else{
            #includes code and comment in line
            comment_lines <- c(comment_lines, paste0(line_number, "::", "COMC;;", line))
            tagged_lines <- c(tagged_lines, paste0(line_number, "::", "COMC;;",  line))
          }
        } else {
          #code detected
          code_lines <- c(code_lines, paste0(line_number, "::", "CODE;;", line))
          tagged_lines <- c(tagged_lines, paste0(line_number, "::", "CODE;;",  line))
        }
      }
    }
    line_number <- line_number + 1 
  }
  return(list(code = code_lines, comments = comment_lines, tagged = tagged_lines))
}
for (file in file_paths) {
  process_file(file)
}
num_files <- length(file_paths)
num_dirs <- length(unique(dirname(file_paths)))

code_list <- c()
comment_list <- c()
tagged_list <- c()
for (file in file_paths) {
  result <- process_file(file)
  code_list <- c(code_list, result$code)
  comment_list <- c(comment_list, result$comments)
  tagged_list <- c(tagged_list, result$tagged)
}

writeLines(tagged_list, "output/_tagged_list.txt")
writeLines(code_list, "output/_code_list.txt")
writeLines(comment_list, "output/_comment_list.txt")

##########################################################################
##########################################################################

smellvocabulary <- read.table(str_c(directory, "/_smellvocabulary.txt"), sep = ";", header = TRUE, stringsAsFactors = FALSE)

smell_patterns_list <- lapply(1:nrow(smellvocabulary), function(i) {
  patterns <- as.character(smellvocabulary[i, -1])
  patterns <- patterns[!is.na(patterns) & patterns != ""]
  toks <- trimws(unlist(strsplit(paste(patterns, collapse=","), ",")))
  toks <- tolower(toks)
  toks[toks != ""]
})
smell_codes <- smellvocabulary[,1]

##########################################################################
##########################################################################

smell_matrix <- matrix(0, nrow = length(comment_list), ncol = length(smell_codes))
colnames(smell_matrix) <- smell_codes
rownames(smell_matrix) <- seq_along(comment_list)

for (i in seq_along(comment_list)) {
  comment_str <- get_clean_comment_nospace(comment_list[i])
  clean_comment_nospace <- comment_str
  
  comment_len <- nchar(comment_list[i])
  if (comment_len < 3 || comment_len > 100) {
    if ("IRRE" %in% smell_codes) {
      smell_matrix[i, "IRRE"] <- 1
    }
  }
  
  for (j in seq_along(smell_patterns_list)) {
    pats <- smell_patterns_list[[j]]
    if (length(pats) == 0) next
    matches <- sum(sapply(pats, function(pat) {
      if (pat == "") return(0) 
      stringr::str_count(comment_str, stringr::fixed(pat)) 
    }))
    smell_matrix[i, j] <- matches
  }
}

##########################################################################
##########################################################################

max_matches <- apply(smell_matrix, 1, max)
assigned_smell <- ifelse(max_matches > 0, colnames(smell_matrix)[apply(smell_matrix, 1, which.max)], "-")
assigned_smell_df <- data.frame(Comment_Index = seq_along(comment_list), Comment = comment_list, Assigned_Smell = assigned_smell, stringsAsFactors = FALSE)

##########################################################################
##########################################################################

sink("output/report_project_details.txt")
cat("::PROJECT REPORT::\n")
cat("Analyzed", num_dirs, ifelse(num_dirs > 1, "projects.\n", "project.\n"))
cat("Found", length(comment_list), "comments in", num_files, pl_name, ifelse(num_files > 1, "files.\n", "file.\n"))
cat("Found", sum(str_detect(comment_list, "COMO|COMC")), "inline comments and", sum(str_detect(comment_list, "COMM")), "multiline comments.\n\n")
sink()

output_path <- "output/smell_matrix.txt"
smell_matrix <- as.data.frame(smell_matrix)
clean_comments <- str_trim(sub(".*;;", "", comment_list))
first_matrix <- cbind(smell_matrix, COMMENT_SMELL = clean_comments)
write.table(first_matrix, file = output_path, sep = "\t", row.names = FALSE, col.names = TRUE, quote = TRUE, fileEncoding = "UTF-8")

#########################################################################
#########################################################################

prepare_clean_comments <- function(comment_list) {
  cleaned_space <- vapply(comment_list, get_clean_comment, FUN.VALUE = character(1))
  kept_idx <- which(nzchar(cleaned_space))
  list(clean_space = unname(cleaned_space[kept_idx]), kept_idx = kept_idx)
}

write_smell_label_list <- function(clean_comments, labels, outfile = "output/smell_label_list.txt") {
  if (!dir.exists("output")) dir.create("output")
  df <- data.frame(clean_comment = clean_comments, label = labels, stringsAsFactors = FALSE)
  write.table(df, file = outfile, sep = ";", row.names = FALSE, col.names = TRUE, quote = TRUE, fileEncoding = "UTF-8")
  cat("Saved smell label list to:", outfile, "\n")
}

clean_result <- prepare_clean_comments(comment_list)
clean_comments <- clean_result$clean_space
kept_idx      <- clean_result$kept_idx
assigned_clean_smell <- assigned_smell[kept_idx]
write_smell_label_list(clean_comments, assigned_clean_smell)