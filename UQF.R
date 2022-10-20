library(tidyverse)
library(tools)
library(readxl)
library(rmarkdown)

read_uqf <- function(uqf_zip_path, verify = T) {
  uqf_paths <- unzip(uqf_zip_path, list = TRUE) %>% pull(Name)
  
  qtbl_paths <- str_extract(uqf_paths, regex("^(?!~\\$|\\.~).*(\\.xlsx|\\.csv|\\.tsv)$", multiline = T))
  
  uqf_dir <- file_path_sans_ext(uqf_zip_path)
  unzip(uqf_zip_path, junkpaths = T, exdir = uqf_dir, overwrite = T)
  uqf_dir_paths <- list.files(uqf_dir, full.names = T)
  
  # Standardise all filenames to lowercase
  renamed <- file.rename(list.files(uqf_dir, full.names = T),
              file.path(uqf_dir, tolower(list.files(uqf_dir))))
  
  if(verify == T) {
    if(all(renamed)) {
      cat("Standardised filenames to lowercase.\n")
    } else {
      stop("Unable to rename files, aborting.")
    }
  }
  
  if(verify == T) {
    # Verify there is only one question table
    if(sum(!is.na(qtbl_paths)) == 1) {
      qtbl_path <- qtbl_paths[!is.na(qtbl_paths)]
      cat("Question table found.\n")
    } else {
      stop("Multiple tables detected, aborting.")
    }
  }
  
  switch(file_ext(qtbl_path),
         "xlsx" = {
           read_qtbl <- read_excel
         },
         "csv" = {
           read_qtbl <- read_csv
         },
         "tsv" = {
           read_qtbl <- read_tsv
         })
  
  qtbl <- read_qtbl(file.path(uqf_dir, basename(qtbl_path))) %>%
    mutate(
      Question = img_refs_to_lowercase(Question),
      Options = img_refs_to_lowercase(Options),
      Explanation = img_refs_to_lowercase(Explanation)
    )

  if(verify == T) {
    # Verify required columns exist in qtbl
    if(all(c("Question", "Options", "Answers", "Explanation") %in% colnames(qtbl))) {
      cat("Required columns found in question table\n")
    } else {
      stop("Missing columns, aborting.")
    }
    # Verify all image references point to an existing image
    img_refs <- unlist(str_extract_all(format_csv(qtbl), "\\[\\[.*\\]\\]")) %>% str_replace_all("(\\[\\[|\\]\\])", "")
    uqf_names <- list.files(uqf_dir)
    if(all(img_refs %in% uqf_names)){
      cat("All referenced image files found.\n")
    } else {
      stop("Referenced image file not found, aborting.")
    }
  }

  cat("UQF successfully imported.\n")
  return(qtbl)
}

qtbl_to_html <- function(qtbl, randomise_op = F, image_dir = "images", output_filename = "questions") {
  qtbl_html <- qtbl %>%
    mutate(
      Question = img_refs_to_html(Question, image_dir),
      Options = img_refs_to_html(Options, image_dir),
      Explanation = img_refs_to_html(Explanation, image_dir)
    ) %>%
    rowwise() %>%
    mutate(
      Op_Ans = list(tibble(
        Alpha = str_extract_all(Options, regex("^[A-Z](?=\\. )", multiline = T)) %>% unlist,
        Choice = str_extract_all(Options, regex("(?<=^[A-Z]\\. )(.*?)(?=^[A-Z]\\. |\\Z)", multiline = T, dotall = T)) %>% unlist %>% str_trim,
        Correct = Alpha %in% (str_split(Answers, ",") %>% unlist %>% str_trim)
      ))
    )
  
  if(randomise_op == T) {
    qtbl_html <- qtbl_html %>%
      mutate(
        Op_Ans = list(Op_Ans %>%
                        slice_sample(prop = 1) %>%
                        mutate(
                          Alpha = LETTERS[1:length(Alpha)]
                        )
        )
      )
  }
  
  qtbl_html <- qtbl_html %>%
    rowid_to_column(var = "Q_No") %>%
    mutate(
      Q_No = paste("Q", Q_No, ") ", sep = ""),
      Question_html = paste('<div class="question">', Q_No, Question, '</div>', sep = ""),
      Explanation_html = paste('<div class="explanation">', Explanation, '</div>', sep = ""),
      Op_Ans = list(Op_Ans %>%
                      mutate(
                        Choice_html = paste0('<div class="option">', Alpha, '. ', Choice, '</div>'),
                        Choice_Ans_html = if_else(Correct, 
                                                  paste0('<div class="option correct">', Alpha, '. ', Choice, '</div>'), 
                                                  paste0(Choice_html))
                      )
      ),
      Options_html = paste('<div class="option_container">', paste(Op_Ans$Choice_html, collapse = ""), '</div>', sep = "\n"),
      Op_Ans_html = paste('<div class="option_container">', paste(Op_Ans$Choice_Ans_html, collapse = ""), '</div>', sep = "\n"),
      Final_Q_html = paste('<div class="question_container">', Question_html, Options_html, '</div>', sep = "\n"),
      Final_QnA_html = paste('<div class="question_container">', Question_html, Op_Ans_html, Explanation_html, '</div>', sep = "\n")
    )
  
  html_head =
  '<head>
    <link rel="stylesheet" href="styles.css"> 
  </head>'
  
  q_output_filename = paste(output_filename, "_Q", sep = "")
  qna_output_filename = paste(output_filename, "_QnA", sep = "")
  
  write_lines(paste(html_head, paste(qtbl_html$Final_Q_html, collapse = "\n"), sep = "\n"), paste(q_output_filename, ".html", sep = ""))
  write_lines(paste(html_head, paste(qtbl_html$Final_QnA_html, collapse = "\n"), sep = "\n"), paste(qna_output_filename, ".html", sep = ""))

  return(qtbl_html)
}

img_refs_to_html <- function(string, image_dir) {
  return(str_replace_all(string, "(\\[\\[)(.*)(\\]\\])", paste('<img src="', image_dir, '/\\2">', sep = "")))
}

img_refs_to_lowercase <- function(string, image_dir) {
  return(str_replace_all(string, "(\\[\\[)(.*)(\\]\\])", replacement = tolower))
}

uqf_to_html <- function(uqf_zip_path, verify = T, randomise_op = F) {
  uqf_name <- file_path_sans_ext(basename(uqf_zip_path))
  qtbl <- read_uqf(uqf_zip_path, verify = verify)
  qtbl_html <- qtbl_to_html(qtbl, randomise_op = randomise_op, image_dir = uqf_name, output_filename = uqf_name)
  return(list(
    qtbl = qtbl, 
    qtbl_html = qtbl_html))
}
