library(tidyverse)
library(tools)
library(readxl)
library(rmarkdown)

read_uqf <- function(uqf_zip_path, verify = T) {
  uqf_paths <- unzip(uqf_zip_path, list = TRUE) %>% pull(Name)
  
  uqf_dir <- file_path_sans_ext(uqf_zip_path)
  unzip(uqf_zip_path, junkpaths = T, exdir = uqf_dir, overwrite = T)
  
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
  
  uqf_dir_paths <- list.files(uqf_dir, full.names = T)
  qtbl_paths <- str_extract(uqf_dir_paths, regex("^(?!~\\$|\\.~).*(\\.xlsx|\\.csv|\\.tsv)$", multiline = T))
  qtbl_path <- qtbl_paths[!is.na(qtbl_paths)][1]
  
  n_qtbl = sum(!is.na(qtbl_paths))
  if(n_qtbl == 1) {
    cat("Question table found.\n")
  } else if(n_qtbl == 0) {
    stop("No table detected, aborting.")
  } else {
    if(verify) {
      stop("Multiple tables detected, aborting.")
    } else {
      cat("Warning: Multiple tables detected.\n")
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
    ) %>%
    select(Question, Options, Answers, Explanation)

  if(verify == T) {
    # Verify required columns exist in qtbl
    if(all(c("Question", "Options", "Answers", "Explanation") %in% colnames(qtbl))) {
      cat("Required columns found in question table\n")
    } else {
      stop("Missing columns, aborting.")
    }
    # Verify all image references point to an existing image
    img_refs <- unlist(str_extract_all(format_csv(qtbl), "\\[\\[.*?\\]\\]")) %>% str_replace_all("(\\[\\[|\\]\\])", "")
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

uqf_to_iqf <- function(uqf_tbl, mode = "strict", custom_alpha_regex, custom_sep_regex) {
  switch(mode,
         "lenient" = {
           alpha_regex = "[A-Za-z]"
           sep_regex = "[\\.\\)\\|\\-:;. ] ?"
         },
         "custom" = {
           alpha_regex = custom_alpha_regex
           sep_regex = custom_sep_regex
         },
         {
           alpha_regex = "[A-Z]"
           sep_regex = "\\. "
         })
  
  iqf_tbl <- uqf_tbl %>%
    rowwise() %>%
    mutate(
      Op_Ans = list(tibble(
        Alpha = str_extract_all(Options, 
                                regex(paste0("^", alpha_regex, "(?=", sep_regex, ")"),
                                multiline = T)) %>% unlist,
        Choice = str_extract_all(Options, 
                                 regex(paste0("(?<=^", alpha_regex, sep_regex, ")(.*?)(?=^", alpha_regex, sep_regex, "|\\Z)"),
                                 multiline = T, 
                                 dotall = T)) %>% unlist %>% str_trim,
        Correct = Alpha %in% (str_split(Answers, ",") %>% unlist %>% str_trim)
      ))
    ) %>%
    select(-Options, -Answers)
  return(iqf_tbl)
}

iqf_to_uqf <- function(iqf_tbl) {
  uqf_tbl <- iqf_tbl %>%
    mutate(
      Answers = Op_Ans %>% filter(Correct) %>% pull(Alpha) %>% paste(collapse = ", "),
      Options = paste(Op_Ans$Alpha, Op_Ans$Choice, sep = ". ", collapse = "\n")
    )
  return(uqf_tbl)
}

iqf_to_html <- function(iqf_tbl, randomise_op = F, image_dir = "images", output_filename = "questions") {
  html_tbl <- iqf_tbl %>%
    mutate(
      Question = img_refs_to_html(Question, image_dir),
      Explanation = img_refs_to_html(Explanation, image_dir),
      Op_Ans = list(Op_Ans %>%
                      mutate(
                        Choice = img_refs_to_html(Choice, image_dir)
                      ))
    )
  
  if(randomise_op == T) {
    html_tbl <- html_tbl %>%
      mutate(
        Op_Ans = list(Op_Ans %>%
                        slice_sample(prop = 1) %>%
                        mutate(
                          Alpha = LETTERS[1:length(Alpha)]
                        )
        )
      )
  }
  
  html_tbl <- html_tbl %>%
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
    <link rel="preconnect" href="https://fonts.googleapis.com">
    <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
    <link href="https://fonts.googleapis.com/css2?family=Inter&display=swap" rel="stylesheet">
  </head>'
  
  q_output_filename = paste(output_filename, "_Q", sep = "")
  qna_output_filename = paste(output_filename, "_QnA", sep = "")
  
  write_lines(paste(html_head, paste(html_tbl$Final_Q_html, collapse = "\n"), sep = "\n"), paste(q_output_filename, ".html", sep = ""))
  write_lines(paste(html_head, paste(html_tbl$Final_QnA_html, collapse = "\n"), sep = "\n"), paste(qna_output_filename, ".html", sep = ""))
  
  return(html_tbl)
}

img_refs_to_lowercase <- function(string, image_dir) {
  return(str_replace_all(string, "(\\[\\[)(.*?)(\\]\\])", replacement = tolower))
}

img_refs_to_html <- function(string, image_dir) {
  return(str_replace_all(string, "(\\[\\[)(.*?)(\\]\\])", paste('<img src="', image_dir, '/\\2">', sep = "")))
}

uqf_to_html <- function(uqf_zip_path, verify = T, mode = "strict", randomise_op = F) {
  uqf_name <- file_path_sans_ext(basename(uqf_zip_path))
  uqf_tbl <- read_uqf(uqf_zip_path, verify = verify)
  iqf_tbl <- uqf_to_iqf(uqf_tbl, mode = mode)
  html_tbl <- iqf_to_html(iqf_tbl, randomise_op = randomise_op, image_dir = uqf_name, output_filename = uqf_name)
  return(list(
    uqf = uqf_tbl,
    iqf = iqf_tbl, 
    html_tbl = html_tbl))
}
