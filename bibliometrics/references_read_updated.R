# ADDING 
# 
# AR: article number

references_read_updated<-function (data = ".", dir = FALSE, include_all = FALSE) 
  # can you see this????
{
  output <- data.frame(filename = character(0), AB = character(0), 
                       AF = character(0), AU = character(0), CA = character(0), 
                       BP = character(0), C1 = character(0), CC = character(0), 
                       CH = character(0), CL = character(0), CR = character(0), 
                       CT = character(0), CY = character(0), DE = character(0), 
                       DI = character(0), DT = character(0), EM = character(0), 
                       EP = character(0), FN = character(0), FU = character(0), 
                       FX = character(0), GA = character(0), GE = character(0), 
                       ID = character(0), IS = character(0), J9 = character(0), 
                       JI = character(0), LA = character(0), LT = character(0), 
                       MC = character(0), MI = character(0), NR = character(0), 
                       PA = character(0), PD = character(0), PG = character(0), 
                       PI = character(0), PN = character(0), PS = character(0), 
                       PT = character(0), PU = character(0), PY = character(0), 
                       RI = character(0), RID = character(0), OI = character(0), 
                       PM = character(0), RP = character(0), SC = character(0), 
                       SI = character(0), SN = character(0), SO = character(0), 
                       SU = character(0), TA = character(0), TC = character(0), 
                       TI = character(0), UT = character(0), VR = character(0), 
                       VL = character(0), WC = character(0), Z9 = character(0), 
                       AR = character(0), 
                       stringsAsFactors = FALSE)
  i <- 1
  if (dir) {
    file_list <- dir(path = data)
  }
  else {
    file_list <- data
  }
  file_list <- file_list[grep(".ciw|.txt", file_list)]
  if (length(file_list) == 0) {
    stop("ERROR:  The specified file or directory does not contain any\n      Web of Knowledge or ISI Export Format records!")
  }
  message("Now processing all references files")
  filename <- file_list[1]
  counter <- 1
  for (filename in file_list) {
    if (dir) {
      in_file <- file(paste0(data, "/", filename), "r")
    }
    if (!dir) {
      in_file <- file(filename, "r")
    }
    field <- ""
    read_line <- readLines(in_file, n = 1, warn = FALSE)
    if (length(read_line) > 0) {
      read_line <- gsub("^[^A-Z]*([A-Z]+)(.*)$", "\\1\\2", 
                        read_line)
      pre_text <- substr(read_line, 1, 2)
      line_text <- substr(read_line, 4, nchar(read_line))
      if (pre_text != "FN") {
        close(in_file)
        error <- paste0("ERROR:  The file ", filename, 
                        " doesn't appear to be a valid ISI or\n          Thomson Reuters reference library file!")
        stop(error)
      }
      if (substr(line_text, 1, 3) == "ISI") {
        field <- pre_text
        matches <- regexec("^(.*) VR (.*) PT (.*)", line_text)
        match_strings <- regmatches(line_text, matches)
        output[i, "FN"] <- paste(match_strings[[1]][2], 
                                 "\n", sep = "")
        output[i, "VR"] <- paste(match_strings[[1]][3], 
                                 "\n", sep = "")
        output[i, "PT"] <- paste(match_strings[[1]][4], 
                                 "\n", sep = "")
      }
      else {
        field <- pre_text
        if (field %in% names(output)) {
          output[i, field] <- ""
          output[i, field] <- trimws(ifelse(length(line_text) == 
                                              1, paste(output[i, field], line_text, sep = "\n")), 
                                     "both")
        }
      }
    }
    else {
      utils::flush.console()
      stop("WARNING:  Nothing contained in the specified file!")
    }
    while (length(read_line <- readLines(in_file, n = 1, 
                                         warn = FALSE)) > 0) {
      pre_text <- substr(read_line, 1, 2)
      line_text <- substr(read_line, 4, nchar(read_line))
      if (pre_text != "  ") {
        field <- pre_text
        if (field %in% names(output)) {
          output[i, field] <- ""
        }
      }
      if (field %in% names(output)) {
        output[i, field] <- trimws(ifelse(length(line_text) == 
                                            1, paste(output[i, field], line_text, sep = "\n")), 
                                   "both")
      }
      if (field == "ER") {
        output[i, "filename"] <- filename
        output[i, "FN"] <- output[1, "FN"]
        output[i, "VR"] <- output[1, "VR"]
        i <- i + 1
      }
    }
    close(in_file)
    total <- length(file_list)
    pb <- utils::txtProgressBar(min = 0, max = total, style = 3)
    utils::setTxtProgressBar(pb, counter)
    counter <- counter + 1
    utils::flush.console()
  }
  output$RI <- gsub("\n", "", output$RI, fixed = TRUE)
  output$RI <- gsub("; ", ";", output$RI, fixed = TRUE)
  output$RI[!grepl("/", output$RI)] < -NA
  output$OI <- gsub("\n", "", output$OI, fixed = TRUE)
  output$OI <- gsub("; ", ";", output$OI, fixed = TRUE)
  output$PY <- gsub("\n", "", output$PY, fixed = TRUE)
  output$C1 <- gsub("\n", "/", output$C1, fixed = TRUE)
  output$AF[is.na(output$AF)] <- output$AU[is.na(output$AF)]
  output$refID <- seq_len(nrow(output))
  dupe_output <- do.call(rbind, lapply(unique(output$UT), function(x) output[output$UT == 
                                                                               x, ][1, ]))
  if (include_all == TRUE) {
    return(dupe_output)
  }
  if (include_all != TRUE) {
    dropnames <- c("CC", "CH", "CL", "CT", "CY", "DT", "FX", 
                   "GA", "GE", "ID", "IS", "J9", "JI", "LA", "LT", "MC", 
                   "MI", "NR", "PA", "PI", "PN", "PS", "RID", "SI", 
                   "SU", "TA", "VR")
    rdo <- dupe_output[, !(names(dupe_output) %in% dropnames)]
    return(rdo)
  }
}
