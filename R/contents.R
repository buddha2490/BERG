contents <- function(data, varnum = T, write = NULL, outfile = NULL, filename = NULL){

packages <- (.packages())
if (!"dplyr" %in% packages) require(dplyr,quietly=T)
if (!"xlsx" %in% packages) require(xlsx,quietly=T)
if (!"officer" %in% packages) require(officer,quietly=T)


  # If write option is indicated, check to make sure output file and filename are given
  if (!is.null(write)){
    if (write %in% c("word", "excel") & (is.null(outfile) | is.null(filename))){
      warning("No file path or file name provided\nNo output file created")
    } else if (!write %in% c("word", "excel")){
      warning('Invalid option for write parameter - must be either "word" or "excel"\nNo output file created')
    }
  }

  df <- data

  # Extract variable names
  vars <- names(df)

  # Get classes of variables
  classes <- data.frame(class = unlist(lapply(vars, function(x) class(df[, x]))))

  # Get labels - only going to be around for some data that was pulled in from SAS data sets
  # so if NULL then just setting as NA
  labels <- data.frame(label = unlist(lapply(df, function(x) ifelse(is.null(attributes(x)$label), NA, attributes(x)$label))))

  # Format for factors
  format <- data.frame(format = unlist(lapply(df, function(x){
    ifelse(is.factor(x), paste(c(1:nlevels(x)), levels(x), sep = " = ", collapse = "; "), "")})))

  # A couple things for a metadata type table
  num_obs <- as.character(nrow(df))
  nvars <- as.character(ncol(df))
  df_name <- deparse(substitute(data))
  size <- object.size(df)

  #------------- Set up output
  # Create meta table
  meta <- data.frame(attributes = c("Data Set Name", "Observations", "Variables", "Size (bytes)"),
                     values = c(df_name, num_obs, nvars, size))

  # Create variable table
  variable.table <- data.frame(index = c(1:length(vars)), variables = vars, class = classes, format = format, labels = labels)
  row.names(variable.table) <- NULL

  if (varnum == F) {
    variable.table <- arrange(variable.table, variables)
  }

  # Wrap in list for output
  contents <- list(metadata = meta, var_table = variable.table)

  # Conditional output statements - if write is not specified, no output file is created
  if (!is.null(write)){
    if (write == "word"){

      doc <- read_docx() %>%
        body_add_table(value = meta, style = "table_template")

      doc<-body_add_par(doc, value = "")
      doc<-body_add_table(x = doc, value = variable.table, pos = "after", style = "table_template")

      print(doc, target = paste0(outfile, "/", filename, ".docx"))

    } else if (write == "excel"){
      write.xlsx(contents[[1]], file = paste0(outfile, "/", filename, ".xlsx"), sheetName = "Meta Data", row.names = F)
      write.xlsx(contents[[2]], file = paste0(outfile, "/", filename, ".xlsx"), sheetName = "Variable Table", row.names = F, append = T)
    }
  }
  # Always returns a list of the meta table and variable table to the global environment
  return(contents)

}
