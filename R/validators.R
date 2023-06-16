# General purpose functions to run checks within customized data processing functions

############################## Formal arguments ####################################

check_defined <- function(y){

  err <- paste(y, "must be specified", sep = " ")
  
  if(!exists(noquote(y))){
    stop(err)
  }
  
}

# Check that the formal arguments are not NULL
check_null <- function(y){
  
  err <- paste(y, "must be specified", sep = " ")
  
  if(is.null(eval(base::as.symbol(y)))){
    stop(err)
  }
  
}


# Check that arguments are strings
check_string <- function(y){
  
  err <- paste("Expected a string but", y, "is not a string", sep = " ")
  
  if(class(eval(base::as.symbol(y))) != "character"){
    stop(err)
  }
  
}

# Check that arguments are numeric 
check_numeric <- function(y){
  
  err <- paste("Expected a numeric value but", y, "is not numeric", sep = " ")
  
  if(class(eval(base::as.symbol(y))) != "numeric"){
    stop(err)
  }
  
}


# Check that arguments are Boolean
check_boolean <- function(y){
  
  err <- paste("Expected a Boolean value but", y, "is not Boolean", sep = " ")
  
  if(class(eval(base::as.symbol(y))) != "logical"){
    stop(err)
  }
  
}

# Check that sensor arguments are written as expected
check_sensor_spelling <- function(y){
  
  err <- paste("The value provided for the sensor argument, ", y, ", is not correct, check your spelling or captialization", sep = "")
  
  if(!grepl("^RFID$|^IRBB$|^Video$", y)){
    stop(err)
  }
  
}

############################## Directories and Files ####################################

# Check that the each directory in a vector of directories exists
check_dirs <- function(path, y){
  
  err <- paste("The directory", y, "does not exist", sep = " ")
  
  if(!dir.exists(file.path(path, y))){
    stop(err)
  }
  
}

# Check that the given directory is not empty and does have files
# For earlier functions combining many raw data files into a single combined raw data file
check_dir_notEmpty <- function(path, pattern){
  
  err <- paste("The directory", path, "does not have", pattern, "files", sep = " ")
  
  if(!list.files(path, pattern)){
    stop(err)
  }
  
}

# Check that the given file in the given directory exists (for later functions)
check_file <- function(path, y){
  
  err <- paste("The file", y, "does not exist in the directory", path, sep = " ")
  
  if(!file.exists(file.path(path, y))){
    stop(err)
  }
  
}

################################### Data #########################################

# Check that the data frame used for subsequent processing is a data frame
check_df_class <- function(df){
  
  if(!any(grepl("data.frame", class(df)))){
    stop('This object needs to be a data frame')
  }
  
}


# Check that a data frame has each column in a vector of columns specified in the formal arguments
check_fArgs_data_cols <- function(y, df){
  
  tmp_cols <- eval(base::as.symbol(y))
  
  if(length(tmp_cols) == 1){
    
    err <- paste("The column", tmp_cols, "was not found in the data frame", sep = " ")
    
    if(!tmp_cols %in% names(df)){
      stop(err)
    }
    
  } else if(length(tmp_cols) > 1){
    
    sapply(1:length(tmp_cols), function(z){
      
      err <- paste("The column", tmp_cols[z], "was not found in the data frame", sep = " ")
      
      if(!tmp_cols[z] %in% names(df)){
        stop(err)
      }
      
    })
    
  }
  
}

# Check that a data frame has each column in a vector of columns NOT specified in the formal arguments
check_data_cols <- function(y, df){
  
    err <- paste("The column", y, "was not found in the data frame", sep = " ")
    
    if(!y %in% names(df)){
      stop(err)
    }
}

# Check that a given column from the formal arguments does not have NAs 
check_fArgs_cols_nas <- function(y, df){
  
  tmp_cols <- eval(base::as.symbol(y))
  
  if(length(tmp_cols) == 1){
    
    err <- paste("The column", tmp_cols, "has NA (missing) values", sep = " ")
    
    if(any(is.na(df[[tmp_cols]]))){
      stop(err)
    }
    
  } else if(length(tmp_cols) > 1){
    
    sapply(1:length(tmp_cols), function(z){
      
      err <- paste("The column", tmp_cols[z], "has NA (missing) values", sep = " ")
      
      if(any(is.na(df[[tmp_cols[z]]]))){
        stop(err)
      }
      
    })
    
  }
  
}

# Check that a data frame has each column in a vector of columns NOT specified in the formal arguments does not have NAs
check_cols_nas <- function(y, df){
  
  err <- paste("The column", y, "has NA (missing) values", sep = " ")
  
  if(any(is.na(df[[y]]))){
    stop(err)
  }
  
}

# Check that any timestamps columns from the formal arguments are in the right format. This conditional also catches NAs in timestamps
check_tstmps_cols <- function(y, df, format){
  
  tmp_cols <- eval(base::as.symbol(y))
  
  if(length(tmp_cols) == 1){
    
    err <- paste("The column", tmp_cols, "needs to be in a format compatible with temporal calculations", sep = " ")
    
    if(any(is.na(as.POSIXct(df[[tmp_cols]], format = format)))){
      stop(err)
    }
    
  } else if(length(tmp_cols) > 1){
    
    sapply(1:length(tmp_cols), function(z){
      
      err <- paste("The column", tmp_cols[z], "needs to be in a format compatible with temporal calculations", sep = " ")
      
      if(any(is.na(as.POSIXct(df[[tmp_cols[z]]], format = format)))){
        stop(err)
      }
      
    })
    
  }
  
}

# Check that a given column has the expected values
check_col_values <- function(col_nm, df, vals){
  
  tmp_col <- col_nm
  tmp_vals <- vals
  
  err <- paste("The column", tmp_col, "does not have one or both of the expected values", paste(tmp_vals, collapse = "; "), sep = " ")
  
  if(!all(unique(df[[tmp_col]]) %in% tmp_vals)){
    stop(err)
  }
  
}
