#' Validate forecast file
#'
#' @param forecast_file forecast csv or csv.gz file
#' @export

forecast_output_validator <- function(forecast_file){


  file_in <- forecast_file

  valid <- TRUE

  message(file_in)

  #usethis::ui_todo("Checking validity of file name...")
  #file_basename <- basename(file_in)
  #parsed_basename <- unlist(stringr::str_split(file_basename, "-"))
  #file_name_parsable <- TRUE

  #if(!(parsed_basename[1] %in% theme_names)){
  #  usethis::ui_warn(paste0("first position of file name (before first -) is not one of the following : ",
  #                          paste(theme_names, collapse = " ")))
  #  valid <- FALSE
  #  file_name_parsable <- FALSE
  #}

  #date_string <- lubridate::as_date(paste(parsed_basename[2:4], collapse = "-"))

  #if(is.na(date_string)){
  #  usethis::ui_warn("file name does not contain parsable date")
  #  file_name_parsable <- FALSE
  #  valid <- FALSE
  #}

  #if(file_name_parsable){
  #  usethis::ui_done("file name is correct")
  #}

  ## check for spaces in filename
  if (length(strsplit(file_in, " ")[[1]]) > 1){
    usethis::ui_warn('Error: Filename cannot contain spaces...please rename')
    valid <- FALSE
  }else{
    usethis::ui_done("file name is in valid format")
  }

  # read list of official target names and omit any NAs
  target_metadata <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1fOWo6zlcWA8F6PmRS9AD6n1pf-dTWSsmGKNpaX3yHNE/edit?usp=sharing")
  target_vars <- unique(target_metadata$`"official" targets name`)
  target_vars <- target_vars[!is.na(target_vars)]



  if(any(vapply(c("[.]csv", "[.]csv\\.gz"), grepl, logical(1), file_in))){

    # if file is csv zip file
    out <- readr::read_csv(file_in, guess_max = 1e6, show_col_types = FALSE)


    if(lexists(out, c("model_id"))){
      usethis::ui_done("file has model_id column")
    }else{
      usethis::ui_warn("file missing model_id column ")
    }


    if("variable" %in% names(out) & "prediction" %in% names(out)){
      usethis::ui_done("forecasted variables found correct variable + prediction column")
    }else{
      usethis::ui_warn("missing the variable and prediction columns")
      valid <- FALSE
    }


    submit_vars <- unique(out['variable'])
    if(submit_vars %in% target_vars){
      usethis::ui_done("forecasted variables are included in official list of targets")
    }else{
      usethis::ui_warn("forecasted variables are not found in the official list of targets")
      valid <- FALSE
    }

    #usethis::ui_todo("Checking that file contains either ensemble or statistic column...")

    if(lexists(out, "ensemble")){
      usethis::ui_warn("ensemble dimension should be named parameter")
      valid <- FALSE
    }else if(lexists(out, "family")){

      #if("normal" %in% unique(out$family)){
      #  usethis::ui_done("file has normal distribution in family column")
      #}else if("ensemble" %in% unique(out$family)){
      #  usethis::ui_done("file has ensemble distribution in family column")
      #}else{
      #  usethis::ui_warn("only normal or ensemble distributions in family columns are currently supported")
      #  valid <- FALSE
      #}

      if(lexists(out, "parameter")){
        #if("mu" %in% unique(out$parameter) & "sigma" %in% unique(out$parameter)){
        usethis::ui_done("file has correct family and parameter columns")
        #}else if("ensemble" %in% unique(out$family)){
        #  usethis::ui_done("file has parameter and family column with ensemble generated distribution")
        #}else{
        #  usethis::ui_warn("file does not have parameter column is not a normal or ensemble distribution")
        #  valid <- FALSE
        #}
      }else{
        usethis::ui_warn("file does not have parameter column ")
        valid <- FALSE
      }

    }else{
      usethis::ui_warn("file does not have ensemble or family and/or parameter column")
      valid <- FALSE
    }

    #usethis::ui_todo("Checking that file contains siteID column...")
    if(lexists(out, c("site_id"))){
      usethis::ui_done("file has site_id column")
    }else{
      usethis::ui_warn("file missing site_id column")
    }

    #usethis::ui_todo("Checking that file contains parsable time column...")
    if(lexists(out, c("datetime"))){
      usethis::ui_done("file has datetime column")
      if(!grepl("-", out$datetime[1])){
        usethis::ui_done("datetime column format is not in the correct YYYY-MM-DD format")
        valid <- FALSE
      }else{
        if(sum(class(out$datetime) %in% c("Date","POSIXct")) > 0){
          usethis::ui_done("file has correct datetime column")
        }else{
          usethis::ui_done("datetime column format is not in the correct YYYY-MM-DD format")
          valid <- FALSE
        }
      }
    }else{
      usethis::ui_warn("file missing datetime column")
      valid <- FALSE
    }


    if(lexists(out, c("duration"))){
      usethis::ui_done("file has duration column")
    }else{
      usethis::ui_warn("file missing duration column (values for the column: daily = P1D, hourly = PT1H)")
      valid <- FALSE
    }

    if(lexists(out, c("depth_m"))){
      usethis::ui_done("file has depth column")
    }else{
      usethis::ui_warn("file missing depth_m column (use depth = NA for variables without a specific depth")
      valid <- FALSE
    }

    if(lexists(out, c("project_id"))){
      usethis::ui_done("file has project_id column")
    }else{
      usethis::ui_warn("file missing project_id column (use `vera4cast` as the project_id")
      valid <- FALSE
    }

    if(lexists(out, c("reference_datetime"))){
      usethis::ui_done("file has reference_datetime column")
    }else if(lexists(out, c("start_time"))){
      usethis::ui_warn("file start_time column should be named reference_datetime. We are converting it during processing but please update your submission format")
    }else{
      usethis::ui_warn("file missing reference_datetime column")
      valid <- FALSE
    }

  }else{
    usethis::ui_warn("incorrect file extension (csv or csv.gz are accepted)")
    valid <- FALSE
  }
  if(!valid){
    message("Forecast file is not valid. The following link provides information about the format:\nhttps://projects.ecoforecast.org/neon4cast-docs/Submission-Instructions.html")
  }else{
    message("Forecast format is valid")
  }
  return(valid)
}


lexists <- function(list,name){
  any(!is.na(match(name, names(list))))
}
