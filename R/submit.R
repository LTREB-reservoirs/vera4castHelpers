## Technically this could become arrow-based

#' submit forecast to Challenge
#'
#' @inheritParams forecast_output_validator
#' @param ask should we prompt for a go before submission?
#' @param s3_region subdomain (leave as is for EFI challenge)
#' @param s3_endpoint root domain (leave as is for EFI challenge)
#' @export
submit <- function(forecast_file,
                   ask = interactive(),
                   s3_region = "submit",
                   s3_endpoint = "ltreb-reservoirs.org",
                   first_submission = TRUE
){
  if(file.exists("~/.aws")){
    warning(paste("Detected existing AWS credentials file in ~/.aws,",
                  "Consider renaming these so that automated upload will work"))
  }
  message("validating that file matches required standard")
  go <- forecast_output_validator(forecast_file)

  googlesheets4::gs4_deauth()
  message("Checking if model_id is registered")
  registered_model_id <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1f177dpaxLzc4UuQ4_SJV9JWIbQPlilVnEztyvZE6aSU/edit?usp=sharing", range = "Sheet1!A:A") 

  df <- readr::read_csv(forecast_file, show_col_types = FALSE)
  model_id <- df$model_id[1]

if( grep("(example)",model_id)){
  message("You are submitting a forecast with example in the model_id. As a example forecast, it will be processed but only retained for 30-days.\n
          No registration is required to submit an example forecast.\n
          If you want your forecast to be retained, please choice a different model_id that does not contain `example` and register you model id at https://forms.gle/kg2Vkpho9BoMXSy57")
}
if(!(model_id %in% registered_model_id$model_id | grep("(example)",model_id))){
  message("Checking if model_id is already used in submissions")
  submitted_model_ids <- read_csv("https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/inventory/model_id/model_id-theme-inventory.csv", show_col_types = FALSE)
  if(model_id %in% submitted_model_ids$model_id){
    warning(paste0("Your model_id (",model_id,") has not been registered yet but is already used in other submissions.  Please use and register another model_id\n",
                   "   Register at [https://forms.gle/kg2Vkpho9BoMXSy57](https://forms.gle/kg2Vkpho9BoMXSy57)"))
  }else{
    warning(paste0("Your model_id (",model_id,") has not been registered\n",
                   "   Register at [https://forms.gle/kg2Vkpho9BoMXSy57](https://forms.gle/kg2Vkpho9BoMXSy57)"))
  }
  return(NULL)
}

  if(!grep("(example)",model_id)){
    if(first_submission & model_id %in% registered_model_id$model_id){
      submitted_model_ids <- read_csv("https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/inventory/model_id/model_id-theme-inventory.csv", show_col_types = FALSE)
      if(model_id %in% submitted_model_ids$model_id){
        warning(paste0("Your model_id (",model_id,") is already used in other submitted forecasts. There are two causess for this error: \n
                    - If you have previously submitted a forecast, set the argument `first_submission = FALSE` to remove this error\n
                    - If you have not previously submitted a forecast, this error message means that the model_id has already been registered and used for submissions.  Please register and use another model_id at [https://forms.gle/kg2Vkpho9BoMXSy57](https://forms.gle/kg2Vkpho9BoMXSy57)"))
      }
    }
  }

  if(!go){

    warning(paste0("forecasts was not in a valid format and was not submitted\n",
                   "First, try read reinstalling neon4cast (remotes::install_github('eco4cast\\neon4cast'), restarting R, and trying again\n",
                   "Second, see https://projects.ecoforecast.org/neon4cast-docs/Submission-Instructions.html for more information on the file format"))
    return(NULL)
  }

  if(go & ask){
    go <- utils::askYesNo("Forecast file is valid, ready to submit?")
  }

  #GENERALIZATION:  Here are specific AWS INFO
  exists <- aws.s3::put_object(file = forecast_file,
                               object = basename(forecast_file),
                               bucket = "vera4cast-submissions",
                               region= s3_region,
                               base_url = s3_endpoint)

  if(exists){
    message("Successfully submitted forecast to server")
  }else{
    warning("Forecasts was not sucessfully submitted to server")
  }
}
