## observe event agency to admin unit dictionary ##
# used in PRESS AGENCY in server
# using input id's for all analysis and data download pages in ui

oe_agency_to_admin_dict <- function(isInput_key, page){
  
  print(paste0("You have chosen agency: ", isInput_key, " on the ", page, " page and the class is: ", class(isInput_key)))
  
  # create choices based on dictionary
  choices <- vector()
  
  for (i in seq_along(isInput_key)) {
    
    if (isInput_key[i] != "") {
      
      choices <- append(choices,
                        agency_to_admin_unit_dict$get(isInput_key[[i]]))
    } # EO if statement
  } # EO for loop
  
  assign("choices", as.vector(choices), envir = .GlobalEnv)
  
} # EO function