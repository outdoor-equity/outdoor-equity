# SEL agency ----
# used in (1) distribution / single variable analysis; 
# (2) comparison / multiple variable analysis; 
# (3) site analysis / maps
select_agency <- function(){
  
  selectizeInput(inputId = "agency",
                 label = "Select an agency",
                 choices = ca_agency,
                 multiple = TRUE,
                 options = list(
                   placeholder = "Type to search for an agency",
                   # Note(HD) when created set a value for the input to an empty string
                   onInitialize = I('function() { this.setValue(""); }')
                 )) 
}



# SEL admin unit ----
# used in (1) distribution / single variable analysis; 
# (2) comparison / multiple variable analysis; 
# (3) site analysis / maps
select_admin_unit <- function(){
  
  selectizeInput(inputId = "admin_unit",
                 label = "Select an administrative unit",
                 choices = admin_units,
                 multiple = TRUE,
                 options = list(
                   placeholder = "Type to search for an admin unit",
                   # Note(HD) when created set a value for the input to an empty string
                   onInitialize = I('function() { this.setValue(""); }')
                 )) 
}


# SEL site ----
# used in (1) distribution / single variable analysis; 
# (2) comparison / multiple variable analysis; 
# (3) site analysis / maps
select_site <- function(){
  
  selectizeInput(inputId = "site",
                 label = "Select a reservable site",
                 choices = sites,
                 multiple = TRUE,
                 options = list(
                   placeholder = "Type to search for a reservable site",
                   # Note(HD) when created set a value for the input to an empty string
                   onInitialize = I('function() { this.setValue(""); }')
                 )) 
 
}



