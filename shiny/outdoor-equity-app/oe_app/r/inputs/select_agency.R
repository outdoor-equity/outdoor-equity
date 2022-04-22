# SEL agency ----
# used in (1) data summary; (2) data relationships; (3) visitorsheds; (4) data download
select_agency <- function(locationId, isMultiple = FALSE){
  
  selectizeInput(inputId = paste("agency", locationId, sep = "_"),
                 label = "Select an agency",
                 choices = ca_agency,
                 multiple = isMultiple,
                 options = list(
                   placeholder = "Type to search for an agency",
                   # Note(HD) when created set a value for the input to an empty string
                   onInitialize = I('function() { this.setValue("NPS"); }')
                 )) 
} # EO SEL agency