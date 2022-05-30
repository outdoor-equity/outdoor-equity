# SEL admin unit ----
# used in (1) data summary; (2) data relationships; (3) visitorsheds; (4) data download
select_admin_unit <- function(locationId, isMultiple = FALSE, isSize = NULL, defaultValue = "Yosemite National Park"){
  
  selectizeInput(inputId = paste("admin_unit", locationId, sep = "_"),
                 label = "Select an administrative unit",
                 choices = admin_units_vec,
                 multiple = isMultiple,
                 size = isSize,
                 options = list(
                   placeholder = "Type to search for a National Park, Forest, or Public Land",
                   # Note(HD) when created set a value for the input to an empty string
                   onInitialize = I(paste0('function() { this.setValue("', defaultValue, '"); }'))
                 )) 
} # EO SEL admin unit