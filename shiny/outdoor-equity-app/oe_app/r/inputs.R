# SEL agency ----
# used in (1) data summary; (2) data relationships; (3) visitorsheds; (4) data download
select_agency <- function(locationId, isMultiple = TRUE){
  
  selectizeInput(inputId = paste("agency", locationId, sep = "_"),
                 label = "Select an agency",
                 choices = ca_agency,
                 multiple = isMultiple,
                 options = list(
                   placeholder = "Type to search for an agency",
                   # Note(HD) when created set a value for the input to an empty string
                   onInitialize = I('function() { this.setValue(""); }')
                 )) 
}


# SEL admin unit ----
# used in (1) data summary; (2) data relationships; (3) visitorsheds; (4) data download
select_admin_unit <- function(locationId, isMultiple = TRUE){
  
  selectizeInput(inputId = paste("admin_unit", locationId, sep = "_"),
                 label = "Select an administrative unit",
                 choices = admin_units,
                 multiple = isMultiple,
                 options = list(
                   placeholder = "Type to search for a National Park, Forest, or Public Land",
                   # Note(HD) when created set a value for the input to an empty string
                   onInitialize = I('function() { this.setValue(""); }')
                 )) 
}


# SEL site ----
# used in (1) data summary; (2) data relationships; (3) visitorsheds; (4) data download
select_site <- function(locationId, isMultiple = TRUE){
  
  selectizeInput(inputId = paste("site", locationId, sep = "_"),
                 label = "Select a reservable site",
                 choices = sites,
                 multiple = isMultiple,
                 options = list(
                   placeholder = "Type to search for a reservable site",
                   # Note(HD) when created set a value for the input to an empty string
                   onInitialize = I('function() { this.setValue(""); }')
                 ))
}

# SEL data source ----
# used in (1) data summary
select_data_source <- function(){
  
selectizeInput(inputId = "data_source",
               label = "Select a dataset to visualize",
               choices = c("RIDB", "US Census"),
               selected = "RIDB",
               multiple = FALSE,
               options = list(
                 placeholder = "RIDB or US Census data?",
                 onInitialize = I('function() { this.setValue(""); }')
               ))
}

# SEL data relationship vars ----
# used in (2) data relationships
select_data_relationship <- function(){
  
  selectizeInput(inputId = "data_relationships",
                 label = "Select a data relationship to visualize",
                 choices = c("Race x Site type",
                             "Race x Booking window",
                             "Race x Daily cost",
                             "Education x Distance traveled",
                             "Education x Site type",
                             "Education x Booking window",
                             "Education x Length of stay",
                             "Language",
                             "Median-income x Distance traveled",
                             "Booking window x Site type",
                             "Booking window x distance traveled"),
                 selected = "RIDB",
                 multiple = FALSE,
                 options = list(
                   placeholder = "Type to search for a data relationship",
                   onInitialize = I('function() { this.setValue(""); }')
                 ))
}

