# SEL data relationships vars ----
# used in (2) data relationships
select_data_relationship <- function(){
  
  selectizeInput(inputId = "data_relationships",
                 label = "Select a data relationship to visualize",
                 choices = sort(c("Race x Distance traveled",
                             "Race x Site type",
                             "Race x Booking window",
                             "Race x Daily cost",
                             "Education x Distance traveled",
                             "Education x Site type",
                             "Education x Booking window",
                             "Education x Length of stay",
                             "Language",
                             "Median-income x Distance traveled",
                             "Booking window x Site type",
                             "Booking window x distance traveled")),
                 multiple = FALSE,
                 options = list(
                   placeholder = "Type to search for a data relationship",
                   onInitialize = I('function() { this.setValue(""); }')
                 ))
} # EO SEL relationship vars

