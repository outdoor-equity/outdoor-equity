# SEL data relationships vars ----
# used in (2) data relationships
select_data_relationship <- function(){
  
  selectizeInput(inputId = "data_relationships",
                 label = "Select two variables to compare graphically",
                 choices = list(
                   "Education" = sort(c("Education x Booking window",
                                      "Education x Daily cost",
                                      "Education x Daily cost per visitor",
                                      "Education x Distance traveled",
                                      "Education x Length of stay",
                                      "Education x Site type")),
                   "Language" = sort(c("Language x Booking window",
                                     "Language x Daily cost",
                                     "Language x Daily cost per visitor",
                                     "Language x Distance traveled",
                                     "Language x Length of stay",
                                     "Language x Site type")),
                   "Median-income" = sort(c("Median-income x Booking window",
                                          "Median-income x Daily cost",
                                          "Median-income x Daily cost per visitor",
                                          "Median-income x Distance traveled",
                                          "Median-income x Length of stay",
                                          "Median-income x Site type")),
                   "Race" = sort(c("Race x Booking window",
                                 "Race x Daily cost",
                                 "Race x Daily cost per visitor",
                                 "Race x Distance traveled",
                                 "Race x Length of stay",
                                 "Race x Site type"))),
                 multiple = FALSE,
                 options = list(
                   placeholder = "Type to search for a data relationship",
                   onInitialize = I('function() { this.setValue("Education x Booking window"); }')
                 ))
} # EO SEL relationship vars

