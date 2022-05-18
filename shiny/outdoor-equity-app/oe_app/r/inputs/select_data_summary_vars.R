# SEL data summary vars ----
# used in (1) data summary
select_data_summary_vars <- function(){
  
  selectizeInput(inputId = "data_summary",
                 label = "1. Select a variable",
                 choices = sort(summary_vars), 
                 multiple = FALSE,
                 options = list(
                   placeholder = "Type to search for a variable",
                   # Note(HD) need to make setValue part of the function
                   onInitialize = I('function() { this.setValue("race"); }')
                 ))
} # EO SEL select summary vars