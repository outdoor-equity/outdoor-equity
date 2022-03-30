select_analysis <- function(){
  
  selectizeInput(inputId = "analysis",
                 label = "2. What kind of analysis do you want to see?",
                 # conditional panel options / "id's" ---- 
                 choices = c(Comparison = "compare", Distribution = "hist"),
                 multiple = FALSE,
                 options = list(
                   placeholder = "Select an analysis type",
                   onInitialize = I('function() { this.setValue(""); }')
                 ))
}