##          This Function selects the best hospital in
##          a given state based on the lowest 30 day 
##          Mortality rate of selected outcome

#         Notes for comments
#         the number stands for which block of code it belongs to 
#         The ******* indicate the beginning of the block of code
#         The +++++++ indicate the end of a block of code


best <- function(state,outcome){
  
  #1****************************validation variables*************************** 
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  #1+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  #2****************************validation*****************************************
  if(!state %in% state.abb | !outcome %in% outcomes){stop("invalid state or outcome")}
  #2+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  
  else{ 
        
  
  #3**************read in data and break down data******************************
  
  data <- read.csv("G:/Rprogramming/Data/outcome-of-care-measures.csv", colClasses = "character")
  data <- data[,c(2,7,11,17,23)]
  names(data) <- c("Name","State","Heart.Attack","Heart.Failure","Pneumonia")
  data$State <- as.factor(data$State)
  data$Heart.Attack <- as.numeric(data$Heart.Attack)
  data$Heart.Failure <- as.numeric(data$Heart.Failure)
  data$Pneumonia <- as.numeric(data$Pneumonia)
  #3+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
   
  
  
        #4**************data split into state data*********************** 	
        data <- split(data, data$State)[[state]]
        #4+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
        
        
        #5*********selecting hospital based on outcome****************************
output<-if(outcome == "heart attack"){
          
          data[data$Heart.Attack == min(data$Heart.Attack, na.rm = T),1][1]
        }
        else if(outcome == "heart failure"){
          
          data[data$Heart.Failure == min(data$Heart.Failure, na.rm = T),1][1]
        }
        else if(outcome == "pneumonia"){
          
          data[data$Pneumonia == min(data$Pneumonia, na.rm = T),1][1]
        }}#End of if Statement
        #5++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




return(output)
}#End of Function