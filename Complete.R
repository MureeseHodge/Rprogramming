##      This function takes a directory as an input and a set number 
##      of files and returns the file number and number of complete
##      observations 

#         Notes for comments
#         the number stands for which block of code it belongs to 
#         The ******* indicate the beginning of the block of code
#         The +++++++ indicate the end of a block of code

complete <- function(directory,id = 1:332){
  
  #1*************VAriables for the function to reutrn**************************** 
  Dataframe <- data.frame(id = c(),nobs = c())
  Error <- "You have entered an incorrect Directory"
  #1+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
  
  #2************Checks to see if valid Directory was entered***************
  if(directory == "specdata"){
        
        for(i in id){
    
            #3**********************Creates file name String****************************
            filename <- read.csv(paste("G:/Rprogramming/",sprintf("%s/%03i.csv",directory,i), sep = ""))
            #3++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++          
    
            #4**********************Turns Date column from factor variable to Date class          
            filename$Date <- as.Date(filename$Date)
            #4+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
            #5**********************Builds Data Frame and binds it to existing***********
            nobss <- sum(complete.cases(filename))
            Dataframe2 <- data.frame(id = i,nobs = nobss)
            Dataframe <- rbind(Dataframe,Dataframe2)
            #5++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        }#End of for loop
        
  }#end of if statment
  
  else{return(Error)}
  #2+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
  
  
  
  return(Dataframe)
}#end of function