#         There are 332 files that are numbered in order representing
#         332 sensors that gather ozone data for sulfate and nitrate

#         The function takes the mean of a selected pollutant and of a selected 
#         set of files that you choose.

#         Notes for comments
#         the number stands for which block of code it belongs to 
#         The ******* indicate the beginning of the block of code
#         The +++++++ indicate the end of a block of code



pollutantmean <- function(directory, pollutant, id = 1:332) {
      
#1*************VAriables for the function to reutrn**************************** 
  
  Totaldata = c() #Bin to hold accumulation of columndata
  Error = "You have entered an incorrect column name"
  Error1 = "You have entered incorrect Directory name"
#1+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++      
  
  
  for(i in id) { 
      
    #2**********************Creates file name String****************************
    filename <- read.csv(paste("G:/Rprogramming/",sprintf("%s/%03i.csv",directory,i), sep = ""))
    #2++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++          
    
    #3**********************Turns Date column from factor variable to Date class          
    filename$Date <- as.Date(filename$Date)
    #3+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            
        #4************Checks to see if valid Directory was entered***************
        if(directory == "specdata"){
        
                
                #5********Checks for valid Pollutant entry************
                if(pollutant == "Nitrate" || pollutant == "nitrate"){
                
                  columndata <- na.omit(filename$nitrate)
                  Totaldata <- append(Totaldata,columndata)
                } #End of first column if statement
                
                else if(pollutant == "sulfate" || pollutant == "Sulfate"){
                
                  columndata <- na.omit(filename$sulfate)
                  Totaldata <- append(Totaldata,columndata)
                
                  } #End of column else if statemment
                
                else { return(Error)}
                #5++++++++++++++++++++++++++++++++++++++++++++++++++++
              }
            
            
            else {return(Error1)} #End of dirctory If statement 
       #4+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            
            } #End of For loop
      
      mean <- mean(Totaldata)
      return(mean)
      
} #End of Function