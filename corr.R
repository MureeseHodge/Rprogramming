##          This function determines the correlation between Sulfate
##          and Nitrate for each sensor given that the complete cases
##          out number the threshold

#         Notes for comments
#         the number stands for which block of code it belongs to 
#         The ******* indicate the beginning of the block of code
#         The +++++++ indicate the end of a block of code

source("G:/Rprogramming/complete.r")   #sourced complete function

corr <- function(directory, threshold = 0) {
  #1*************VAriables for the function to reutrn****************************
  allcorr <- c()
  #1+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
  
  #2***********build date frame for threshold comparison***************
  dataframe <- complete(directory,1:332)
  dataframe <- dataframe[dataframe$nobs > threshold,]
  #2++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

            if(length(dataframe$id) > 0){
for( j in seq_along(dataframe$id)) {

    #3********************Pulls out id#********************** 
    i <- dataframe$id[j]
    #3++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    #4*******creates string of file path, removes NA, calculates correlation
    #********Then appends to allcorr vector**********************************
    filename <- read.csv(paste("G:/Rprogramming/",sprintf("%s/%03i.csv",directory,i), sep = ""))
    filename <- na.omit(filename)
    corr <- cor(x=filename$nitrate,y=filename$sulfate)
    allcorr <- append(allcorr,corr)
    #4++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
}#End of for loop
            }#End of If Statment

            else{return(0)}

return(allcorr)
  
}#End of function