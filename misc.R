#Useful code samples to be used across projects

#to assign a value to a field based on selected rows which fit a criteria
interactions$mStatus[(interactions$cpi_InteractionType == type)] <- 'Open'

#demo of date
interactions$date <- as.Date(as.character(interactions$date),"%d/%m/%Y") 

#demo of 'in'
drops <- c("X.1","X")
interactions <- interactions[ ,!(names(interactions) %in% drops)]


#demo of ifelse
interactions$childEventOpen <- ifelse((interactions$Events.open == 1) & (interactions$isChild == TRUE), 1, 0)

#the operator is matrix multiplication; 
p2 <- A %*% B 

#take the transpose of the function modelFit$x gives the design matrix
B <- t(designmat)   

#create a dataframe
df <- data.frame(ref, value)

#demo of sample function
trainRows <- sample(1:nrow(modelinteractions),nrow(modelinteractions)*2/3)

#-1 indicates everything except the first value
weightVector <- generateOddratioWeights(lasso.coef[-1],interactions)

#pipe operator from magrittr
interactions <- interactions %>%    # %>% is the pipe operator (magrittr package), calls one function after the other passing in interactions automatically
  cleanData()                %>%
  checkIfValid(0,filename)   %>%
  wrapTransforms(0,filenameOut)


# Return the dataframe with NA's removed from desired columns
removeNA <- function(datafr, desiredCols) {
  completeVec <- complete.cases(datafr[, desiredCols]) #returns a logical vector of same length with TRUE for rows which are Non NA and FALSE if there are NA's
  return(datafr[completeVec, ])
}

#usage of which function
combineRelatedColumns <- function(df,startCategory,endCategory,noneCategory,newCategory) {
  df[,noneCategory] <- 0
  start <- which(colnames(df) == startCategory)
  end <- which(colnames(df) == endCategory)
  none <- which(colnames(df) == noneCategory)
  cols <- c(start:end,none)
  df[,newCategory] <- colnames(df)[cols][max.col(df[,cols],"last")] #noneCategory is always the last category in the list, and is selected as the reference category for modelling
  df <- df[,-cols] #after combining, remove the separate 
  return(df)  
}

#usage of barplot, 
#usage of writing into the file
createBarPlot <- function(tab,legendText,mainText,plotFile) {
  
  df1 <- as.data.frame(tab)
  #print(df1)
  png(filename = plotFile)
  op <- par(mar = c(10,4,4,2) + 0.1)
  barplot(`colnames<-`(t(df1[-1]), df1[,1]), beside=TRUE, 
          legend.text = legendText, col = c("red", "green"), 
          args.legend = list(x = "topleft", bty = "n", inset=c(-0.05, 0)),las=2,ylim=c(0,1400),main=mainText)
  par(op)
  dev.off()
  
}

#Returns the best JW distance between str1 and any string in strlist (strings in strlist is separated by comma)
strCompare <- function(firstname, fullname, strlist, processList) { 
  
  v <- strsplit(strlist,",")[[1]]  # split the strlist into individual items
  
  if(processList){ #remove leading unwanted characters (title)
    v <- sapply(v, function(x) {x <- sub("Miss |Mr |Ms |Mrs |Mstr |Master ","",x)})
    v <- sapply(v,function(x){x <- sub("^\\s+|\\s+$", "", x)}) #get rid of leading and trailing spaces
    v <- toupper(v)
  }
  
  y <- stringdist(fullname,v,'jw')    # y will have JW distance with each individual string in the list
  x <-  min(y)                        # select the one closest
  jwFull <- 1-x                 # JW distance as per wikipedia is 1-value calculated by stringdist
  
  z <- strsplit(v," ")                # split strlist into strings seperating first and other names
  for(i in 1:length(z)){
    y[i] <- z[[i]][1]                 # extract the first name of all strings in the strlist
  }
  f <- strsplit(firstname, " ")
  g <- stringdist(f[[1]][1],as.character(y),'jw')
  x <- min(g)
  jwFirst <- 1-x                # JW distance between the first name of the strings in the list
  h  <- c(jwFull,jwFirst)
  return(h)
  
}

#1. Does a Jaro-winkler comparison between interacting person's first name and full name with those on the str list
#2. Creates a new column jwFlag based on the similarity criterion
checkIfTenant <- function(interactions,processList,filename){
  interactions$fullname <- apply(interactions[ ,c("Fullname"),drop=F], MARGIN=1, function(x) {x <- sub("Miss |Mr |Ms |Mrs |Mstr |Master ","",x)})
  interactions$fullname <- as.character(interactions$fullname)
  interactions$strList <- apply(interactions[ ,c("strList"),drop=F], MARGIN=1, function(x) { x[is.na(x)] <- "NULL" ; x })
  #1. JW comparison
  interactions <- cbind(interactions, t(mapply(strCompare,toupper(interactions$forename), toupper(interactions$fullname), as.character(interactions$strList),processList)))
  names(interactions)[length(names(interactions))-1] <- "Full"
  names(interactions)[length(names(interactions))] <- "First"
  #2. isTenant criterion
  interactions$jwFlag <- ifelse(((interactions$First < 0.92) | (interactions$Full < 0.7)),0,1)
  write.csv(interactions,file=filename)
  return (interactions)
}


#use of melt and cast
createInteractionsPerPerson <- function(interactions){
  
  # Expand the interaction types to each separate column
  interactions <- data.frame(interactions,model.matrix(~cpi_InteractionType-1,data=interactions))
  
  drops <- c("cpi_InteractionType")
  interactions <- interactions[ ,!(names(interactions) %in% drops)]
  
  #The melt function takes data in wide format and stacks a set of columns into a single column of data. 
  #To make use of the function we need to specify a data frame, the id variables (which will be left at their settings) 
  #and the measured variables (columns of data) to be stacked. 
  #The default assumption on measured variables is that it is all columns that are not specified as id variables.
  
  interactions$fullname <- tolower(interactions$fullname)
  molted = melt(interactions,id=c("ref","Fullname"))
  
  # Opposite of melt. For each unique ref+person, have a single row with presence of all interaction types associated with that person
  # Where the person has interacted more than once with the council, truncate it to just one.     
  casted = dcast(molted, formula = ref+fullname ~ variable, max)

  return(casted)
  
}

#demo of group_by and do (dplyr)
validate <- function(interactions,isModel,validationResultFilename){
  
  interactions1 <- interactions[,c('key','code','fullname','ranking','strList','jwFlag','weightLogistic','weightLasso')]  
  inter2 <- group_by(interactions1,key)
  
  df <- do(inter2,createValidationFactors(.,isModel))
  validationResultFilename1 <- paste(validationResultFilename,Sys.Date())
  write.csv(df,file=validationResultFilename1)
  
}


#demo of merge code
loadMergeInputFiles <- function(){
  # load the dataset.
  infile1 <- read.csv(interactionFile, na.strings=c("NA"))
  infile2 <- read.csv(subletFile, na.strings=c("NA"))
  
  #merge files
  mergeFile <- merge(infile1,infile2,by.x="ref",by.y="ReferenceNo",all.x=TRUE,all.y=TRUE)

}

