#Useful functions to be used across projects

#to assign a value to a field based on selected rows which fit a criteria
interactions$mStatus[(interactions$cpi_InteractionType == type)] <- 'Open'

#demo of date
interactions$InvestDate <- as.Date(as.character(interactions$InvestDate),"%d/%m/%Y") 

#demo of 'in'
drops <- c("X.1","X")
interactions <- interactions[ ,!(names(interactions) %in% drops)]

#demo of ifelse
interactions$childSocialcareOpen <- ifelse((interactions$cpi_InteractionTypeSocialCare.CaseOpen == 1) & (interactions$isChild == TRUE), 1, 0)

#the operator is matrix multiplication; 
p2 <- A %*% B 

#take the transpose of the function modelFit$x gives the design matrix
B <- t(designmat)   

#create a dataframe
df <- data.frame(propRef, calculatedSublet,typeSublet1Adult,typeWeight1Adult,typeScore1Adult,typeSublet1Child,typeWeight1Child,typeScore1Child,typeSublet,typeWeight2,typeScore2)

#demo of sample function
trainRows <- sample(1:nrow(modelinteractions),nrow(modelinteractions)*2/3)

#-1 indicates everything except the first value
weightVector <- generateOddratioWeights(lasso.coef[-1],interactions)

#pipe operator from magrittr
trueSubletInteractions <- trueSubletInteractions %>%    # %>% is the pipe operator (magrittr package), calls one function after the other passing in LBNInteractions automatically
  cleanData()            %>%
  checkIfTenant(0,trueSubletfilename) %>%
  wrapTransforms(0,trueSubletfilenameOut)


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
strCompare <- function(firstname, fullname, tenantlist, processTenantList) { 
  
  v <- strsplit(tenantlist,",")[[1]]  # split the tenant list into individual tenants
  
  if(processTenantList){ #This code is here, because sublet extract have tennat list in one format, and the LBN extract has tenant list in another format
    v <- sapply(v, function(x) {x <- sub("Miss |Mr |Ms |Mrs |Mstr |Master ","",x)})
    v <- sapply(v,function(x){x <- sub("^\\s+|\\s+$", "", x)}) #get rid of leading and trailing spaces
    v <- toupper(v)
  }
  
  y <- stringdist(fullname,v,'jw')    # y will have JW distance with each individual tenant in the list
  x <-  min(y)                        # select the one closest
  isTenantFull <- 1-x                 # JW distance as per wikipedia is 1-value calculated by stringdist
  
  z <- strsplit(v," ")                # split tenant list into strings seperating first and other names
  for(i in 1:length(z)){
    y[i] <- z[[i]][1]                 # extract the first name of all tenants in the tenant list
  }
  f <- strsplit(firstname, " ")
  g <- stringdist(f[[1]][1],as.character(y),'jw')
  x <- min(g)
  isTenantFirst <- 1-x                # JW distance between the first name of the interacting person and tenants in the list
  h  <- c(isTenantFull,isTenantFirst)
  return(h)
  
}

#1. Does a Jaro-winkler comparison between interacting person's first name and full name with those on the tenant list
#2. Creates a new column isTenant based on the similarity criterion
checkIfTenant <- function(interactions,processTenantList,filename){
  print("++checkIfTenant")
  interactions$cpi_Fullname <- apply(interactions[ ,c("cpi_Fullname"),drop=F], MARGIN=1, function(x) {x <- sub("Miss |Mr |Ms |Mrs |Mstr |Master ","",x)})
  interactions$cpi_Fullname <- as.character(interactions$cpi_Fullname)
  interactions$TennatListAtInvestigation <- apply(interactions[ ,c("TennatListAtInvestigation"),drop=F], MARGIN=1, function(x) { x[is.na(x)] <- "NULL" ; x })
  #1. JW comparison
  interactions <- cbind(interactions, t(mapply(strCompare,toupper(interactions$cpi_Forename), toupper(interactions$cpi_Fullname), as.character(interactions$TennatListAtInvestigation),processTenantList)))
  names(interactions)[length(names(interactions))-1] <- "Full"
  names(interactions)[length(names(interactions))] <- "First"
  #2. isTenant criterion
  interactions$isTenant <- ifelse((interactions$WasPersonOnTenancyAtInvestigationFlag == 0) & ((interactions$First < 0.92) | (interactions$Full < 0.7)),0,1)
  write.csv(interactions,file=filename)
  print("--checkIfTenant")
  return (interactions)
}


#use of melt and cast
createInteractionsPerPerson <- function(interactions){
  
  print("++createInteractionsPerPerson")
  
  # Expand the interaction types to each separate column
  interactions <- data.frame(interactions,model.matrix(~cpi_InteractionType-1,data=interactions))
  
  drops <- c("cpi_InteractionType")
  interactions <- interactions[ ,!(names(interactions) %in% drops)]
  
  #The melt function takes data in wide format and stacks a set of columns into a single column of data. 
  #To make use of the function we need to specify a data frame, the id variables (which will be left at their settings) 
  #and the measured variables (columns of data) to be stacked. 
  #The default assumption on measured variables is that it is all columns that are not specified as id variables.
  
  interactions$cpi_Fullname <- tolower(interactions$cpi_Fullname)
  molted = melt(interactions,id=c("InvestRef","cpi_Fullname"))
  
  # Opposite of melt. For each unique investigation ref+person, have a single row with presence of all interaction types associated with that person
  # Where the person has interacted more than once with the council, truncate it to just one.     
  casted = dcast(molted, formula = InvestRef+cpi_Fullname ~ variable, max)
  print("--createInteractionsPerPerson")
  return(casted)
  
}

#demo of group_by and do (dplyr)
validate <- function(interactions,isModel,validationResultFilename){
  
  interactions1 <- interactions[,c('cpi_PropertyKey','cpi_Postcode','cpi_Fullname','MaxRanking','TennatListAtInvestigation','isTenant','isChildTenant','isChild','weightLogistic','weightLasso','IsRelatedToATenantFlag','HasAlreadyBeenAudited')]  
  inter2 <- group_by(interactions1,cpi_PropertyKey)
  
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
  mergeFile <- merge(infile1,infile2,by.x="InvestRef",by.y="hti_InvestigationReferenceNo",all.x=TRUE,all.y=TRUE)
  
  #create a new file with just the columns I need
  
}

