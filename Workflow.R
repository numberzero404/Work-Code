# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------
#                                 Tableau Data Prep
# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------

##Set this to 3 if you're running it on a Monday, 4 if on a Tuesday, etc
DAYS <- 3

#set directory and create file list, directory should be folder with rasch output files in it
setwd("C:/Users/kcm35/Dropbox/Reader Dashboard & Analysis/rasch output files")
file.list<- list.files()
library(xlsx)

# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------
#                                 Unexpected Data Prep
# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------

#Creating a list of file names that contain "unexpected"
unexpected.output.names <- file.list[which(grepl("unexpected", file.list))]
dateorder <- NULL
for (i in unexpected.output.names){
  dateorder[i] <- as.Date(file.info(i)$ctime)
}

newunexpected <- unexpected.output.names[which.max(dateorder)]
rm(dateorder)
#Reading file just assigned as most recent into a data frame, etc

#Reading in, cleaning file, naming variables
x <- read.table(newunexpected, fill = T, col.names=c("1","2","3","4","5","6","7","8","9","10","11","12","13", "14"), stringsAsFactors = FALSE)
x<- x[-c(1:3),-c(1,7)]
colnames(x) <- c("Cat","Score","Exp.","Resd","StRes","N","Raters","Nu1","Ap","Nu2","Tra", "Empty")
  
#Replacing blanks with NA 
for(c in 1:ncol(x)){
  x[which(x[,c] == ""),c]<- NA
}
  
#Only keeping rows with complete data (no NA's)
x<- x[which(complete.cases(x)),]
#Removing the first and the last row from the data
#x<- x[-c(1,nrow(x)),]


#shift over columns with last names
for (i in which((x$Empty)!="|"))
{
  x$Raters[i] <- paste(x$Raters[i], x$Nu1[i])
  x$Nu1[i] <- x$Ap[i]
  x$Ap[i] <- x$Nu2[i]
  x$Nu2[i] <- x$Tra[i]
  x$Tra[i] <- x$Empty[i]
}
x <- x[,-12]

district <- NULL
rater <- NULL
  
#Split reader name on period, assigning to two separate variables
for (j in 1:nrow(x)){
    
  rd <- unlist((strsplit(toString(x$Raters[j]), "[.]")))
  district <- c(district, rd[1])
  rater <- c(rater, rd[2])
}
  
rm(rd)
x$District <- district
x$Raters <- rater
  
#Adding date and week columns to match date of server files
x$Date <- rep(as.Date(file.info(newunexpected)$ctime)-DAYS, nrow(x))
x$Week <- rep(as.Date(file.info(newunexpected)$ctime)-(DAYS+4), nrow(x))

#Creating data frame to keep using file name 
unexpected.output <- x

#Removing the temp data frame from the working environment
remove(x, district, rater)

#Append new data onto final Output data set
write.table(unexpected.output, "C:/Users/kcm35/Dropbox/Reader Dashboard & Analysis/final weekly dashboard files/Unexpected Output.csv", append=TRUE, row.names = F, col.names=FALSE, sep=",", na = "")
rm(unexpected.output, i, j, unexpected.output.names)

# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------
#                                 Rater Data Prep
# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------

#Creating a list of file names that contain "raters"
rater.output.names <- file.list[which(grepl("raters", file.list))]

dateorder <- NULL
for (i in rater.output.names){
  dateorder[i] <- as.Date(file.info(i)$ctime)
}

newraters <- rater.output.names[which.max(dateorder)]
rm(dateorder)
#Reading "raters.district" files into R

#Reading in the file - This is a temp data frame
x <- read.xlsx(newraters, 1, startRow=2, header=TRUE, stringsAsFactors=FALSE)
  
district <- NULL
rater <- NULL
  
#Split reader name on period on assign to two different variables
for (j in 1:nrow(x)){
  rd <- unlist((strsplit(toString(x$Raters[j]), "[.]")))
  district <- c(district, rd[1])
  rater <- c(rater, rd[2])
}

x$Raters <- rater
x$District <- district
  
#Adding date and week column to match date of server files
x$Date <- rep(as.Date(file.info(newraters)$ctime)-DAYS, nrow(x))
x$Week <- rep(as.Date(file.info(newraters)$ctime)-(DAYS+4), nrow(x))
  
#Creating data frame to keep using file name 
rater.output <- x
  
#Removing the temp data frame from the working environment
remove(x, rd, district, rater)

#Append rater data onto existing final output file
write.table(rater.output, "C:/Users/kcm35/Dropbox/Reader Dashboard & Analysis/final weekly dashboard files/Rater Output.csv", append=TRUE, row.names = F, col.names=FALSE, sep=",", na = "")
rm(rater.output.all, i, j, rater.output.names)

# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------
#                                 Graph Data Prep
# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------

library(reshape2)

#creates list of all files with "graph" in the title
graph.output.names <- file.list[which(grepl("graph", file.list))]

#select graph file with latest creation date
dateorder <- NULL
for (i in graph.output.names){
  dateorder[i] <- as.Date(file.info(i)$ctime)
}

newgraph <- graph.output.names[which.max(dateorder)]
rm(dateorder)

x <- read.csv(newgraph, header=TRUE, stringsAsFactors=FALSE)

#Creating data frame to keep using file name 
graph.output <- melt(x, id.vars = c("Scale", "Measure", "Expected", "ExpCat"))
colnames(graph.output) <- c("Scale", "Measure", "Expected", "ExpCat", "Prob.Name", "Prob.Value")

#Adding date and week column to match date of server files
graph.output$Date <- rep(as.Date(file.info(newgraph)$ctime)-DAYS, nrow(graph.output))
graph.output$Week <- rep(as.Date(file.info(newgraph)$ctime)-(DAYS+4), nrow(graph.output))

#Append to graph output file
write.table(graph.output, "C:/Users/kcm35/Dropbox/Reader Dashboard & Analysis/final weekly dashboard files/Graph Output.csv", append=TRUE, row.names = F, col.names=FALSE, sep=",", na = "")
