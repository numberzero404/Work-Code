# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------
#                                 Graph Data Prep
# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------

##Set this to 3 if you're running it on a Monday, 4 if on a Tuesday, etc
DAYS <- 3
setwd("C:/Users/kcm35/Dropbox/Reader Dashboard & Analysis/rasch output files/Items curves separated by groups")
library(xlsx)
library(reshape2)

#Creating a list of all file in folder
file.list<- list.files()

#Creating a list of file names that contain "district"
graph.district.names <- file.list[which(grepl("district", file.list))]

#Reading "graph_output_district" files into R
for(i in graph.district.names){
  #Reading in the file - This is a temp data frame
  x <- read.csv(i, stringsAsFactors=FALSE, header=TRUE)
  #Creating data frame to keep using file name 
  assign(paste0(i), x)
}
#Removing the temp data frame from the working environment
remove(x)

#creating blank data frame to keep data from all districts
graph.output.all <- data.frame(Scale = NA, Measure = NA,Expected=NA,ExpCat=NA,Prob.Name=NA,Prob.Value=NA,Group = NA)
#Removing the first row of NA's from the data frame
graph.output.all <- graph.output.all[-1,]

#Converting all districts from wide to long and combining into a single data frame
for(i in graph.district.names){
  x <- melt(get(i), id.vars = c("Scale","Measure","Expected","ExpCat"))
  
  #Clean District Name - getting it to just be the district name and not the file name
  i.1 <- unlist(strsplit(i, "[.]"))
  
  #adding the district name to the data frame
  x$Group <- paste(i.1[3])
  
  #Adding date and week column to match date of server files
  x$Date <- rep(as.Date(file.info(i)$ctime)-DAYS, nrow(x))
  x$Week <- rep(as.Date(file.info(i)$ctime)-(DAYS+4), nrow(x))
  
  #Adding the data to the combined data frame
  graph.output.all<- rbind(graph.output.all, x)
}

colnames(graph.output.all) <- c("Scale", "Measure", "Expected", "ExpCat", "Prob.Name", "Prob.Value", "Group", "Date", "Week")

##Write to graph output file
write.table(graph.output.all, "C:/Users/kcm35/Dropbox/Reader Dashboard & Analysis/final weekly dashboard files/Graphs (by district).csv", row.names = F, col.names=T, sep=",", na = "")