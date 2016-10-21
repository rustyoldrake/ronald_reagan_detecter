

getwd()
setwd("/Users/ryan/Desktop")

data <- read.csv2("presidential_speeches_clean_data2.csv", header=FALSE, sep = ",")
data <- data.frame(data) #
data$V2 <- paste(data$V2) # kills levels
data$V2 <- enc2utf8(data$V2)

#data <- gsub("\\\\","",data) # remove punct we dont like
#data <- gsub("\"","",data) # remove punct we dont like
data$V2 <- gsub(";"," ",data$V2) # remove punct we dont like
data$V2 <- gsub("-"," ",data$V2) # remove punct we dont like
data$V2 <- gsub("<"," ",data$V2) # remove punct we dont like
data$V2 <- gsub(">"," ",data$V2) # remove punct we dont like
data$V2 <- gsub("\\*"," ",data$V2) # remove punct we dont like

head(data)
write.csv(data,"presidential_speeches_clean_data6.csv",row.names=FALSE,col.names=FALSE)
