#####################################################
### Experimental Code. R Interface for IBM Watson Services
### Playing around to build a Ronald Reagan Detecter 
### Trained off of ~100 speeches from 20th century - 6 were Reagan
### Focus: PERSONALITY INSIGHTS - USER PERSONAE SEGMENTATION - R Programming Language Interface
### Adapting the PERSONALITY INSIGHTS APIs https://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/apis/ to RCURL and HTTR
### DOCS: https://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/doc/personality-insights/
### REFERENCE https://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/personality-insights/api/v2/
### Before you begin you will need (1) An IBM Bluemix demo account (2) A dialog App and (3) Credentials to that Service and confirm you're able to CURL service with
######################################################

### Warning - this is experimental code - it runs, but has not been reviewed and is rough around the edges
### Demo - try this to familiarize yourself with service:  https://watson-pi-demo.mybluemix.net

library(RCurl) # install.packages("RCurl") # if the package is not already installed
library(httr)
library(XML)
library(data.table)
library(reshape2)
library(dplyr)
library(stringr)
library(reshape2)

#setwd("/Users/ryan/Documents/Project Daisy")  # contains a CSV for me to look up external content to scrape
setwd("C:/Users/Ryan Anderson/Documents/_IBM Experimental/ML Predictive Personality Insights")


################## IBM PERSONALITY INSIGHTS - CREDENTIALS AND FUNCTION DECLARATIONS 

# Personality-Insights-Service-Blue - credentials"

pi_url="https://gateway.watsonplatform.net/personality-insights/api/v2/profile"
username = "1234567-4444-4444-4444-999999999" # your credentials here
password = "99999999999" # your credentials here
username_password = paste(username,":",password,sep="")
alldata <- data.frame(NULL) # where we will store our analysis

###### FUNCTION - ANalyze TEXT/DOC RECEIVED with Personality Insights servicen and return Analysis
watson.personality_insights.analyze <- function(TEXT)
{
  return(POST(url=pi_url,
              authenticate(username,password),
              add_headers("Content-Type"="text/plain","charset"="utf-8" ),
              body = TEXT
  ))
}
## Works: ## curl -X POST -u $USERNAME:$PASSWORD -H "Content-Type: text/plain" -d "$PROFILE" "https://gateway.watsonplatform.net/personality-insights/api/v2/profile"


#### FUNCTION TO TIDY UP THE PI RESPONSE TO TABLE FORMAT - some rough methods that can be improved below
tidyResponse <- function(data)
{
  data <- as.data.frame(strsplit(as.character(data),"\"id\":\""))
  data <- data[-c(1:5), ] # remove dud first row
  data <- data.frame(matrix(data))
  data[,1]  <- gsub("\"","",data[,1] )
  data <- data.frame(do.call('rbind', strsplit(as.character(data$matrix.data),',',fixed=TRUE)))
  data <- data[!grepl('name:',data$X5),]
  data <- data[!grepl('children:',data$X5),]
  data <- data[,-c(2,6), ] # remove columns we dont need - duplicates or dont care for SAMPLING ERROR (now) but mght later
  setnames(data,c("trait","category","percentage","error"))
  data$percentage <- gsub("percentage:","",data$percentage)
  data$category <- gsub("category:","",data$category)
  data$error <- gsub("sampling_error:","",data$error)
  data$error <- gsub("}","",data$error) # crude but effective
  data$error <- gsub("]","",data$error) # crude but effective
  data$percentage <- round((as.numeric(data$percentage)),4) # if you prefer % format like this
  data$error <- round((as.numeric(data$error)),4) # if you prefer % format like this
  rownames(data) <- NULL # resets row names to remove 'gaps'
  data$row <- as.numeric(rownames(data))
  return(data)
}

########## FUNCTION - ScreenScrape support Clean up Response from HTML to DOC.TXT (import speeches)
cleanDoc <- function(doc) {
  doc <- gsub("<.*?>", "", doc)
  doc <- gsub("\\t","",doc)
  doc <- gsub("\\n","",doc)
  doc <- gsub("\\r","",doc)
  doc <- gsub("\\t","",doc)
  doc <- gsub("\\n","",doc)
  doc <- gsub("\\r","",doc)
  doc <- gsub("&nbsp;","",doc)
  doc <- gsub("&quot;","",doc)
  return(doc)
}
## this is optimized for test code from http://www.americanrhetoric.com/top100speechesall.html - but as you substitute your own, you should tailor
## source site is copyright ar and many speeches, as they noted, are copyright or public domain, as noted in text.  Can also scrape library of congress - for example only


###### OK - LET'S GO!  ACTION!  

################ PART 1A - LET"S LOOP THROUGH OUR LIST AND RUN PI

####################### THIS PART OF THE CODE IS FIGURE OUT HOW TO SCRAPE/IMPORT TEST TEXT/CORPUS FOR ANALYSIS
getwd()
speeches <- read.csv("presidential_speeches_clean_data.csv",header=TRUE)
speeches <- data.frame(speeches) # $body is field with all the information we care about
# speeches big  - dont do this

# quick test:
content(watson.personality_insights.analyze(cleanDoc(speeches$body[7])),"text") # quick check to see Watson PI is listening and returning good data

# OK - let's loop through entire list of speeches
length <- dim(speeches)[1]+0  # how long is our list?
for (i in 1:length)
{
  doc <- speeches$body[i]
  doc <- cleanDoc(doc)
  response <- watson.personality_insights.analyze(doc)
  data <- content(response, "text") # here are the personality insights!
  data <- tidyResponse(data)
  data$id <- speeches$id[i] # adds speec
  alldata <- rbind(alldata,data)
  print(paste("processed: ","#",i,speeches$id[i]))
} # takes about 60 seconds to process 95 speeches  - 4836 objects (52 traits X 93 rows)
head(alldata)

#####

write.csv(alldata,"alldata.csv")
## MANUAL STEP - I went in and manually marked the reagan column/field to flag RR and JFK


#########################################  PART 1B - THE REAGAN DETECTOR ################

#PART 1B - We have data from nearly 100 famous speakers and speechs of the 20th century
#8 are Ronald Reagan. That's not many data points, but for grins, let's try to 
## build a RR RF (Ronald Reagan Random Forest) machine and then crack it open for a look inside

library(diffusionMap)
library(randomForest)
library(ggplot2)
library(reshape2)  
library(plyr)
library(phom)

### Let's get data
setwd("C:/Users/Ryan Anderson/Documents/_IBM Experimental/ML Predictive Personality Insights")
getwd() # are we in the right directory?
data = read.csv("reagan_detecter_train.csv")  # 93 rows and 54 columns - ID, Reagan yes/no, and then 52 PI Traits
dim(data)
head(data) 
summary(data)
table(data$reagan) # 87 no, 6 gippers "R" (6 speeches) (6 JFKs "J")

# PI data comes from here - http://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/personality-insights.html 

## Let's just take a quick peek
plot(data[2:5], main="Ronald Reagan Detecter", 
     pch=23, bg = c("red", "green", "blue")
     [unclass(data$reagan)]) 

reagan <- data$reagan ##  reagan is our target and classifier. what we care about
euclid <- data[3:54]

D = dist(scale(euclid)) # use Euclidean distance on data
## DIST: This function computes and returns the distance matrix computed by using the specified distance measure to compute the distances between the rows of a data matrix.

dmap = diffuse(D,eps.val=500, t=1, neigen=2)
## DIFFUSE: Description : Uses the pair-wise distance matrix for a data set to compute the diffusion map coefficients. Computes the Markov transition probability matrix, 
## and its eigenvalues and left & right eigenvectors. Returns a 'dmap' object.
## Usage - diffuse(D, eps.val = epsilonCompute(D), neigen = NULL, t = 0, maxdim = 50, delta=10^-5)
## Arguments D - n-by-n pairwise distance matrix for a data set with n points, or alternatively output from the dist() function
## eps.val     epsilon parameter for the diffusion weight matrix, exp(-D$^2$/(eps.val)). Default is to use the epsilon corresponding to the median distance to the 0.01*n nearest neighbor
## neigen - number of dimensions of final diffusion map representation. Default uses number of dimensions corresponding to a 95% drop-off in eigenvalue multiplier.
## t     optional time-scale parameter in the diffusion map. The (recommended) default uses multiscale geometry.

plot(dmap$X[,1],dmap$X[,2],col=reagan,pch=paste(reagan), 
     xlab="Diffusion Map Coordinate 1", 
     ylab="Diffusion Map Coordinate 2",
     main="Diffusion Map of Personality Traits: Reagan & JFK",
     sub="R = Reagan;  J = JFK ; n = everyone else")


## Let's do some Machine learning!
## Use random forest "Department" classifier to  fit https://en.wikipedia.org/wiki/Random_forest
ronald_forest <- data[3:54]
fit = randomForest(ronald_forest, reagan, ntree=30, proximity=TRUE) 
print(fit)  # not our finest day on the confusion matrix :-) 
varImpPlot(fit) # we're going to look at the FIT and see what traits are most impactful to model

## Let's cherry pick some of the most impactful (we believe) Traits for RR/JFK that help them stand out from pack
data_focus <- cbind(data[names(data) %in% 
              c("reagan","Agreeableness","Altruism","Extraversion",
                "Friendliness","Dutifulness","Challenge","Hedonism",
                "Self-transcendence","Depression")])

## And Plot - Reagan is Blue; JFK is red. 
plot(data_focus, main="Ronald Reagan (and JFK) Detecter - Focus Traits", 
     pch=23, bg = c("red", "green", "blue")[unclass(data$reagan)]) 


## And Plot - just two Traits for easier viewing - Reagan is Blue; JFK is red. Note grouping of RR/JFK relative to their own other speeches, and compared to wider group 
data_focus <- cbind(data[names(data) %in% c("reagan","Extraversion","Friendliness")])
plot(data_focus, main="Reagan JFK and 85 others - Focus Traits", pch=23, bg = c("red", "green", "blue") [unclass(data$reagan)]) 


## Let's build a black box and see how well it does
ronald_forest.rf <- randomForest(reagan ~ ., data=ronald_forest)
ronald_forest.rf



## testing our train data (expect 100%)
ronald_forest.pred <- predict(ronald_forest.rf, ronald_forest)
ronald_forest.pred  ## WE HAD BETTER GET ALL THESE RIGHT< THIS IS OUR TRAINIGN SET we're TESTINZG!
table(ronald_forest.pred) #7,24,28,29,56,85 - ok - that's good - (we should hope our train set comes in at 100%)

write.csv(ronald_forest,"ronald_forest.csv")
## Now let's make things harder - let's RANDOMIZE the data a little and see how model does .95%-1.05% on all traits



## testing our TEST data -- SPECIAL TREAT - 100+ REAGAN CLONES - how will model do?
ronald_forest_test_all_R <- read.csv("ronald_forest_test_all_RR.csv")
ronald_forest.pred <- predict(ronald_forest.rf, ronald_forest_test_all_R)
ronald_forest.pred  ## TESTING SYNTHETICS - what's going to happen?
table(ronald_forest.pred) # 49+53=102 SYNTHETIC REAGANS -> 52% of the SYNTHETIC REAGANS WERE CORRECTLY CLASSIFIED!

dim(ronald_forest_test_all_R)
D = dist(scale(ronald_forest_test_all_R[2:53])) # use Euclidean distance on data
dmap = diffuse(D,eps.val=200, t=1, neigen=2)
plot(dmap$X[,1],dmap$X[,2],col=TRUE,pch=paste(reagan), 
     xlab="Diffusion Map Coordinate 1", 
     ylab="Diffusion Map Coordinate 2",
     main="Synthetic Reagans Clustering Together",
     sub="")



## testing our TEST data -- SPECIAL TREAT - 1000+ REAGAN WANNABES - how will model do?
ronald_forest_test_no_R <- read.csv("ronald_forest_test_no_RR.csv")
ronald_forest.pred <- predict(ronald_forest.rf, ronald_forest_test_no_R)
ronald_forest.pred  ## 
table(ronald_forest.pred) # 1111 Wannabes - 1080 were correctly rejected, 31 (2.7%) erroroneously Reaganized 

dim(ronald_forest_test_no_R) ## 1111 Tall
D = dist(scale(ronald_forest_test_no_R[2:53])) # use Euclidean distance on data
dmap = diffuse(D,eps.val=200, t=1, neigen=2)
plot(dmap$X[,1],dmap$X[,2],col=TRUE,pch=paste(reagan), 
     xlab="Diffusion Map Coordinate 1", 
     ylab="Diffusion Map Coordinate 2",
     main="Reagan Wannabes",
     sub="")
# and no, we did not make the model work that hard with the nature of synthetic data and the fairly tight randomization 95-105% - for demo mainly


################  CLUSTERING
dim(ronald_forest_test_all_R)
D = dist(scale(ronald_forest_test_all_R[2:53])) # use Euclidean distance on data
dmap = diffuse(D,eps.val=200, t=1, neigen=2)
plot(dmap$X[,1],dmap$X[,2],col=TRUE,pch=paste(reagan), 
     xlab="Diffusion Map Coordinate 1", 
     ylab="Diffusion Map Coordinate 2",
     main="Synthetic Reagans Clustering Together",
     sub="")



## PLOT CLUSTER DENDOGRAM - 
dend = 1-fit$proximity # use 1 - proximity
dmap2 = diffuse(dend,eps.val=250, t=.1, neigen=2)   #original dmap1 = diffuse(D1,eps.val=.1, t=1, neigen=2)
head(dmap2)
cluster2 = hclust(dist(dmap2$X[,1:2]))
plot(cluster2); abline(h=0.8, col='blue',lwd=3)

#PLOT 6 - clustering 
clustering2 = cutree(cluster2,k=5)  ## this is how many nodes there are
clustering2

par(mfrow=c(1,2))
## OK - here are the 
plot(dmap2$X[,1],dmap2$X[,2],col=reagan,pch=paste(reagan), 
     xlab="Diffusion Map Coordinate 1", 
     ylab="Diffusion Map Coordinate 2",
     main="Diffusion Map of Personality Traits",
     sub="R = Reagan;  J = JFK ; n = everyone else")


plot(dmap2$X[,1],dmap2$X[,2],col=clustering2, pch=19,
     xlab="Diffusion Map Coordinate 1", 
     ylab="Diffusion Map Coordinate 2", 
     main="Segmentation / Bucketization",
     sub="Colors are nearest neighbour group")

## End of file
## 


