## Basic Code to Explore the Visualiation of high dimensional data in 3d space
## plot 3d basics
## https://cran.r-project.org/web/packages/plot3D/vignettes/plot3D.pdf 
## Fifty ways to draw a volcano using packageplot3D
## https://cran.r-project.org/web/packages/plot3D/vignettes/volcano.pdf
## https://plot.ly/r/3d-scatter-plots/

library(plot3D)

###

# let's get our data:
Volcano <- volcano[seq(1, nrow(volcano), by = 3), 
                   seq(1, ncol(volcano), by = 3)]

par(mfrow = c(2, 2), mar = c(2, 2, 2, 2))
ix <- seq(1, nrow(Volcano), length.out = 20)
iy <- seq(1, ncol(Volcano), length.out = 20)
ribbon3D(z = Volcano[, iy])
ribbon3D(z = Volcano[ix, ], along = "y",
         curtain = TRUE, space = 0.8, shade = 0.2)
ribbon3D(z = Volcano[ix, iy], along = "xy")
hist3D(z = Volcano[ix,iy], shade = 0.5)


####

library(plotly)
packageVersion('plotly')

# Create a free Plotly account: https://plot.ly/feed/ - 
Sys.setenv("plotly_username"="ryan77anderson")          
Sys.setenv("plotly_api_key"="YOURCREDSHERE")


mtcars$am[which(mtcars$am == 0)] <- 'Automatic'
mtcars$am[which(mtcars$am == 1)] <- 'Manual'
mtcars$am <- as.factor(mtcars$am)

p <- plot_ly(mtcars, x = ~wt, y = ~hp, z = ~qsec, color = ~am, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Weight'),
                      yaxis = list(title = 'Gross horsepower'),
                      zaxis = list(title = '1/4 mile time')))

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = api_create(p, filename="scatter3d-basic")
chart_link



### SUpER ROUGH CODE BELOW THIS LINE CAREFUL


## Next Trick? 
## Let's use some older 'fun' data from here - https://github.com/rustyoldrake/ronald_reagan_detecter  and https://dreamtolearn.com/ryan/r_journey_to_watson/10 (three beer science experiement - so handele with care)

data <- read.csv2("politics_personality2.csv", header=TRUE, sep=",")
head(data)  # should see 93 speakers/speeches and 52 personality attributes on speeches https://www.ibm.com/watson/services/personality-insights/

data$generation_shifter[which(data$generation_shifter == 0)] <- 'Others'
data$generation_shifter[which(data$generation_shifter == 1)] <- 'Generation-Shifter'
data$generation_shifter <- as.factor(data$generation_shifter)


p <- plot_ly(data, x = ~Excitement, y = ~Ideal, z = ~Intellect, color = ~generation_shifter, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Excitement'),
                      yaxis = list(title = 'Ideal'),
                      zaxis = list(title = 'Self.transcendence')))

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = api_create(p, filename="scatter3d-personality")
chart_link
 

### Remember the good old days - Kohonen
# Load the kohonen package 
# https://cran.r-project.org/web/packages/kohonen/kohonen.pdf
library("kohonen")
library("plyr")

head(data)

myvars <- c("Adventurousness", "Assertiveness", "Challenge", "Curiosity","Dutifulness","Excitement.seeking","Imagination","Self.enhancement","Self.transcendence")
data_k <- data[myvars]
head(data_k)

data_k <- data.matrix(data_k, rownames.force = NA)
data_k.sc <- scale(data_k)
set.seed(7)
data_k.som <- som(data_k.sc, grid = somgrid(4, 4, "hexagonal"))

## PLOT 1
par(mfrow=c(1,1))
plot(data_k.som, main = "20th Century Influencers")
graphics.off()

## PLOT 2
par(mfrow=c(2,2))
plot(data_k.som, main = "20th Century Influencers")
plot(data_k.som, type="count") 
plot(data_k.som, type="dist.neighbours") 
plot(data[,1], main = "Speech Count by person")
graphics.off()



## Diffusion Map

library(diffusionMap)
head(data)
outcome <- data$generation_shifter ## and species is our target and classifier 

data <- data.matrix(data, rownames.force = NA)

D = dist(scale(data_k)) # use Euclidean distance on data
D
dmap = diffuse(D, eps.val=100, t=1, neigen=2) ## just run with the standard default settings
plot(dmap$X[,1],dmap$X[,2],col=outcome,pch=paste(outcome), 
     xlab="Diffusion Map Coordinate 1", 
     ylab="Diffusion Map Coordinate 2",
     main="Generation Shifters vs not")
## not ideal

dmap = diffuse(D,eps.val=2, t=100, neigen=2)
plot(dmap$X[,1],dmap$X[,2],col=outcome,pch=paste(outcome), 
     xlab="Diffusion Map Coordinate 1", 
     ylab="Diffusion Map Coordinate 2",
     main="Generation Shifters vs not")
## yes, good  

# 2) OK - now Use random forest "Department" classifier to define distances
library(randomForest)
dim(data)
fit = randomForest(data[,3:54], outcome, ntree=20, proximity=TRUE) 
print(fit)
varImpPlot(fit)

D2 = 1-fit$proximity # use 1 - proximity
dmap2 = diffuse(D2,eps.val=40, t=.01, neigen=2)   #original dmap1 = diffuse(D1,eps.val=.1, t=1, neigen=2)
head(dmap2)

cluster2 = hclust(dist(dmap2$X[,1:2]))
plot(cluster2); abline(h=1.8, col='blue',lwd=3)
clustering2 = cutree(cluster2,k=8)  ## this is how many bundles there are

plot(dmap2$X[,1],dmap2$X[,2],col=clustering2, pch=19,
     xlab="Diffusion Map Coordinate 1", 
     ylab="Diffusion Map Coordinate 2", 
     main="Starting to look Interesting")

## See plot 8 - note number of 'buckets' 

output2 = data.frame(dmap2$X,outcome,clustering2)
colnames(output2)[4] <- "group"  ## rename column
head(output2)
output2

#data$generation_shifter[which(data$generation_shifter == 0)] <- 'Others'
#data$generation_shifter[which(data$generation_shifter == 1)] <- 'Generation-Shifter'
#data$generation_shifter <- as.factor(data$generation_shifter)

plot(dmap2$X[,1],dmap2$X[,2],col=clustering2, pch=19,
     xlab="Diffusion Map Coordinate 1", 
     ylab="Diffusion Map Coordinate 2", 
     main="Diffusion Map Clustered Speakers")

dmap2$X
ddd <- data.frame(dmap2$X[,1],dmap2$X[,2],clustering2)
dim(ddd)
colnames(ddd) <- c("X","Y","Z")
head(ddd)

p <- plot_ly(ddd, x = ~X, y = ~Y, z = ~Z, color = ~Z, colors = c('red','blue','orange','green')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Personality Insights'),
                      yaxis = list(title = 'All Attributes'),
                      zaxis = list(title = 'Clustered with Diffusion Map')))

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = api_create(p, filename="scatter3d-post-kohonen")
chart_link
