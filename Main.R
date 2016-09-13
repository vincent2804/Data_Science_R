library(devtools)
library(twitteR)
library(ROAuth)
library(stringr)
library(plyr)
library(ggplot2)
library(RCurl)
library(tm)
library(wordcloud)
library(RJSONIO) 
library(httr)
library(igraph)
library(tcltk)



download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem") 
cKey = "GLhOMI4NtDPPAGdTUCrQRuUpi"
cSecret = "HraiQ0JzRJPxgkXrs3KCfAAwhtrvPkyhYFokRA6lamJYpQqKYB" 
reqURL = "https://api.twitter.com/oauth/request_token"
accURL = "https://api.twitter.com/oauth/access_token"
authURL = "https://api.twitter.com/oauth/authorize"
cred = OAuthFactory$new(consumerKey=cKey,consumerSecret=cSecret, requestURL=reqURL, accessURL=accURL, authURL=authURL)
cred$handshake(cainfo="cacert.pem") 

accToken = "78620540-ygtY3G8E35asiO9rFapMd3SgPfHViY6MpJr0nV8SJ"
accTokenSecret = "dlh1cYE8bPHqFiontjjB8tBSbiia8DWW83iN5JC4yGrz8" 
setup_twitter_oauth(cKey,cSecret,accToken,accTokenSecret)


#harvest tweets from that has keywords
keyword = "#cfaware" #CHANGE THIS KEYWORD
list=searchTwitter(keyword,n=10000) 
text=do.call('rbind',lapply(list,as.data.frame)) 

#OPTIONAL : To export the raw data into a csv file, run this code.
write.csv(text,file='list.csv')
read.csv("list.csv")


#START: Main code for followers count query
#Parameters
nFollowers = c()
start =1#starting point for query.
end = nrow(text)  #ending point for query
rep = 10 # number of data to process in 1 period.Try to keep it reasonable to avoid limits

for(i in start:end){
  i
  if(i>start+rep){
    Sys.sleep(sample(20:30, 1))  # random sleep function, between 10 - 20 seconds
    start = start + rep
  } 
  user<-text$screenName[i]
  nFollowers[i]=getUser(user)$followersCount
  cat("Status: ",i," out of",end," completed... \n") 
} 

#List of target users and the number of followers,sorted by most followers
users = as.matrix(text$screenName)
followers = as.matrix(nFollowers)
list <-cbind(users[1:length(nFollowers),],followers)
list <- list[order(-nFollowers),]
list<-unique(list) #removes duplicates
colnames(list) <- c("Twitter Handle","Number of Followers")
write.csv(list,file='rank.csv')

#END : Main code for followers count query




#Optional : if you already have the saved csv, run this instead. 
rank = read.csv("rank.csv", sep=",")[1:50,2:3]

adj = matrix(0,nrow=nrow(rank),ncol = nrow(rank))




#PARAMETERS
start =1
end= nrow(rank)
totaltime = 0

for(i in start:end){ # loop to go through each row
  
  time <- proc.time() # timer code
  cat("Starting user in rank: ",i,"\n")
  friends<-getUser(list[i,1])$getFollowers(n=NULL) 
  friends <-twListToDF(friends) #convert to dataframe
  for(j in 1:nrow(rank)){ #loop to go through each column 
    for(k in 1:nrow(friends)){ # loop to go through each of the friends  
      if(rank[j,1]==friends$screenName[k]){
        
        adj[i,j]=1
        
        
        
        cat("Yes in :[",i,",",j,",",k,"].","User ", as.matrix(rank$Twitter.Handle)[i]," is followed by ",as.matrix(rank$Twitter.Handle)[j],"\n")
      }
    }
  }
  
  
  friends<-NULL
  
  #timer code
  time <- time - proc.time()
  totaltime = time[3]+totaltime-60
  #Sys.sleep(60) 
  cat("Completed user in rank:",i," in ",time[3]*-1," seconds.\n")
  cat("Time elapsed:",-1*totaltime," seconds.\n")
  
  
}

cat("Completed adjacency matrix operation in: ",-1*totaltime," seconds")
write.csv(adj,file="adj.csv")





#RUN FROM HERE IF YOU HAVE THE ADJACENCY MATRIX SAVED AS CSV
#change beg and end to cut down on the number of nodes displayed by plot function
beg=1
end=50# make sure it's still within bounds of adjacency matrix


adj <- read.csv("adj.csv")
adj<-as.matrix(adj[,2:ncol(adj)])
adj = adj [beg:end,beg:end]
dim(adj)

#Creates a graph from the adjacency
g = graph.adjacency(adj)
tkplot(g) # maps out the network and allows you to move the nodes around.


#CENTRALITY(results are stored in b.csv,c.csv,d.csv,e.csv)
b<-betweenness(g)#betweeness centrality
b <- cbind(as.matrix(list$Twitter.Handle[1:ncol(adj)]),b)
colnames(b)<-c("name","score")
rownames(b)<-c(1:nrow(b)) 
write.csv(b,file = "b.csv")



c<-closeness(g)# closeness centrality
c <-cbind(as.matrix(list$Twitter.Handle[1:ncol(adj)]),c)
colnames(c)<-c("name","score")
rownames(c)<-c(1:nrow(c))
write.csv(c,file = "c.csv") 


d<-degree(g) #degree centrality

#creates csv file of degree centrality
d <-cbind(as.matrix(list$Twitter.Handle[1:ncol(adj)]),d)
colnames(d)<-c("name","score")
rownames(d)<-c(1:nrow(d))
write.csv(d,file = "d.csv")


e<- evcent(g) # eigenvector centrality
e <- c(e$vector)
e <- cbind(as.matrix(list$Twitter.Handle[1:ncol(adj)]),e) # little bug (ranks tiny number on the top)
colnames(e)<-c("name","score")
rownames(e)<-c(1:nrow(e))tkplot(g)

write.csv(e,file = "e.csv")

#WALKTRAP COMMUNITY
wt = walktrap.community(g) # walktrap
plot(wt,g,
     edge.arrow.size=0.05, 
     vertex.label.cex=0.6, 
     vertex.label.family="Helvetica",
     vertex.shape="circle", 
     vertex.size=5, 
     vertex.label.color="black", 
     edge.width=0.1,
     edge.color="black")





