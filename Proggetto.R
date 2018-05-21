install.packages("igraph")
setwd("D:/Università/DataMining/Progetto")
library("igraph")
source("My_functions.R")

####load file
adjlist<-read.table("Nets/out.facebook-wosn-links")
####rename column
names(adjlist)<-c("Node1","Node2","Weight","Timestamp")
#network creation
adjlist[100,"Node1"]
T.net<-graph_from_data_frame(adjlist[1:100,c("Node1","Node2")],directed = T, vertices = NULL)
#### set attributes to network
E(T.net)$Timestamp<-c(adjlist[1:100,"Timestamp"])
#edge.attributes(T.net,index=c("500"))#Search on the rows
d_value<-data.frame(d_value=0,Pairs=0)
D<-0
i<-1
while(i<201){  
  print(c("ciclo",i))
  d_value[i,]<-c(D/86400,Aeps(D))
  D<-D+86400
  i<-i+1

}
plot(d_value)
d_value
pattern<-graph_from_data_frame(read.table("QueryGraphs/g2.txt"),directed = T,vertices=NULL)
plot(pattern)
####subgraph matchings
result<-subgraph_isomorphisms(pattern,T.net,method="vf2")
result
emb<-list_to_dataFrame(result)
v<-result[[1]]
v
help("induced.subgraph")
for(v in result)
  sa<-unlist(induced_subgraph(T.net, v))
sa
