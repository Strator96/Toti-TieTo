###### Adj Edges Pairs search #################
Aeps<-function(D){

  numCoppieValide <-0
  time1<-0
  time2<-0
 for (i in   V(T.net)[1:5]){
    adiac<-attr((T.net[[sprintf("%s",i)]])[[1]],"names")
    adiac
    if(length(adiac)!=0)
    for ( j in adiac){
        print(sprintf("%s|%s",i,j))
        time1 <-strtoi(edge_attr(T.net,"Timestamp",index=sprintf("%s|%s",i,j)))
        adiacAdiac<-attr((T.net[[sprintf("%s",j)]])[[1]],"names")
      if(length(adiacAdiac)!=0)
        for ( k in adiacAdiac){
          #print(k)
          print(sprintf("%s|%s",j,k))
            time2<-edge_attr(T.net,"Timestamp",index=sprintf("%s|%s",j,k))
            print(sprintf("(%s -> %s ->%s),(%d - %d) ",i,j,k,time2,time1))
          if(abs(time2-time1) <= D)
            numCoppieValide<-numCoppieValide+1
        }    
        
      }
    
  }
  return(numCoppieValide)
}






####### Conversion from list to DataFrame , with name as value ######

list_to_dataFrame<-function(list){
  data<-data.frame(V1=0,V2=0,V3=0,V4=0)
  r<-1
  while(r<=length(list)){
    ver<-attr(list[[r]],("names"))
    data[r,]<-c(ver[1],ver[2],ver[3],ver[4])
    r<-r+1
  }
  return(data)
}
######################################################################

#get Timestamp from id of edge
#id<-get.edge.ids(Net.facebook,c(emb[1,"V1"],emb[1,"V2"]), directed = T)
#E(Net.facebook)[id]$Timestamp
#Facebook.Timestamps[id]
