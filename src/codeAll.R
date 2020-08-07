#Φόρτωση δεδομέων
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
dataset = read.csv("dataset.csv", sep=';', header = TRUE, na.strings = c("NA"),
                   colClasses = c("character", "character", "numeric", "numeric", "integer", "integer",
                                  "integer", "numeric", "numeric", "integer", "integer", "integer", "integer",
                                  "integer", "integer", "numeric", "integer", "integer", "integer", "integer",
                                  "integer", "integer", "integer", "integer", "integer", "integer", "integer"))



#Καταγραφη των διαφορετικων repositories
rep = unique(dataset[,2])
dataRep <- list()
for(i in 1:length(rep)){
  dataRep[[rep[i]]] = dataset[dataset[,2]==rep[i],]
}

#Συναρτηση που κανει κανονικοποιηση τα repositories και επισης απορριπτει εξωκείμενες τιμες
normRep<-function(data)
{
  
 for(i in 3:ncol(data)){
  #Kanonikopoioume
  a = data[,i]
  s1 = boxplot(a,plot=FALSE,na.action=na.omit)$stats
  a[is.na(a)] = -1000000
  flag1 = a > s1[1] & a < s1[5] 
  Max_a = suppressWarnings(max(as.numeric(a[flag1]),na.rm = TRUE))
  data[,i] = data[,i]/Max_a
  #Aporeiptoume e3wkeimenes times
  data[!flag1,i] = NA
  }
  return(data)
}

#Καταμετρηση των στοιχείων καθε repository
dimlength = c()
for(i in 1:length(rep)){
  dimlength[i] = nrow(data.frame(dataRep[[i]]))
}


#Χρησιμοποιούμε τα πρώτα 10 σε αριθμό δεδομένων repositories
myData <- data.frame()
for(i in 1:10){
  firstRepos = rep[order(dimlength,decreasing = TRUE)[i]]
  normPepositoryData = normRep(dataset[dataset[,2]==firstRepos,])
  myData = rbind(myData,normPepositoryData)
}


combinations3d<-function(data)
{ 
  N = ncol(data)
  mydata <- list()
  for(i in c(3:(N-2))){
    for(j in c((i+1):(N-1))){
      for(k in c((j+1):N)){
        a = data[,c(i,j,k)]
        mydata[[i*N*N+j*N+k]] = a[!is.na(a[,1]) & !is.na(a[,2]) & !is.na(a[,3]),c(1,2,3)]
      }
    }
  }
  return(mydata)
}

#Δημιουργούμε ολους τους δυνατους συνδυασμούς
comData = combinations3d(myData)

#========================Kmean Clustering============================#
#Χρησιμοποιούμε τον kmean
plotKmean<-function(data,clusters){
  N=27
  library("scatterplot3d")
  library("cluster")
  mydata <- list()
  evaluaton <- list()
  modelPoints<- c()
  cohesion<-c()
  separation<-c()
  mean_silhouette<-c()
  index = 0;
  for(i in c(3:(N-2))){
    for(j in c((i+1):(N-1))){
      for(k in c((j+1):N)){
        if(nrow(data[[i*N*N+j*N+k]])>2*clusters & nrow(unique(data.frame(data[[i*N*N+j*N+k]])))>2*clusters){
          index = index +1;
          print(paste("model",index))
          model = kmeans(data[[i*N*N+j*N+k]], centers = clusters)
          titleN = paste("figure_Kmean",i,j,k,".jpg",sep = "_" )
          jpeg(titleN, width = 1950, height = 1000)
          #par(mfrow=c(1,4))
          layout(matrix(c(1,1,1,1,2,3,4,1,1,1,1,5,5,5), 2, 7, byrow = TRUE))
          s3d<-scatterplot3d(data[[i*N*N+j*N+k]],angle=230, color = model$cluster,type = "h", pch = 16,cex.symbols = 3,cex.axis=2,cex.lab = 2)
          s3d$points3d(model$centers, col = 1:length(model$centers),type = "h", pch = "+", cex = 3)
         
          #EVALUATION
          modelPoints[index] = nrow(data[[i*N*N+j*N+k]])
          cohesion[index] = model$tot.withinss
          separation[index] = model$betweenss
          mean_silhouette[index] = mean(silhouette(model$cluster, dist(data[[i*N*N+j*N+k]]))[,3])
          
          title(paste("Kmean Clustering","\nModel_index",index,"\ncohesion =",cohesion[index]," | seperation =",separation[index]," | mean_silhouette =",mean_silhouette[index], sep=" "),cex.main=2)
          
          
          boxplot(data[[i*N*N+j*N+k]][model$cluster == 1,1], data[[i*N*N+j*N+k]][model$cluster == 2,1],data[[i*N*N+j*N+k]][model$cluster == 3,1],horizontal=FALSE, axes=TRUE,
                  main= colnames(data[[i*N*N+j*N+k]])[1],
                  col= c(1,2,3) ,
                  border="brown",
                  ylim = c(0, 1),
                  cex.lab=2,
                  cex.axis=2,
                  cex.main=2,
                  at = c(1,2,3),
                  names = c("cluster1", "cluster2","cluster3"),
                  las = 1
          )
          boxplot(data[[i*N*N+j*N+k]][model$cluster == 1,2], data[[i*N*N+j*N+k]][model$cluster == 2,2],data[[i*N*N+j*N+k]][model$cluster == 3,2],horizontal=FALSE, axes=TRUE,
                  main= colnames(data[[i*N*N+j*N+k]])[2],
                  col=c(1,2,3),
                  border="brown",
                  ylim = c(0, 1),
                  cex.lab=2,
                  cex.axis=2,
                  cex.main=2,
                  at = c(1,2,3),
                  names = c("cluster1", "cluster2","cluster3"),
                  las = 1
          )
          boxplot(data[[i*N*N+j*N+k]][model$cluster == 1,3], data[[i*N*N+j*N+k]][model$cluster == 2,3],data[[i*N*N+j*N+k]][model$cluster == 3,3],horizontal=FALSE, axes=TRUE,
                  main= colnames(data[[i*N*N+j*N+k]])[3],
                  col=c(1,2,3),
                  border="brown",
                  ylim = c(0, 1),
                  cex.lab=2,
                  cex.axis=2,
                  cex.main=2,
                  at = c(1,2,3),
                  names = c("cluster1", "cluster2","cluster3"),
                  las = 1
          )
          
          dev.off()
          
        }
      }
    }
  }
  
  #PLOT EVALUATION
  titleN = paste("EVALUATION_Cohesion",".jpg",sep = "" )
  jpeg(titleN, width = 950, height = 950)
  plot(cohesion,cex= 3,cex.axis=2,cex.lab = 3,ylab = "cohesion")
  title("cohesion",cex.main=3)
  dev.off()
  
  titleN = paste("EVALUATION_separation",".jpg",sep = "" )
  jpeg(titleN, width = 950, height = 950)
  plot(separation,cex= 3,cex.axis=2,cex.lab = 3,ylab = "separation")
  title("separation",cex.main=3)
  dev.off()
  
  titleN = paste("EVALUATION_mean_silhouette",".jpg",sep = "" )
  jpeg(titleN, width = 950, height = 950)
  plot(mean_silhouette,cex= 3,cex.axis=2,cex.lab = 3,ylab = "mean_silhouette")
  title("mean_silhouette",cex.main=3)
  dev.off()
  
  return(data.frame("cohesion"=cohesion,"separation"=separation,"mean_silhouette"=mean_silhouette,"modelPoints" = modelPoints))
}

eval1 = plotKmean(comData,3)


#========================Hierarchical Clustering============================#
#Χρησιμοποιούμε τον kmean
plotHier<-function(data,clusters){
  N=27
  library("cluster")
  library("scatterplot3d")
  library("cluster")
  mydata <- list()
  evaluaton <- list()
  modelPoints<- c()
  cohesion<-c()
  separation<-c()
  mean_silhouette<-c()
  index = 0;
  for(i in c(3:(N-2))){
    for(j in c((i+1):(N-1))){
      for(k in c((j+1):N)){
        if(nrow(data[[i*N*N+j*N+k]])>2*clusters & nrow(unique(data.frame(data[[i*N*N+j*N+k]])))>2*clusters){
          index = index +1;
          print(paste("model",index))
          d = dist(data[[i*N*N+j*N+k]])
          hc <- hclust(d, method = 'complete')
          clusters_hc = cutree(hc, k = 3)
          tempCl = clusters_hc
          clusters_hc[tempCl==1] = 2
          clusters_hc[tempCl==2] = 1
          titleN = paste("figure_Hierarchical",i,j,k,".jpg",sep = "_" )
          jpeg(titleN, width = 1950, height = 1000)
          #par(mfrow=c(1,4))
          layout(matrix(c(1,1,1,1,2,3,4,1,1,1,1,5,5,5), 2, 7, byrow = TRUE))
          s3d<-scatterplot3d(data[[i*N*N+j*N+k]],angle=230, color = clusters_hc,type = "h", pch = 16,cex.symbols = 3,cex.axis=2,cex.lab = 2)
         # centers= data[[i*N*N+j*N+k]][clusters_hc==1,1] 
         # s3d$points3d(, col = 1:3,type = "h", pch = "+", cex = 3)
          
          #EVALUATION
          modelPoints[index] = nrow(data[[i*N*N+j*N+k]])
          #cohesion[index] = model$tot.withinss
          #separation[index] = model$betweenss
          mean_silhouette[index] = mean(silhouette(clusters_hc,d)[,3])
          
          title(paste("Hierarchical clustering","\nModel_index",index,"\n mean_silhouette =",mean_silhouette[index], sep=" "),cex.main=2)
          
          
          boxplot(data[[i*N*N+j*N+k]][clusters_hc == 1,1], data[[i*N*N+j*N+k]][clusters_hc == 2,1],data[[i*N*N+j*N+k]][clusters_hc == 3,1],horizontal=FALSE, axes=TRUE,
                  main= colnames(data[[i*N*N+j*N+k]])[1],
                  col= c(1,2,3) ,
                  border="brown",
                  ylim = c(0, 1),
                  cex.lab=2,
                  cex.axis=2,
                  cex.main=2,
                  at = c(1,2,3),
                  names = c("cluster1", "cluster2","cluster3"),
                  las = 1
          )
          boxplot(data[[i*N*N+j*N+k]][clusters_hc == 1,2], data[[i*N*N+j*N+k]][clusters_hc == 2,2],data[[i*N*N+j*N+k]][clusters_hc == 3,2],horizontal=FALSE, axes=TRUE,
                  main= colnames(data[[i*N*N+j*N+k]])[2],
                  col=c(1,2,3),
                  border="brown",
                  ylim = c(0, 1),
                  cex.lab=2,
                  cex.axis=2,
                  cex.main=2,
                  at = c(1,2,3),
                  names = c("cluster1", "cluster2","cluster3"),
                  las = 1
          )
          boxplot(data[[i*N*N+j*N+k]][clusters_hc == 1,3], data[[i*N*N+j*N+k]][clusters_hc == 2,3],data[[i*N*N+j*N+k]][clusters_hc == 3,3],horizontal=FALSE, axes=TRUE,
                  main= colnames(data[[i*N*N+j*N+k]])[3],
                  col=c(1,2,3),
                  border="brown",
                  ylim = c(0, 1),
                  cex.lab=2,
                  cex.axis=2,
                  cex.main=2,
                  at = c(1,2,3),
                  names = c("cluster1", "cluster2","cluster3"),
                  las = 1
          )
          
          dev.off()
          
        }
      }
    }
  }
  

  titleN = paste("EVALUATION_Hierarchical_mean_silhouette",".jpg",sep = "" )
  jpeg(titleN, width = 950, height = 950)
  plot(mean_silhouette,cex= 3,cex.axis=2,cex.lab = 3,ylab = "mean_silhouette")
  title("mean_silhouette",cex.main=3)
  dev.off()
  
  return(data.frame("mean_silhouette"=mean_silhouette,"modelPoints" = modelPoints))
}

eval2 = plotHier(comData,3)



#==============Αξιολόγηση μοντέλου============#
#Χρησιμοποιούμε τα πρώτα 10 επόμενα σε αριθμό δεδομένων repositories
myData <- data.frame()
for(i in 10:20){
  firstRepos = rep[order(dimlength,decreasing = TRUE)[i]]
  normPepositoryData = normRep(dataset[dataset[,2]==firstRepos,])
  myData = rbind(myData,normPepositoryData)
}
modeldata = myData[,c(3,6,7,10,11,17,18,24)]
modeldata = modeldata[!is.na(modeldata[,1]) & !is.na(modeldata[,2]) & !is.na(modeldata[,3]) & !is.na(modeldata[,4])
                      & !is.na(modeldata[,5]) & !is.na(modeldata[,6]) & !is.na(modeldata[,7]) & !is.na(modeldata[,8]),]

#Χρησιμοποιούμε τον kmean
plotEvalKmean<-function(data,clusters){
  N=27
  library("scatterplot3d")
  library("cluster")
  mydata <- list()
  evaluaton <- list()
  modelPoints<- c()
  cohesion<-c()
  separation<-c()
  mean_silhouette<-c()
  index = 1;
 
          print(paste("model",index))
          model = kmeans(data, centers = clusters)
          titleN = paste("EvaluationTotal",".jpg",sep = "_" )
          jpeg(titleN, width = 1950, height = 1000)
          
          #EVALUATION
          modelPoints[index] = nrow(data)
          cohesion[index] = model$tot.withinss
          separation[index] = model$betweenss
          mean_silhouette[index] = mean(silhouette(model$cluster, dist(data))[,3])
          
          layout(matrix(c(1:8), 1, 8, byrow = TRUE)) 
          
          for(i in 1:8) {
          boxplot(data[model$cluster == 1,i], data[model$cluster == 2,i],data[model$cluster == 3,i],
                  horizontal=FALSE, axes=TRUE,
                  main= colnames(data)[i],
                  col= c(1,2,3) ,
                  border="brown",
                  ylim = c(0, 1),
                  cex.lab=2,
                  cex.axis=2,
                  cex.main=2,
                  at = c(1,2,3),
                  names = c("cluster1", "cluster2","cluster3"),
                  las = 1
          )
         
         }
          dev.off()
          
  return(data.frame("cohesion"=cohesion,"separation"=separation,"mean_silhouette"=mean_silhouette,"modelPoints" = modelPoints))
}

eval3 = plotEvalKmean(modeldata,3)




