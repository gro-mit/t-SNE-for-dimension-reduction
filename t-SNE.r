library(ggplot2)
library(scatterplot3d)
library(rgl)
library(tsne)
setwd("")#自行设定
rawdata <- read.csv("")
classLabel<-unique(rawdata$Classification)
highdata <- rawdata[,-(1:2)]
highdata_scored <- scale(highdata)
#----------------------------------------- PC=2
pcadata2 <- prcomp(highdata_scored)
preddata2<-as.data.frame(predict(pcadata2))
cptwo2 <- data.frame(Samples=rawdata$Samples,Classification=rawdata$Classification,preddata2[,1:2])
graph2 <- ggplot(cptwo2,mapping=aes(x=PC1,y=PC2,color=Classification,label=Samples))
graph2 <- graph2+geom_point(alpha=0.6)+geom_text(vjust=0,nudge_y=0.5,size=3)+ggtitle("PCA PC = 2")+theme(plot.title = element_text(hjust = 0.5))+scale_color_brewer(palette = "Set1")
filename <- paste("PCA PC = 2",".png",sep='')
ggsave(filename,width=10,heigh=6.18,dpi=600)
#------------------------------------------k=2 每次初始化t-SNE后error不固定
tsne2 <- tsne(highdata_scored,k=2,max_iter=1000)
tdata2 <- data.frame(rawdata[,1:2],X=tsne2[,1],Y=tsne2[,2])
tgraph2 <- ggplot(tdata2,mapping=aes(x=X,y=Y,color=Classification,label=Samples))
tgraph2 <- tgraph2+geom_point(alpha=0.6)+geom_text(vjust=0,nudge_y=0.5,size=3)+ggtitle("t-SNE k = 2")+theme(plot.title = element_text(hjust = 0.5))
filename <- paste("t-SNE k = 2",".png",sep='')
ggsave(filename,width=10,heigh=6.18,dpi=600)
#-------------------------------------------PC=3
cpthree2 <- data.frame(Samples=rawdata$Samples,Classification=rawdata$Classification,preddata2[,1:3])
plot3d(cpthree2$PC1,cpthree2$PC2,cpthree2$PC3, col=as.numeric(cpthree2$Classification), size=5,main="PCA PC = 3",xlab = "PC1",ylab = "PC2",zlab = "PC3")
text3d(cpthree2$PC1,cpthree2$PC2,cpthree2$PC3,texts = cpthree2$Samples,font=1)
legend3d("topright", legend = unique(cpthree2$Classification), pch = 16, col = unique(cpthree2$Classification), cex=1, inset=c(0.02))
#-------------------------------------------k=3
tsnethree2 <- tsne(highdata_scored,k=3,max_iter=1000)
tdatathree2 <- data.frame(rawdata[,1:2],X=tsnethree2[,1],Y=tsnethree2[,2],Z=tsnethree2[,3])
plot3d(tdatathree2$X,tdatathree2$Y,tdatathree2$Z, col=as.numeric(tdatathree2$Classification), size=5,main="t-SNE k = 3",xlab = "X",ylab = "Y",zlab = "Z")
text3d(tdatathree2$X,tdatathree2$Y,tdatathree2$Z,texts = tdatathree2$Samples,font=1)
legend3d("topright", legend = unique(tdatathree2$Classification), pch = 16, col = unique(tdatathree2$Classification), cex=1, inset=c(0.02))

#------------------------------两两降维code
#------------------------------维度2
for(i in 1:(length(classLabel)-1)){
	for(j in ((i+1):length(classLabel))){
		tempdata <- subset(rawdata,Classification==classLabel[i]|Classification==classLabel[j])
		highdata <- tempdata[,-(1:2)]
		pcadata2 <- prcomp(highdata)
		preddata2 <- as.data.frame(pcadata2$x)
		cptwo2 <- data.frame(Samples=tempdata$Samples,Classification=tempdata$Classification,preddata2[,1:2])
		graph2 <- ggplot(cptwo2,mapping=aes(x=PC1,y=PC2,color=Classification,label=Samples))
		graph2 <- graph2+geom_point(alpha=0.6)+geom_text(vjust=0,nudge_y=0.5,size=3)+ggtitle(paste("PCA PC = 2 ",classLabel[i]," VS ",classLabel[j],sep=''))+theme(plot.title = element_text(hjust = 0.5))+scale_color_brewer(palette = "Set1")
		filename <- paste("PCA PC = 2 ",classLabel[i]," VS ",classLabel[j],".png",sep='')
		ggsave(filename,width=10,heigh=6.18,dpi=600)
	}
}

for(i in 1:(length(classLabel)-1)){
	for(j in ((i+1):length(classLabel))){
		tempdata <- subset(rawdata,Classification==classLabel[i]|Classification==classLabel[j])
		highdata <- tempdata[,-(1:2)]
		tsne2 <- tsne(highdata,k=2,max_iter=1000)
		tdata2 <- data.frame(tempdata[,1:2],X=tsne2[,1],Y=tsne2[,2])
		tgraph2 <- ggplot(tdata2,mapping=aes(x=X,y=Y,color=Classification,label=Samples))
		tgraph2 <- tgraph2+geom_point(alpha=0.6)+geom_text(vjust=0,nudge_y=0.5,size=3)+ggtitle(paste("t-SNE k = 2 ",classLabel[i]," VS ",classLabel[j],sep=' '))+theme(plot.title = element_text(hjust = 0.5))
		filename <- paste("t-SNE k = 2 ",classLabel[i]," VS ",classLabel[j],".png",sep='')
		ggsave(filename,width=10,heigh=6.18,dpi=600)
	}
}

#------------------------------维度3
for(i in 1:(length(classLabel)-1)){
	for(j in ((i+1):length(classLabel))){
		tempdata <- subset(rawdata,Classification==classLabel[i]|Classification==classLabel[j])
		highdata <- tempdata[,-(1:2)]
		pcadata2 <- prcomp(highdata)
		preddata2 <- as.data.frame(pcadata2$x)
		cpthree2 <- data.frame(Samples=tempdata$Samples,Classification=tempdata$Classification,preddata2[,1:3])
		plot3d(cpthree2$PC1,cpthree2$PC2,cpthree2$PC3, col=as.numeric(cpthree2$Classification), size=5,main=paste("PCA PC = 3 ",classLabel[i]," VS ",classLabel[j],sep=''),xlab = "PC1",ylab = "PC2",zlab = "PC3")
		text3d(cpthree2$PC1,cpthree2$PC2,cpthree2$PC3,texts = cpthree2$Samples,font=1)
		legend3d("topright", legend = unique(cpthree2$Classification), pch = 16, col = unique(cpthree2$Classification), cex=1, inset=c(0.02))
		filename <- paste("PCA PC = 3 ",classLabel[i]," VS ",classLabel[j],".png",sep='')
		rgl.snapshot(filename,fmt='png')
	}
}


for(i in 1:(length(classLabel)-1)){
	for(j in ((i+1):length(classLabel))){
		tempdata <- subset(rawdata,Classification==classLabel[i]|Classification==classLabel[j])
		highdata <- tempdata[,-(1:2)]
		tsnethree2 <- tsne(highdata,k=3,max_iter=1000)
		tdatathree2 <- data.frame(tempdata[,1:2],X=tsnethree2[,1],Y=tsnethree2[,2],Z=tsnethree2[,3])
		plot3d(tdatathree2$X,tdatathree2$Y,tdatathree2$Z, col=as.numeric(tdatathree2$Classification), size=5,main=paste("t-SNE k = 3 ",classLabel[i]," VS ",classLabel[j],sep=''),xlab = "X",ylab = "Y",zlab = "Z")
		text3d(tdatathree2$X,tdatathree2$Y,tdatathree2$Z,texts = tdatathree2$Samples,font=1)
		legend3d("topright", legend = unique(tdatathree2$Classification), pch = 16, col = unique(tdatathree2$Classification), cex=1, inset=c(0.02))
		filename <- paste("t-SNE k = 3 ",classLabel[i]," VS ",classLabel[j],".png",sep='')
		rgl.snapshot(filename,fmt='png')
	}
}
