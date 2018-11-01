# Read the data files.
setwd("data/train")

num.class <- 10

num.training <- 100
training.data <- array(NA, c(30, 30, num.training, num.class))
for (i in seq_len(num.class)) {
    f <- file(paste0("tr", i - 1, ".bin"), open="rb")
    tr <- readBin(f, logical(), size=1, n=30*30*num.training)
    dim(tr) <- c(30, 30, num.training)
    training.data[,,,i] <- aperm(tr, c(2, 1, 3))
    close(f)
}

setwd("data/test")
num.test <- 1000
test.data <- array(NA, c(30, 30, num.test, num.class))
for (i in seq_len(num.class)) {
    f <- file(paste0("te", i - 1, ".bin"), open="rb")
    te <- readBin(f, logical(), size=1, n=30*30*num.test)
    dim(te) <- c(30, 30, num.test)
    test.data[,,,i] <- aperm(te, c(2, 1, 3))
    close(f)
}

# A function to plot a digit.
plot.digit <- function(digit.data) {
    # Here digit.data is a matrix of values in [0,1].
    image(t(1 - digit.data)[,nrow(digit.data):1],
        col=gray(seq(0, 1, length.out=256)), axes=FALSE, asp=1)
}

# An example.
par(mfrow=c(2,5))
par(mar=c(1,1,1,1))
for (i in seq_len(num.class)) {
    plot.digit(apply(training.data[,,,i], c(1,2), mean))
}


## M=1  FINDING MLE


td<- array(NA, c(100, 900, 10))
for(j in 1:10) for(i in 1:100) td[i,,j]=as.vector(t(training.data[,,i,j]))

phat3<-t((colSums(td[,,1:10])+0.1)/(100+0.2))

for(i in 1:10){
	for(j in 1:900){
		phat2[i,j]=(sum(td[,j,i])+.1)/100.2
	}
}

ted<- array(NA, c(1000, 900, 10))
for(j in 1:10) for(i in 1:1000) ted[i,,j]=as.vector(t(test.data[,,i,j]))
e2<-array(NA,c(10,1000))
for(j in 1:10) for(i in 1:1000) {
	e2[j,i]<-which.max(rowSums(log(t(t(phat2)^ted[i,,j])*t(t(1-phat2)^(1-ted[i,,j])))))-1
}

tot=correct=0
for(i in 1:10){
	correct=correct+length(which(e2[i,]==(i-1)))
	tot=tot+length(e2[i,])
	print((1-length(which(e2[i,]==(i-1)))/length(e2[1,]))*100)
}
1-(correct)/tot

par(mfrow=c(3,4))
par(mar=c(.5,0.5,.5,.5))
for(i in 1:10)  plot.digit(matrix((phat2[i,]),nrow=30,ncol=30,byrow=TRUE))



#M=3 N=1
td<- array(NA, c(100, 900, 10))
for(j in 1:10) for(i in 1:100) td[i,,j]=as.vector(t(training.data[,,i,j]))

m=3
phat_final<- array(NA, c(900,m, 10))
pi_final<-array(NA,c(10,m))	
phat<-array(NA,c(3,900))
for(l in 1:10){
	qn<-cbind(c(rep(1,33),rep(0,67)),c(rep(0,33),rep(1,33),rep(0,34)),c(rep(0,66),rep(1,34)))
	for(i in 1:3){
		phat[i,]=(colSums(qn[,i]*td[,,l])+.1)/(sum(qn[,i])+.2)
		pi=(colSums(qn)+0.1)/(3*1.1-3+100)
	}
	for(k in 1:20){
		#plot.digit(matrix(round(phat[1,]),nrow=30,ncol=30,byrow=TRUE))
		for(i in 1:100){
			x=log(pi)+colSums((td[i,,l]*log(t(phat))+(1-td[i,,l])*log(t(1-phat))))
			qn[i,]=exp(x-max(x))/sum(exp(x-max(x)))			
		}
		for(i in 1:m) phat[i,]=(colSums(qn[,i]*td[,,l])+.1)/(sum(qn[,i])+.2)
		pi=(colSums(qn)+0.1)/(m*1.1-3+100)
	}
	phat_final[,1,l]<-phat[1,]
	phat_final[,2,l]<-phat[2,]
	phat_final[,3,l]<-phat[3,]
	pi_final[l,]<-pi
	plot.digit(matrix(round(phat_final[,1,l]),nrow=30,ncol=30,byrow=TRUE))

}
par(mfrow=c(5,3))
par(mar=c(.5,0.5,.5,.5))
for(i in 1:5) for(j in 1:3) plot.digit(matrix((phat_final[,j,i]),nrow=30,ncol=30,byrow=TRUE))
dev.new()
par(mfrow=c(5,3))
par(mar=c(.5,0.5,.5,.5))
for(i in 6:10) for(j in 1:3) plot.digit(matrix((phat_final[,j,i]),nrow=30,ncol=30,byrow=TRUE))


tot=correct=0
e<-array(NA,c(10,1000))
for(j in 1:10) { 
	for(i in 1:1000) {
		probs<-rep(0,10)
		for(k in 1:10) {
			for(l in 1:m) {
				temp<-ted[i,,j]*log(phat_final[,l,k])+(1-ted[i,,j])*log(1-phat_final[,l,k])
				probs[k]=probs[k]+pi_final[k,l]*exp(sum(temp))
			}
		}
		e[j,i]=which.max(probs)
		}
		correct=correct+length(which(e[j,]==(j)))
		tot=tot+length(e[j,])	
		print((1-length(which(e[j,]==j))/length(e[j,]))*100)
}
1-correct/tot


