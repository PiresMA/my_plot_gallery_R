


# Lets generate some artificial data
nr = 3

vlen   = c(NULL)
mylist = list(NULL)

for( i in 1:nr){
 vv         = sample( 1:30, 10*i ) 
 mylist[[i]]= round(vv/sum(vv), 2)
 vlen[i]    = length(vv)
}




# Lets organize the data
mylist2 = list(NULL)
mymat = matrix(0,nrow=nr, ncol=max(vlen) )
for( i in 1:nr){
 vv = mylist[[i]]

 vnew = rep(0, max(vlen) )
 vnew[ 1:length(vv) ] = vv
 
 mylist2[[i]] = rev(sort(vnew))
}

mymat <- t( as.matrix(as.data.frame(mylist2))  )


deg             = 1:max(vlen)
colnames(mymat) = deg
rownames(mymat) = sprintf("parameter=%d",c(1,3,9) )





# Now lets plot
png('barplot-grouped.png')

# grouped barplot
vcol = rev(rainbow(nr))
barplot(mymat, col=vcol ,border="black",  ylim=c(0,round(max(mymat),1) ), font.axis=1, beside=T, 
  legend=rownames(mymat), xlab="x", ylab="Frequency", font.lab=2, cex.names=0.8, names=deg, las=2 )


dev.off()
