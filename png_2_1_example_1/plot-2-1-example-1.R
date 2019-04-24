

##=========================================================================##
vecT = c(1,2,4)
nT   = length(vecT)

tmax  = 100
vtime = seq(0,tmax,1)

ymax  = 1
yaxis = seq(0,ymax,0.1)
##=========================================================================##




##=========================================================================##
namepng = sprintf("output-2-1-example-1.png")
png( namepng, width=600, height=300 )

par( mfrow = c(1, 2) ) 
par( mar = c(0, 0, 0, 0),     oma = c(4, 4, 0.5, 0.5) )
##=========================================================================##







##################################  KEMPE-like
 plot(1, 1, type='n',xlab='',ylab='',axes=FALSE, xlim=c(0,tmax), ylim=c(0,ymax) )

 mtext(side=2,line=2.4,cex=1.7,bquote(' '~S[e] ) )
 mtext(side=1,line=2.7,cex=1.4,bquote('time') )

 # eixo Y
 axis(2, las=1, cex.axis=1.4, tck=.02, col="black", at=yaxis, label=yaxis)  

 # eixo X
 xaxis = seq(min(vtime), max(vtime), 10)
 axis(1, las=2, cex.axis=1.4, tck=.02, col="black", at=xaxis, label=xaxis)  



 for(indT in 1:length(vecT)  ){ 
  TT = vecT[indT] 

  nameIN = sprintf( "eqw_kcoin_t_Se_T%05d.txt", TT) 
  matin  = data.matrix(read.table( nameIN ))
  meanSe = matin[vtime,1]

  lines(meanSe, type='o', pch=15+indT, col=indT,cex=1.2-0.01*indT, ylim=c(0,ymax)  )
  
 }

 legend(5,0.5,legend=c('K coin'), cex=1.6, bty='n' )
 legend(60,0.65,col=1:nT, pch=15+1:nT,legend=sprintf('T=%d',vecT), cex=1.5, bty='n' )
 box(col = "black")
 abline(h=0.87243, col='blue',lty=2, lwd=2.5 )
##=========================================================================##


















##################################  HADAMARD-like COIN


plot(1, 1, type='n',xlab='',ylab='',axes=FALSE, xlim=c(0,tmax), ylim=c(0,ymax) )
mtext(side=1,line=2.7,cex=1.4,bquote('time') )

# eixo Y
axis(2, las=1, cex.axis=1.4, tck=.02, col="black", at=yaxis, label=NA)  

# eixo x
xaxis = seq(min(vtime),max(vtime),10)
axis(1, las=2, cex.axis=1.4, tck=.02, col="black", at=xaxis, label=xaxis)  


 
 for(indT in 1:length(vecT)  ){
  TT = vecT[indT] 

  nameIN = sprintf( "eqw_hcoin_t_Se_T%05d.txt", TT) 
  matin  = data.matrix(read.table( nameIN ))
  meanSe = matin[vtime,1]

  lines(meanSe, type='o', pch=15+indT, col=indT,cex=1.2-0.01*indT, ylim=c(0,ymax)  )
  
 }


 legend(5,0.5,legend=c('H coin'), cex=1.6, bty='n' )
 legend(60,0.65,col=1:nT, pch=15+1:nT,legend=sprintf('T=%d',vecT), cex=1.5, bty='n' )

 box(col = "black")
 abline(h=0.87243, col='blue',lty=2, lwd=2.5 )


dev.off()
##================================================================##
