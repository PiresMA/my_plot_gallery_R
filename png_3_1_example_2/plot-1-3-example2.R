##--------------------------------------------------------------------------------------------------##
## Import
##--------------------------------------------------------------------------------------------------##
tmax      = 100
namein_h  = sprintf('eqw_hcoin_T_nv_nl_meanPk_sdPk_skewPk_apl_vclust_assort_diam_tmax%d.txt',tmax ) 
namein_k  = sprintf('eqw_kcoin_T_nv_nl_meanPk_sdPk_skewPk_apl_vclust_assort_diam_tmax%d.txt',tmax ) 

matin_h   = data.matrix( read.table(namein_h) )
matin_k   = data.matrix( read.table(namein_k) )


vecT          = matin_h[,1]

vnvertMean_h  = matin_h[,2]
vnlinkMean_h  = matin_h[,3]
vaplMean_h    = matin_h[,7]
vclustMean_h  = matin_h[,8]
vassortMean_h = matin_h[,9]
vdiamMean_h   = matin_h[,10]


vnvertMean_k  = matin_k[,2]
vnlinkMean_k  = matin_k[,3]
vaplMean_k    = matin_k[,7]
vclustMean_k  = matin_k[,8]
vassortMean_k = matin_k[,9]
vdiamMean_k   = matin_h[,10]
##--------------------------------------------------------------------------------------------------##
##--------------------------------------------------------------------------------------------------##







##--------------------------------------------------------------------------------------------------##
#  general setup
namepng = sprintf("output-2coins-meanPk-sdPk-skewPk-tmax%04d-example2.png", tmax )
png( namepng, width=900, height=300 )


par( mfrow=c(1,3) )
par( mar = c(0, 2, 0, 5), oma = c(4, 1, 0.5, 0.1) ) #c(b,l,t,r)

##------------------------------------------------------------##













##------------------------ PANEL 1 ------------------------------------##

## LEFT side

ylim = range( c(vnvertMean_h,vnvertMean_h)  ) 
log2T = log2( vecT )

plot(  log2T, vnvertMean_h, col="blue", lty=1, pch=0, type="o", cex=2.1, xlab="", ylab="", ylim=ylim, xaxt="n", yaxt="n" )
lines( log2T, vnvertMean_k, col="blue", lty=2, pch=1, type="o", cex=2.1, xlab="", ylab="")
mtext( side=2,line=-2.3,cex=1.0,font=1, bquote( 'Number of vertices' ) ,  col="blue" )

aty = axTicks(2)  
axis(2, at=aty, labels=aty, col="blue", col.axis="blue", las=0, cex.axis=1.5,font=1.1, font.axis=1, tcl=0.5)


## RIGHT side
par(new=TRUE)  ## Allow a 2nd plot on the same frame   #Plot the 2nd plot and put axis scale on right
ylim = range( c(vnlinkMean_h,vnlinkMean_k) )
plot( log2T, vnlinkMean_h, col="red",lty=1, pch=0, type="o", cex=2.1, xlab="", ylab="",axes=FALSE, ylim=ylim )
lines(log2T, vnlinkMean_k, col="red",lty=2, pch=1, type="o", cex=2.1, xlab="", ylab="")
## a little farther out (line=4) to make room for labels

mtext(side=4,line=-2.0,cex=1.0,font=1, bquote( 'Number of links' ),  col="red" )
axis(4, ylim=ylim, col="red",col.axis="red", las=0, cex.axis=1.5,font=1.1, font.axis=1, tcl=0.5)


atx    = axTicks(1)
myatx  = seq(min(atx),max(atx),1)
labels = sapply( myatx, function(i) as.expression(bquote(2^ .(i)) ) )
axis(1,at=myatx,labels=labels, cex.axis=1.5 , font.axis=2, tcl=0.6 ) # 2 means bold
mtext(side=1,line=2.6,cex=1.0,font=1, bquote( 'Memory range T' ) )


legend('topleft',      cex=1.9, col=c('blue','blue'),  lty=c(1,2),pch=c(0,1), bty="n", ncol=1,   text.col='blue',
                     legend=c( expression(N[V]~~'H coin'), expression(N[V]~~'K coin') ) )
legend('bottomright',  cex=1.9, col=c('red','red'),  lty=c(1,2),pch=c(0,1), bty="n", ncol=1, text.col='red',
                     legend=c( expression(N[L]~~'H coin'), expression(N[L]~~'K coin') ) )
                     
##------------------------  END ------------------------------------##























##------------------------ PANEL 2 ------------------------------------##

## LEFT side

ylim = range( c(vaplMean_h,vaplMean_k)  ) 
log2T = log2( vecT )

plot(  log2T, vaplMean_h, col="blue", lty=1, pch=0, type="o", cex=2.1, xlab="", ylab="", ylim=ylim, xaxt="n", yaxt="n" )
lines( log2T, vaplMean_k, col="blue", lty=2, pch=1, type="o", cex=2.1, xlab="", ylab="")
mtext( side=2,line=-2.1,cex=1.0,font=1, bquote( 'Average Path Length' ) ,  col="blue" )

aty = axTicks(2)  
axis(2, at=aty, labels=aty, col="blue", col.axis="blue", las=0, cex.axis=1.5,font=1.1, font.axis=1, tcl=0.5)


## RIGHT side
par(new=TRUE)  ## Allow a 2nd plot on the same frame   #Plot the 2nd plot and put axis scale on right
ylim = range( c(vdiamMean_h,vdiamMean_k) )
plot( log2T, vdiamMean_h, col="red",lty=1, pch=0, type="o", cex=2.1, xlab="", ylab="",axes=FALSE, ylim=ylim )
lines(log2T, vdiamMean_k, col="red",lty=2, pch=1, type="o", cex=2.1, xlab="", ylab="")
## a little farther out (line=4) to make room for labels

mtext(side=4,line=-2.0,cex=1.0,font=1, bquote( 'Diameter' ),  col="red" )
axis(4, ylim=ylim, col="red",col.axis="red", las=0, cex.axis=1.5,font=1.1, font.axis=1, tcl=0.5)


atx    = axTicks(1)
myatx  = seq(min(atx),max(atx),1)
labels = sapply( myatx, function(i) as.expression(bquote(2^ .(i)) ) )
axis(1,at=myatx,labels=labels, cex.axis=1.5 , font.axis=2, tcl=0.6 ) # 2 means bold
mtext(side=1,line=2.6,cex=1.0,font=1, bquote( 'Memory range T' ) )


legend('bottomleft',    cex=1.9, col=c('blue','blue'),  lty=c(1,2),pch=c(0,1), bty="n", ncol=1,   text.col='blue',
                     legend=c( expression(APL~~'H coin'), expression(APL~~'K coin') ) )
legend('topright',    cex=1.9, col=c('red','red'),  lty=c(1,2),pch=c(0,1), bty="n", ncol=1, text.col='red',
                     legend=c( expression(D~~'H coin'), expression(D~~'K coin') ) )
                     
##------------------------  END ------------------------------------##


































##------------------------ PANEL 3 ------------------------------------##
## LEFT side

ylim = range( c(vclustMean_h,vclustMean_k)  ) 
log2T = log2( vecT )

plot(  log2T, vclustMean_h, col="blue", lty=1, pch=0, type="o", cex=2.1, xlab="", ylab="", ylim=ylim, xaxt="n", yaxt="n" )
lines( log2T, vclustMean_k, col="blue", lty=2, pch=1, type="o", cex=2.1, xlab="", ylab="")
mtext( side=2,line=-2.1,cex=1.0,font=1, bquote( 'Clustering' ) ,  col="blue" )

aty = axTicks(2)  
axis(2, at=aty, labels=aty, col="blue", col.axis="blue", las=0, cex.axis=1.5,font=1.1, font.axis=1, tcl=0.5)


## RIGHT side
par(new=TRUE)  ## Allow a 2nd plot on the same frame   #Plot the 2nd plot and put axis scale on right
ylim = range( c(vassortMean_h,vassortMean_k) )
plot( log2T, vassortMean_h, col="red",lty=1, pch=0, type="o", cex=2.1, xlab="", ylab="",axes=FALSE, ylim=ylim )
lines(log2T, vassortMean_k, col="red",lty=2, pch=1, type="o", cex=2.1, xlab="", ylab="")
## a little farther out (line=4) to make room for labels

mtext(side=4,line=-2.0,cex=1.0,font=1, bquote( 'Assortativity' ),  col="red" )
axis(4, ylim=ylim, col="red",col.axis="red", las=0, cex.axis=1.5,font=1.1, font.axis=1, tcl=0.5)


atx    = axTicks(1)
myatx  = seq(min(atx),max(atx),1)
labels = sapply( myatx, function(i) as.expression(bquote(2^ .(i)) ) )
axis(1,at=myatx,labels=labels, cex.axis=1.5 , font.axis=2, tcl=0.6 ) # 2 means bold
mtext(side=1,line=2.6,cex=1.0,font=1, bquote( 'Memory range T' ) )


vcol = c('blue','blue','red','red')
legend('bottomright',    cex=1.9, col=vcol,  lty=c(1,2,1,2),pch=c(0,1,0,1), bty="n", ncol=1, text.col=vcol,
                     legend=c( expression(A~~'H coin'),  expression(A~~'K coin'),
                               expression(Cl~~'H coin'), expression(Cl~~'K coin')
                                ) )
                     
##------------------------  END ------------------------------------##

dev.off()

##--------------------------------------------------------------------------------------------------##
##--------------------------------------------------------------------------------------------------##
