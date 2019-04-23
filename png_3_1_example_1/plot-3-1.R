
##--------------------------------------------------------------------------------------------------##
## Import
##--------------------------------------------------------------------------------------------------##
tmax      = 100
namein_h  = sprintf('eqw_hcoin_T_nv_nl_meanPk_sdPk_skewPk_apl_vclust_assort_diam_tmax%d.txt',tmax ) 
namein_k  = sprintf('eqw_kcoin_T_nv_nl_meanPk_sdPk_skewPk_apl_vclust_assort_diam_tmax%d.txt',tmax ) 

matin_h   = data.matrix( read.table(namein_h) )
matin_k   = data.matrix( read.table(namein_k) )



vecT        = matin_h[,1]

vmdegMean_h = matin_h[,4]
vsdMean_h   = matin_h[,5]
vskewMean_h = matin_h[,6]

vmdegMean_k = matin_k[,4]
vsdMean_k   = matin_k[,5]
vskewMean_k = matin_k[,6]
##--------------------------------------------------------------------------------------------------##
##--------------------------------------------------------------------------------------------------##







##--------------------------------------------------------------------------------------------------##
#  general setup
namepng = sprintf("output-2coins-meanPk-sdPk-skewPk-tmax%04d.png", tmax )
png( namepng, width=900, height=300 )


par( mfrow=c(1,3) )
par( mar = c(0, 2, 0, 1), oma = c(2, 2, 0.5, 0.5) )  #c(b,l,t,r)
##------------------------------------------------------------##







##------------------------------------------------------------##
#panel 1
ylim  = range( c(vmdegMean_h,vmdegMean_k)  ) 
log2T = log2( vecT )

plot(  log2T, vmdegMean_h, pch=8,  col='red',   type='o', ylim=ylim, xaxt="n", yaxt="n", xlab=' ', ylab='' )
lines( log2T, vmdegMean_k, pch=16, col='blue',  type='o', xaxt="n", xlab=' ', ylab='')
mtext( side=2,line=-2.3,cex=1.0,font=1, bquote('mean of'~P(k) ) )

atx    = axTicks(1) 
myatx  = seq(min(atx),max(atx),1)
labels = sapply( myatx, function(i) as.expression(bquote(2^ .(i)) ) )
axis(1,at=myatx,labels=labels, cex.axis=1.5 , font.axis=2, tcl=0.6 ) # 2 means bold

aty = axTicks(2)  
axis(2, at=aty, labels=aty, cex.axis=1.5, font.axis=1, tcl=0.5)

legend('topleft', cex=1.5, col=c('red','blue'), pch=c(8,16), bty="n", ncol=1,
                     legend=c( "H coin","K coin" ) )
##------------------------------------------------------------##







##------------------------------------------------------------##
#panel 2
ylim = range( c(vsdMean_h,vsdMean_k)  ) 
log2T = log2( vecT )

plot(  log2T, vsdMean_h, pch=8,  col='red',   type='o', ylim=ylim, xaxt="n", yaxt="n", xlab=' ', ylab='' )
lines( log2T, vsdMean_k, pch=16, col='blue',  type='o', xaxt="n", xlab=' ', ylab='')
mtext( side=2,line=-2.3,cex=1.0,font=1, bquote('Standard deviation of'~P(k) ) )

atx    = axTicks(1) #print( atx )
myatx  = seq(min(atx),max(atx),1)
labels = sapply( myatx, function(i) as.expression(bquote(2^ .(i)) ) )
axis(1,at=myatx,labels=labels, cex.axis=1.5 , font.axis=2, tcl=0.6 ) # 2 means bold


aty = axTicks(2)  
axis(2, at=aty, labels=aty, cex.axis=1.5, font.axis=1, tcl=0.5)

legend('topleft', cex=1.5, col=c('red','blue'), pch=c(8,16), bty="n", ncol=1,
                     legend=c( "H coin","K coin" ) )

##------------------------------------------------------------##







##------------------------------------------------------------##
#panel 3

ylim = range( c(vskewMean_h,vskewMean_k)  ) 
log2T = log2( vecT )

plot(  log2T, vskewMean_h, pch=8,  col='red',   type='o', ylim=ylim, xaxt="n", yaxt="n", xlab=' ', ylab='' )
lines( log2T, vskewMean_k, pch=16, col='blue',  type='o', xaxt="n", xlab=' ', ylab='')
mtext(side=2,line=-2.3,cex=1.0,font=1, bquote('Skewness of'~P(k) ) )

atx    = axTicks(1) #print( atx )
myatx  = seq(min(atx),max(atx),1)
labels = sapply( myatx, function(i) as.expression(bquote(2^ .(i)) ) )
axis(1,at=myatx,labels=labels, cex.axis=1.5 , font.axis=2, tcl=0.6 ) # 2 means bold

aty = axTicks(2)  
axis(2, at=aty, labels=aty, cex.axis=1.5, font.axis=1, tcl=0.5)

legend('topleft', cex=1.5, col=c('red','blue'), pch=c(8,16), bty="n", ncol=1,
                     legend=c( "H coin","K coin" ) )


##--------------------------------------------------------------------------------------------------##
dev.off()  # END
##--------------------------------------------------------------------------------------------------##

