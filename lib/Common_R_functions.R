# File: Common_R_functions.R
# Author: Guillermo Martin 
# Template Created: Fri Jan 15 09:59:51 2021
# ---------------------------------------------------------------------------
# Description:
# A library of useful R functions

# Ploting data by columns and specific colour column. Returns plot and values of the extreme observations
dot.outliers<-function(dat,cols,color=NA){
  require(ggplot2)
  
  dat$ID<-1:nrow(dat)
  
  lowerq = quantile(dat[,cols],na.rm=T)[2]
  midq = quantile(dat[,cols], na.rm=T)[3]
  upperq = quantile(dat[,cols], na.rm=T)[4]
  iqr = max(upperq - lowerq)
  extreme.threshold.upper = (iqr * 3) + upperq
  extreme.threshold.lower = lowerq - (iqr * 3)
  #max.Out<- quantile(x, 0.75,na.rm=TRUE) + 1.5 * IQR(x,na.rm = TRUE)
  
  if(extreme.threshold.upper < midq*2) extreme.threshold.upper <- midq*2
  if(extreme.threshold.lower > midq/2) extreme.threshold.lower <- midq/2
  # if the lower threshold is very low then:                                 
  if(extreme.threshold.lower < midq/10) extreme.threshold.lower <- midq/10
  
  #Outlier identification
  out<- dat[which(dat[,cols]>extreme.threshold.upper),]
  ext <- dat[which(dat[,cols]>extreme.threshold.upper*1.5),]
  
  if (!is.na(color)) {
    p<-ggplot(dat)+
      geom_point(aes(x=ID,y=dat[,cols],colour=dat[,color]),size=1) + 
      geom_rect(aes(xmin=min(ID),xmax=max(ID),ymin=lowerq, ymax=upperq),fill="lightblue",alpha=.01)+
      geom_hline(yintercept = extreme.threshold.upper,colour="red",size=1,linetype="dashed")+
      geom_hline(yintercept = extreme.threshold.lower,colour="red",size=1,linetype="dashed")+
      geom_hline(yintercept = midq,colour="green",size=1)+
      scale_color_grey()+
      labs(y = cols)+
      theme_bw()+
      theme(axis.title.y = element_text(face = "bold",size=11),
            axis.title = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x =  element_blank(),
            legend.position=c(.8,.8),
            legend.title = element_blank())
    
    if(dim(out)[[1]]>0) {
      p<-p+
        geom_point(out,mapping=aes(x=ID,y=out[,cols]),colour="red",size=1)
    }
    
    if(dim(ext)[[1]]>0) {
      p<-p+
        geom_point(ext,mapping=aes(x=ID,y=ext[,cols]),colour="red",shape=4,size=4)
    }
  } else {
    p<-ggplot(dat)+
      geom_point(aes(x=ID,y=dat[,cols]),size=1) + 
      geom_rect(aes(xmin=min(ID),xmax=max(ID),ymin=lowerq, ymax=upperq),fill="lightblue",alpha=.01)+
      geom_hline(yintercept = extreme.threshold.upper,colour="red",size=1,linetype="dashed")+
      geom_hline(yintercept = extreme.threshold.lower,colour="red",size=1,linetype="dashed")+
      geom_hline(yintercept = midq,colour="green",size=1)+
      labs(y = cols)+
      theme_bw()+
      theme(axis.title.y = element_text(face = "bold",size=11),
            axis.title = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x =  element_blank())
    
    if(dim(out)[[1]]>0) {
      p<-p+
        geom_point(out,mapping=aes(x=ID,y=out[,cols]),colour="red",size=1)
    }
    if(dim(ext)[[1]]>0) {
      p<-p+
        geom_point(ext,mapping=aes(x=ID,y=ext[,cols]),colour="red",shape=4,size=4)
    }
  }
  return(list(plot=p,ExtremeObservation_threshold=extreme.threshold.upper[[1]]*1.5))
  
}
