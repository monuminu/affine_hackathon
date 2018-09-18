autodetect_datatype<-function(x){
  #This function autodetects datatypes that are coded as strings.
  x=as.character(x)
  Reason=NA  
  type='numeric'
  xclean=notmissingOrNone(x)$values
  
  distinctValues=length(unique(xclean))
  
  cat('Non NA Values:',length(xclean),' ',length(x) ,'\n')
  
  if(length(xclean)/length(x) < 0.05){
    type='junk'
    Reason='Too few non NA data points'
    return(c(type,Reason,distinctValues))
  }
  
  xcleanAndNumeric=as.numeric(xclean)
  
  if(any(is.na(xcleanAndNumeric))){
    type='factor'
    Reason='Junk in Numeric Values'
    
    if(length(unique(xclean))>150){
      type='id'
      Reason='Too many distinct factors probably an id variable'
    }
    
    #write log message
    return(c(type,Reason,distinctValues))
  } 
  
  #Now let us try if there is atleast 10 distinct types
  nlevels=length(unique(xcleanAndNumeric))
  
  if(nlevels<10){type='factor'
  Reason='Too few unique values'
  }
  
  
  
  return(c(type,Reason,distinctValues))
}





autodetect_transformations_bnsf<-function(x){
  #This function autodetects datatypes that are coded as strings.
  res=autodetect_datatype(x)
  type=res[1]
  
  TF='NA'
  #Now depending on type we should 
  xclean=notmissingOrNone(x)$values
  if(type=='numeric'){xclean=as.numeric(xclean);TF=paste(as.character(c(max(xclean),min(xclean),mean(xclean)) ),collapse=',' ) }
  if(type=='factor' ){TF= paste(names(sort(table(xclean),decreasing=T)),collapse=',') }
  if(type=='id'){TF='NA'}
  
  res[4]=TF
  names(res)=c('Type','Reason','Unique','TF')
  return(res)
  
}


get_ready_for_logistic_bnsf<-function(X,TFDF,train=FALSE){
  
  TypeInfo=TFDF
  
  TypeInfo_Factor=subset(TypeInfo,TypeInfo$Type=='factor')
  TypeInfo_Numer =subset(TypeInfo,TypeInfo$Type=='numeric')
  TypeInfo_ID=subset(TypeInfo,TypeInfo$Type=='id') 
  
  #For Numeric  do cleanup 
  X_NUMER=lapply(rownames(TypeInfo_Numer),function(name){  
    
    TFmaxminmean=as.numeric(unlist(strsplit(TypeInfo_Numer[name,'TF'],',') ))
    
    xw=cleanNumeric( X[ ,name],Name=name, Missing=TFmaxminmean[3]) #
    
    xw[ ,1]=(xw[ ,1] - TFmaxminmean[2]) /(TFmaxminmean[1] -TFmaxminmean[2] )
    return(xw)
  } )
  
  X_FACTOR=lapply(rownames(TypeInfo_Factor),function(name){
    
    keeplevels= unlist(strsplit(TypeInfo_Factor[name,'TF'],','))
    nk=min(length(keeplevels),10)
    x1=reduceFactors_predefined( X[ ,name],keeplevels[1:nk])      
    factorToNumericDF(x1,Name=name,alllevs=T)
  })                                                             
  XR=data.frame(do.call(cbind,X_NUMER), do.call(cbind,X_FACTOR))
  
  ####
  uniqueness_filter=sapply(XR,function(x){length(unique(x))})>1
  columns_to_keep=names(which(uniqueness_filter))
  if(train==FALSE){columns_to_keep=colnames(XR)}
  
  
  return(XR[ ,columns_to_keep])
  
  
}

#####
cleanNumeric<-function(Xvec,Name='Var', Missing=0){
  YValues=rep(Missing,length(Xvec))
  LObj=notmissingOrNone(Xvec)
  YValues[LObj$indexes]=as.numeric(LObj$values)
  
  YMissing=rep(1,length(Xvec))
  YMissing[LObj$indexes]=0
  
  DF=data.frame(YValues, YMissing)
  colnames(DF)=paste(Name,c('Values','IsMissing'),sep='.')
  return(DF)
}

quantize_by_prc<-function(x,n=3){
  #Should only have non zeros 
  cat('NAs not supported \n')
  breaks=quantile(x, seq(0,1,by=1/n))
  breaks[1]=floor(breaks[1])
  breaks[n+1]=ceiling(breaks[n+1])
  
  y=cut(x,breaks=breaks)
  
  return(y)  
  
}
keepn<-function(x,n=15){
  x=as.character(x)
  y=lapply(x,substr,1,n) 
  unlist(y)
}

naToZero<-function(x){
  #returns 0s for NAs
  x[is.na(x)]=0
  x
}

#Check for NA and empty

notmissingOrNone<-function(x){
  #This function return 2 arrays, one of non missing indexes and the other of non missing values
  x=gsub('\\s+','',x)
  y=unrecordedToNone(x,missing='AlienSequenceGTCAGCCG')
  indexes=which(x==y)
  values=x[indexes]
  return(list(indexes=indexes,values=values))
}



##### Change type #####################

unrecordedToNone<-function(x,missing='None'){
  #Changes empty/missing to character string in "missing"
  x[is.na(x)]=missing
  x[x=='']=missing
  return(x)
}
reduceFactors<-function(x,n=6,catchall='_allElse',Missing='_UD'){
  #this function turns a factor with multiple levels into a factor with only a few levels, the top n and all else.
  xf=notmissingOrNone(x) 
  
  y=xf$values
  
  ty=sort(table(y),decreasing=T)
  
  n=min(n,length(ty))
  
  keeplevels=names(ty[1:n])
  
  values=reduceFactors_predefined(x,keeplevels)
  return(values)  
  
}
reduceFactors_predefined<-function(x,keeplevels,catchall='_allElse',Missing='_UD'){
  #this function turns a factor into  a factor with predefined levels defined. 
  xf=notmissingOrNone(x) 
  
  y=xf$values
  y[!(y%in%keeplevels)] =  catchall
  
  values=rep(Missing,length(x))
  values[xf$indexes]=y
  
  labs=unique(c(keeplevels,catchall,Missing))
  v=factor(values,levels=labs)
  
  return(v)  
  
}
discretizeNumeric<-function(x,n=6,Missing='_UD'){
  #Should only have non zeros 
  xf=notmissingOrNone(x)
  
  njv=as.numeric(xf$values) #non junk values
  
  struct<- Ckmeans.1d.dp(njv,n)
  
  RangeNames=unlist(lapply(sort(unique(struct$cluster)),function(cx){ 
    paste(range(njv[which(struct$cluster==cx)]),collapse='-')
  }))
  
  y=RangeNames[struct$cluster]
  #y=round(struct$centers[struct$cluster],2)
  
  values=rep(Missing,length(x))
  values[xf$indexes]=y
  
  values=factor(values)
  
  return(values)  
  
}
factorToNumericDF<-function(Xvec,alllevs=F,Name='Var'){
  levs=unique(Xvec)
  if(alllevs==T){levs=levels(Xvec)}
  Z=lapply(levs,function(lev){ (Xvec==lev) *1})
  Z2=data.frame(do.call(cbind,Z)) 
  colnames(Z2)=paste(Name,as.character(levs),sep='.')
  return(Z2)   
}
idToindicator<-function(x){
  y=rep(0,length(x))
  y[notmissingOrNone(x)[[1]]]=1
  y
}
unit_transform<-function(x){
  y=(x-min(x))/(max(x)-min(x))
  return(y)
}

###########AUC Functions #######################
auc_calc<-function(target_pred,target_class){
  
  if(length(unique(target_class))!=2){warning('No diversity of Label');return(NA)}
  
  pred <- prediction(target_pred, target_class)
  perf <- performance(pred,"tpr","fpr")
  auc <- performance(pred,"auc") 
  auc@y.values[[1]]
}
auc_df<-function(PredsVsLabs,sampling=5){
  Z <- prediction(predictions=PredsVsLabs$PredVals,labels=PredsVsLabs$Labs)
  Z2<-performance(Z, measure = "tpr", x.measure = "fpr")
  DF=data.frame(
    FPR=unlist(Z2@x.values),
    TPR=unlist(Z2@y.values),
    THR=unlist(Z2@alpha.values))
  return(DF)
}
auc_multiple<-function(PREDS_DF,Labs){
  
  Pred_Cols=colnames(PREDS_DF)
  L=lapply(Pred_Cols,function(clm){ 
    Z=data.frame(Preds=PREDS_DF[ ,clm],Labs=Labs)
    auc_val=auc_calc(Preds=PREDS_DF[ ,clm],Labs=Labs)
    cs=paste(clm,auc_val[2],auc_val[1],sep=',',collapse='')
    Zout=data.frame(auc_df(Z),Var=cs)
  })
  DF_Out=do.call(rbind,L)
}

