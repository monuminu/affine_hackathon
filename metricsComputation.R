suppressMessages(library(ROCR))

computeMetrics <- function(pred_labels,true_labels){
  predObjrocr <- prediction(pred_labels,true_labels=="1")
  df_metrics = data.frame(cutoffs = unlist(predObjrocr@cutoffs) , 
                          fp = unlist(predObjrocr@fp), tp = unlist(predObjrocr@tp),
                          tn = unlist(predObjrocr@tn), fn = unlist(predObjrocr@fn))
  
  acc.perf = performance(predObjrocr, measure = "acc")
  df_metrics$accuracy <- unlist(acc.perf@y.values)
  
  err.perf = performance(predObjrocr, measure ="err")
  df_metrics$errorrate <- unlist(err.perf@y.values)
  
  rec.perf = performance(predObjrocr, measure ="rec")
  df_metrics$recall_or_sens_or_tpr <- unlist(rec.perf@y.values)
  
  spec.perf = performance(predObjrocr, measure ="spec")
  df_metrics$spec_or_tnr <- unlist(spec.perf@y.values)
  
  ppv.perf = performance(predObjrocr, measure ="ppv")
  df_metrics$ppv_or_prec <- unlist(ppv.perf@y.values)
  
  npv.perf = performance(predObjrocr, measure ="npv")
  df_metrics$npv <- unlist(npv.perf@y.values)

  return(df_metrics)
}

plotPPVvsRec <- function(pred_labels,true_labels){
  predObjrocr <- prediction(pred_labels,true_labels=="RED")
  
  rec.perf = performance(predObjrocr, measure ="rec")
  ppv.perf = performance(predObjrocr, measure ="ppv")
  plot(x = unlist(predObjrocr@cutoffs) , y = unlist(rec.perf@y.values) , col = "RED", 
       xlab = "Cutoffs", ylab = "Recall, PPV", main = "Recall vs PPV", panel.first = grid())
  points(x= unlist(predObjrocr@cutoffs) , y = unlist(ppv.perf@y.values) , col = "Blue")
  legend("topright", col = c("RED", "BLUE"), legend = c("Recall", "PPV"), pch = c(16,16), bty = 'n')
}


calcF1Scores=function(act,prd){
  #treats the vectors like classes
  #act and prd must be whole numbers
  df=data.frame(act=act,prd=prd);
  scores=list();
  for(i in seq(min(act),max(act))){
    tp=nrow(df[df$prd==i & df$act==i,]);        
    fp=nrow(df[df$prd==i & df$act!=i,]);
    fn=nrow(df[df$prd!=i & df$act==i,]);
    f1=(2*tp)/(2*tp+fp+fn)
    scores[[i]]=f1;
  }      
  print(scores)
  return(scores);
}

