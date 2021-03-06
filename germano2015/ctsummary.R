
ctsummary<-function(object,ridging=FALSE,timeInterval=1,...){
  
  if(ridging==TRUE) ridging<- .0001 else ridging <- 0
  
  outlist<-c()
  
  if(class(object)!= 'ctsemFit') mxobj<-object
  if(class(object)== 'ctsemFit') mxobj<-mxobj
  
  #set defaults so mxobj can be used also
  asymptotes<-FALSE
  stationary<-TRUE
  discreteTime<-FALSE
  n.TIpred<-0
  n.TDpred<-0
  n.latent<-nrow(OpenMx::mxEval(DRIFT,mxobj,compute=T))
  n.manifest<-nrow(OpenMx::mxEval(LAMBDA,mxobj,compute=T))
  latentNames<-paste0('latent',1:n.latent)
  manifestNames<-paste0('manifest',1:n.latent)

  if(!is.null(object$ctfitargs)){
  try(asymptotes<-object$ctfitargs$asymptotes)
  try(stationary<-object$ctfitargs$stationary)
  try(discreteTime <- object$ctfitargs$discreteTime)
    try(n.TIpred<-object$ctmodelobj$n.TIpred)
    try(n.TDpred<-object$ctmodelobj$n.TDpred)
    try(latentNames<-object$ctmodelobj$latentNames)
    try(manifestNames<-object$ctmodelobj$manifestNames)
  }
  
  out<-list()
  omxsummary<-getMethod("summary","MxModel")(mxobj) #get openmx summary

  output<-list()

  if(discreteTime==FALSE){
    
    DRIFT<-tryCatch({ OpenMx::mxEval(DRIFT, mxobj,compute=TRUE)}, error=function(e) e )
    tryCatch({  dimnames(DRIFT)<-list(latentNames,latentNames)}, error=function(e) e )
    outlist<-c(outlist,'DRIFT')
    
    LAMBDA<-tryCatch({ OpenMx::mxEval(LAMBDA, mxobj,compute=TRUE)}, error=function(e) e )
    tryCatch({  dimnames(LAMBDA)<-list(manifestNames,latentNames)}, error=function(e) e )
    outlist<-c(outlist,'LAMBDA')
    
    discreteDRIFT<-tryCatch({ OpenMx::expm(DRIFT * timeInterval)}, error=function(e) e )
    tryCatch({  dimnames(discreteDRIFT)<-list(latentNames,latentNames)}, error=function(e) e )
    outlist<-c(outlist,'discreteDRIFT')
    
    outlist<-c(outlist,'discreteDRIFTstd')
    
    DRIFTHATCH<-tryCatch({ (DRIFT %x% diag(n.latent)) + (diag(n.latent) %x% DRIFT)}, error=function(e) e )
    
    MANIFESTVAR<-tryCatch({ mxobj$MANIFESTVAR$result}, error=function(e) e )
    tryCatch({  dimnames(MANIFESTVAR)<-list(manifestNames,manifestNames)}, error=function(e) e )
    outlist<-c(outlist,'MANIFESTVAR')
    
    CINT<-tryCatch({ mxobj$CINT$values}, error=function(e) e )
    if(asymptotes==TRUE) CINT <- tryCatch({ -DRIFT %*% CINT}, error=function(e) e )
    tryCatch({  rownames(CINT)<-latentNames}, error=function(e) e )
    outlist<-c(outlist,'CINT')
    
    discreteCINT<-tryCatch({ solve(DRIFT) %*%(discreteDRIFT - diag(n.latent)) %*% CINT}, error=function(e) e )
    tryCatch({  rownames(discreteCINT)<-latentNames}, error=function(e) e )
    outlist<-c(outlist,'discreteCINT')
    
    asymCINT<-tryCatch({ -solve(DRIFT) %*% CINT}, error=function(e) e )
    tryCatch({  rownames(asymCINT)<-latentNames}, error=function(e) e )
    outlist<-c(outlist,'asymCINT')
    
    
    
    
    DIFFUSION<-tryCatch({ mxobj$DIFFUSION$result}, error=function(e) e )
    if(asymptotes==TRUE) DIFFUSION <- tryCatch({ matrix(-DRIFTHATCH %*% OpenMx::rvectorize(DIFFUSION), nrow=n.latent, ncol=n.latent)}, error=function(e) e )
    tryCatch({  dimnames(DIFFUSION)<-list(latentNames,latentNames)}, error=function(e) e )
    outlist<-c(outlist,'DIFFUSION')
    
    discreteDIFFUSION<-tryCatch({ matrix(solve(DRIFTHATCH) %*% ((OpenMx::expm(DRIFTHATCH * timeInterval)) - 
        (diag(n.latent) %x% diag(n.latent)) ) %*% OpenMx::rvectorize(DIFFUSION), nrow=n.latent) }, error=function(e) e )
    tryCatch({  dimnames(discreteDIFFUSION)<-list(latentNames,latentNames)}, error=function(e) e )
    outlist<-c(outlist,'discreteDIFFUSION')
    
    asymDIFFUSION<-tryCatch({ matrix(-solve(DRIFTHATCH) %*% OpenMx::cvectorize(DIFFUSION),nrow=n.latent)}, error=function(e) e )
    tryCatch({  dimnames(asymDIFFUSION)<-list(latentNames,latentNames)}, error=function(e) e )
    outlist<-c(outlist,'asymDIFFUSION')
    
    asymDIFFUSIONdiag<-tryCatch({ diag(diag(asymDIFFUSION),n.latent)+diag(ridging,n.latent)}, error=function(e) e )
    asymDIFFUSIONstd<-tryCatch({ suppressWarnings(solve(sqrt(asymDIFFUSIONdiag)) %&% asymDIFFUSION)}, error=function(e) e )
    tryCatch({  dimnames(asymDIFFUSIONstd)<-list(latentNames,latentNames)}, error=function(e) e )
    outlist<-c(outlist,'asymDIFFUSIONstd')
    
    #std dev of affecting latent divided by std dev of affected latent
    standardiser<-tryCatch({ suppressWarnings(rep(sqrt(diag(asymDIFFUSION)),each=n.latent) / rep(diag(sqrt(asymDIFFUSION)),times=n.latent))}, error=function(e) e )
    discreteDRIFTstd<-tryCatch({ discreteDRIFT * standardiser    }, error=function(e) e )
    tryCatch({  dimnames(discreteDRIFTstd)<-list(latentNames,latentNames)}, error=function(e) e )
    #added to outlist above
    
    #             asymTOTALVAR<-tryCatch({ asymDIFFUSION }, error=function(e) e )#set totalvar to t0withinvar to begin
    #     tryCatch({  dimnames(asymTOTALVAR)<-list(latentNames,latentNames)}, error=function(e) e )
    #     outlist<-c(outlist,'asymTOTALVAR')
    #     
    #     
    #             asymTOTALVARdiag<-tryCatch({ diag(diag(asymTOTALVAR),n.latent)+diag(ridging,n.latent)}, error=function(e) e )
    #             asymTOTALVARstd<-tryCatch({ suppressWarnings(solve(sqrt(asymTOTALVARdiag)) %&% asymTOTALVAR)}, error=function(e) e )
    #     tryCatch({  dimnames(asymTOTALVARstd)<-list(latentNames,latentNames)}, error=function(e) e )
    #     outlist<-c(outlist,'asymTOTALVARstd')
    
    
    if('T0VAR' %in% stationary == FALSE){ #then include base T0 matrices
      T0VAR<-tryCatch({ OpenMx::mxEval(T0VAR, mxobj,compute=T)}, error=function(e) e )
      tryCatch({  dimnames(T0VAR)<-list(latentNames,latentNames)}, error=function(e) e )
      outlist<-c(outlist,'T0VAR')
      
      #       T0TOTALVAR<-tryCatch({ T0VAR }, error=function(e) e )#set t0totalvar to t0withinvar to begin
      #       tryCatch({  dimnames(T0TOTALVAR)<-list(latentNames,latentNames)}, error=function(e) e )
      #       outlist<-c(outlist,'T0TOTALVAR')
      
      T0VARdiag<-tryCatch({ diag(diag(T0VAR),n.latent)+diag(ridging,n.latent)}, error=function(e) e )
      T0VARstd<-tryCatch({ suppressWarnings(solve(sqrt(T0VARdiag)) %&% T0VAR)}, error=function(e) e )
      tryCatch({  dimnames(T0VARstd)<-list(latentNames,latentNames)}, error=function(e) e )
      outlist<-c(outlist,'T0VARstd')
      
    } # end T0VAR matrices
    
    if('T0MEANS' %in% stationary == FALSE){ #then include base T0 matrices
      T0MEANS<-tryCatch({ OpenMx::mxEval(T0MEANS, mxobj,compute=T)}, error=function(e) e )
      tryCatch({  rownames(T0MEANS)<-latentNames}, error=function(e) e )
      outlist<-c(outlist,'T0MEANS')
    }
    
    
    #trait matrices
    if(any(object$ctmodelobj$TRAITVAR != 0)) {
      
      TRAITVAR<-tryCatch({ OpenMx::mxEval(TRAITVAR, mxobj,compute=T)}, error=function(e) e )
      # if(asymptotes==TRUE) TRAITVAR<-tryCatch({ DRIFT %&% TRAITVAR }, error=function(e) e )
      tryCatch({  dimnames(TRAITVAR)<-list(latentNames,latentNames)}, error=function(e) e )
      outlist<-c(outlist,'TRAITVAR')
      
      #         asymTRAITVAR<-tryCatch({ solve(DRIFT) %*% TRAITVAR %*% t(solve(DRIFT))}, error=function(e) e )
      #       tryCatch({  dimnames(asymTRAITVAR)<-list(latentNames,latentNames)}, error=function(e) e )
      #       outlist<-c(outlist,'asymTRAITVAR')
      #       
      #       
      # #       discreteTRAITVAR<-tryCatch({ (diag(n.latent)-OpenMx::expm(DRIFT * timeInterval)) %*% asymTRAITVAR %*% t((diag(n.latent)-OpenMx::expm(DRIFT * timeInterval)))}, error=function(e) e )
      # #       tryCatch({  dimnames(discreteTRAITVAR)<-list(latentNames,latentNames)}, error=function(e) e )
      # #       outlist<-c(outlist,'discreteTRAITVAR')
      #       
      TRAITVARdiag<-tryCatch({ diag(diag(TRAITVAR))}, error=function(e) e )
      TRAITVARstd<-tryCatch({ suppressWarnings(solve(sqrt(TRAITVARdiag)) %&% TRAITVAR)}, error=function(e) e )
      tryCatch({  dimnames(TRAITVARstd)<-list(latentNames,latentNames)}, error=function(e) e )
      outlist<-c(outlist,'TRAITVARstd')
      
      #           asymTOTALVAR<-tryCatch({ asymDIFFUSION + asymTRAITVAR}, error=function(e) e )
      #       tryCatch({  dimnames(asymTOTALVAR)<-list(latentNames,latentNames)}, error=function(e) e )
      #       
      #           asymTOTALVARdiag<-tryCatch({ diag(diag(asymTOTALVAR),n.latent)+diag(ridging,n.latent)}, error=function(e) e )
      #           asymTOTALVARstd<-tryCatch({ suppressWarnings(solve(sqrt(asymTOTALVARdiag)) %&% asymTOTALVAR)}, error=function(e) e )
      #       tryCatch({  dimnames(asymTOTALVARstd)<-list(latentNames,latentNames)}, error=function(e) e )
      
      #       if('T0TRAITEFFECT' %in% stationary == FALSE) { #then include T0 trait matrices
      #         
      #          T0TRAITEFFECT<-tryCatch({mxEval(T0TRAITEFFECT, mxobj,compute=T)}, error=function(e) e )
      #         tryCatch({  dimnames(T0TRAITEFFECT)<-list(latentNames,latentNames)}, error=function(e) e )
      #         outlist<-c(outlist,'T0TRAITEFFECT')
      #         
      #          T0TRAITVAR<-tryCatch({ T0TRAITEFFECT %*% TRAITVAR %*% t(T0TRAITEFFECT) }, error=function(e) e )#is this valid?
      #         if(asymptotes
      #         tryCatch({  dimnames(T0TRAITVAR)<-list(latentNames,latentNames)}, error=function(e) e )
      #         outlist<-c(outlist,'T0TRAITVAR')
      #         
      #         
      #          T0TRAITVARdiag<-tryCatch({ suppressWarnings(sqrt(diag(diag(T0TRAITVAR),n.latent)))}, error=function(e) e )
      #          T0TRAITVARstd<-tryCatch({ solve(T0TRAITVARdiag) %*% T0TRAITVAR %*% solve(T0TRAITVARdiag)}, error=function(e) e )
      #         tryCatch({  dimnames(T0TRAITVARstd)<-list(latentNames,latentNames)}, error=function(e) e )
      #         outlist<-c(outlist,'T0TRAITVARstd')
      #         
      #          T0TOTALVAR<-tryCatch({ T0TRAITVAR+T0VAR}, error=function(e) e )
      #         tryCatch({  dimnames(T0TOTALVAR)<-list(latentNames,latentNames)}, error=function(e) e )
      #         
      #          T0TOTALVARdiag<-tryCatch({ diag(diag(T0TOTALVAR),n.latent)}, error=function(e) e )
      #          T0TOTALVARstd<-tryCatch({ suppressWarnings(solve(sqrt(T0TOTALVARdiag)) %*% T0TOTALVAR %*% solve(sqrt(T0TOTALVARdiag)))}, error=function(e) e )
      #         tryCatch({  dimnames(T0TOTALVARstd)<-list(latentNames,latentNames)}, error=function(e) e )
      #         
      #       } #end T0 trait matrices
    }#end trait matrices
    
    
    
    
    
    
    
    
    if(any(object$ctmodelobj$MANIFESTTRAITVAR != 0)) {
      
      MANIFESTTRAITVAR<-tryCatch({ mxobj$MANIFESTTRAITVAR$result}, error=function(e) e )
      tryCatch({  dimnames(MANIFESTTRAITVAR)<-list(manifestNames,manifestNames)}, error=function(e) e )
      outlist<-c(outlist,'MANIFESTTRAITVAR')
      
      MANIFESTTRAITVARdiag<-tryCatch({ diag(diag(MANIFESTTRAITVAR),n.manifest)+diag(ridging,n.manifest)}, error=function(e) e )
      MANIFESTTRAITVARstd<-tryCatch({ suppressWarnings(solve(sqrt(MANIFESTTRAITVARdiag)) %&% MANIFESTTRAITVAR)}, error=function(e) e )
      tryCatch({  dimnames(MANIFESTTRAITVARstd)<-list(manifestNames,manifestNames)}, error=function(e) e )
      outlist<-c(outlist,'MANIFESTTRAITVARstd')
      
    }#end manifesttrait
    
    
    
    
    
    #TIpred matrices
    if(n.TIpred > 0) {
      TIpredNames<-object$ctmodelobj$TIpredNames      
      if(asymptotes==FALSE) TIPREDEFFECT<-tryCatch({mxobj$TIPREDEFFECT$values}, error=function(e) e )
      if(asymptotes==TRUE)   TIPREDEFFECT<-tryCatch({-DRIFT %*% mxobj$TIPREDEFFECT$values}, error=function(e) e )
      tryCatch({  dimnames(TIPREDEFFECT)<-list(latentNames,TIpredNames)}, error=function(e) e )
      outlist<-c(outlist,'TIPREDEFFECT')
      
      #       if (randomPredictors==TRUE) TIPREDVAR<-tryCatch({mxobj$S$values[TIpredNames,TIpredNames] }, error=function(e) e )
      #       if(randomPredictors==FALSE) 
      TIPREDVAR<-tryCatch({OpenMx::mxEval(TIPREDVAR,mxobj,compute=T) }, error=function(e) e )
      outlist<-c(outlist,'TIPREDVAR')
      
      
      TIPREDVARdiag<-tryCatch({diag(diag(TIPREDVAR),n.TIpred)+diag(ridging,n.TIpred)}, error=function(e) e )
      TIPREDVARstd<-tryCatch({suppressWarnings(solve(sqrt(TIPREDVARdiag)) %&% TIPREDVAR)}, error=function(e) e )
      tryCatch({  dimnames(TIPREDVARstd)<-list(TIpredNames,TIpredNames)}, error=function(e) e )
      outlist<-c(outlist,'TIPREDVARstd')
      
      discreteTIPREDEFFECT<-tryCatch({solve(DRIFT) %*%(discreteDRIFT - diag(n.latent)) %*% TIPREDEFFECT }, error=function(e) e )#disrete from continuous
      tryCatch({  dimnames(discreteTIPREDEFFECT)<-list(latentNames,TIpredNames) }, error=function(e) e )
      outlist<-c(outlist,'discreteTIPREDEFFECT')
      
      asymTIPREDEFFECT<-tryCatch({solve(diag(n.latent)-discreteDRIFT)  %*%  (discreteTIPREDEFFECT)}, error=function(e) e )
      tryCatch({  dimnames(asymTIPREDEFFECT)<-list(latentNames,TIpredNames)}, error=function(e) e )
      outlist<-c(outlist,'asymTIPREDEFFECT')
      
      #sqrt of affecting latent variance divided by sqrt of affected
      standardiser<-tryCatch({suppressWarnings(rep(sqrt(diag(TIPREDVAR)),each=n.latent) / rep(diag(sqrt(asymDIFFUSION)),times=n.TIpred))}, error=function(e) e )
      asymTIPREDEFFECTstd<-tryCatch({asymTIPREDEFFECT * standardiser}, error=function(e) e )
      tryCatch({  dimnames(asymTIPREDEFFECTstd)<-list(latentNames,TIpredNames)}, error=function(e) e )
      outlist<-c(outlist,'asymTIPREDEFFECTstd')
      
      addedTIPREDVAR <-tryCatch({asymTIPREDEFFECT %*% TIPREDVAR %*% t(asymTIPREDEFFECT) }, error=function(e) e )#valid?
      tryCatch({  dimnames(addedTIPREDVAR)<-list(latentNames,latentNames)}, error=function(e) e )
      outlist<-c(outlist,'addedTIPREDVAR')
      
      addedTIPREDVARdiag<-tryCatch({diag(diag(addedTIPREDVAR),n.latent)+diag(ridging,n.latent)}, error=function(e) e )
      addedTIPREDVARstd<-tryCatch({suppressWarnings(solve(sqrt(addedTIPREDVARdiag)) %&% addedTIPREDVAR)}, error=function(e) e )
      tryCatch({  dimnames(addedTIPREDVARstd)<-list(latentNames,latentNames)}, error=function(e) e )
      outlist<-c(outlist,'addedTIPREDVARstd')
      
      #            asymTOTALVAR<-tryCatch({asymTOTALVAR + addedTIPREDVAR}, error=function(e) e )
      #             tryCatch({  dimnames(asymTOTALVAR)<-list(latentNames,latentNames)}, error=function(e) e )
      #       
      #               asymTOTALVARdiag<-tryCatch({diag(diag(asymTOTALVAR),n.latent)+diag(ridging,n.latent)}, error=function(e) e )
      #             asymTOTALVARstd<-tryCatch({suppressWarnings(solve(sqrt(asymTOTALVARdiag)) %&% asymTOTALVAR)}, error=function(e) e )
      #               tryCatch({  dimnames(asymTOTALVARstd)<-list(latentNames,latentNames)}, error=function(e) e )
      
      if('TOTIPRED' %in% stationary == FALSE) { #include TIPRED T0 matrices
        T0TIPREDEFFECT<-tryCatch({OpenMx::mxEval(T0TIPREDEFFECT, mxobj,compute=T)}, error=function(e) e )
        tryCatch({  dimnames(T0TIPREDEFFECT)<-list(latentNames,TIpredNames)}, error=function(e) e )
        outlist<-c(outlist,'T0TIPREDEFFECT')
        
        #sqrt of affecting latent variance divided by sqrt of affected
        standardiser<-tryCatch({suppressWarnings(rep(sqrt(diag(TIPREDVAR)),each=n.latent) / rep(diag(sqrt(T0VAR)),times=n.TIpred))}, error=function(e) e )
        T0TIPREDEFFECTstd<-tryCatch({T0TIPREDEFFECT * standardiser}, error=function(e) e )
        tryCatch({  dimnames(T0TIPREDEFFECTstd)<-list(latentNames,TIpredNames)}, error=function(e) e )
        outlist<-c(outlist,'T0TIPREDEFFECTstd')
        
        
        addedT0TIPREDVAR<-tryCatch({T0TIPREDEFFECT %*% TIPREDVAR %*% t(T0TIPREDEFFECT)}, error=function(e) e )#is this valid?
        tryCatch({  dimnames(addedT0TIPREDVAR)<-list(latentNames,latentNames)}, error=function(e) e )
        outlist<-c(outlist,'addedT0TIPREDVAR')
        #       
        #       T0TOTALVAR<-tryCatch({addedT0TIPREDVAR+T0TOTALVAR }, error=function(e) e )#update totalvar
        #       tryCatch({  dimnames(T0TOTALVAR)<-list(latentNames,latentNames)}, error=function(e) e )
        #       
        #       T0TOTALVARdiag<-tryCatch({diag(diag(T0TOTALVAR),n.latent)+diag(ridging,n.latent)}, error=function(e) e )
        #       T0TOTALVARstd<-tryCatch({suppressWarnings(solve(sqrt(T0TOTALVARdiag)) %&% T0TOTALVAR)}, error=function(e) e )
        #       tryCatch({  dimnames(T0TOTALVARstd)<-list(latentNames,latentNames)}, error=function(e) e )
        
      } #end T0 TIPRED matrices
      
    } # end TIPRED matrices
    
    
    
    if(n.TDpred > 0) {
      
      TDpredNames<-object$ctmodelobj$TDpredNames      
      
      TDPREDEFFECT<-tryCatch({mxobj$TDPREDEFFECT$values}, error=function(e) e )
      tryCatch({  dimnames(TDPREDEFFECT)<-list(latentNames,TDpredNames)}, error=function(e) e )
      outlist<-c(outlist,'TDPREDEFFECT')
      
      discreteTDPREDEFFECT<- OpenMx::expm(DRIFT * timeInterval) %*% TDPREDEFFECT
      tryCatch({  dimnames(discreteTDPREDEFFECT)<-list(latentNames,TDpredNames)}, error=function(e) e )
      outlist<-c(outlist,'discreteTDPREDEFFECT')
      
      
      TDPREDVAR<-tryCatch({OpenMx::mxEval(TDPREDVAR,mxobj,compute=T) }, error=function(e) e )
      tryCatch({  dimnames(TDPREDVAR)<-list(TDpredNames,TDpredNames)}, error=function(e) e )
      outlist<-c(outlist,'TDPREDVAR')
      
      TDPREDVARdiag<-tryCatch({diag((diag(TDPREDVAR))+diag(ridging,n.TDpred))}, error=function(e) e )
      TDPREDVARstd<-tryCatch({suppressWarnings(solve(sqrt(TDPREDVARdiag)) %&% TDPREDVAR)}, error=function(e) e )
      tryCatch({  dimnames(TDPREDVARstd)<-list(TDpredNames,TDpredNames)}, error=function(e) e )
      outlist<-c(outlist,'TDPREDVARstd')
      
    } # end TIpred section
    
    ctsummary<-mget(outlist)
    output<-ctsummary
  }  #end summary from continuous params
  
  ##Openmx params
  
  omxsummary<-c(omxsummary['modelName'],omxsummary['wallTime'],omxsummary['mxVersion'],omxsummary['timestamp'],
    omxsummary['parameters'], omxsummary['estimatedParameters'], omxsummary['CI'],
    omxsummary['AIC.Mx'],omxsummary['BIC.Mx'],omxsummary['observedStatistics'],
    omxsummary['npsolMessage'],
    omxsummary['degreesOfFreedom'],omxsummary['Minus2LogLikelihood'] )
  
  
  output$omxsummary<-omxsummary
  
  return(output)
}
