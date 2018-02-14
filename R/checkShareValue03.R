##' Check if values of shares are consistent and substitute inconsistent values
##' 
##' This function checks between a subset of the Tree having official shares 
##' but with the sum not equal to 1. 
##' It checks where is the problem and correct it
##' The structure of this function is articulated on purpose
##' for sake of clarity
##' 
##' @param One single subset of the Tree, which means, a subset having all the parent of a single child
##' where the child is an official one. The subset has the characteristis of having shares not summing at 1
##' 
##' @return The function returns the subTree with the corrected shares and
##' with a severity index to be saved back in order to have a different color also where there 
##' are shares that have been changed
##' 
##' @export

checkShareValue=function(tree2Subset){
  
  # Check and count cases 
  # Number of rows 
  
  linesT=nrow(tree2Subset)
  
  ########################
  #### Number of PROTECTED
  Pt=tree2Subset[checkFlags=="(E,f)"]
  nPt=nrow(tree2Subset[checkFlags=="(E,f)"])
  ##############
  # Number of PROTECTED + MISSING
  PtM=tree2Subset[checkFlags=="(E,f)"&is.na(share)]
  nPtM=nrow(tree2Subset[checkFlags=="(E,f)"&is.na(share)])
  ##############
  # Number of PROTECTED + NoMISSING
  PtnoM=tree2Subset[checkFlags=="(E,f)"&!(is.na(share))]
  nPtnoM=nrow(tree2Subset[checkFlags=="(E,f)"&!(is.na(share))])
  ##############
  # Number of PROTECTED + NegAvailabilities
  PtnegAv=tree2Subset[checkFlags=="(E,f)"&(availability.child<=0|is.na(availability.child))]
  nPtnegAv=nrow(tree2Subset[checkFlags=="(E,f)"&(availability.child<=0|is.na(availability.child))])
  ##############
  # Number of PROTECTED + PosAvailabilities
  PtposAv=tree2Subset[checkFlags=="(E,f)"&availability.child>0]
  nPtposAv=nrow(tree2Subset[checkFlags=="(E,f)"&availability.child>0])
  
  ########################
  # Number of PROTECTED + MISSING + NegAvailability
  PtMnegAv=tree2Subset[checkFlags=="(E,f)"&is.na(share)&(availability.child<=0|is.na(availability.child))]
  nPtMnegAv=nrow(tree2Subset[checkFlags=="(E,f)"&is.na(share)&(availability.child<=0|is.na(availability.child))])
  
  # Number of PROTECTED + MISSING + PosAvailability
  PtMposAv=tree2Subset[checkFlags=="(E,f)"&is.na(share)&availability.child>0]
  nPtMposAv=nrow(tree2Subset[checkFlags=="(E,f)"&is.na(share)&availability.child>0])
  
  
  # Number of PROTECTED + noMISSING + NegAvailability
  PtnoMnegAv=tree2Subset[checkFlags=="(E,f)"&!(is.na(share))&(availability.child<=0|is.na(availability.child))]
  nPtnoMnegAv=nrow(tree2Subset[checkFlags=="(E,f)"&!(is.na(share))&(availability.child<=0|is.na(availability.child))])
  
  # Number of PROTECTED + noMISSING + PosAvailability
  PtnoMposAv=tree2Subset[checkFlags=="(E,f)"&!(is.na(share))&availability.child>0]
  nPtnoMposAv=nrow(tree2Subset[checkFlags=="(E,f)"&!(is.na(share))&availability.child>0])
  ########################
  
  
  
  ########################
  #### Number of NOT Protected
  numNoProt=nrow(tree2Subset[checkFlags!="(E,f)"])
  
  # Number of No PROTECTED + NegAvailability (doesn't matter if NA)
  NoPtnegAv=tree2Subset[checkFlags!="(E,f)"&(availability.child<=0|is.na(availability.child))]
  nNoPtnegAv=nrow(tree2Subset[checkFlags!="(E,f)"&(availability.child<=0|is.na(availability.child))])
  # Number of No PROTECTED + PosAvailability (doesn't matter if NA)
  NoPtposAv=tree2Subset[checkFlags!="(E,f)"&availability.child>0]
  nNoPtposAv=nrow(tree2Subset[checkFlags!="(E,f)"&availability.child>0])
  ########################
  
  ############################################################
  #################   defining alternatives  #################
  ############################################################
  
  if(linesT==1){
    ##################### 1. #####################
    # 1. linesT=1 (Extraction Rate is never NA)
    if(nPtM==1){
      ###################### 1.A share = NA
      if(nPtMnegAv==1){
        # 1.A.a Yes Negative Availability  
        case="1.A.a"
      }else{
        # 1.A.b No Negative Availability 
        case="1.A.b"
      }
    }else{# end of 1.A
      ###################### 1.B share != NA
      if(nPtnoMnegAv>0){
        # 1.B.a Yes Negative Availability  
        case="1.B.a"
      }else{
        # 1.B.b No Negative Availability  
        case="1.B.b"
      }
    }
  }else{# END of 1
    ##################### 2. #####################
    # linesT>1 (Extraction Rate is never NA)
    if(linesT==nPt){
      # 2.A. All protected line
      if(nPtnoM==nPt){
        ##################### 2.A.a share != NA
        if(nPtnoM==nPtnoMposAv){
          # 2.A.a.1 No Neg Availability
          case="2.A.a.1"
        }else{
          # 2.A.a.2 Yes Negative Availability 
          case="2.A.a.2"
        }
      }else{
        # 2.A.b some share == NA
        if(nPtnegAv==0){
          # 2.A.b.1 No Neg Availability 
          case="2.A.b.1"
        }else{
          # 2.A.b.2 Yes Negative Availability
          case="2.A.b.2"
        }
      }
    }else{ # END of 2.A
      ###################### 2.B. NOT All protected line
      ##########################################################
      if(nNoPtnegAv==0){
        ########## 2.B.a. No neg Av in No Protected
        if(nPtnegAv==0){
          # 2.B.a.1. No neg Av in Protected
          if(nPtM==0){
            # 2.B.a.1.i. No Missing 
            case="2.B.a.1.i"
          }else{
            # 2.B.a.1.ii.Yes Missing
            case="2.B.a.1.ii"
          }
        }else{
          # 2.B.a.2. Yes neg Av in Protected
          if(nPtM==0){
            # 2.B.a.2.i. No Missing
            case="2.B.a.2.i"
          }else{
            # 2.B.a.2.ii.Yes Missing
            case="2.B.a.2.ii"
          }          
        }
        
      }else{
        ########## 2.B.b. Yes neg Av in No Protected
        if(nPtnegAv==0){
          # 2.B.b.1. No neg Av in Protected
          if(nPtMposAv==0){
            # 2.B.b.1.i. No Missing
            case="2.B.b.1.i"
            
          }else{
            # 2.B.b.1.ii. Yes Missing
            case="2.B.b.1.ii"
          }
        }else{
          # 2.B.b.2. Yes neg Av in Protected
          if(nPtM==0){
            # 2.B.b.2.i.No Missing
            case="2.B.b.2.i"
          }else{
            # 2.B.b.2.ii.YES Missing
            case="2.B.b.2.ii"
          }          
        }
      }
    }
  }
  
  ############################################################
  ########   Define function for Alternative cases    ########
  ############################################################ 
  
  ############ fun1
  # 1. linesT=1  
  # (protected line)
  # A. Yes Missing
  # a. Yes Negative Availability  
  fun1=function(tree2Subset,case="1.A.a"){
    tree2Subset[,newShare:=1]
    
    tree2Subset[,severity:=3]
    tree2Subset[,message:="NA Shares, Neg Avail, share changet to 1"]
    return(tree2Subset)
  }
  
  ############ fun2
  # 1. linesT=1  
  # (protected line)
  # A. Yes Missing
  # b. No Negative Availability  
  fun2=function(tree2Subset,case="1.A.b"){
    tree2Subset[,newShare:=1]
    
    tree2Subset[,severity:=3]
    tree2Subset[,message:="NA Shares, share changet to 1"]
    return(tree2Subset)
  }
  
  ############ fun3
  # 1. linesT=1  
  # (protected line)
  # B. share != NA
  # a. Yes Negative Availability  
  fun3=function(tree2Subset,case="1.B.a"){ 
    tree2Subset[,newShare:=1]
    
    tree2Subset[,severity:=3]
    tree2Subset[,message:="Share !=1, Neg Avail, share changet to 1"]
    return(tree2Subset)
    }
  
  ############ fun4
  # 1. linesT=1  
  # (protected line)
  # B. No Missing
  # b. No Negative Availability  
  fun4=function(tree2Subset,case="1.B.b"){
    tree2Subset[,newShare:=1]
    
    tree2Subset[,severity:=3]
    tree2Subset[,message:="Share !=1,share changet to 1"]
    return(tree2Subset)
    }
  
  ############ fun5
  # 2. linesT>1
  # A. All protected line
  # a. No Missing
  # 1. No Neg Availability     
  fun5=function(tree2Subset,case="2.A.a.1"){
    tree2Subset[,newShare:=round(availability.child/sum(availability.child,na.rm = TRUE),4)]
    
    tree2Subset[,severity:=2]
    tree2Subset[,message:="all prot, sum!=0, all recalculated"]
    return(tree2Subset)
    }
  
  ############ fun6
  # 2. linesT>1
  # A. All protected line
  # a. No Missing
  # 2. Yes Negative Availability    
  
  # In this case recalculate shares and give alert for negativa availablity
  # but do nothing about it, as the shares are protected
  fun6=function(tree2Subset,case="2.A.a.2"){
    tree2Subset[,newShare:=round(availability.child/sum(availability.child,na.rm = TRUE),4)]
    
    freqChild= data.table(table(tree2Subset[, get("measuredItemChildCPC")]))
    setnames(freqChild, c("V1","N"), c("measuredItemChildCPC", "freq"))
    tree2Subset=merge(tree2Subset, freqChild , by="measuredItemChildCPC")
    
    tree2Subset[, negShare:=1/freq]
    
    tree2Subset[,sumPositiveAvail:=sum(availability.child*ifelse(availability.child>0,1,0),na.rm=TRUE),by = c("measuredItemChildCPC")]
    
    tree2Subset[,tempAvailability:=ifelse(availability.child<=0|is.na(availability.child),negShare*sumPositiveAvail,availability)]
    
    tree2Subset[,newShare := ifelse(tempAvailability==0|is.na(tempAvailability),negShare,
                                   (tempAvailability / sum(tempAvailability, na.rm = TRUE)))]
    
    tree2Subset[,c("freq","tempAvailability","sumPositiveAvail","negShare"):=NULL]
    
    
    tree2Subset[(availability.child<=0|is.na(availability.child)),severity:=3]
    tree2Subset[(availability.child<=0|is.na(availability.child)),message:="all prot, neg avail, sum!=0, all recalculated"]
    
    tree2Subset[availability.child>0,severity:=2]
    tree2Subset[availability.child>0,message:="all prot, sum!=0, all recalculated"]
    return(tree2Subset)
    }
  
  ############ fun7
  # 2. linesT>1
  # A. All protected line
  # b. Yes Missing
  # 1. No Neg Availability  
  
  # In this case recalculate all shares and give alert for missing shares
  fun7=function(tree2Subset,case="2.A.b.1"){ 
    tree2Subset[,newShare:=round(availability.child/sum(availability.child,na.rm = TRUE),4)]
    
    tree2Subset[is.na(share),severity:=3]
    tree2Subset[is.na(share),message:="all protected, missing share, sum!=0, recalculated"]
    
    tree2Subset[!is.na(share),severity:=2]
    tree2Subset[!is.na(share),message:="all protected, sum!=0, missing shares, recalculated"]
    return(tree2Subset) 
  }
  
  ############ fun8
  # 2. linesT>1
  # A. All protected line
  # b. Yes Missing
  # 2. Yes Negative Availability  
  
  # This is as the previous, only messages changes
  fun8=function(tree2Subset,case="2.A.b.2"){ 
    tree2Subset[,newShare:=round(availability.child/sum(availability.child,na.rm = TRUE),4)]

      # if negative availability is on NA share
      freqChild= data.table(table(tree2Subset[, get("measuredItemChildCPC")]))
      setnames(freqChild, c("V1","N"), c("measuredItemChildCPC", "freq"))
      tree2Subset=merge(tree2Subset, freqChild , by="measuredItemChildCPC")
      
      tree2Subset[, negShare:=1/freq]
      
      tree2Subset[, sumPositiveAvail:=sum(availability.child*ifelse(availability.child>0,1,0),na.rm=TRUE),by = c("measuredItemChildCPC")]
      
      tree2Subset[,tempAvailability:=ifelse(availability.child<=0|is.na(availability.child),negShare*sumPositiveAvail,availability)]
      
      tree2Subset[,newShare := ifelse(tempAvailability==0|is.na(tempAvailability),negShare,
                                     (tempAvailability / sum(tempAvailability, na.rm = TRUE)))]
      
      tree2Subset[,c("freq","tempAvailability","sumPositiveAvail","negShare"):=NULL]
      
      tree2Subset[is.na(share)&availability.child>0,severity:=2]
      tree2Subset[is.na(share)&(availability.child<=0|is.na(availability.child)),severity:=3]
      
      tree2Subset[is.na(share)&availability.child>0,message:="all prot, missing share, sum!=0, recalculated"]
      tree2Subset[is.na(share)&(availability.child<=0|is.na(availability.child)),message:="all prot, missing share, neg Avail, sum!=0, recalculated"]
      
      tree2Subset[!(is.na(share))&availability.child>0,severity:=2]
      tree2Subset[!(is.na(share))&(availability.child<=0|is.na(availability.child)),severity:=2]
      
      
      tree2Subset[!(is.na(share))&availability.child>0,message:="all prot, sum!=0, recalculated"]
      tree2Subset[!(is.na(share))&(availability.child<=0|is.na(availability.child)),message:="all prot, sum!=0, neg Avail, recalculated"]
    return(tree2Subset)
  }
  
  ############ fun9
  # 2. linesT>1
  # B. NOT All protected line
  # a. No neg Av in No Protected
  # 1. No neg Av in Protected
  # i. No Missing
  fun9=function(tree2Subset,case="2.B.a.1.i"){
    shareProt=tree2Subset[checkFlags=="(E,f)",sum(share)]
    # there is a difference if the sum of protected is >1
    if(shareProt>1){
      # in this case recalculate all
      tree2Subset[,newShare:=round(availability.child/sum(availability.child,na.rm = TRUE),4)]
      
      tree2Subset[checkFlags=="(E,f)",severity:=3]
      tree2Subset[checkFlags=="(E,f)",message:="sum of prot shares>1, recalculated"]
      
      tree2Subset[checkFlags!="(E,f)",severity:=1]
      tree2Subset[checkFlags!="(E,f)",message:="also prot shares recalculated"]
    }else{
      tree2Subset[checkFlags!="(E,f)",newShare:=round(availability/sum(availability,na.rm = TRUE)-shareProt*(availability/sum(availability,na.rm = TRUE)),4)]
      
      tree2Subset[checkFlags!="(E,f)",severity:=as.integer(1)]
      tree2Subset[checkFlags!="(E,f)",message:="only not prot shares recalculated"]
      
      tree2Subset[checkFlags=="(E,f)",severity:=as.integer(0)]
      tree2Subset[checkFlags=="(E,f)",message:="prot share"]
    }
    return(tree2Subset)
  }
  
  ############ fun10
  # 2. linesT>1
  # B. NOT All protected line
  # a. No neg Av in No Protected
  # 1. No neg Av in Protected
  # ii. Yes Missing 
  fun10=function(tree2Subset,case="2.B.a.1.ii"){ 
    
    shareProt=tree2Subset[checkFlags=="(E,f)",sum(share,na.rm=T)]
    if(shareProt>1){
      tree2Subset[,newShare:=round(availability.child/sum(availability.child,na.rm = TRUE),4)]
      
      tree2Subset[checkFlags=="(E,f)",severity:=3]
      tree2Subset[checkFlags=="(E,f)",message:="sum of prot shares>1, recalculated"]
      
      tree2Subset[checkFlags!="(E,f)",severity:=1]
      tree2Subset[checkFlags!="(E,f)",message:="also prot shares recalculated"]
    }else{
      tree2Subset[!(checkFlags=="(E,f)")|(is.na(share)),
                  newShare:=round(availability/sum(availability,na.rm = TRUE)-shareProt*(availability/sum(availability,na.rm = TRUE)),4)]
      
      tree2Subset[checkFlags=="(E,f)"&!(is.na(share)),severity:=0]
      tree2Subset[checkFlags=="(E,f)"&!(is.na(share)),message:="valid prot share"]
      
      tree2Subset[checkFlags=="(E,f)"&is.na(share),severity:=3]
      tree2Subset[checkFlags=="(E,f)"&is.na(share),message:="Missing prot share recalculated"]
      
      tree2Subset[checkFlags!="(E,f)",severity:=1]
      tree2Subset[checkFlags!="(E,f)",message:="also prot shares recalculated"]      
    }
    return(tree2Subset)
  }
  
  ############ fun11
  # 2. linesT>1
  # B. NOT All protected line
  # a. No neg Av in No Protected
  # 2. Yes neg Av in Protected
  # i. No Missing
  fun11=function(tree2Subset,case="2.B.a.2.i"){ 
    shareProt=tree2Subset[checkFlags=="(E,f)",sum(share)]
    # there is a difference if the sum of protected is >1
    if(shareProt>1){
      # in this case recalculate all
      tree2Subset[,newShare:=round(availability.child/sum(availability.child,na.rm = TRUE),4)]
      
      freqChild= data.table(table(tree2Subset[, get("measuredItemChildCPC")]))
      setnames(freqChild, c("V1","N"), c("measuredItemChildCPC", "freq"))
      tree2Subset=merge(tree2Subset, freqChild , by="measuredItemChildCPC")
      
      tree2Subset[, negShare:=1/freq]
      
      tree2Subset[, sumPositiveAvail:=sum(availability.child*ifelse(availability.child>0,1,0),na.rm=TRUE),by = c("measuredItemChildCPC")]
      
      tree2Subset[,tempAvailability:=ifelse(availability.child<=0|is.na(availability.child),negShare*sumPositiveAvail,availability)]
      
      tree2Subset[, newShare := ifelse(tempAvailability==0|is.na(tempAvailability),negShare,
                                     (tempAvailability / sum(tempAvailability, na.rm = TRUE)))]
      
      tree2Subset[,c("freq","tempAvailability","sumPositiveAvail","negShare"):=NULL]
      
      
      tree2Subset[checkFlags=="(E,f)",severity:=3]
      tree2Subset[checkFlags=="(E,f)",message:="sum of prot shares>1, recalculated"]
      
      tree2Subset[checkFlags=="(E,f)"&(availability.child<=0|is.na(availability.child)),severity:=3]
      tree2Subset[checkFlags=="(E,f)"&(availability.child<=0|is.na(availability.child)),message:="sum of prot shares>1 & neg avail, recalculated"]
      
      tree2Subset[checkFlags=="(E,f)"&availability.child>0,severity:=3]
      tree2Subset[checkFlags=="(E,f)"&availability.child>0,message:="sum of prot shares>1, recalculated"]
      
      tree2Subset[checkFlags!="(E,f)",severity:=1]
      tree2Subset[checkFlags!="(E,f)",message:="also prot shares recalculated"]
    }else{
      tree2Subset[checkFlags!="(E,f)",newShare:=round(availability/sum(availability,na.rm = TRUE)-shareProt*(availability/sum(availability,na.rm = TRUE)),4)]
      
      tree2Subset[checkFlags!="(E,f)",severity:=as.integer(1)]
      tree2Subset[checkFlags!="(E,f)",message:="only not prot shares recalculated"]
      
      tree2Subset[checkFlags=="(E,f)"&(availability.child<=0|is.na(availability.child)),severity:=as.integer(1)]
      tree2Subset[checkFlags=="(E,f)"&(availability.child<=0|is.na(availability.child)),message:="prot share, neg avail"]
      
      tree2Subset[checkFlags=="(E,f)"&availability.child>0,severity:=as.integer(0)]
      tree2Subset[checkFlags=="(E,f)"&availability.child>0,message:="prot share"]
    }
    return(tree2Subset)
  }
  
  ############ fun12
  # 2. linesT>1
  # B. NOT All protected line
  # a. No neg Av in No Protected
  # 2. Yes neg Av in Protected
  # ii. Yes Missing  
  fun12=function(tree2Subset,case="2.B.a.2.ii"){ 
    shareProt=tree2Subset[checkFlags=="(E,f)",sum(share,na.rm=T)]
    if(shareProt>1){
      tree2Subset[,newShare:=round(availability.child/sum(availability.child,na.rm = TRUE),4)]
      
      freqChild= data.table(table(tree2Subset[, get("measuredItemChildCPC")]))
      setnames(freqChild, c("V1","N"), c("measuredItemChildCPC", "freq"))
      tree2Subset=merge(tree2Subset, freqChild , by="measuredItemChildCPC")
      
      tree2Subset[, negShare:=1/freq]
      
      tree2Subset[,sumPositiveAvail:=sum(availability.child*ifelse(availability.child>0,1,0),na.rm=TRUE),by = c("measuredItemChildCPC")]

      tree2Subset[,tempAvailability:=ifelse(availability.child<=0|is.na(availability.child),negShare*sumPositiveAvail,availability)]
      
      tree2Subset[,newShare := ifelse(tempAvailability==0|is.na(tempAvailability),negShare,
                                     (tempAvailability / sum(tempAvailability, na.rm = TRUE)))]
      
      tree2Subset[,c("freq","tempAvailability","sumPositiveAvail","negShare"):=NULL]
      
      tree2Subset[checkFlags=="(E,f)"&(availability.child<=0|is.na(availability.child))&is.na(share),severity:=3]
      tree2Subset[checkFlags=="(E,f)"&(availability.child<=0|is.na(availability.child))&is.na(share),message:="sum of prot shares>1, neg avail, missing Share, recalculated"]
      
      tree2Subset[checkFlags=="(E,f)"&(availability.child<=0|is.na(availability.child))&!(is.na(share)),severity:=2]
      tree2Subset[checkFlags=="(E,f)"&(availability.child<=0|is.na(availability.child))&!(is.na(share)),message:="sum of prot shares>1, neg avail, recalculated"]
      
      tree2Subset[checkFlags=="(E,f)"&availability.child>0&is.na(share),severity:=3]
      tree2Subset[checkFlags=="(E,f)"&availability.child>0&is.na(share),message:="sum of prot shares>1, missing share, recalculated"]
      
      tree2Subset[checkFlags=="(E,f)"&availability.child>0&!(is.na(share)),severity:=2]
      tree2Subset[checkFlags=="(E,f)"&availability.child>0&!(is.na(share)),message:="sum of prot shares>1, recalculated"]
      
      tree2Subset[checkFlags!="(E,f)",severity:=1]
      
      tree2Subset[checkFlags!="(E,f)",message:="also prot shares recalculated"]
    }else{
      tree2Subset[!(checkFlags=="(E,f)")|(is.na(share)),
                  newShare:=round(availability.child/sum(availability.child,na.rm = TRUE)-shareProt*(availability/sum(availability,na.rm = TRUE)),4)]
      
      freqChild= data.table(table(tree2Subset[, get("measuredItemChildCPC")]))
      setnames(freqChild, c("V1","N"), c("measuredItemChildCPC", "freq"))
      tree2Subset=merge(tree2Subset, freqChild , by="measuredItemChildCPC")
      
      tree2Subset[(checkFlags=="(E,f)")|(is.na(share)), negShare:=1/freq]
      
      tree2Subset[(checkFlags=="(E,f)")|(is.na(share)),
                  sumPositiveAvail:=sum(availability.child*ifelse(availability.child>0,1,0),na.rm=TRUE),by = c("measuredItemChildCPC")]
      
      tree2Subset[(checkFlags=="(E,f)")|(is.na(share)),
                  tempAvailability:=ifelse(availability.child<=0|is.na(availability.child),negShare*sumPositiveAvail,availability)]
      
      tree2Subset[(checkFlags=="(E,f)")|(is.na(share)), 
                  newShare := ifelse(tempAvailability==0|is.na(tempAvailability),negShare-shareProt*(negShare),
                                     (tempAvailability / sum(tempAvailability, na.rm = TRUE)))-shareProt*(tempAvailability / sum(tempAvailability, na.rm = TRUE))]
      
      tree2Subset[,c("freq","tempAvailability","sumPositiveAvail","negShare"):=NULL]
      
      tree2Subset[checkFlags=="(E,f)"&!(is.na(share))&(availability.child<=0|is.na(availability.child)),severity:=1]
      tree2Subset[checkFlags=="(E,f)"&!(is.na(share))&(availability.child<=0|is.na(availability.child)),message:="valid prot share, neg avail"]
      
      tree2Subset[checkFlags=="(E,f)"&!(is.na(share))&availability.child>0,severity:=0]
      tree2Subset[checkFlags=="(E,f)"&!(is.na(share))&availability.child>0,message:="valid prot share"]
      
      tree2Subset[checkFlags=="(E,f)"&is.na(share)&(availability.child<=0|is.na(availability.child)),severity:=3]
      tree2Subset[checkFlags=="(E,f)"&is.na(share)&(availability.child<=0|is.na(availability.child)),message:="Missing prot share, neg avail, recalculated"]
      
      tree2Subset[checkFlags=="(E,f)"&is.na(share)&availability.child>0,severity:=3]
      tree2Subset[checkFlags=="(E,f)"&is.na(share)&availability.child>0,message:="Missing prot share recalculated"]
      
      tree2Subset[checkFlags!="(E,f)",severity:=1]
      tree2Subset[checkFlags!="(E,f)",message:="also prot shares recalculated"]      
    }  
    return(tree2Subset)
  }
  
  ############ fun13
  # 2. linesT>1
  # B. NOT All protected line
  # b. Yes neg Av in No Protected
  # 1. No neg Av in Protected
  # i. No Missing
  fun13=function(tree2Subset,case="2.B.b.1.i"){ 
    shareProt=tree2Subset[checkFlags=="(E,f)",sum(share)]
    # there is a difference if the sum of protected is >1
    if(shareProt>1){
      tree2Subset[,newShare:=round(availability.child/sum(availability.child,na.rm = TRUE),4)]
      
      freqChild= data.table(table(tree2Subset[, get("measuredItemChildCPC")]))
      setnames(freqChild, c("V1","N"), c("measuredItemChildCPC", "freq"))
      tree2Subset=merge(tree2Subset, freqChild , by="measuredItemChildCPC")
      
      tree2Subset[, negShare:=1/freq]
      
      tree2Subset[,sumPositiveAvail:=sum(availability.child*ifelse(availability.child>0,1,0),na.rm=TRUE),by = c("measuredItemChildCPC")]
      
      tree2Subset[,tempAvailability:=ifelse(availability.child<=0|is.na(availability.child),negShare*sumPositiveAvail,availability)]
      
      tree2Subset[,newShare := ifelse(tempAvailability==0|is.na(tempAvailability),negShare,
                                     (tempAvailability / sum(tempAvailability, na.rm = TRUE)))]
      
      tree2Subset[,c("freq","tempAvailability","sumPositiveAvail","negShare"):=NULL]
      
      tree2Subset[checkFlags=="(E,f)",severity:=2]
      tree2Subset[checkFlags=="(E,f)",message:="sum of prot shares>1, recalculated"]
      
      tree2Subset[checkFlags!="(E,f)",severity:=1]
      tree2Subset[checkFlags!="(E,f)",message:="also prot shares recalculated"]    
    }else{
      tree2Subset[checkFlags!="(E,f)",newShare:=round(availability/sum(availability,na.rm = TRUE)-shareProt*(availability/sum(availability,na.rm = TRUE)),4)]
      
      freqChild= data.table(table(tree2Subset[, get("measuredItemChildCPC")]))
      setnames(freqChild, c("V1","N"), c("measuredItemChildCPC", "freq"))
      tree2Subset=merge(tree2Subset, freqChild , by="measuredItemChildCPC")
      
      tree2Subset[!(checkFlags=="(E,f)"), negShare:=1/freq]
      
      tree2Subset[!(checkFlags=="(E,f)"),
                  sumPositiveAvail:=sum(availability.child*ifelse(availability.child>0,1,0),na.rm=TRUE),by = c("measuredItemChildCPC")]
      
      tree2Subset[!(checkFlags=="(E,f)"),
                  tempAvailability:=ifelse(availability.child<=0|is.na(availability.child),negShare*sumPositiveAvail,availability)]
      
      tree2Subset[!(checkFlags=="(E,f)")|(is.na(share)), 
                  newShare := ifelse(tempAvailability==0|is.na(tempAvailability),negShare-shareProt*(negShare),
                                     (tempAvailability / sum(tempAvailability, na.rm = TRUE)))-shareProt*(tempAvailability / sum(tempAvailability, na.rm = TRUE))]
      
      tree2Subset[,c("freq","tempAvailability","sumPositiveAvail","negShare"):=NULL]
      
      tree2Subset[checkFlags!="(E,f)",severity:=as.integer(1)]
      tree2Subset[checkFlags!="(E,f)",message:="only not prot shares recalculated"]
      
      tree2Subset[checkFlags=="(E,f)",severity:=as.integer(0)]
      tree2Subset[checkFlags=="(E,f)",message:="prot share"]
    }
    return(tree2Subset)
  }
  
  ############ fun14
  # 2. linesT>1
  # B. NOT All protected line
  # b. Yes neg Av in No Protected
  # 1. No neg Av in Protected
  # ii. Yes Missing  
  fun14=function(tree2Subset,case="2.B.b.1.ii"){ 
    shareProt=tree2Subset[checkFlags=="(E,f)",sum(share,na.rm=T)]
    # there is a difference if the sum of protected is >1
    if(shareProt>1){
      tree2Subset[,newShare:=round(availability.child/sum(availability.child,na.rm = TRUE),4)]
      
      freqChild= data.table(table(tree2Subset[, get("measuredItemChildCPC")]))
      setnames(freqChild, c("V1","N"), c("measuredItemChildCPC", "freq"))
      tree2Subset=merge(tree2Subset, freqChild , by="measuredItemChildCPC")
      
      tree2Subset[, negShare:=1/freq]
      
      tree2Subset[,sumPositiveAvail:=sum(availability.child*ifelse(availability.child>0,1,0),na.rm=TRUE),by = c("measuredItemChildCPC")]
      
      tree2Subset[,tempAvailability:=ifelse(availability.child<=0|is.na(availability.child),negShare*sumPositiveAvail,availability)]
      
      tree2Subset[,newShare := ifelse(tempAvailability==0|is.na(tempAvailability),negShare,
                                     (tempAvailability / sum(tempAvailability, na.rm = TRUE)))]
      
      tree2Subset[,c("freq","tempAvailability","sumPositiveAvail","negShare"):=NULL]
      
      tree2Subset[checkFlags=="(E,f)",severity:=3]
      tree2Subset[checkFlags=="(E,f)",message:="sum of prot shares>1, recalculated"]
      
      tree2Subset[checkFlags!="(E,f)",severity:=1]
      tree2Subset[checkFlags!="(E,f)",message:="also prot shares recalculated"]
    }else{
      tree2Subset[checkFlags!="(E,f)",newShare:=round(availability/sum(availability,na.rm = TRUE)-shareProt*(availability/sum(availability,na.rm = TRUE)),4)]
      
      freqChild= data.table(table(tree2Subset[, get("measuredItemChildCPC")]))
      setnames(freqChild, c("V1","N"), c("measuredItemChildCPC", "freq"))
      tree2Subset=merge(tree2Subset, freqChild , by="measuredItemChildCPC")
      
      tree2Subset[!(checkFlags=="(E,f)"), negShare:=1/freq]
      
      tree2Subset[!(checkFlags=="(E,f)"),
                  sumPositiveAvail:=sum(availability.child*ifelse(availability.child>0,1,0),na.rm=TRUE),by = c("measuredItemChildCPC")]
      
      tree2Subset[!(checkFlags=="(E,f)"),
                  tempAvailability:=ifelse(availability.child<=0|is.na(availability.child),negShare*sumPositiveAvail,availability)]
      
      tree2Subset[!(checkFlags=="(E,f)")|(is.na(share)), 
                  newShare := ifelse(tempAvailability==0|is.na(tempAvailability),negShare-shareProt*(negShare),
                                     (tempAvailability / sum(tempAvailability, na.rm = TRUE)))-shareProt*(tempAvailability / sum(tempAvailability, na.rm = TRUE))]
      
      tree2Subset[,c("freq","tempAvailability","sumPositiveAvail","negShare"):=NULL]
      
      tree2Subset[checkFlags=="(E,f)"&!(is.na(share)),severity:=1]
      tree2Subset[checkFlags=="(E,f)"&!(is.na(share)),message:="valid prot share"]
      
      
      tree2Subset[checkFlags=="(E,f)"&is.na(share),severity:=2]
      tree2Subset[checkFlags=="(E,f)"&is.na(share),message:="Missing prot share, recalculated"]
      
      tree2Subset[checkFlags!="(E,f)",severity:=1]
      tree2Subset[checkFlags!="(E,f)",message:="also prot shares recalculated"]      
    }
    return(tree2Subset)
  }
  
  ############ fun15
  # 2. linesT>1
  # B. NOT All protected line
  # b. Yes neg Av in No Protected
  # 2. Yes neg Av in Protected
  # i. No Missing
  fun15=function(tree2Subset,case="2.B.b.2.i"){ 
    shareProt=tree2Subset[checkFlags=="(E,f)",sum(share)]
    # there is a difference if the sum of protected is >1
    if(shareProt>1){
      tree2Subset[,newShare:=round(availability.child/sum(availability.child,na.rm = TRUE),4)]
      
      freqChild= data.table(table(tree2Subset[, get("measuredItemChildCPC")]))
      setnames(freqChild, c("V1","N"), c("measuredItemChildCPC", "freq"))
      tree2Subset=merge(tree2Subset, freqChild , by="measuredItemChildCPC")
      
      tree2Subset[, negShare:=1/freq]
      
      tree2Subset[,sumPositiveAvail:=sum(availability.child*ifelse(availability.child>0,1,0),na.rm=TRUE),by = c("measuredItemChildCPC")]
      
      tree2Subset[,tempAvailability:=ifelse(availability.child<=0|is.na(availability.child),negShare*sumPositiveAvail,availability)]
      
      tree2Subset[,newShare := ifelse(tempAvailability==0|is.na(tempAvailability),negShare,
                                     (tempAvailability / sum(tempAvailability, na.rm = TRUE)))]
      
      tree2Subset[,c("freq","tempAvailability","sumPositiveAvail","negShare"):=NULL]
      
      tree2Subset[checkFlags=="(E,f)"&(availability.child<=0|is.na(availability.child)),severity:=3]
      tree2Subset[checkFlags=="(E,f)"&(availability.child<=0|is.na(availability.child)),message:="sum of prot shares>1, neg availlability, recalculated"]
      
      tree2Subset[checkFlags=="(E,f)"&availability.child>0,severity:=2]
      tree2Subset[checkFlags=="(E,f)"&availability.child>0,message:="sum of prot shares>1, recalculated"]
      
      tree2Subset[checkFlags!="(E,f)",severity:=1]
      tree2Subset[checkFlags!="(E,f)",message:="also prot shares recalculated"]
    }else{
      tree2Subset[checkFlags!="(E,f)",newShare:=round(availability/sum(availability,na.rm = TRUE)-shareProt*(availability/sum(availability,na.rm = TRUE)),4)]
      
      freqChild= data.table(table(tree2Subset[, get("measuredItemChildCPC")]))
      setnames(freqChild, c("V1","N"), c("measuredItemChildCPC", "freq"))
      tree2Subset=merge(tree2Subset, freqChild , by="measuredItemChildCPC")
      
      tree2Subset[!(checkFlags=="(E,f)"), negShare:=1/freq]
      
      tree2Subset[!(checkFlags=="(E,f)"),
                  sumPositiveAvail:=sum(availability.child*ifelse(availability.child>0,1,0),na.rm=TRUE),by = c("measuredItemChildCPC")]
      
      tree2Subset[!(checkFlags=="(E,f)"),
                  tempAvailability:=ifelse(availability.child<=0|is.na(availability.child),negShare*sumPositiveAvail,availability)]
      
      tree2Subset[!(checkFlags=="(E,f)")|(is.na(share)), 
                  newShare := ifelse(tempAvailability==0|is.na(tempAvailability),negShare-shareProt*(negShare),
                                     (tempAvailability / sum(tempAvailability, na.rm = TRUE)))-shareProt*(tempAvailability / sum(tempAvailability, na.rm = TRUE))]
      
      tree2Subset[,c("freq","tempAvailability","sumPositiveAvail","negShare"):=NULL]
      
      tree2Subset[checkFlags!="(E,f)",severity:=as.integer(1)]
      tree2Subset[checkFlags!="(E,f)",message:="only not prot shares recalculated"]
      
      tree2Subset[checkFlags=="(E,f)"&(availability.child<=0|is.na(availability.child)),severity:=as.integer(1)]
      tree2Subset[checkFlags=="(E,f)"&(availability.child<=0|is.na(availability.child)),message:="prot share, neg avail"]
      
      tree2Subset[checkFlags=="(E,f)"&availability.child>0,severity:=as.integer(0)]
      tree2Subset[checkFlags=="(E,f)"&availability.child>0,message:="prot share"]
    }  
    return(tree2Subset)
  }
  
  ############ fun16
  # 2. linesT>1
  # B. NOT All protected line
  # b. Yes neg Av in No Protected
  # 2. Yes neg Av in Protected
  # ii. YES Missing
  fun16=function(tree2Subset,case="2.B.b.2.ii"){ 
    shareProt=tree2Subset[checkFlags=="(E,f)",sum(share,na.rm=T)]
    # there is a difference if the sum of protected is >1
    if(shareProt>1){
      tree2Subset[,newShare:=round(availability.child/sum(availability.child,na.rm = TRUE),4)]
      
      freqChild= data.table(table(tree2Subset[, get("measuredItemChildCPC")]))
      setnames(freqChild, c("V1","N"), c("measuredItemChildCPC", "freq"))
      tree2Subset=merge(tree2Subset, freqChild , by="measuredItemChildCPC")
      
      tree2Subset[, negShare:=1/freq]
      
      tree2Subset[,sumPositiveAvail:=sum(availability.child*ifelse(availability.child>0,1,0),na.rm=TRUE),by = c("measuredItemChildCPC")]
      
      tree2Subset[,tempAvailability:=ifelse(availability.child<=0|is.na(availability.child),negShare*sumPositiveAvail,availability)]
      
      tree2Subset[,newShare := ifelse(tempAvailability==0|is.na(tempAvailability),negShare,
                                     (tempAvailability / sum(tempAvailability, na.rm = TRUE)))]
      
      tree2Subset[,c("freq","tempAvailability","sumPositiveAvail","negShare"):=NULL]
      
      tree2Subset[checkFlags=="(E,f)"&(availability.child<=0|is.na(availability.child))&is.na(share),severity:=3]
      tree2Subset[checkFlags=="(E,f)"&(availability.child<=0|is.na(availability.child))&is.na(share),message:="sum of prot shares>1, neg availlability, missing Share, recalculated"]
      
      tree2Subset[checkFlags=="(E,f)"&(availability.child<=0|is.na(availability.child))&!(is.na(share)),severity:=3]
      tree2Subset[checkFlags=="(E,f)"&(availability.child<=0|is.na(availability.child))&!(is.na(share)),message:="sum of prot shares>1, neg availlability,recalculated"]
      
      tree2Subset[checkFlags=="(E,f)"&availability.child>0&is.na(share),severity:=2]
      tree2Subset[checkFlags=="(E,f)"&availability.child>0&is.na(share),message:="sum of prot shares>1, missing share, recalculated"]
      
      tree2Subset[checkFlags=="(E,f)"&availability.child>0,severity:=2]
      tree2Subset[checkFlags=="(E,f)"&availability.child>0,message:="sum of prot shares>1, recalculated"]

      tree2Subset[checkFlags!="(E,f)",severity:=1]
      tree2Subset[checkFlags!="(E,f)",message:="also prot shares recalculated"]
    }else{
      tree2Subset[checkFlags!="(E,f)",newShare:=round(availability/sum(availability,na.rm = TRUE)-shareProt*(availability/sum(availability,na.rm = TRUE)),4)]
      
      freqChild= data.table(table(tree2Subset[, get("measuredItemChildCPC")]))
      setnames(freqChild, c("V1","N"), c("measuredItemChildCPC", "freq"))
      tree2Subset=merge(tree2Subset, freqChild , by="measuredItemChildCPC")
      
      tree2Subset[!(checkFlags=="(E,f)"), negShare:=1/freq]
      
      tree2Subset[!(checkFlags=="(E,f)"),
                  sumPositiveAvail:=sum(availability.child*ifelse(availability.child>0,1,0),na.rm=TRUE),by = c("measuredItemChildCPC")]
      
      tree2Subset[!(checkFlags=="(E,f)"),
                  tempAvailability:=ifelse(availability.child<=0|is.na(availability.child),negShare*sumPositiveAvail,availability)]
      
      tree2Subset[!(checkFlags=="(E,f)")|(is.na(share)), 
                  newShare := ifelse(tempAvailability==0|is.na(tempAvailability),negShare-shareProt*(negShare),
                                     (tempAvailability / sum(tempAvailability, na.rm = TRUE)))-shareProt*(tempAvailability / sum(tempAvailability, na.rm = TRUE))]
      
      tree2Subset[,c("freq","tempAvailability","sumPositiveAvail","negShare"):=NULL]
      
      tree2Subset[checkFlags=="(E,f)"&(availability.child<=0|is.na(availability.child))&!(is.na(share)),severity:=1]
      tree2Subset[checkFlags=="(E,f)"&(availability.child<=0|is.na(availability.child))&!(is.na(share)),message:="neg, avail, valid prot share"]
      
      tree2Subset[checkFlags=="(E,f)"&(availability.child<=0|is.na(availability.child))&is.na(share),severity:=2]
      tree2Subset[checkFlags=="(E,f)"&(availability.child<=0|is.na(availability.child))&is.na(share),message:="Neg avail, Missing prot share, recalculated"]
      
      tree2Subset[checkFlags=="(E,f)"&(availability.child>0)&!(is.na(share)),severity:=1]
      tree2Subset[checkFlags=="(E,f)"&(availability.child>0)&!(is.na(share)),message:="valid prot share"]
      
      tree2Subset[checkFlags=="(E,f)"&(availability.child>0)&is.na(share),severity:=3]
      tree2Subset[checkFlags=="(E,f)"&(availability.child>0)&is.na(share),message:="Missing prot share, recalculated"]
      
      tree2Subset[checkFlags!="(E,f)",severity:=1]
      tree2Subset[checkFlags!="(E,f)",message:="also prot shares recalculated"]      
    }
    
    
    return(tree2Subset)
  }
  
  ############################################################
  ###############   Run the alternative case    ##############
  ############################################################   
  tree2Subset = switch(case,
         "1.A.a"= fun1(tree2Subset,case),
         "1.A.b" = fun2(tree2Subset,case),
         "1.B.a" = fun3(tree2Subset,case),
         "1.B.b" = fun4(tree2Subset,case),
         "2.A.a.1" = fun5(tree2Subset,case),
         "2.A.a.2" = fun6(tree2Subset,case),
         "2.A.b.1" = fun7(tree2Subset,case),
         "2.A.b.2" = fun8(tree2Subset,case),
         "2.B.a.1.i" = fun9(tree2Subset,case),
         "2.B.a.1.ii" = fun10(tree2Subset,case),
         "2.B.a.2.i" = fun11(tree2Subset,case),
         "2.B.a.2.ii" = fun12(tree2Subset,case),
         "2.B.b.1.i" = fun13(tree2Subset,case),
         "2.B.b.1.ii" = fun14(tree2Subset,case),
         "2.B.b.2.i" = fun15(tree2Subset,case),
         "2.B.b.2.ii" = fun16(tree2Subset,case)
  )
  
  
  tree2Subset[,share:=ifelse(!is.na(newShare),round(newShare,4),round(share,4))]
  tree2Subset[,newShare:=NULL]
  setcolorder(tree2Subset,c("measuredItemParentCPC", "geographicAreaM49", "measuredItemChildCPC", 
                            "timePointYears", "flagObservationStatus", "flagMethod", "share", 
                            "extractionRate", "availability", "checkFlags", "availability.child", 
                            "shareSum","severity", "message"))
  
  tree2Subset[share==0, message:="Share = 0 removes connection"]
  tree2Subset[share==0, severity:=5]
  
  
  return(tree2Subset)
}
