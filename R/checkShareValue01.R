

checkShareValue=function(tree2Subset){
  # 1.
  # if is just one value(Extraction Rate is never NA)
  if(nrow(tree2Subset)==1){
    ## 1.A. if share is NA but flagged as official
    ## replace it with 1 and mark as "high severity" 3
    tree2Subset[,newShare:=1]
    if(is.na(tree2Subset[,share])){
      tree2Subset[,severity:=3]
      tree2Subset[,message:="single child with NA, share changet to 1"]
      return(tree2Subset)
    }else{
      ## 1.B. if is not NA 
      ## replace it with 1 and mark as "high severity" 2
      tree2Subset[,severity:=2]
      tree2Subset[,message:="single child, share changet to 1"]
      return(tree2Subset)
    }
  }else{
    # 2.
    # if is more than 1 value (Extraction Rate is never NA)
    ## 2.A. if all lines are protected (E,f)
    if(!any(tree2Subset[,checkFlags]!="(E,f)")){
      # redistribute shares proportionally to parent availability 
      # and mark as severity 2
      tree2Subset[,newShare:=round(availability/sum(availability,na.rm = TRUE),4)]
      tree2Subset[,severity:=2]
      tree2Subset[,message:="all s. protected, sum!=0, all prop. recalc."]
      return(tree2Subset)
      
    }else{
      ## 2.B. if not all lines protected (E,f)
      # redistribute porportionally those lines that are not protected
      # first calculate share of protected
      shareProt = round(sum(tree2Subset[checkFlags=="(E,f)",share]),4)
      # if there is some NA, calcualte share also for these and set severity to 3
      # if is an NA, calcualte share also for this and set severity to 3
      if(is.na(shareProt)){
        tree2Subset[,newShare:=round(availability/sum(availability,na.rm = TRUE),4)]
        tree2Subset[checkFlags=="(E,f)"&is.na(share),severity:=as.integer(3)]
        tree2Subset[checkFlags=="(E,f)"&is.na(share),message:="protected share Missing. Value changed"]
        tree2Subset[checkFlags!="(E,f)",severity:=as.integer(1)]
        tree2Subset[checkFlags!="(E,f)",message:="not protected shares have been recalculated"]
      }else{
        tree2Subset[checkFlags!="(E,f)",newShare:=round(availability/sum(availability,na.rm = TRUE)-shareProt*(availability/sum(availability,na.rm = TRUE)),4)]
        tree2Subset[,severity:=as.integer(1)]
        tree2Subset[,message:="not protected shares have been recalculated"]
      }
      # check if some availability was negative

      if(dim(tree2Subset[(availability.child<=0|is.na(availability.child))&(checkFlags=="(E,f)")])[1]>0){
        # if there was some negative availability but only for the protected share
        # don't do anything on values but change message, if missing
          tree2Subset[(availability.child<=0|is.na(availability.child))&(checkFlags=="(E,f)"),
                      severity:=ifelse(is.na(severity),as.integer(1),as.integer(3))]
          tree2Subset[(availability.child<=0|is.na(availability.child))&(checkFlags=="(E,f)"),
                      message:=ifelse(is.na(message),"negative availabilities on protected shares"," protected share Missing. Value changed")]
          # if there are also other connection non protected with availability <0
          if(dim(tree2Subset[(availability.child<=0|is.na(availability.child))&(checkFlags!="(E,f)")])[1]!=0){
            # if the protected shares were null
            # Change values
            
            shareProt = round(sum(tree2Subset[checkFlags=="(E,f)",share]),4)
            
            freqChild= data.table(table(tree2Subset[, get(params$childVar)]))
            setnames(freqChild, c("V1","N"), c(params$childVar, "freq"))
            tree2Subset=merge(tree2Subset, freqChild , by=params$childVar)
            tree2Subset[(availability.child<=0|is.na(availability.child))&(checkFlags=="(E,f)"), negShare:=1/freq]
            
            }else{
          # if there was some negative availability not only for the protected share
          # recalculate shares
          # mark as severity 1
          
          tree2Subset[,severity:=as.integer(2)]
          tree2Subset[,message:="not protected shares have been recalculated, some had negative availabilities"]
          return(tree2Subset)
          # ...
        }
      }
      
    }
  }
}
  
  
  tree2Subset[,share:=ifelse(!is.na(newShare),newShare,share)]
  tree2Subset[,newShare:=NULL]
  
  # if availability is negative
  # shares are  a proportion of the number of child of each parent
  
  if(dim(tree2Subset)[1]!=0){
    freqChild= data.table(table(tree2Subset[, get(params$childVar)]))
    setnames(freqChild, c("V1","N"), c(params$childVar, "freq"))
    tree2Subset=merge(tree2Subset, freqChild , by=params$childVar)
  }
  ### CRISTINA this function has to be used also when availability is NA
  
  tree2Subset[availability.child<=0|is.na(availability.child), negShare:=1/freq]
  
  # because a child can have some positive and negative availabilities
  # new avail. and shares have to be calculated for all the child
  # in order to make shares sum at 1
  
  tree2Subset[checkFlags!="(E,f)",sumPositiveAvail:=sum(availability.child*ifelse(availability.child>0,1,0),na.rm=TRUE),by = c(params$childVar)]
  
  tree2Subset[checkFlags!="(E,f)",tempAvailability:=ifelse(availability.child<=0|is.na(availability.child),negShare*sumPositiveAvail,availability)]
  
  tree2Subset[checkFlags!="(E,f)", newShare := ifelse(tempAvailability==0,negShare, tempAvailability / sum(tempAvailability, na.rm = TRUE)),
              by = c(params$childVar)]
  tree2Subset[,availability.child:=tempAvailability]
  tree2Subset[,availability:=availability.child]
  
  tree2Subset[,c("freq","tempAvailability","sumPositiveAvail","negShare","availability.child"):=NULL]
  tree2Subset[, c(params$shareVar) :=
                ifelse(is.na(newShare), get(params$shareVar), newShare)]
  tree2Subset[, newShare := NULL]
  
  
  
}
