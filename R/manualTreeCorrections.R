##' This function makes some manual corrections on TREE
##' for all those cases in which there is not an extraction rate
##' 
##' @param tree the Tree to be modified
##' @param cereals if to modify or not cereals. 
##' this has been added because some changes on cereals were tested as bad
##' @return the tree updated
##'   




manualTreeCorrections <- function(tree=tree,cereals=FALSE){

  

if(cereals==TRUE){
  
  ############################################################
  ##                                                        ##
  ##                       Cereals                          ##
  ##                                                        ##
  ############################################################
  ##01199.02	Mixed grain		23110	Wheat and meslin flour	This connection does not exist in TCF (p. 733),
  ##but it exists in Veronica's files (75)
  
  ##tree[(measuredItemParentCPC=="01199.02" & measuredItemChildCPC=="23110"), extractionRate:=0.75]
  
  
  ##01194	Quinoa		23120.90	Flour of Cereals nes, this connection does not exist in TCF (p. 733),
  ##but it exists in Veronica's files (72)
  
  ##tree[(measuredItemParentCPC=="01194" & measuredItemChildCPC=="23120.90"), extractionRate:=0.72]
  
  ##0116	Rye		23140.03	Breakfast Cereals	This connection exists both in TCF (but no extraction rate), and in Veronica's file (extraction rate= 90)
  ## and in Veronica's file (extraction rate= 90)
  
  ##tree[(measuredItemParentCPC=="0116" & measuredItemChildCPC=="23140.03"), extractionRate:=0.9]
  
  
  ##23161.01	Rice, Milled (Husked)		23140.03	Breakfast Cereals	
  ##This connection does not exist in TCF , but it exists in Veronica's files (100)
  
  ##tree[(measuredItemParentCPC=="23161.01" & measuredItemChildCPC=="23140.03"), extractionRate:=1]
  
  
  ##24320	Malt, whether or not roasted		23140.03	Breakfast Cereals
  ##This connection exists both in TCF (but no extraction rate), and in Veronica's file (extraction rate= 95)
  
  
  ##tree[(measuredItemParentCPC=="24320" & measuredItemChildCPC=="23140.03"), extractionRate:=0.95]
  
  
  ##01194	Quinoa		39120.13	Bran of Cereals nes
  ##Quinoa is not taken into account in TCF, but it exists in Veronica's files (27)
  
  ##tree[(measuredItemParentCPC=="01194" & measuredItemChildCPC=="39120.13"), extractionRate:=0.27]
  
  
  ##0111	Wheat		23140.02	
  ##Bulgur	This connection exist only in TCF with 93-98%
  tree[(measuredItemParentCPC=="0111" & measuredItemChildCPC=="23140.02"), extractionRate:=0.95]
  
  
  
  ##23161.01	Rice, Milled (Husked)		23161.03	Rice, Broken	TCF=100%, Vero=99
  
  
  tree[(measuredItemParentCPC=="23161.01" & measuredItemChildCPC=="23161.03"), extractionRate:=1]
  
  
  ##23220.04	Starch of Maize		23210.03	Other Fructose and Syrup	TCF=100%, Vero=100
  
  tree[(measuredItemParentCPC=="23220.04" & measuredItemChildCPC=="23210.03"), extractionRate:=1]
  
  ##0112	Maize (corn)						24310.01	Beer of Barley, malted	This connection exists only in Veronica's file (601)
  ##01659	Hop cones (fresh and dried)			24310.01	Beer of Barley, malted	This connection exists only in Veronica's file (606)
  ##23120.06	Flour of Sorghum				24310.01	Beer of Barley, malted	This connection exists only in Veronica's file (358)
  ##23161.03	Rice, Broken					24310.01	Beer of Barley, malted	This connection exists only in Veronica's file (580)
  
  
  ##tree[(measuredItemParentCPC=="0112" & measuredItemChildCPC=="24310.01"), extractionRate:=6.01]
  ##tree[(measuredItemParentCPC=="01659" & measuredItemChildCPC=="24310.01"), extractionRate:=6.06]
  ##tree[(measuredItemParentCPC=="23120.06" & measuredItemChildCPC=="24310.01"), extractionRate:=3.58]
  ##tree[(measuredItemParentCPC=="23161.03" & measuredItemChildCPC=="24310.01"), extractionRate:=5.8]
  
} 
  
  ############################################################
  ##                                                        ##
  ##              Edible Roots and tubers                   ##
  ##                                                        ##
  ############################################################
  
  
  ## Edible Roots and Tubers
  
  ## in FBS tree associated to fbsID4 we have to add 01599.10 which is a primary (DONE)
  ## measuredItemCPC                                             measuredItemCPC_description
  ##    01599.10 Edible roots and tubers with high starch or inulin content, n.e., fresh
  ##I have ADDED this commodity to the FBS tree:
  
  ##fbsID4 measuredItemSuaFbs fbsID1 fbsID2 fbsID3
  ##1:   2534           01599.10   2901   2903   2907
  
  ##Issue chidere  a Veronica Giorgio di raggiungerlo nel Tree del FBS!!!!!
  
  ## Taro and Yautia have 2 childrem "Flour of Roots and Tubers nes" and "Edible roots and tubers with high starch or inulin content, n.e., dried", 
  ## the both with extraction rates =1
  
  
  ## approximation for "Flour of Roots and Tubers nes": 
  ## I am looking at the extraction rate of the same tipology of commodity when they are child of "01599.10"
  ##unique(tree[measuredItemParentCPC %in% c("01599.10") & measuredItemChildCPC=="23170.02" , extractionRate])
  ##[1] 0.2694402 0.2500000 0.2000000 0.3575742 0.2800000
  
  ## mean(0.2694402, 0.2500000, 0.2000000 ,0.3575742 ,0.2800000)
  ##[1] 0.2694402
  ## The same for "Edible roots and tubers with high starch or inulin content, n.e., dried"
  ##unique(tree[measuredItemParentCPC %in% c("01599.10") & measuredItemChildCPC=="01599.20" , extractionRate])
  ##[1] 0.4
  
  
  tree[(measuredItemParentCPC=="01540" & measuredItemChildCPC=="01599.20"), extractionRate:=0.4]
  tree[(measuredItemParentCPC=="01550" & measuredItemChildCPC=="01599.20"), extractionRate:=0.4]
  tree[(measuredItemParentCPC=="01591" & measuredItemChildCPC=="01599.20"), extractionRate:=0.4]
  
  
  tree[(measuredItemParentCPC=="01540" & measuredItemChildCPC=="23170.02"), extractionRate:=0.27]
  
  tree[(measuredItemParentCPC=="01591" & measuredItemChildCPC=="23170.02"), extractionRate:=0.27]
  
  
  ############################################################
  ##                                                        ##
  ##                       Margarine                        ##
  ##                                                        ##
  ############################################################
  
  
  
  tree[measuredItemParentCPC=="2162" & measuredItemChildCPC=="21700.01", extractionRate:=1.3]
  tree[measuredItemParentCPC=="21631.01" & measuredItemChildCPC=="21700.01", extractionRate:=1.3]
  tree[measuredItemParentCPC=="21641.01" & measuredItemChildCPC=="21700.01", extractionRate:=1.3]
  tree[measuredItemParentCPC=="2165" & measuredItemChildCPC=="21700.01", extractionRate:=1.3]
  tree[measuredItemParentCPC=="2166" & measuredItemChildCPC=="21700.01", extractionRate:=1.3]
  tree[measuredItemParentCPC=="2168" & measuredItemChildCPC=="21700.01", extractionRate:=1.3]
  tree[measuredItemParentCPC=="21691.07" & measuredItemChildCPC=="21700.01", extractionRate:=1.3]
  tree[measuredItemParentCPC=="21691.14" & measuredItemChildCPC=="21700.01", extractionRate:=1.3]
  
  
  tree[measuredItemParentCPC=="21691.01" & measuredItemChildCPC=="21700.02", extractionRate:=1.1]
  tree[measuredItemParentCPC=="21691.02" & measuredItemChildCPC=="21700.02", extractionRate:=1.1]
  tree[measuredItemParentCPC=="21691.07" & measuredItemChildCPC=="21700.02", extractionRate:=1.1]
  
  
  
  ############################################################
  ##                                                        ##
  ##                       Meat                             ##
  ##                                                        ##
  ############################################################
  
  
  ##This connections are not meaningful
  
  tree= tree[!(measuredItemParentCPC=="21511.02" & measuredItemChildCPC %in% c("21184.02","21113.02"))]
  
  tree= tree[!(measuredItemParentCPC=="21113.02" & measuredItemChildCPC %in% c("21181"))]
  tree[measuredItemParentCPC=="21121" & measuredItemChildCPC %in% c("23991.04"), extractionRate:=1]
  
  
  
  
  
  ############################################################
  ##                                                        ##
  ##                       OIL                              ##
  ##                                                        ##
  ############################################################
  
  
  measuredItemParentCPC
  
  
  tree[(measuredItemParentCPC=="2161" & measuredItemChildCPC=="34120"), extractionRate:=0.98]
  tree[(measuredItemParentCPC=="2162" & measuredItemChildCPC=="34120"), extractionRate:=0.98]
  tree[(measuredItemParentCPC=="2166" & measuredItemChildCPC=="34120"), extractionRate:=0.98]
  
  tree[(measuredItemParentCPC=="21691.14" & measuredItemChildCPC=="34550"), extractionRate:=1]
  tree[(measuredItemParentCPC=="2161" & measuredItemChildCPC=="F1243"), extractionRate:=1]
  tree[(measuredItemParentCPC=="2162" & measuredItemChildCPC=="F1243"), extractionRate:=1]
  
  
  
  
  
  ##tree[(measuredItemParentCPC=="2165" & measuredItemChildCPC=="F1243"), extractionRate:=???]
  
  ##tree[(measuredItemParentCPC=="2168" & measuredItemChildCPC=="F1243"), extractionRate:=???]
  ##tree[(measuredItemParentCPC=="21691.14" & measuredItemChildCPC=="F1243"), extractionRate:=???]
  
  
  
  ##tree[(measuredItemParentCPC=="21691.01" & measuredItemChildCPC=="F1275"), extractionRate:=???]
  ##tree[(measuredItemParentCPC=="01499.02" & measuredItemChildCPC=="21691.05"), extractionRate:=???]
  ##tree[(measuredItemParentCPC=="01448" & measuredItemChildCPC=="21691.08"), extractionRate:=???]
  
  
  
  ########################################################################
  ##                                                                    ##
  ##                                FRUIT                               ##
  ##                                                                    ##
  ########################################################################
  ## JUICE 
  ## I have noticed that many fruit itemes are linked with "Juice of fruits n.e" with an extraction rate which is 
  ## equal to 1
  
  ## it makes sense that "juice" is child of other fruit items, the point is that it is not possible to 
  ##have an extaction rate=1. The othe fruit (parent of juice) report an extraction rate generally lower
  ##that 0.5 (the maximum is 0.8)
  
  
  ##   juiceExR=unique(tree[measuredItemChildCPC=="21439.9", .(extractionRate,measuredItemParentCPC)])
  ##      extractionRate measuredItemParentCPC
  ##   1:             NA                 01229
  ##   2:             NA                 01316
  ##   3:           0.55                 01316
  ##   4:           0.60                 01316
  ##   5:           0.50                 01316
  ##   6:           0.40                 01316
  ##   7:           0.65                 01316
  ##   8:             NA                 01318
  ##   9:           0.40                 01318
  ##  10:           0.65                 01318
  ##  11:             NA                 01319
  ##  12:           0.50                 01319
  ##  13:           0.70                 01319
  ##  14:           0.12                 01319
  ##  15:           0.60                 01319
  ##  16:             NA                 01321
  ##  17:           0.40                 01321
  ##  18:           0.65                 01321
  ##  19:             NA                 01322
  ##  20:           0.60                 01322
  ##  21:             NA                 01323
  ##  22:           0.80                 01323
  ##  23:           0.65                 01323
  ##  24:           0.50                 01323
  ##  25:           0.30                 01323
  ##  26:           0.90                 01323
  ##  27:           0.40                 01323
  ##  28:           0.70                 01323
  ##  29:           0.60                 01323
  ##  30:             NA                 01324
  ##  31:           0.65                 01324
  ##  32:             NA                 01329
  ##  33:           0.40                 01329
  ##  34:           0.17                 01329
  ##  35:             NA                 01341
  ##  36:           0.50                 01341
  ##  37:             NA              01342.01
  ##  38:           0.80              01342.01
  ##  39:           0.70              01342.01
  ##  40:           0.65              01342.01
  ##  41:           0.60              01342.01
  ##  42:             NA              01344.01
  ##  43:             NA              01344.02
  ##  44:             NA                 01345
  ##  45:           0.80                 01345
  ##  46:           0.50                 01345
  ##  47:           0.40                 01345
  ##  48:           0.65                 01345
  ##  49:           0.70                 01345
  ##  50:           0.60                 01345
  ##  51:             NA              01349.10
  ##  52:             NA              01349.20
  ##  53:             NA              01351.01
  ##  54:           0.65              01351.01
  ##  55:             NA              01351.02
  ##  56:             NA              01353.01
  ##  57:             NA              01355.01
  ##  58:             NA              01355.02
  ##  59:             NA              01355.90
  ##  60:             NA              01359.90
  ##  61:           0.50              01359.90
  ##  62:           0.65              01359.90
  ##  63:           0.60              01359.90
  ##  64:           0.40              01359.90
  ##  65:           0.15              01359.90
  
  ##Fruit items for which extractionrate is missing
  ##01229
  ##01344.01
  ##01344.02
  ##01351.02
  ##01353.01
  ##01355.01
  ##01355.02
  ##01355.90
  ##01349.10
  ##01349.20
  
  ## we have two options: 
  ##1) delete these connections from the tree
  ##2) insert a correct extraction rate
  
  ##My first attempt is to insert an extraction rates 
  ##mean(juiceExR[,extractionRate], na.rm = TRUE)
  ##[1] 0.5533333
  
  
  tree[(measuredItemParentCPC=="01229" & measuredItemChildCPC=="21439.9"), extractionRate:=0.55]
  tree[(measuredItemParentCPC=="01344.01" & measuredItemChildCPC=="21439.9"), extractionRate:=0.55]
  tree[(measuredItemParentCPC=="01344.02" & measuredItemChildCPC=="21439.9"), extractionRate:=0.55]
  tree[(measuredItemParentCPC=="01351.02" & measuredItemChildCPC=="21439.9"), extractionRate:=0.55]
  tree[(measuredItemParentCPC=="01353.01" & measuredItemChildCPC=="21439.9"), extractionRate:=0.55]
  tree[(measuredItemParentCPC=="01355.01" & measuredItemChildCPC=="21439.9"), extractionRate:=0.55]
  tree[(measuredItemParentCPC=="01355.02" & measuredItemChildCPC=="21439.9"), extractionRate:=0.55]
  tree[(measuredItemParentCPC=="01355.90" & measuredItemChildCPC=="21439.9"), extractionRate:=0.55]
  tree[(measuredItemParentCPC=="01349.10" & measuredItemChildCPC=="21439.9"), extractionRate:=0.55]
  tree[(measuredItemParentCPC=="01349.20" & measuredItemChildCPC=="21439.9"), extractionRate:=0.55]
  
  ## This two are more specific: 
  
  ##Juice of Pineapples, Concentrated
  ##Juice of plum
  ##At the moment we have used the average extraction rate 
  tree[(measuredItemParentCPC=="21433" & measuredItemChildCPC=="21433.01"), extractionRate:=0.55]
  tree[(measuredItemParentCPC=="01346" & measuredItemChildCPC=="21439.06"), extractionRate:=0.55]
  
  
  
  #####################################################################################################################
  ## Dries Fruits
  ## CPC=21419.99=Other fruit n.e.c., dried
  
  
  ##figs, it already exists 21419.02 which is exactly "fig dried" whose extraction rate should be around 0.35
  tree=tree[!(measuredItemParentCPC=="01315" & measuredItemChildCPC=="21419.99"), ]
  
  ##apples
  tree[(measuredItemParentCPC=="01341" & measuredItemChildCPC=="21419.99"), extractionRate:=0.12]
  
  
  ## this is an approximation: I have used 0.12 which is in the TCF but associated to APPLES!!!
  ##Cherries, no info about the extraction rates it is better to remove the connection
  tree=tree[!(measuredItemParentCPC=="01344.02" & measuredItemChildCPC=="21419.99"), ]
  ##Pome fruit n.e. no info
  tree=tree[!(measuredItemParentCPC=="01349.10" & measuredItemChildCPC=="21419.99"), ]
  #####################################################################################################################
  
  
  ########################################################################
  ##                                                                    ##
  ##                                MILK                                ##
  ##                                                                    ##
  ########################################################################
  
  
  ## Correction on milk tree
  
  ##MILK and PROD (Excluding Butter)
  
  ##This connection must be removed
  ## there is no reason why 22251.01 Cheese from whole cow milk is child of 22130.01 Whey, fresh 
  tree=tree[!(measuredItemParentCPC=="22130.01" & measuredItemChildCPC=="22251.01"), ]
  
  ##Goat milk , I took the extraction rates of sheep milk:
  tree[(measuredItemParentCPC=="02292" & measuredItemChildCPC=="22110.06"), extractionRate:=0.93]
  tree[(measuredItemParentCPC=="02292" & measuredItemChildCPC=="22249.02"), extractionRate:=0.06]
  
  tree[(measuredItemParentCPC=="22230.03" & measuredItemChildCPC=="22230.04"), extractionRate:=0.1]
  tree[(measuredItemParentCPC=="22120" & measuredItemChildCPC=="22251.04"), extractionRate:=1.05]
  
  
  ##22110.02	Skim Milk of Cows	22221.02	Skim Milk, Evaporated
  ##22110.02	Skim Milk of Cows	22222.01	Whole Milk, Condensed
  
  tree[(measuredItemParentCPC=="22110.02" & measuredItemChildCPC=="22221.02"), extractionRate:=0.4]
  tree[(measuredItemParentCPC=="22110.02" & measuredItemChildCPC=="22222.01"), extractionRate:=0.38]
  
  
  ##Study of the tree: these connections do not exist
  
  tree=tree[!(measuredItemParentCPC=="22251.03" & measuredItemChildCPC=="22251.04"), ]
  tree=tree[!(measuredItemParentCPC=="22130.01" & measuredItemChildCPC=="22251.01"), ]
  
  tree=tree[!(measuredItemParentCPC=="22110.02" & measuredItemChildCPC=="22251.01"), ]
  tree=tree[!(measuredItemParentCPC=="22110.02" & measuredItemChildCPC=="22222.01"), ]
  
  tree=tree[!(measuredItemParentCPC=="02211" & measuredItemChildCPC=="22251.02"), ]
  tree=tree[!(measuredItemParentCPC=="02211.02" & measuredItemChildCPC=="22212"), ]
  
  
  
  
}