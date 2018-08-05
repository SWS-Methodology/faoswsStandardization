sua_unbalanced

|             itemName|     Item| Production|  Imports|   Exports| StockChange|     Food| Food Processing| Feed| Seed| Tourist| Industrial|     Loss| Residuals|
|--------------------:|--------:|----------:|--------:|---------:|-----------:|--------:|---------------:|----:|----:|-------:|----------:|--------:|---------:|
|          Cocoa beans|    01640|     10,865|  35.6281| 8,326.011|           -|        -|               -|    -|    -|       -|          -| 742.9409|         -|
| Cocoa paste not defa| 23610.01|          0|   10.152|    10.152|           -|        0|               -|    -|    -|       0|          -|        -|         -|
| Cocoa butter, fat an|    23620|          -|  19.9939|     0.104|           -|  19.8899|               -|    -|    -|   0.011|          -|        -|         -|
| Cocoa Husks and Shel| 39150.02|          0|        -|      0.02|           -|        -|               -|    0|    -|       -|          -|        -|         -|
| cocoa powder and cak|    F0665|          -| 209.4576|       0.1|           -| 209.3576|               -|    -|    -|  0.1161|          -|        -|         -|
| chocolate products n|    F0666|          -| 563.5522|  166.1728|           -| 397.3794|               -|    -|    -|  0.2203|          -|        -|         -|


sua_unbalanced + production filled (step 1 of suaFilling):

|             itemName|     Item| Production|  Imports|   Exports| StockChange|     Food| Food Processing| Feed| Seed| Tourist| Industrial|     Loss| Residuals|
|--------------------:|--------:|----------:|--------:|---------:|-----------:|--------:|---------------:|----:|----:|-------:|----------:|--------:|---------:|
|          Cocoa beans|    01640|     10,865|  35.6281| 8,326.011|           -|        -|               -|    -|    -|       -|          -| 742.9409|         -|
| Cocoa paste not defa| 23610.01|          0|   10.152|    10.152|           -|        0|               -|    -|    -|       0|          -|        -|         -|
| Cocoa butter, fat an|    23620|          -|  19.9939|     0.104|           -|  19.8899|               -|    -|    -|   0.011|          -|        -|         -|
| Cocoa Husks and Shel| 39150.02|          0|        -|      0.02|           -|        -|               -|    0|    -|       -|          -|        -|         -|
| cocoa powder and cak|    F0665|          -| 209.4576|       0.1|           -| 209.3576|               -|    -|    -|  0.1161|          -|        -|         -|
| chocolate products n|    F0666|          -| 563.5522|  166.1728|           -| 397.3794|               -|    -|    -|  0.2203|          -|        -|         -|


Availability of Parent for Food Processing Calculation = Prod+Imp-Exp | Shares by Child | weight of children:

|    Child|            ChildName|   Parent|           ParentName| extractionRate| availability| share| weight|
|--------:|--------------------:|--------:|--------------------:|--------------:|------------:|-----:|------:|
| 23610.01| Cocoa paste not defa|    01640|          Cocoa beans|       0.800000|    2,574.617|  1.00|      1|
|    F0665| cocoa powder and cak| 23610.01| Cocoa paste not defa|       0.524218|            0|  1.00|      0|
|    F0666| chocolate products n|    23620| Cocoa butter, fat an|       4.705593|      19.8899|  0.09|      1|
|    F0666| chocolate products n|    F0665| cocoa powder and cak|       5.007124|     209.3576|  0.91|      1|
| 39150.02| Cocoa Husks and Shel|    01640|          Cocoa beans|       0.200000|    2,574.617|  1.00|      0|
|    23620| Cocoa butter, fat an| 23610.01| Cocoa paste not defa|       0.474964|            0|  1.00|      1|

sua_unbalanced + production + food processing caluclated (step 2 of suaFilling):

|             itemName|     Item| Production|  Imports|   Exports| StockChange|     Food| Food Processing| Feed| Seed| Tourist| Industrial|     Loss| Residuals|
|--------------------:|--------:|----------:|--------:|---------:|-----------:|--------:|---------------:|----:|----:|-------:|----------:|--------:|---------:|
|          Cocoa beans|    01640|     10,865|  35.6281| 8,326.011|           -|        -|           **0**|    -|    -|       -|          -| 742.9409|         -|
| Cocoa paste not defa| 23610.01|          0|   10.152|    10.152|           -|        0|               -|    -|    -|       0|          -|        -|         -|
| Cocoa butter, fat an|    23620|          -|  19.9939|     0.104|           -|  19.8899|               -|    -|    -|   0.011|          -|        -|         -|
| Cocoa Husks and Shel| 39150.02|          0|        -|      0.02|           -|        -|               -|    0|    -|       -|          -|        -|         -|
| cocoa powder and cak|    F0665|          -| 209.4576|       0.1|           -| 209.3576|               -|    -|    -|  0.1161|          -|        -|         -|
| chocolate products n|    F0666|          -| 563.5522|  166.1728|           -| 397.3794|               -|    -|    -|  0.2203|          -|        -|         -|

sua_balanced:

|             itemName|     Item| Production|  Imports|   Exports| StockChange|          Food| Food Processing| Feed| Seed| Tourist|   Industrial|     Loss| Residuals| DESfoodSupply_kCd|
|--------------------:|--------:|----------:|--------:|---------:|-----------:|-------------:|---------------:|----:|----:|-------:|------------:|--------:|---------:|-----------------:|
|          Cocoa beans|    01640|     10,865|  35.6281| 8,326.011|           -| **1,221.117**|               0|    -|    -|       -| **610.5586**| 742.9409|         -|        **0.6467**|
| Cocoa paste not defa| 23610.01|          0|   10.152|    10.152|           -|             0|               -|    -|    -|       0|            -|        -|         -|             **0**|
| Cocoa butter, fat an|    23620|          -|  19.9939|     0.104|           -|       19.8899|               -|    -|    -|   0.011|            -|        -|         -|        **0.0164**|
| Cocoa Husks and Shel| 39150.02|          0|        -|      0.02|           -|             -|               -|    0|    -|       -|            -|        -|         -|                 -|
| cocoa powder and cak|    F0665|          -| 209.4576|       0.1|           -|      209.3576|               -|    -|    -|  0.1161|            -|        -|         -|        **0.0635**|
| chocolate products n|    F0666|          -| 563.5522|  166.1728|           -|      397.3794|               -|    -|    -|  0.2203|            -|        -|         -|        **0.1814**|


Availability of parents in terms of their children = FoodProc * eR | Final Shares by child:

|    Child|            ChildName| Parent|           ParentName| extractionRate| availability| share| weight|
|--------:|--------------------:|------:|--------------------:|--------------:|------------:|-----:|------:|
| 23610.01| Cocoa paste not defa|  01640|          Cocoa beans|           0.80|            0|   1.0|      1|
|    F0665| cocoa powder and cak|  01640|          Cocoa beans|           0.42|            0|   1.0|      0|
|    F0666| chocolate products n|  01640|          Cocoa beans|           2.10|            0|   0.5|      1|
|    F0666| chocolate products n|  23620| Cocoa butter, fat an|           4.71|            0|   0.5|      1|
| 39150.02| Cocoa Husks and Shel|  01640|          Cocoa beans|           0.20|            0|   1.0|      0|


fbs_standardized

|             itemName|  Item| Production|      Imports|       Exports| StockChange|          Food| Food Processing|  Feed|  Seed|    Tourist| Industrial|     Loss| Residuals| DESfoodSupply_kCd|
|--------------------:|-----:|----------:|------------:|-------------:|-----------:|-------------:|---------------:|-----:|-----:|----------:|----------:|--------:|---------:|-----------------:|
|          Cocoa beans| 01640|     10,865| **182.5061**| **8,378.269**|       **0**| **1,315.738**|               0| **0**| **0**| **0.0525**|   610.5586| 742.9409|     **0**|        **0.8008**|
| Cocoa butter, fat an| 23620|          -|   **79.875**|   **17.7609**|       **0**|    **62.114**|           **0**| **0**| **0**| **0.0344**|      **0**|    **0**|     **0**|        **0.1071**|


fbs_balanced:

|             itemName|  Item| Production|      Imports|       Exports| StockChange|          Food| Food Processing| Feed| Seed|    Tourist|   Industrial|         Loss| Residuals| DESfoodSupply_kCd|
|--------------------:|-----:|----------:|------------:|-------------:|-----------:|-------------:|---------------:|----:|----:|----------:|------------:|------------:|---------:|-----------------:|
|          Cocoa beans| 01640| **10,865**| **182.5065**| **8,378.254**|           0| **1,315.732**|               0|    0|    0| **0.0525**| **610.5449**| **742.9243**|         0|            0.8008|
| Cocoa butter, fat an| 23620|      **0**|  **79.8858**|   **17.7585**|           0|   **62.0929**|               0|    0|    0| **0.0344**|            0|            0|         0|            0.1071|


fbs_balanced with updated nutrient values:

|             itemName|  Item| Production|  Imports|   Exports| StockChange|      Food| Food Processing| Feed| Seed| Tourist| Industrial|     Loss| Residuals| DESfoodSupply_kCd|
|--------------------:|-----:|----------:|--------:|---------:|-----------:|---------:|---------------:|----:|----:|-------:|----------:|--------:|---------:|-----------------:|
|          Cocoa beans| 01640|     10,865| 182.5065| 8,378.254|           0| 1,315.732|               0|    0|    0|  0.0525|   610.5449| 742.9243|         0|        **0.8008**|
| Cocoa butter, fat an| 23620|          0|  79.8858|   17.7585|           0|   62.0929|               0|    0|    0|  0.0344|          0|        0|         0|        **0.1071**|


FBS Table at first level of aggregation (fbs items):


|             itemName| Item| Production|   Imports|   Exports| StockChange|      Food| Food Processing| Feed| Seed| Tourist| Industrial|     Loss| Residuals| DESfoodSupply_kCd|
|--------------------:|----:|----------:|---------:|---------:|-----------:|---------:|---------------:|----:|----:|-------:|----------:|--------:|---------:|-----------------:|
|   OILCROPS OTHER OIL| 2586|     39.864| 2,190.952|   69.2169|           0| 1,791.189|               0|    0|    0|  0.9916|   369.4188|        0|         0|            1.5746|
| COCOA BEANS & PRODUC| 2633|     10,865|  182.5065| 8,378.254|           0| 1,315.732|               0|    0|    0|  0.0525|   610.5449| 742.9243|         0|            0.8008|


FBS Table at second level of aggregation (fbs aggregates):


|             itemName| Item| Production|   Imports|   Exports| StockChange|      Food| Food Processing| Feed| Seed| Tourist| Industrial|      Loss| Residuals| DESfoodSupply_kCd|
|--------------------:|----:|----------:|---------:|---------:|-----------:|---------:|---------------:|----:|----:|-------:|----------:|---------:|---------:|-----------------:|
| VEGETABLE OILS & PRO| 2914|  18,447.86| 78,488.47|   734.021|           0| 48,738.03|               0|    0|    0| 27.0042|  47,437.27|         0|    0.0006|           49.7735|
|       BEVERAGE CROPS| 2922|   60,472.1|  448.7452| 17,353.77|           0| 37,285.55|               0|    0|    0|  1.0845|   610.5449| 5,669.895|         0|            2.7574|


FBS Table at third level of aggregation (fbs macro aggregates):


|             itemName| Item| Production|   Imports|   Exports| StockChange|      Food| Food Processing|      Feed|      Seed|   Tourist| Industrial|      Loss| Residuals| DESfoodSupply_kCd|
|--------------------:|----:|----------:|---------:|---------:|-----------:|---------:|---------------:|---------:|---------:|---------:|----------:|---------:|---------:|-----------------:|
| VEGETABLE PROD. (DEM| 2903| 12,786,065| 868,172.4| 177,858.1|   64,980.02| 8,337,145|       767,030.1| 740,098.8| 393,354.6| 4,585.053|  2,140,248| 919,853.6| 109,084.6|         1,828.932|


FBS Table at final level of aggregation (Grand Total):


|             itemName| Item| Production|   Imports|   Exports| StockChange|      Food| Food Processing|      Feed|      Seed|   Tourist| Industrial|      Loss| Residuals| DESfoodSupply_kCd|
|--------------------:|----:|----------:|---------:|---------:|-----------:|---------:|---------------:|---------:|---------:|---------:|----------:|---------:|---------:|-----------------:|
| GRAND TOTAL - DEMAND| 2901| 13,690,338| 907,553.5| 179,827.4|   64,980.02| 9,239,999|       767,030.1| 740,098.8| 397,216.8| 5,039.158|  2,143,164| 951,377.5| 109,158.9|         1,951.665|
