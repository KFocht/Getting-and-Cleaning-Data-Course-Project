# Creating the run_analysis.R document

First, place all necessary files into one working directory.  
  
Read in all neccessary files:

```r
subjectTest <- read.table("C:/Users/Kandy.WAZZABI/Desktop/R Programming/getdata-projectfiles-UCI HAR Dataset/subject_test.txt")
xTest <- read.table("C:/Users/Kandy.WAZZABI/Desktop/R Programming/getdata-projectfiles-UCI HAR Dataset/X_test.txt")
yTest <- read.table("C:/Users/Kandy.WAZZABI/Desktop/R Programming/getdata-projectfiles-UCI HAR Dataset/y_test.txt")
subjectTrain <- read.table("C:/Users/Kandy.WAZZABI/Desktop/R Programming/getdata-projectfiles-UCI HAR Dataset/subject_train.txt")
xTrain <- read.table("C:/Users/Kandy.WAZZABI/Desktop/R Programming/getdata-projectfiles-UCI HAR Dataset/X_train.txt")
yTrain <- read.table("C:/Users/Kandy.WAZZABI/Desktop/R Programming/getdata-projectfiles-UCI HAR Dataset/y_train.txt")
featuresLabels <- read.table("C:/Users/Kandy.WAZZABI/Desktop/R Programming/getdata-projectfiles-UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)
```

Bind all the tables together using cbind and rbind:

```r
subjects <- rbind(subjectTest, subjectTrain)
activities <- rbind(yTest, yTrain)
features <- rbind(xTest, xTrain)
data <- cbind(subjects, activities, features)
```

Add column names to the data set:

```r
colnames(data) <- c("Subject", "Activity", featuresLabels[,2])
```

Extract on the columns that contain mean and standard deviation:

```r
dataExtract <- data[,c("Subject", "Activity", colnames(data)[grep("mean",colnames(data))],
                       colnames(data)[grep("std", colnames(data))])]
```


Add descriptive names to activity column:

```r
dataExtract$Activity[dataExtract$Activity == 1] = "Walking"
dataExtract$Activity[dataExtract$Activity == 2] = "Walking Upstairs"
dataExtract$Activity[dataExtract$Activity == 3] = "Walking Downstairs"
dataExtract$Activity[dataExtract$Activity == 4] = "Sitting"
dataExtract$Activity[dataExtract$Activity == 5] = "Standing"
dataExtract$Activity[dataExtract$Activity == 6] = "Laying"
```

Tidy up the labels by renaming them.  (I am aware there is probably a more elegant solution to this. However, I could not figure it out, so I just typed all 81 labels out.)

```r
colnames(dataExtract) <- c("Subject", "Activity", "tBodyAccXMean", "tBodyAccYMean",
                           "tBodyAccZMean", "tGravityAccXMean", "tGravityAccYMean",
                           "tGravityAccZMean", "tBodyAccJerkXMean", "tBodyAccJerkYMean",
                           "tBodyAccJerkZMean", "tBodyGyroXMean", "tBodyGyroYMean",
                           "tBodyGyroZMean", "tBodyGyroJerkXMean", "tBodyGyroJerkYMean",
                           "tBodyGyroJerkZMean", "tBodyAccMagMean", "tGravityAccMagMean",
                           "tBodyAccJerkMagMean", "tBodyGyroMagMean", "tBodyGyroJerkMagMean",
                           "fBodyAccXMean", "fBodyAccYMean", "fBodyAccZMean",
                           "fBodyAccXMeanFrequency", "fBodyAccYMeanFrequency",
                           "fBodyAccZMeanFrequency", "fBodyAccJerkXMean",
                           "fBodyAccJerkYMean", "fBodyAccJerkZMean",
                           "fBodyAccJerkXMeanFrequency", "fBodyAccJerkYMeanFrequency",
                           "fBodyAccJerkZMeanFrequency", "fBodyGyroXMean", "fBodyGyroYMean",
                           "fBodyZMean", "fBodyGyroXMeanFrequency",
                           "fBodyGyroYMeanFrequency", "fBodyGyroZMeanFrequency",
                           "fBodyAccMagMean", "fBodyAccMagMeanFrequency",
                           "fBodyAccJerkMagMean", "fBodyAccJerkMagMeanFrequency",
                           "fBodyGyroMagMean", "fBodyGyroMagMeanFrequency",
                           "fBodyGyroJerkMagMean", "fBodyGyroJerkMagMeanFrequency",
                           "tBodyAccXStandardDeviation", "tBodyAccYStandardDeviation",
                           "tBodyAccZStandardDeviation", "tGravityAccXStandardDeviation",
                           "tGravityAccYStandardDeviation", "tGravityAccZStandardDeviation",
                           "tBodyAccJerkXStandardDeviation", "tBodyAccJerkYStandardDeviation",
                           "tBodyAccJerkZStandardDeviation", "tBodyGyroXStandardDeviation",
                           "tBodyGyroYStandardDeviation", "tBodyGyroZStandardDeviation",
                           "tBodyGyroJerkXStandardDeviation", "tBodyGyroJerkYStandardDeviation",
                           "tBodyGyroJerkZStandardDeviation", "tBodyAccMagStandardDeviation",
                           "tGravityAccMagStandardDeviation", "tBodyAccJerkMagStandardDeviation",
                           "tBodyGyroMagStandardDeviation", "tBodyGyroJerkMagStandardDeviation",
                           "fBodyAccXStandardDeviation", "fBodyAccYStandardDeviation",
                           "fBodyAccZStandardDeviation", "fBodyAccJerkXStandardDeviation",
                           "fBodyAccJerkYStandardDeviation", "fBodyAccJerkZStandardDeviation",
                           "fBodyGyroXStandardDeviation", "fBodyGyroYStandardDeviation",
                           "fBodyGyroZStandardDeviation", "fBodyAccMagStandardDeviation",
                           "fBodyAccJerkMagStandardDeviation", "fBodyGyroMagStandardDeviation",
                           "fBodyGyroJerkMagStandardDeviation")
```

The next block of code splits the data up into 6 separate data frames based on the 6 different activities.  
Then, I used the aggregate function to create 6 new data frames that show the means of each column split by the Subject number.  
Next, I added the Activity column back into each of the 6 new data frames:


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
##Data frame #1 - Activity - Walking
walkingData <- filter(dataExtract, Activity == "Walking")
walkingMeans <- aggregate(walkingData[,3:81], walkingData["Subject"],
                            function(x) mean(x))
Activity <- rep("Walking", 30)
walkingMeans <- cbind(Activity, walkingMeans)


##Data frame #2 - Activity - Walking Upstairs
walkingUpstairsData <- filter(dataExtract, Activity == "Walking Upstairs")
walkingUpstairsMeans <- aggregate(walkingUpstairsData[,3:81], walkingUpstairsData["Subject"],
                                    function(x) mean(x))
Activity <- rep("Walking Upstairs", 30)
walkingUpstairsMeans <- cbind(Activity, walkingUpstairsMeans)


##Data frame #3 - Activity - Walking Downstairs
walkingDownstairsData <- filter(dataExtract, Activity == "Walking Downstairs")
walkingDownstairsMeans <- aggregate(walkingDownstairsData[,3:81], walkingDownstairsData["Subject"],
                                    function(x) mean(x))
Activity <- rep("Walking Downstairs", 30)
walkingDownstairsMeans <- cbind(Activity, walkingDownstairsMeans)


##Data frame #4 - Activity - Sitting
sittingData <- filter(dataExtract, Activity == "Sitting")
sittingMeans <- aggregate(sittingData[,3:81], sittingData["Subject"],
                            function(x) mean(x))
Activity <- rep("Sitting", 30)
sittingMeans <- cbind(Activity, sittingMeans)


##Data frame #5 - Activity - Standing
standingData <- filter(dataExtract, Activity == "Standing")
standingMeans <- aggregate(standingData[,3:81], standingData["Subject"],
                            function(x) mean(x))
Activity <- rep("Standing", 30)
standingMeans <- cbind(Activity, standingMeans)


##Data frame #6 - Activity - Laying
layingData <- filter(dataExtract, Activity == "Laying")
layingMeans <- aggregate(layingData[,3:81], layingData["Subject"],
                            function(x) mean(x))
Activity <- rep("Laying", 30)
layingMeans <- cbind(Activity, layingMeans)
```

Combine all 6 data frames into one tidy dataset:

```r
tidyData <- rbind(walkingMeans, walkingDownstairsMeans, walkingUpstairsMeans, sittingMeans,
      standingMeans, layingMeans)
```

You can easily see the data is tidy if you look at the first few columns of the data frame:

```r
tidyData[,1:5]
```

```
##               Activity Subject tBodyAccXMean tBodyAccYMean tBodyAccZMean
## 1              Walking       1     0.2773308  -0.017383819   -0.11114810
## 2              Walking       2     0.2764266  -0.018594920   -0.10550036
## 3              Walking       3     0.2755675  -0.017176784   -0.11267486
## 4              Walking       4     0.2785820  -0.014839948   -0.11140306
## 5              Walking       5     0.2778423  -0.017285032   -0.10774178
## 6              Walking       6     0.2836589  -0.016895419   -0.11030317
## 7              Walking       7     0.2755930  -0.018653665   -0.11091223
## 8              Walking       8     0.2746863  -0.018662887   -0.10725209
## 9              Walking       9     0.2785028  -0.018089197   -0.11082051
## 10             Walking      10     0.2785741  -0.017022351   -0.10905751
## 11             Walking      11     0.2718219  -0.016647583   -0.10609630
## 12             Walking      12     0.2771287  -0.015560928   -0.10316111
## 13             Walking      13     0.2758831  -0.018566065   -0.11143146
## 14             Walking      14     0.2719596  -0.021778538   -0.10675637
## 15             Walking      15     0.2738992  -0.017080974   -0.10762182
## 16             Walking      16     0.2760236  -0.020428693   -0.10880405
## 17             Walking      17     0.2723419  -0.018487540   -0.10979212
## 18             Walking      18     0.2738878  -0.017802363   -0.10421832
## 19             Walking      19     0.2739312  -0.019177359   -0.12273667
## 20             Walking      20     0.2725893  -0.021199992   -0.11353948
## 21             Walking      21     0.2791835  -0.018161032   -0.10431933
## 22             Walking      22     0.2788646  -0.016721361   -0.10711251
## 23             Walking      23     0.2732119  -0.018361871   -0.11338299
## 24             Walking      24     0.2769808  -0.022544640   -0.11059595
## 25             Walking      25     0.2789928  -0.018647762   -0.10873764
## 26             Walking      26     0.2792644  -0.015425959   -0.10892701
## 27             Walking      27     0.2768495  -0.016647878   -0.11282229
## 28             Walking      28     0.2812282  -0.015675972   -0.10371471
## 29             Walking      29     0.2719999  -0.016291560   -0.10663243
## 30             Walking      30     0.2764068  -0.017588039   -0.09862471
## 31  Walking Downstairs       1     0.2891883  -0.009918505   -0.10756619
## 32  Walking Downstairs       2     0.2776153  -0.022661416   -0.11681294
## 33  Walking Downstairs       3     0.2924235  -0.019355408   -0.11613984
## 34  Walking Downstairs       4     0.2799653  -0.009802009   -0.10677752
## 35  Walking Downstairs       5     0.2935439  -0.008501075   -0.10031993
## 36  Walking Downstairs       6     0.2770453  -0.019536840   -0.10720936
## 37  Walking Downstairs       7     0.2803071  -0.016633237   -0.09693831
## 38  Walking Downstairs       8     0.2834841  -0.021105867   -0.10759814
## 39  Walking Downstairs       9     0.2959234  -0.020399427   -0.09102788
## 40  Walking Downstairs      10     0.2904016  -0.020005077   -0.11084860
## 41  Walking Downstairs      11     0.2916056  -0.017809772   -0.11107015
## 42  Walking Downstairs      12     0.2815211  -0.018084267   -0.10955593
## 43  Walking Downstairs      13     0.2949076  -0.014372384   -0.10199394
## 44  Walking Downstairs      14     0.2934221  -0.020014141   -0.09283302
## 45  Walking Downstairs      15     0.2801989  -0.005630478   -0.11052607
## 46  Walking Downstairs      16     0.2955868  -0.018389858   -0.11987371
## 47  Walking Downstairs      17     0.2939183  -0.016735940   -0.08924327
## 48  Walking Downstairs      18     0.2884395  -0.016866692   -0.10339081
## 49  Walking Downstairs      19     0.2626881  -0.014594228   -0.13369524
## 50  Walking Downstairs      20     0.2961444  -0.009641208   -0.10460293
## 51  Walking Downstairs      21     0.3014610  -0.017319539   -0.09816866
## 52  Walking Downstairs      22     0.2844590  -0.019799937   -0.10741578
## 53  Walking Downstairs      23     0.2898974  -0.016211146   -0.09881237
## 54  Walking Downstairs      24     0.2886312  -0.014573035   -0.10481882
## 55  Walking Downstairs      25     0.2913297  -0.021017052   -0.10719238
## 56  Walking Downstairs      26     0.2792846  -0.012625810   -0.10643962
## 57  Walking Downstairs      27     0.2975442  -0.013556447   -0.11283769
## 58  Walking Downstairs      28     0.2936421  -0.022023047   -0.10858897
## 59  Walking Downstairs      29     0.2931404  -0.014941215   -0.09813400
## 60  Walking Downstairs      30     0.2831906  -0.017438390   -0.09997814
## 61    Walking Upstairs       1     0.2554617  -0.023953149   -0.09730200
## 62    Walking Upstairs       2     0.2471648  -0.021412113   -0.15251390
## 63    Walking Upstairs       3     0.2608199  -0.032410941   -0.11006486
## 64    Walking Upstairs       4     0.2708767  -0.031980430   -0.11421946
## 65    Walking Upstairs       5     0.2684595  -0.032526976   -0.10747145
## 66    Walking Upstairs       6     0.2682294  -0.027242539   -0.12208244
## 67    Walking Upstairs       7     0.2487069  -0.027563901   -0.14376969
## 68    Walking Upstairs       8     0.2588802  -0.028241514   -0.11512212
## 69    Walking Upstairs       9     0.2624365  -0.019510110   -0.12521900
## 70    Walking Upstairs      10     0.2671219  -0.014385492   -0.11818042
## 71    Walking Upstairs      11     0.2637759  -0.030315782   -0.10679722
## 72    Walking Upstairs      12     0.2729703  -0.026357490   -0.10733973
## 73    Walking Upstairs      13     0.2582039  -0.027739225   -0.12576727
## 74    Walking Upstairs      14     0.2624211  -0.020438818   -0.11227969
## 75    Walking Upstairs      15     0.2701876  -0.028752386   -0.11695246
## 76    Walking Upstairs      16     0.2559861  -0.014365495   -0.12407452
## 77    Walking Upstairs      17     0.2526048  -0.022864561   -0.12129044
## 78    Walking Upstairs      18     0.2654012  -0.022206068   -0.11265589
## 79    Walking Upstairs      19     0.2421188  -0.030398662   -0.15105461
## 80    Walking Upstairs      20     0.2520983  -0.028225805   -0.12102395
## 81    Walking Upstairs      21     0.2651945  -0.023721870   -0.12541427
## 82    Walking Upstairs      22     0.2483915  -0.026862256   -0.11755086
## 83    Walking Upstairs      23     0.2499952  -0.032384403   -0.12689062
## 84    Walking Upstairs      24     0.2698811  -0.025197943   -0.11424861
## 85    Walking Upstairs      25     0.2779954  -0.026986352   -0.12621044
## 86    Walking Upstairs      26     0.2726914  -0.028163380   -0.12194346
## 87    Walking Upstairs      27     0.2657703  -0.020095332   -0.12353044
## 88    Walking Upstairs      28     0.2620058  -0.027944394   -0.12151403
## 89    Walking Upstairs      29     0.2654231  -0.029946531   -0.11800059
## 90    Walking Upstairs      30     0.2714156  -0.025331170   -0.12469749
## 91             Sitting       1     0.2612376  -0.001308288   -0.10454418
## 92             Sitting       2     0.2770874  -0.015687994   -0.10921827
## 93             Sitting       3     0.2571976  -0.003502998   -0.09835792
## 94             Sitting       4     0.2715383  -0.007163065   -0.10587460
## 95             Sitting       5     0.2736941  -0.009900835   -0.10854030
## 96             Sitting       6     0.2767785  -0.014591162   -0.11012773
## 97             Sitting       7     0.2846746  -0.014610976   -0.12246460
## 98             Sitting       8     0.2674915  -0.006725506   -0.10446105
## 99             Sitting       9     0.2483267  -0.027016777   -0.07537847
## 100            Sitting      10     0.2706121  -0.015042682   -0.10425324
## 101            Sitting      11     0.2765902  -0.014919562   -0.11284022
## 102            Sitting      12     0.2750072  -0.015789299   -0.10630723
## 103            Sitting      13     0.2743285  -0.005877329   -0.09724651
## 104            Sitting      14     0.2799906  -0.008705655   -0.10040149
## 105            Sitting      15     0.2729034  -0.011718847   -0.11366125
## 106            Sitting      16     0.2807686  -0.010249794   -0.08914040
## 107            Sitting      17     0.2773570  -0.014156274   -0.11362409
## 108            Sitting      18     0.2772700  -0.012869128   -0.11190478
## 109            Sitting      19     0.2738303  -0.016739260   -0.10870765
## 110            Sitting      20     0.2780454  -0.014722931   -0.10836963
## 111            Sitting      21     0.2775396  -0.014400971   -0.11205789
## 112            Sitting      22     0.2735838  -0.012346845   -0.10582737
## 113            Sitting      23     0.2733513  -0.013394502   -0.10380868
## 114            Sitting      24     0.2734757  -0.013125510   -0.10304106
## 115            Sitting      25     0.2785415  -0.014769681   -0.10916044
## 116            Sitting      26     0.2582435  -0.007133645   -0.09744487
## 117            Sitting      27     0.2739413  -0.015526980   -0.10552191
## 118            Sitting      28     0.2769776  -0.018540444   -0.11151794
## 119            Sitting      29     0.2771800  -0.016630680   -0.11041182
## 120            Sitting      30     0.2683361  -0.008047313   -0.09951545
## 121           Standing       1     0.2789176  -0.016137590   -0.11060182
## 122           Standing       2     0.2779115  -0.018420827   -0.10590854
## 123           Standing       3     0.2800465  -0.014337656   -0.10162172
## 124           Standing       4     0.2804997  -0.009489111   -0.09615749
## 125           Standing       5     0.2825444  -0.007004186   -0.10217110
## 126           Standing       6     0.2803462  -0.018123633   -0.11217283
## 127           Standing       7     0.2827235  -0.014574034   -0.09977783
## 128           Standing       8     0.2796210  -0.014811307   -0.10611500
## 129           Standing       9     0.2823101  -0.020045495   -0.09527475
## 130           Standing      10     0.2766503  -0.015541860   -0.10796408
## 131           Standing      11     0.2777156  -0.017199089   -0.10868663
## 132           Standing      12     0.2774058  -0.016900254   -0.10552700
## 133           Standing      13     0.2777584  -0.016789199   -0.11212414
## 134           Standing      14     0.2805456  -0.015207813   -0.10381770
## 135           Standing      15     0.2789158  -0.018351636   -0.10591083
## 136           Standing      16     0.2834974  -0.016599967   -0.10365773
## 137           Standing      17     0.2779425  -0.017412006   -0.11143215
## 138           Standing      18     0.2784588  -0.016635352   -0.10845336
## 139           Standing      19     0.2781723  -0.015424406   -0.10904190
## 140           Standing      20     0.2780769  -0.018069505   -0.10040276
## 141           Standing      21     0.2769522  -0.016708473   -0.11041786
## 142           Standing      22     0.2790539  -0.015856321   -0.10496742
## 143           Standing      23     0.2778993  -0.017747555   -0.11059702
## 144           Standing      24     0.2803489  -0.014478534   -0.10822255
## 145           Standing      25     0.2780137  -0.016356973   -0.10735296
## 146           Standing      26     0.2811270  -0.016660831   -0.11023780
## 147           Standing      27     0.2795669  -0.016593236   -0.10783582
## 148           Standing      28     0.2777951  -0.017263512   -0.10657939
## 149           Standing      29     0.2779651  -0.017260587   -0.10865907
## 150           Standing      30     0.2771127  -0.017016389   -0.10875621
## 151             Laying       1     0.2215982  -0.040513953   -0.11320355
## 152             Laying       2     0.2813734  -0.018158740   -0.10724561
## 153             Laying       3     0.2755169  -0.018955679   -0.10130048
## 154             Laying       4     0.2635592  -0.015003184   -0.11068815
## 155             Laying       5     0.2783343  -0.018304212   -0.10793760
## 156             Laying       6     0.2486565  -0.010252917   -0.13311957
## 157             Laying       7     0.2501767  -0.020441152   -0.10136104
## 158             Laying       8     0.2612543  -0.021228173   -0.10224537
## 159             Laying       9     0.2591955  -0.020526822   -0.10754972
## 160             Laying      10     0.2802306  -0.024294484   -0.11716864
## 161             Laying      11     0.2805930  -0.017659805   -0.10878658
## 162             Laying      12     0.2601134  -0.017520392   -0.10816013
## 163             Laying      13     0.2767164  -0.020440454   -0.10433186
## 164             Laying      14     0.2332754  -0.011342465   -0.08683333
## 165             Laying      15     0.2894757  -0.016629654   -0.11853024
## 166             Laying      16     0.2742272  -0.016610351   -0.10731049
## 167             Laying      17     0.2697801  -0.016846201   -0.10700628
## 168             Laying      18     0.2746916  -0.017393768   -0.10769893
## 169             Laying      19     0.2726537  -0.017142686   -0.10898146
## 170             Laying      20     0.2395079  -0.014440628   -0.10427432
## 171             Laying      21     0.2713255  -0.018423305   -0.10325383
## 172             Laying      22     0.2799597  -0.014262986   -0.11080092
## 173             Laying      23     0.2740380  -0.021655384   -0.10425678
## 174             Laying      24     0.2728505  -0.017355521   -0.10723624
## 175             Laying      25     0.2507918  -0.018894366   -0.10042883
## 176             Laying      26     0.2716459  -0.019189573   -0.10500254
## 177             Laying      27     0.2741025  -0.017986761   -0.10769973
## 178             Laying      28     0.2759135  -0.016753786   -0.10834485
## 179             Laying      29     0.2872952  -0.017196548   -0.10946207
## 180             Laying      30     0.2810339  -0.019449410   -0.10365815
```
