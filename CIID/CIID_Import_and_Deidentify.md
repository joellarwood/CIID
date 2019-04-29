Describe Clinical Data
================
Joel Larwood
29/04/2019

Raw data file imported (hard coded from Joel Larwoods device) to show names and retained variables. Along with descriptives of removed clinical variables.

Variables that are removed have been done so to prevent demographic information being matched with answers

``` r
library(tidyverse)
library(knitr)
library(sjmisc)
library(sjlabelled)
```

``` r
raw <- read_csv("~/Desktop/Data File Masters/Mater Arts in Helath/MYAHCB arts project data.csv")
names(raw)
```

    ##   [1] "id"                              "code"                           
    ##   [3] "Diabetes"                        "Autoimmune_IBD"                 
    ##   [5] "Gynaecology"                     "Chronicpain"                    
    ##   [7] "Cancers"                         "Epilepsy"                       
    ##   [9] "Autism"                          "Other"                          
    ##  [11] "HealthCond"                      "program"                        
    ##  [13] "condition"                       "gender"                         
    ##  [15] "age"                             "education"                      
    ##  [17] "ethnicity"                       "ethnic_identity"                
    ##  [19] "ATSI"                            "emotionlexicon_1"               
    ##  [21] "emotionwords_1"                  "TAS1_1"                         
    ##  [23] "TAS2_1"                          "TAS3_1"                         
    ##  [25] "TAS4_1"                          "TAS5_1"                         
    ##  [27] "TAS6_1"                          "TAS7_1"                         
    ##  [29] "TAS8_1"                          "TAS9_1"                         
    ##  [31] "TAS10_1"                         "TAS11_1"                        
    ##  [33] "TAS12_1"                         "TAS13_1"                        
    ##  [35] "TAS14_1"                         "TAS15_1"                        
    ##  [37] "TAS16_1"                         "TAS17_1"                        
    ##  [39] "TAS18_1"                         "TAS19_1"                        
    ##  [41] "TAS20_1"                         "DERS6_1_1"                      
    ##  [43] "DERS6_2_1"                       "DERS6_4_1"                      
    ##  [45] "DERS6_5_1"                       "DERS6_6_1"                      
    ##  [47] "Instrument_or_choir"             "Hours_listening_music"          
    ##  [49] "Days_a_week_listen_music"        "Song"                           
    ##  [51] "Like_about_song_music"           "Art_familiar_comfortable"       
    ##  [53] "How_often_make_art"              "Express_recently"               
    ##  [55] "Manage_thoughts_and_feelings"    "Express_easier_than_words"      
    ##  [57] "Arts_subject"                    "Own_art"                        
    ##  [59] "Express_youself"                 "Hours_for_making_art"           
    ##  [61] "Days_for_making_art"             "Material"                       
    ##  [63] "Art_change"                      "If_yes"                         
    ##  [65] "Made_podcast_1"                  "Doing_creative_things_1"        
    ##  [67] "Express_through_recording_1"     "Hours_making_sound_recordings_1"
    ##  [69] "Days_making_sound_recordings_1"  "emotionlexicon_2"               
    ##  [71] "emotionwords_2"                  "MUSE1_1"                        
    ##  [73] "MUSE2_1"                         "MUSE3_1"                        
    ##  [75] "MUSE4_1"                         "MUSE5_1"                        
    ##  [77] "MUSE6_1"                         "MUSE7_1"                        
    ##  [79] "MUSE8_1"                         "MUSE9_1"                        
    ##  [81] "MUSE10_1"                        "MUSE1_2"                        
    ##  [83] "MUSE2_2"                         "MUSE3_2"                        
    ##  [85] "MUSE4_2"                         "MUSE5_2"                        
    ##  [87] "MUSE6_2"                         "MUSE7_2"                        
    ##  [89] "MUSE8_2"                         "MUSE9_2"                        
    ##  [91] "MUSE10_2"                        "Family_identity_1"              
    ##  [93] "Friends_identity_1"              "School_identity_1"              
    ##  [95] "Work_identity_1"                 "Health_identity_1"              
    ##  [97] "Arts_identity_1"                 "Podcast_Identity1_1"            
    ##  [99] "Podcast_identity2_1"             "Podcast_identity3_1"            
    ## [101] "Podcast_identity4_1"             "Podcast_identity5_1"            
    ## [103] "Podcast_identity6_1"             "VAR00018"                       
    ## [105] "Family_identity_2"               "Friends_identity_2"             
    ## [107] "School_identity_2"               "Work_identity_2"                
    ## [109] "Health_identity_2"               "Arts_identity_2"                
    ## [111] "Program_Evaluation1_2"           "Program_Evaluation2_2"          
    ## [113] "Program_Evaluation3_2"           "Program_Evaluation4_2"          
    ## [115] "Program_Evaluation5_2"           "Program_Evaluation6_2"          
    ## [117] "Podcast_identity1_2"             "Podcast_identity2_2"            
    ## [119] "Podcast_identity3_2"             "Podcast_identity4_2"            
    ## [121] "Podcast_identity5_2"             "Podcast_identity6_2"            
    ## [123] "TAS4_1R"                         "TAS5_1R"                        
    ## [125] "TAS10_1R"                        "TAS18_1R"                       
    ## [127] "TAS19_1R"                        "Alexithymia_1"                  
    ## [129] "Diff_describ_feel_1"             "Diff_identif_feel_1"            
    ## [131] "Extern_orient_think_1"           "TAS4_2R"                        
    ## [133] "TAS5_2R"                         "TAS10_2R"                       
    ## [135] "TAS18_2R"                        "TAS19_2R"                       
    ## [137] "Alexithymia_2"                   "Diff_describ_feel_2"            
    ## [139] "Diff_identif_feel_2"             "Extern_orient_think_2"          
    ## [141] "TAS1_2"                          "TAS2_2"                         
    ## [143] "TAS3_2"                          "TAS4_2"                         
    ## [145] "TAS5_2"                          "TAS6_2"                         
    ## [147] "TAS7_2"                          "TAS8_2"                         
    ## [149] "TAS9_2"                          "TAS10_2"                        
    ## [151] "TAS11_2"                         "TAS12_2"                        
    ## [153] "TAS13_2"                         "TAS14_2"                        
    ## [155] "TAS15_2"                         "TAS16_2"                        
    ## [157] "TAS17_2"                         "TAS18_2"                        
    ## [159] "TAS19_2"                         "TAS20_2"                        
    ## [161] "DERS6_1_2"                       "DERS6_2_2"                      
    ## [163] "DERS6_4_2"                       "DERS6_5_2"                      
    ## [165] "DERS6_6_2"                       "idcode"                         
    ## [167] "Aqol1_1"                         "Aqol2_1"                        
    ## [169] "Aqol3_1"                         "Aqol4_1"                        
    ## [171] "Aqol5_1"                         "Aqol6_1"                        
    ## [173] "Aqol7_1"                         "Aqol8_1"                        
    ## [175] "Aqol9_1"                         "Aqol10_1"                       
    ## [177] "Aqol11_1"                        "Aqol12_1"                       
    ## [179] "Aqol13_1"                        "Aqol14_1"                       
    ## [181] "Aqol15_1"                        "Aqol16_1"                       
    ## [183] "Aqol17_1"                        "Aqol18_1"                       
    ## [185] "Aqol19_1"                        "Aqol20_1"                       
    ## [187] "idcodes"                         "Aqol1_2"                        
    ## [189] "Aqol2_2"                         "Aqol3_2"                        
    ## [191] "Aqol4_2"                         "Aqol5_2"                        
    ## [193] "Aqol6_2"                         "Aqol7_2"                        
    ## [195] "Aqol8_2"                         "Aqol9_2"                        
    ## [197] "Aqol10_2"                        "Aqol11_2"                       
    ## [199] "Aqol12_2"                        "Aqol13_2"                       
    ## [201] "Aqol14_2"                        "Aqol15_2"                       
    ## [203] "Aqol16_2"                        "Aqol17_2"                       
    ## [205] "Aqol18_2"                        "Aqol19_2"                       
    ## [207] "Aqol20_2"                        "Aqol_IL_std_1"                  
    ## [209] "Aqol_Rel_std_1"                  "Aqol_MH_std_1"                  
    ## [211] "Aqol_Cope_std_1"                 "Aqol_Pain_std_1"                
    ## [213] "Aqol_Sens_std_1"                 "Aqol_TOT_std_1"                 
    ## [215] "Aqol_IL_std_2"                   "Aqol_Rel_std_2"                 
    ## [217] "Aqol_MH_std_2"                   "Aqol_Cope_std_2"                
    ## [219] "Aqol_Pain_std_2"                 "Aqol_Sens_std_2"                
    ## [221] "Aqol_TOT_std_2"                  "DERS5_total_1"                  
    ## [223] "DERS5_total_2\\"

missing data

``` r
condense <- raw %>% filter (id > 0) %>% select(("id"),
                                              contains("emotion"),
                                               contains("TAS"),
                                               contains("MUSE"),
                                               contains("Aqol"),
                                               contains("DERS"),
                                               contains("music"),
                                               contains("song"),
                                               -contains("_2"))

obs <- raw %>% filter (id > 0) %>% nrow()

missvar <- data.frame(nacount=colSums(is.na(condense)))
missvar$pctNA <- (missvar$nacount/obs)*100

missp <- data.frame(row.names = condense$id, nacount=rowSums(is.na(condense)))

missp$pctNA <- (missp$nacount/ncol(condense))*100
```

``` r
knitr::kable(missvar)
```

|                              |  nacount|      pctNA|
|------------------------------|--------:|----------:|
| id                           |        0|   0.000000|
| emotionlexicon\_1            |        7|  19.444444|
| emotionwords\_1              |        9|  25.000000|
| TAS1\_1                      |        0|   0.000000|
| TAS2\_1                      |        0|   0.000000|
| TAS3\_1                      |        0|   0.000000|
| TAS4\_1                      |        0|   0.000000|
| TAS5\_1                      |        0|   0.000000|
| TAS6\_1                      |        0|   0.000000|
| TAS7\_1                      |        0|   0.000000|
| TAS8\_1                      |        0|   0.000000|
| TAS9\_1                      |        0|   0.000000|
| TAS10\_1                     |        0|   0.000000|
| TAS11\_1                     |        0|   0.000000|
| TAS12\_1                     |        0|   0.000000|
| TAS13\_1                     |        0|   0.000000|
| TAS14\_1                     |        0|   0.000000|
| TAS15\_1                     |        0|   0.000000|
| TAS16\_1                     |        0|   0.000000|
| TAS17\_1                     |        0|   0.000000|
| TAS18\_1                     |        0|   0.000000|
| TAS19\_1                     |        0|   0.000000|
| TAS20\_1                     |        0|   0.000000|
| TAS4\_1R                     |        0|   0.000000|
| TAS5\_1R                     |        0|   0.000000|
| TAS10\_1R                    |        0|   0.000000|
| TAS18\_1R                    |        0|   0.000000|
| TAS19\_1R                    |        0|   0.000000|
| MUSE1\_1                     |        3|   8.333333|
| MUSE2\_1                     |        3|   8.333333|
| MUSE3\_1                     |        3|   8.333333|
| MUSE4\_1                     |        3|   8.333333|
| MUSE5\_1                     |        3|   8.333333|
| MUSE6\_1                     |        3|   8.333333|
| MUSE7\_1                     |        3|   8.333333|
| MUSE8\_1                     |        3|   8.333333|
| MUSE9\_1                     |        3|   8.333333|
| MUSE10\_1                    |        3|   8.333333|
| Aqol1\_1                     |        2|   5.555556|
| Aqol2\_1                     |        2|   5.555556|
| Aqol3\_1                     |        2|   5.555556|
| Aqol4\_1                     |        3|   8.333333|
| Aqol5\_1                     |        1|   2.777778|
| Aqol6\_1                     |        1|   2.777778|
| Aqol7\_1                     |        2|   5.555556|
| Aqol8\_1                     |        1|   2.777778|
| Aqol9\_1                     |        1|   2.777778|
| Aqol10\_1                    |        1|   2.777778|
| Aqol11\_1                    |        2|   5.555556|
| Aqol12\_1                    |        1|   2.777778|
| Aqol13\_1                    |        1|   2.777778|
| Aqol14\_1                    |        1|   2.777778|
| Aqol15\_1                    |        2|   5.555556|
| Aqol16\_1                    |        2|   5.555556|
| Aqol17\_1                    |        2|   5.555556|
| Aqol18\_1                    |        2|   5.555556|
| Aqol19\_1                    |        1|   2.777778|
| Aqol20\_1                    |        2|   5.555556|
| Aqol\_IL\_std\_1             |        2|   5.555556|
| Aqol\_Rel\_std\_1            |        1|   2.777778|
| Aqol\_MH\_std\_1             |        1|   2.777778|
| Aqol\_Cope\_std\_1           |        1|   2.777778|
| Aqol\_Pain\_std\_1           |        3|   8.333333|
| Aqol\_Sens\_std\_1           |        1|   2.777778|
| Aqol\_TOT\_std\_1            |        1|   2.777778|
| DERS6\_1\_1                  |        0|   0.000000|
| DERS6\_4\_1                  |        0|   0.000000|
| DERS6\_5\_1                  |        0|   0.000000|
| DERS6\_6\_1                  |        0|   0.000000|
| DERS5\_total\_1              |        0|   0.000000|
| Hours\_listening\_music      |        5|  13.888889|
| Days\_a\_week\_listen\_music |        5|  13.888889|
| Like\_about\_song\_music     |        7|  19.444444|
| Song                         |        7|  19.444444|

``` r
knitr::kable(missp)
```

|     |  nacount|      pctNA|
|-----|--------:|----------:|
| 1   |        0|   0.000000|
| 2   |        1|   1.351351|
| 3   |        3|   4.054054|
| 4   |        1|   1.351351|
| 5   |        0|   0.000000|
| 6   |        0|   0.000000|
| 7   |       16|  21.621622|
| 8   |       27|  36.486486|
| 9   |        4|   5.405405|
| 10  |        5|   6.756757|
| 11  |       14|  18.918919|
| 12  |        0|   0.000000|
| 13  |       14|  18.918919|
| 14  |        1|   1.351351|
| 15  |        2|   2.702703|
| 16  |        0|   0.000000|
| 19  |        4|   5.405405|
| 20  |        0|   0.000000|
| 22  |        0|   0.000000|
| 23  |        0|   0.000000|
| 24  |        1|   1.351351|
| 25  |        0|   0.000000|
| 26  |        2|   2.702703|
| 27  |        0|   0.000000|
| 28  |        0|   0.000000|
| 30  |        2|   2.702703|
| 31  |        2|   2.702703|
| 32  |        2|   2.702703|
| 33  |        2|   2.702703|
| 34  |        2|   2.702703|
| 35  |        0|   0.000000|
| 36  |        0|   0.000000|
| 37  |        0|   0.000000|
| 38  |        1|   1.351351|
| 39  |        5|   6.756757|
| 40  |        1|   1.351351|

Descriptives

``` r
condition <- raw %>% filter (id > 0) %>%  select(Diabetes, Autoimmune_IBD, Gynaecology, Chronicpain, Cancers, Epilepsy, Autism, Other) %>% add_labels(labels =c("No" = 0, "Yes" = 1))

gender <- raw %>% filter (id > 0) %>%  select(gender) %>% add_labels(labels = c("Male" = 1, "Female" = 2))

sjmisc::frq(condition) 
```

    ## 
    ## # Diabetes <numeric> 
    ## # total N=36  valid N=35  mean=0.37  sd=0.49
    ##  
    ##  val label frq raw.prc valid.prc cum.prc
    ##    0    No  22   61.11     62.86   62.86
    ##    1   Yes  13   36.11     37.14  100.00
    ##   NA    NA   1    2.78        NA      NA
    ## 
    ## # Autoimmune_IBD <numeric> 
    ## # total N=36  valid N=35  mean=0.17  sd=0.38
    ##  
    ##  val label frq raw.prc valid.prc cum.prc
    ##    0    No  29   80.56     82.86   82.86
    ##    1   Yes   6   16.67     17.14  100.00
    ##   NA    NA   1    2.78        NA      NA
    ## 
    ## # Gynaecology <numeric> 
    ## # total N=36  valid N=35  mean=0.09  sd=0.28
    ##  
    ##  val label frq raw.prc valid.prc cum.prc
    ##    0    No  32   88.89     91.43   91.43
    ##    1   Yes   3    8.33      8.57  100.00
    ##   NA    NA   1    2.78        NA      NA
    ## 
    ## # Chronicpain <numeric> 
    ## # total N=36  valid N=35  mean=0.14  sd=0.36
    ##  
    ##  val label frq raw.prc valid.prc cum.prc
    ##    0    No  30   83.33     85.71   85.71
    ##    1   Yes   5   13.89     14.29  100.00
    ##   NA    NA   1    2.78        NA      NA
    ## 
    ## # Cancers <numeric> 
    ## # total N=36  valid N=35  mean=0.03  sd=0.17
    ##  
    ##  val label frq raw.prc valid.prc cum.prc
    ##    0    No  34   94.44     97.14   97.14
    ##    1   Yes   1    2.78      2.86  100.00
    ##   NA    NA   1    2.78        NA      NA
    ## 
    ## # Epilepsy <numeric> 
    ## # total N=36  valid N=35  mean=0.06  sd=0.24
    ##  
    ##  val label frq raw.prc valid.prc cum.prc
    ##    0    No  33   91.67     94.29   94.29
    ##    1   Yes   2    5.56      5.71  100.00
    ##   NA    NA   1    2.78        NA      NA
    ## 
    ## # Autism <numeric> 
    ## # total N=36  valid N=35  mean=0.06  sd=0.24
    ##  
    ##  val label frq raw.prc valid.prc cum.prc
    ##    0    No  33   91.67     94.29   94.29
    ##    1   Yes   2    5.56      5.71  100.00
    ##   NA    NA   1    2.78        NA      NA
    ## 
    ## # Other <numeric> 
    ## # total N=36  valid N=35  mean=0.29  sd=0.46
    ##  
    ##  val label frq raw.prc valid.prc cum.prc
    ##    0    No  25   69.44     71.43   71.43
    ##    1   Yes  10   27.78     28.57  100.00
    ##   NA    NA   1    2.78        NA      NA

``` r
sjmisc::frq(gender)
```

    ## 
    ## # gender <numeric> 
    ## # total N=36  valid N=36  mean=1.72  sd=0.45
    ##  
    ##  val  label frq raw.prc valid.prc cum.prc
    ##    1   Male  10   27.78     27.78   27.78
    ##    2 Female  26   72.22     72.22  100.00
    ##   NA     NA   0    0.00        NA      NA

``` r
table(raw$age)
```

    ## 
    ## 16 17 18 19 20 21 22 23 24 26 27 
    ##  2  1  7  4  3  5  4  3  1  3  2

``` r
mean(raw$age, na.rm = T)
```

    ## [1] 20.82857

``` r
sd(raw$age, na.rm = T)
```

    ## [1] 3.062925

Export condenses data set which will be analysed

``` r
write_csv(condense, path = "CIID_PublicData.csv")
```
