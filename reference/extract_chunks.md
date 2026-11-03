# Extract log-frames from an Eprime log file

Almost all of the information in an Eprime file comes in chunks of text
bracketed by the lines `*** LogFrame Start ***` and
`*** LogFrame End ***`. The exception is the header information which is
bracketed by `*** Header Start ***` and `*** Header End ***`.

## Usage

``` r
extract_chunks(eprime_log)
```

## Arguments

- eprime_log:

  a character vector containing the lines of text from an Eprime txt
  file

## Value

a list of character vectors, where each vector contains the lines of a
log-frame

## Details

`extract_chunks` extracts the bracketed text, storing each log-frame of
text in a list. The lists also include some additional lines of text as
metadata: `Eprime.FrameNumber` and `Eprime.Basename` (the name of the
source file). The header log-frame also gets dummy lines:
`Procedure: Header` and `Running: Header`.

These character vectors of colon-separated lines are converted into
proper lists by
[`FrameList()`](https://www.tjmahr.com/rprime/reference/FrameList.md).

## Examples

``` r
lines <- read_eprime(example_eprime_file("MINP"))
extract_chunks(lines)
#> [[1]]
#>  [1] "*** Header Start ***"               "VersionPersist: 1"                 
#>  [3] "LevelName: Session"                 "LevelName: Block"                  
#>  [5] "LevelName: Trial"                   "LevelName: SubTrial"               
#>  [7] "LevelName: LogLevel5"               "LevelName: LogLevel6"              
#>  [9] "LevelName: LogLevel7"               "LevelName: LogLevel8"              
#> [11] "LevelName: LogLevel9"               "LevelName: LogLevel10"             
#> [13] "Experiment: SAE_MinimalPairDiscrim" "SessionDate: 07-10-2013"           
#> [15] "SessionTime: 10:46:22"              "SessionTimeUtc: 3:46:22 PM"        
#> [17] "Dialect: Yes"                       "Subject: 001"                      
#> [19] "ExpTyp: L"                          "Age: 00"                           
#> [21] "Gender: X"                          "Session: 1"                        
#> [23] "RandomSeed: -261296366"             "Group: 1"                          
#> [25] "Display.RefreshRate: 60.003"        "Running: Header"                   
#> [27] "Procedure: Header"                  "Eprime.FrameNumber: 1"             
#> [29] "Eprime.Basename: MINP_001L00XS1"    "*** Header End ***"                
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[2]]
#>  [1] "\t*** LogFrame Start ***"        "\tProcedure: FamTask"           
#>  [3] "\titem1: bear"                   "\titem2: chair"                 
#>  [5] "\tCorrectResponse: bear"         "\tImageSide: Left"              
#>  [7] "\tDuration: 885"                 "\tFamiliarization: 1"           
#>  [9] "\tFamInforcer: 1"                "\tReinforcerImage: Bicycle1"    
#> [11] "\tFamiliarization.Cycle: 1"      "\tFamiliarization.Sample: 1"    
#> [13] "\tRunning: Familiarization"      "\tFamTarget.RESP: "             
#> [15] "\tCorrect: True"                 "Eprime.FrameNumber: 2"          
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[3]]
#>  [1] "\t*** LogFrame Start ***"        "\tProcedure: FamTask"           
#>  [3] "\titem1: hat"                    "\titem2: cat"                   
#>  [5] "\tCorrectResponse: cat"          "\tImageSide: Right"             
#>  [7] "\tDuration: 853"                 "\tFamiliarization: 2"           
#>  [9] "\tFamInforcer: 1"                "\tReinforcerImage: Bicycle1"    
#> [11] "\tFamiliarization.Cycle: 1"      "\tFamiliarization.Sample: 2"    
#> [13] "\tRunning: Familiarization"      "\tFamTarget.RESP: "             
#> [15] "\tCorrect: True"                 "Eprime.FrameNumber: 3"          
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[4]]
#>  [1] "\t*** LogFrame Start ***"        "\tProcedure: DiscTask"          
#>  [3] "\titem1: man"                    "\titem2: moon"                  
#>  [5] "\tCorrectResponse: moon"         "\tImageSide: Right"             
#>  [7] "\tDuration: 1045"                "\tFamiliarization: 3"           
#>  [9] "\tFamInforcer: 1"                "\tReinforcerImage: Bicycle1"    
#> [11] "\tFamiliarization.Cycle: 1"      "\tFamiliarization.Sample: 3"    
#> [13] "\tRunning: Familiarization"      "\tCorrect: True"                
#> [15] "\tTarget.RESP: "                 "Eprime.FrameNumber: 4"          
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[5]]
#>  [1] "\t*** LogFrame Start ***"        "\tProcedure: DiscTask"          
#>  [3] "\titem1: hat"                    "\titem2: cat"                   
#>  [5] "\tCorrectResponse: hat"          "\tImageSide: Left"              
#>  [7] "\tDuration: 877"                 "\tFamiliarization: 4"           
#>  [9] "\tFamInforcer: 1"                "\tReinforcerImage: Bicycle1"    
#> [11] "\tFamiliarization.Cycle: 1"      "\tFamiliarization.Sample: 4"    
#> [13] "\tRunning: Familiarization"      "\tCorrect: False"               
#> [15] "\tTarget.RESP: "                 "Eprime.FrameNumber: 5"          
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[6]]
#>  [1] "\t*** LogFrame Start ***"        "\tProcedure: DiscTask"          
#>  [3] "\titem1: man"                    "\titem2: moon"                  
#>  [5] "\tCorrectResponse: man"          "\tImageSide: Left"              
#>  [7] "\tDuration: 967"                 "\tFamiliarization: 5"           
#>  [9] "\tFamInforcer: 1"                "\tReinforcerImage: Bicycle1"    
#> [11] "\tFamiliarization.Cycle: 1"      "\tFamiliarization.Sample: 5"    
#> [13] "\tRunning: Familiarization"      "\tCorrect: True"                
#> [15] "\tTarget.RESP: "                 "Eprime.FrameNumber: 6"          
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[7]]
#>  [1] "\t*** LogFrame Start ***"        "\tProcedure: DiscTask"          
#>  [3] "\titem1: bear"                   "\titem2: chair"                 
#>  [5] "\tCorrectResponse: chair"        "\tImageSide: Right"             
#>  [7] "\tDuration: 828"                 "\tFamiliarization: 6"           
#>  [9] "\tFamInforcer: 1"                "\tReinforcerImage: Bicycle1"    
#> [11] "\tFamiliarization.Cycle: 1"      "\tFamiliarization.Sample: 6"    
#> [13] "\tRunning: Familiarization"      "\tCorrect: False"               
#> [15] "\tTarget.RESP: "                 "Eprime.FrameNumber: 7"          
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[8]]
#>  [1] "\t*** LogFrame Start ***"        "\tTest: 5"                      
#>  [3] "\tProcedure: DiscTask"           "\titem1: keys"                  
#>  [5] "\titem2: cheese"                 "\tCorrectResponse: keys"        
#>  [7] "\tImageSide: Left"               "\tDuration: 1062"               
#>  [9] "\tReinforcerImage: Bicycle1"     "\tReinforcer: 1"                
#> [11] "\tRunning: Test"                 "\tCorrect: False"               
#> [13] "\tTarget.RESP: "                 "\tTest.Cycle: 1"                
#> [15] "\tTest.Sample: 1"                "Eprime.FrameNumber: 8"          
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[9]]
#>  [1] "\t*** LogFrame Start ***"        "\tTest: 21"                     
#>  [3] "\tProcedure: DiscTask"           "\titem1: keys"                  
#>  [5] "\titem2: peas"                   "\tCorrectResponse: peas"        
#>  [7] "\tImageSide: Right"              "\tDuration: 991"                
#>  [9] "\tReinforcerImage: Bicycle2"     "\tReinforcer: 2"                
#> [11] "\tRunning: Test"                 "\tCorrect: True"                
#> [13] "\tTarget.RESP: "                 "\tTest.Cycle: 1"                
#> [15] "\tTest.Sample: 2"                "Eprime.FrameNumber: 9"          
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[10]]
#>  [1] "\t*** LogFrame Start ***"        "\tTest: 20"                     
#>  [3] "\tProcedure: DiscTask"           "\titem1: keys"                  
#>  [5] "\titem2: cheese"                 "\tCorrectResponse: cheese"      
#>  [7] "\tImageSide: Right"              "\tDuration: 1253"               
#>  [9] "\tReinforcerImage: Bicycle3"     "\tReinforcer: 3"                
#> [11] "\tRunning: Test"                 "\tCorrect: False"               
#> [13] "\tTarget.RESP: "                 "\tTest.Cycle: 1"                
#> [15] "\tTest.Sample: 3"                "Eprime.FrameNumber: 10"         
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[11]]
#>  [1] "\t*** LogFrame Start ***"        "\tTest: 23"                     
#>  [3] "\tProcedure: DiscTask"           "\titem1: mouse"                 
#>  [5] "\titem2: moose"                  "\tCorrectResponse: moose"       
#>  [7] "\tImageSide: Right"              "\tDuration: 1104"               
#>  [9] "\tReinforcerImage: Bicycle4"     "\tReinforcer: 4"                
#> [11] "\tRunning: Test"                 "\tCorrect: True"                
#> [13] "\tTarget.RESP: "                 "\tTest.Cycle: 1"                
#> [15] "\tTest.Sample: 4"                "Eprime.FrameNumber: 11"         
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[12]]
#>  [1] "\t*** LogFrame Start ***"        "\tTest: 11"                     
#>  [3] "\tProcedure: DiscTask"           "\titem1: pig"                   
#>  [5] "\titem2: big"                    "\tCorrectResponse: pig"         
#>  [7] "\tImageSide: Left"               "\tDuration: 835"                
#>  [9] "\tReinforcerImage: Bicycle4"     "\tReinforcer: 5"                
#> [11] "\tRunning: Test"                 "\tCorrect: False"               
#> [13] "\tTarget.RESP: "                 "\tTest.Cycle: 1"                
#> [15] "\tTest.Sample: 5"                "Eprime.FrameNumber: 12"         
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[13]]
#>  [1] "\t*** LogFrame Start ***"        "\tTest: 4"                      
#>  [3] "\tProcedure: DiscTask"           "\titem1: horse"                 
#>  [5] "\titem2: house"                  "\tCorrectResponse: horse"       
#>  [7] "\tImageSide: Left"               "\tDuration: 838"                
#>  [9] "\tReinforcerImage: Bicycle5"     "\tReinforcer: 6"                
#> [11] "\tRunning: Test"                 "\tCorrect: True"                
#> [13] "\tTarget.RESP: "                 "\tTest.Cycle: 1"                
#> [15] "\tTest.Sample: 6"                "Eprime.FrameNumber: 13"         
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[14]]
#>  [1] "\t*** LogFrame Start ***"        "\tTest: 1"                      
#>  [3] "\tProcedure: DiscTask"           "\titem1: bee"                   
#>  [5] "\titem2: key"                    "\tCorrectResponse: bee"         
#>  [7] "\tImageSide: Left"               "\tDuration: 758"                
#>  [9] "\tReinforcerImage: Bicycle6"     "\tReinforcer: 7"                
#> [11] "\tRunning: Test"                 "\tCorrect: False"               
#> [13] "\tTarget.RESP: "                 "\tTest.Cycle: 1"                
#> [15] "\tTest.Sample: 7"                "Eprime.FrameNumber: 14"         
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[15]]
#>  [1] "\t*** LogFrame Start ***"        "\tTest: 9"                      
#>  [3] "\tProcedure: DiscTask"           "\titem1: mouse"                 
#>  [5] "\titem2: mouth"                  "\tCorrectResponse: mouse"       
#>  [7] "\tImageSide: Left"               "\tDuration: 958"                
#>  [9] "\tReinforcerImage: Bicycle7"     "\tReinforcer: 8"                
#> [11] "\tRunning: Test"                 "\tCorrect: False"               
#> [13] "\tTarget.RESP: "                 "\tTest.Cycle: 1"                
#> [15] "\tTest.Sample: 8"                "Eprime.FrameNumber: 15"         
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[16]]
#>  [1] "\t*** LogFrame Start ***"        "\tTest: 25"                     
#>  [3] "\tProcedure: DiscTask"           "\titem1: pen"                   
#>  [5] "\titem2: hen"                    "\tCorrectResponse: hen"         
#>  [7] "\tImageSide: Right"              "\tDuration: 808"                
#>  [9] "\tReinforcerImage: Bicycle8"     "\tReinforcer: 9"                
#> [11] "\tRunning: Test"                 "\tCorrect: True"                
#> [13] "\tTarget.RESP: "                 "\tTest.Cycle: 1"                
#> [15] "\tTest.Sample: 9"                "Eprime.FrameNumber: 16"         
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[17]]
#>  [1] "\t*** LogFrame Start ***"        "\tTest: 30"                     
#>  [3] "\tProcedure: DiscTask"           "\titem1: jar"                   
#>  [5] "\titem2: car"                    "\tCorrectResponse: car"         
#>  [7] "\tImageSide: Right"              "\tDuration: 924"                
#>  [9] "\tReinforcerImage: Bicycle9"     "\tReinforcer: 10"               
#> [11] "\tRunning: Test"                 "\tCorrect: True"                
#> [13] "\tTarget.RESP: "                 "\tTest.Cycle: 1"                
#> [15] "\tTest.Sample: 10"               "Eprime.FrameNumber: 17"         
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[18]]
#>  [1] "\t*** LogFrame Start ***"        "\tTest: 7"                      
#>  [3] "\tProcedure: DiscTask"           "\titem1: moose"                 
#>  [5] "\titem2: juice"                  "\tCorrectResponse: moose"       
#>  [7] "\tImageSide: Left"               "\tDuration: 1104"               
#>  [9] "\tReinforcerImage: Bicycle10"    "\tReinforcer: 11"               
#> [11] "\tRunning: Test"                 "\tCorrect: False"               
#> [13] "\tTarget.RESP: "                 "\tTest.Cycle: 1"                
#> [15] "\tTest.Sample: 11"               "Eprime.FrameNumber: 18"         
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[19]]
#>  [1] "\t*** LogFrame Start ***"        "\tTest: 18"                     
#>  [3] "\tProcedure: DiscTask"           "\titem1: hold"                  
#>  [5] "\titem2: cold"                   "\tCorrectResponse: cold"        
#>  [7] "\tImageSide: Right"              "\tDuration: 795"                
#>  [9] "\tReinforcerImage: Bicycle11"    "\tReinforcer: 12"               
#> [11] "\tRunning: Test"                 "\tCorrect: True"                
#> [13] "\tTarget.RESP: "                 "\tTest.Cycle: 1"                
#> [15] "\tTest.Sample: 12"               "Eprime.FrameNumber: 19"         
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[20]]
#>  [1] "\t*** LogFrame Start ***"        "\tTest: 27"                     
#>  [3] "\tProcedure: DiscTask"           "\titem1: sick"                  
#>  [5] "\titem2: sit"                    "\tCorrectResponse: sit"         
#>  [7] "\tImageSide: Right"              "\tDuration: 851"                
#>  [9] "\tReinforcerImage: Bicycle12"    "\tReinforcer: 13"               
#> [11] "\tRunning: Test"                 "\tCorrect: True"                
#> [13] "\tTarget.RESP: "                 "\tTest.Cycle: 1"                
#> [15] "\tTest.Sample: 13"               "Eprime.FrameNumber: 20"         
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[21]]
#>  [1] "\t*** LogFrame Start ***"        "\tTest: 3"                      
#>  [3] "\tProcedure: DiscTask"           "\titem1: hold"                  
#>  [5] "\titem2: cold"                   "\tCorrectResponse: hold"        
#>  [7] "\tImageSide: Left"               "\tDuration: 931"                
#>  [9] "\tReinforcerImage: Bicycle12"    "\tReinforcer: 14"               
#> [11] "\tRunning: Test"                 "\tCorrect: False"               
#> [13] "\tTarget.RESP: "                 "\tTest.Cycle: 1"                
#> [15] "\tTest.Sample: 14"               "Eprime.FrameNumber: 21"         
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[22]]
#>  [1] "\t*** LogFrame Start ***"        "\tTest: 15"                     
#>  [3] "\tProcedure: DiscTask"           "\titem1: jar"                   
#>  [5] "\titem2: car"                    "\tCorrectResponse: jar"         
#>  [7] "\tImageSide: Left"               "\tDuration: 833"                
#>  [9] "\tReinforcerImage: Bicycle13"    "\tReinforcer: 15"               
#> [11] "\tRunning: Test"                 "\tCorrect: False"               
#> [13] "\tTarget.RESP: "                 "\tTest.Cycle: 1"                
#> [15] "\tTest.Sample: 15"               "Eprime.FrameNumber: 22"         
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[23]]
#>  [1] "\t*** LogFrame Start ***"        "\tTest: 19"                     
#>  [3] "\tProcedure: DiscTask"           "\titem1: horse"                 
#>  [5] "\titem2: house"                  "\tCorrectResponse: house"       
#>  [7] "\tImageSide: Right"              "\tDuration: 796"                
#>  [9] "\tReinforcerImage: Bicycle14"    "\tReinforcer: 16"               
#> [11] "\tRunning: Test"                 "\tCorrect: False"               
#> [13] "\tTarget.RESP: "                 "\tTest.Cycle: 1"                
#> [15] "\tTest.Sample: 16"               "Eprime.FrameNumber: 23"         
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[24]]
#>  [1] "\t*** LogFrame Start ***"        "\tTest: 6"                      
#>  [3] "\tProcedure: DiscTask"           "\titem1: keys"                  
#>  [5] "\titem2: peas"                   "\tCorrectResponse: keys"        
#>  [7] "\tImageSide: Left"               "\tDuration: 1062"               
#>  [9] "\tReinforcerImage: Bicycle15"    "\tReinforcer: 17"               
#> [11] "\tRunning: Test"                 "\tCorrect: False"               
#> [13] "\tTarget.RESP: "                 "\tTest.Cycle: 1"                
#> [15] "\tTest.Sample: 17"               "Eprime.FrameNumber: 24"         
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[25]]
#>  [1] "\t*** LogFrame Start ***"        "\tTest: 14"                     
#>  [3] "\tProcedure: DiscTask"           "\titem1: store"                 
#>  [5] "\titem2: star"                   "\tCorrectResponse: store"       
#>  [7] "\tImageSide: Left"               "\tDuration: 1075"               
#>  [9] "\tReinforcerImage: Bicycle16"    "\tReinforcer: 18"               
#> [11] "\tRunning: Test"                 "\tCorrect: False"               
#> [13] "\tTarget.RESP: "                 "\tTest.Cycle: 1"                
#> [15] "\tTest.Sample: 18"               "Eprime.FrameNumber: 25"         
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[26]]
#>  [1] "\t*** LogFrame Start ***"        "\tTest: 8"                      
#>  [3] "\tProcedure: DiscTask"           "\titem1: mouse"                 
#>  [5] "\titem2: moose"                  "\tCorrectResponse: mouse"       
#>  [7] "\tImageSide: Left"               "\tDuration: 958"                
#>  [9] "\tReinforcerImage: Bicycle17"    "\tReinforcer: 19"               
#> [11] "\tRunning: Test"                 "\tCorrect: False"               
#> [13] "\tTarget.RESP: "                 "\tTest.Cycle: 1"                
#> [15] "\tTest.Sample: 19"               "Eprime.FrameNumber: 26"         
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[27]]
#>  [1] "\t*** LogFrame Start ***"        "\tTest: 28"                     
#>  [3] "\tProcedure: DiscTask"           "\titem1: sleep"                 
#>  [5] "\titem2: sweep"                  "\tCorrectResponse: sweep"       
#>  [7] "\tImageSide: Right"              "\tDuration: 1350"               
#>  [9] "\tReinforcerImage: Bicycle18"    "\tReinforcer: 20"               
#> [11] "\tRunning: Test"                 "\tCorrect: True"                
#> [13] "\tTarget.RESP: "                 "\tTest.Cycle: 1"                
#> [15] "\tTest.Sample: 20"               "Eprime.FrameNumber: 27"         
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[28]]
#>  [1] "\t*** LogFrame Start ***"        "\tTest: 10"                     
#>  [3] "\tProcedure: DiscTask"           "\titem1: pen"                   
#>  [5] "\titem2: hen"                    "\tCorrectResponse: pen"         
#>  [7] "\tImageSide: Left"               "\tDuration: 855"                
#>  [9] "\tReinforcerImage: Bicycle19"    "\tReinforcer: 21"               
#> [11] "\tRunning: Test"                 "\tCorrect: False"               
#> [13] "\tTarget.RESP: "                 "\tTest.Cycle: 1"                
#> [15] "\tTest.Sample: 21"               "Eprime.FrameNumber: 28"         
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[29]]
#>  [1] "\t*** LogFrame Start ***"        "\tTest: 13"                     
#>  [3] "\tProcedure: DiscTask"           "\titem1: sleep"                 
#>  [5] "\titem2: sweep"                  "\tCorrectResponse: sleep"       
#>  [7] "\tImageSide: Left"               "\tDuration: 1201"               
#>  [9] "\tReinforcerImage: Bicycle20"    "\tReinforcer: 22"               
#> [11] "\tRunning: Test"                 "\tCorrect: True"                
#> [13] "\tTarget.RESP: "                 "\tTest.Cycle: 1"                
#> [15] "\tTest.Sample: 22"               "Eprime.FrameNumber: 29"         
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[30]]
#>  [1] "\t*** LogFrame Start ***"        "\tTest: 29"                     
#>  [3] "\tProcedure: DiscTask"           "\titem1: store"                 
#>  [5] "\titem2: star"                   "\tCorrectResponse: star"        
#>  [7] "\tImageSide: Right"              "\tDuration: 1387"               
#>  [9] "\tReinforcerImage: Bicycle20"    "\tReinforcer: 23"               
#> [11] "\tRunning: Test"                 "\tCorrect: True"                
#> [13] "\tTarget.RESP: "                 "\tTest.Cycle: 1"                
#> [15] "\tTest.Sample: 23"               "Eprime.FrameNumber: 30"         
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[31]]
#>  [1] "\t*** LogFrame Start ***"        "\tTest: 2"                      
#>  [3] "\tProcedure: DiscTask"           "\titem1: goose"                 
#>  [5] "\titem2: juice"                  "\tCorrectResponse: goose"       
#>  [7] "\tImageSide: Left"               "\tDuration: 870"                
#>  [9] "\tReinforcerImage: Bicycle21"    "\tReinforcer: 24"               
#> [11] "\tRunning: Test"                 "\tCorrect: False"               
#> [13] "\tTarget.RESP: "                 "\tTest.Cycle: 1"                
#> [15] "\tTest.Sample: 24"               "Eprime.FrameNumber: 31"         
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[32]]
#>  [1] "\t*** LogFrame Start ***"        "\tTest: 12"                     
#>  [3] "\tProcedure: DiscTask"           "\titem1: sick"                  
#>  [5] "\titem2: sit"                    "\tCorrectResponse: sick"        
#>  [7] "\tImageSide: Left"               "\tDuration: 1047"               
#>  [9] "\tReinforcerImage: Bicycle22"    "\tReinforcer: 25"               
#> [11] "\tRunning: Test"                 "\tCorrect: False"               
#> [13] "\tTarget.RESP: "                 "\tTest.Cycle: 1"                
#> [15] "\tTest.Sample: 25"               "Eprime.FrameNumber: 32"         
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[33]]
#>  [1] "\t*** LogFrame Start ***"        "\tTest: 22"                     
#>  [3] "\tProcedure: DiscTask"           "\titem1: moose"                 
#>  [5] "\titem2: juice"                  "\tCorrectResponse: juice"       
#>  [7] "\tImageSide: Right"              "\tDuration: 864"                
#>  [9] "\tReinforcerImage: Bicycle23"    "\tReinforcer: 26"               
#> [11] "\tRunning: Test"                 "\tCorrect: True"                
#> [13] "\tTarget.RESP: "                 "\tTest.Cycle: 1"                
#> [15] "\tTest.Sample: 26"               "Eprime.FrameNumber: 33"         
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[34]]
#>  [1] "\t*** LogFrame Start ***"        "\tTest: 24"                     
#>  [3] "\tProcedure: DiscTask"           "\titem1: mouse"                 
#>  [5] "\titem2: mouth"                  "\tCorrectResponse: mouth"       
#>  [7] "\tImageSide: Right"              "\tDuration: 1030"               
#>  [9] "\tReinforcerImage: Bicycle24"    "\tReinforcer: 27"               
#> [11] "\tRunning: Test"                 "\tCorrect: True"                
#> [13] "\tTarget.RESP: "                 "\tTest.Cycle: 1"                
#> [15] "\tTest.Sample: 27"               "Eprime.FrameNumber: 34"         
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[35]]
#>  [1] "\t*** LogFrame Start ***"        "\tTest: 17"                     
#>  [3] "\tProcedure: DiscTask"           "\titem1: goose"                 
#>  [5] "\titem2: juice"                  "\tCorrectResponse: juice"       
#>  [7] "\tImageSide: Right"              "\tDuration: 864"                
#>  [9] "\tReinforcerImage: Bicycle24"    "\tReinforcer: 28"               
#> [11] "\tRunning: Test"                 "\tCorrect: True"                
#> [13] "\tTarget.RESP: "                 "\tTest.Cycle: 1"                
#> [15] "\tTest.Sample: 28"               "Eprime.FrameNumber: 35"         
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[36]]
#>  [1] "\t*** LogFrame Start ***"        "\tTest: 16"                     
#>  [3] "\tProcedure: DiscTask"           "\titem1: bee"                   
#>  [5] "\titem2: key"                    "\tCorrectResponse: key"         
#>  [7] "\tImageSide: Right"              "\tDuration: 917"                
#>  [9] "\tReinforcerImage: Bicycle25"    "\tReinforcer: 29"               
#> [11] "\tRunning: Test"                 "\tCorrect: True"                
#> [13] "\tTarget.RESP: "                 "\tTest.Cycle: 1"                
#> [15] "\tTest.Sample: 29"               "Eprime.FrameNumber: 36"         
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[37]]
#>  [1] "\t*** LogFrame Start ***"        "\tTest: 26"                     
#>  [3] "\tProcedure: DiscTask"           "\titem1: pig"                   
#>  [5] "\titem2: big"                    "\tCorrectResponse: big"         
#>  [7] "\tImageSide: Right"              "\tDuration: 737"                
#>  [9] "\tReinforcerImage: Bicycle26"    "\tReinforcer: 30"               
#> [11] "\tRunning: Test"                 "\tCorrect: True"                
#> [13] "\tTarget.RESP: "                 "\tTest.Cycle: 1"                
#> [15] "\tTest.Sample: 30"               "Eprime.FrameNumber: 37"         
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
#> [[38]]
#>  [1] "*** LogFrame Start ***"             "Experiment: SAE_MinimalPairDiscrim"
#>  [3] "SessionDate: 07-10-2013"            "SessionTime: 10:46:22"             
#>  [5] "SessionTimeUtc: 3:46:22 PM"         "Dialect: Yes"                      
#>  [7] "Subject: 001"                       "ExpTyp: L"                         
#>  [9] "Age: 00"                            "Gender: X"                         
#> [11] "Session: 1"                         "RandomSeed: -261296366"            
#> [13] "Group: 1"                           "Display.RefreshRate: 60.003"       
#> [15] "Eprime.FrameNumber: 38"             "Eprime.Basename: MINP_001L00XS1"   
#> [17] "*** LogFrame End ***"              
#> attr(,"class")
#> [1] "EprimeChunk" "character"  
#> 
```
