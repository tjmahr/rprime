
#### listify

lines <- c("\t*** LogFrame Start ***",
           "\tProcedure: PracticeProc",
           "\tStimulus1: toothbrush",
           "\t*** LogFrame End ***")
str(listify(lines))
# List of 2
# $ Procedure: chr "PracticeProc"
# $ Stimulus1: chr "toothbrush"

lines2 <- c("Item: 1",
            "  WhiteSpaceItem: Stuff\t",
            "AnotherItem: Two Colons: Yes",
            "",
            "Last Item: Whatever")
str(listify(lines2))
# List of 4
# $ Item          : chr "1"
# $ WhiteSpaceItem: chr "Stuff"
# $ AnotherItem   : chr "Two Colons: Yes"
# $ Last Item     : chr "Whatever"




#### make_eprime_frames

lines <- c("\t*** LogFrame Start ***",
           "\tProcedure: FamTask",
           "\titem1: bear",
           "\titem2: chair",
           "\tCorrectResponse: bear",
           "\tImageSide: Left",
           "\tDuration: 885",
           "\tFamiliarization: 1",
           "\tFamInforcer: 1",
           "\tReinforcerImage: Bicycle1",
           "\tFamiliarization.Cycle: 1",
           "\tFamiliarization.Sample: 1",
           "\tRunning: Familiarization",
           "\tFamTarget.RESP: ",
           "\tCorrect: True",
           "\t*** LogFrame End ***")

make_eprime_frames(lines)
# List of 16
# $ Eprime.LevelName  : chr "Familiarization_1"
# $ Eprime.Level      : num 2
# $ Eprime.Basename   : logi NA
# $ Eprime.FrameNumber: logi NA
# $ Procedure         : chr "FamTask"
# $ Running           : chr "Familiarization"
# $ item1             : chr "bear"
# $ item2             : chr "chair"
# $ CorrectResponse   : chr "bear"
# $ ImageSide         : chr "Left"
# $ Duration          : chr "885"
# $ FamInforcer       : chr "1"
# $ ReinforcerImage   : chr "Bicycle1"
# $ Cycle             : chr "1"
# $ Sample            : chr "1"
# $ Correct           : chr "True"
# - attr(*, "class")= chr [1:2] "EprimeFrame" "list"

# Not tidied: The value of "Running" still appears in "Familiarization.Cycle"
# and "Familiarization.Sample" and none ofthe "Eprime." metadata attributes have
# been updated.
make_eprime_frames(lines, tidy = FALSE)
# List of 17
# $ Eprime.LevelName      : logi NA
# $ Eprime.Level          : num 2
# $ Eprime.Basename       : logi NA
# $ Eprime.FrameNumber    : logi NA
# $ Procedure             : chr "FamTask"
# $ Running               : chr "Familiarization"
# $ item1                 : chr "bear"
# $ item2                 : chr "chair"
# $ CorrectResponse       : chr "bear"
# $ ImageSide             : chr "Left"
# $ Duration              : chr "885"
# $ Familiarization       : chr "1"
# $ FamInforcer           : chr "1"
# $ ReinforcerImage       : chr "Bicycle1"
# $ Familiarization.Cycle : chr "1"
# $ Familiarization.Sample: chr "1"
# $ Correct               : chr "True"
# - attr(*, "class")= chr [1:2] "EprimeFrame" "list"




#### EprimeFrame

key_value_list <- list(
  Procedure = "FamTask",
  item1 = "bear",
  item2 = "chair",
  CorrectResponse = "bear",
  Familiarization.Cycle = "1",
  Familiarization.Sample = "1",
  Running = "Familiarization",
  Correct = "True")

EprimeFrame(key_value_list)
# List of 12
# $ Eprime.LevelName      : logi NA
# $ Eprime.Level          : logi NA
# $ Eprime.Basename       : logi NA
# $ Eprime.FrameNumber    : logi NA
# $ Procedure             : chr "FamTask"
# $ Running               : chr "Familiarization"
# $ item1                 : chr "bear"
# $ item2                 : chr "chair"
# $ CorrectResponse       : chr "bear"
# $ Familiarization.Cycle : chr "1"
# $ Familiarization.Sample: chr "1"
# $ Correct               : chr "True"
# - attr(*, "class")= chr [1:2] "EprimeFrame" "list"

# Additional parameters are treated as a list
EprimeFrame(Procedure = "Demo", Running = "Practice", Scored = FALSE)
# List of 12
# $ Eprime.LevelName      : logi NA
# $ Eprime.Level          : logi NA
# $ Eprime.Basename       : logi NA
# $ Eprime.FrameNumber    : logi NA
# $ Procedure             : chr "FamTask"
# $ Running               : chr "Familiarization"
# $ item1                 : chr "bear"
# $ item2                 : chr "chair"
# $ CorrectResponse       : chr "bear"
# $ Familiarization.Cycle : chr "1"
# $ Familiarization.Sample: chr "1"
# $ Correct               : chr "True"
# - attr(*, "class")= chr [1:2] "EprimeFrame" "list"

# The additional parameters overwrite values in the list
EprimeFrame(key_value_list, Procedure = "Demo", Running = "Practice")
# List of 12
# $ Eprime.LevelName      : logi NA
# $ Eprime.Level          : logi NA
# $ Eprime.Basename       : logi NA
# $ Eprime.FrameNumber    : logi NA
# $ Procedure             : chr "Demo"
# $ Running               : chr "Practice"
# $ item1                 : chr "bear"
# $ item2                 : chr "chair"
# $ CorrectResponse       : chr "bear"
# $ Familiarization.Cycle : chr "1"
# $ Familiarization.Sample: chr "1"
# $ Correct               : chr "True"
# - attr(*, "class")= chr [1:2] "EprimeFrame" "list"
