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


key_value_list <- listify(lines)
EprimeFrame(key_value_list)
EprimeFrame(New = "New Attribute", Running = "Running")
EprimeFrame(key_value_list, Practice = TRUE)

