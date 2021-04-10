## I'm getting some weird behavior where the here() function is not working properly.
## But the setwd() command works when I set the exact same working directory as the here() package says it is.
## If I run the exact same here() command after running the setwd() command, it works fine.
## I'm starting with a clean global environment

## To make this a full reprex, you can clone the repo at https://github.com/don-radcliffe/nps_fx.

## Single hashtags denote R's output in response to a command, double hashtags are my writing.


##### First try of here() doesn't open file connection ######

library(here)
# here() starts at C:/ProgramR/nps_fx
trees <- read.csv(here('data/data_raw/trees_raw.csv'))
# [1] "C:/ProgramR/nps_fx/data"
trees <- read.csv('data_raw/trees_raw.csv')
# Error in file(file, "rt") : cannot open the connection
# In addition: Warning message:
  # In file(file, "rt") :
  # cannot open file 'data_raw/trees_raw.csv': No such file or directory


##### setwd() successfully opens file connection with the exact same path ######

## Sorry Jenny Bryan
setwd('C:/ProgramR/nps_fx/data')
trees <- read.csv('data_raw/trees_raw.csv')
head(trees, 2)
#Date Monitoring.Status MacroPlot.Name MacroPlot.UTM.Zone MacroPlot.UTM.Y MacroPlot.UTM.X
#1 5/29/1996 0:00             00Pre   FPSME2D08-21                11U         5357384          668304
#2 5/29/1996 0:00             00Pre   FPSME2D08-21                11U         5357384          668304


##### here() successfully open file connection after the setwd() command is called

here('data')
trees <- read.csv('data_raw/trees_raw.csv')
head(trees, 2)
#Date Monitoring.Status MacroPlot.Name MacroPlot.UTM.Zone MacroPlot.UTM.Y MacroPlot.UTM.X
#1 5/29/1996 0:00             00Pre   FPSME2D08-21                11U         5357384          668304
#2 5/29/1996 0:00             00Pre   FPSME2D08-21                11U         5357384          668304



##### Session Information ######
sessionInfo()
#R version 4.0.5 (2021-03-31)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 10 x64 (build 19041)

#Matrix products: default

#locale:
#  [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252   
#[3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
#[5] LC_TIME=English_United States.1252    

#attached base packages:
#  [1] stats     graphics  grDevices utils     datasets  methods   base     

#other attached packages:
#  [1] here_1.0.1

#loaded via a namespace (and not attached):
#  [1] compiler_4.0.5  rprojroot_2.0.2 tools_4.0.5    