# replace NA in one datatable with values in another

# code from: (23 Dec 2015)
# http://stackoverflow.com/questions/8349909/
# creating-a-function-to-replace-nas-from-one-data-frame-with-values-from-another

# Convert data.frames to data.tables (i.e. data.frames with extra powers;)
library(data.table)
fillDT <- data.table(fillDf, key=c("a", "b"))
naDT <- data.table(naDf, key=c("a", "b"))


# Merge data.tables, based on their keys (columns a & b)
outDT <- naDT[fillDT]    
#      a b  f  g f.1 g.1
# [1,] 1 3 NA  0 100  11
# [2,] 1 3 NA NA 100  11
# [3,] 1 3 NA  0 100  11
# [4,] 1 3  0  0 100  11
# [5,] 1 3  0 NA 100  11
# First 5 rows of 200 printed.

# In outDT[i, j], on the following two lines 
#   -- i is a Boolean vector indicating which rows will be operated on
#   -- j is an expression saying "(sub)assign from right column (e.g. f.1) to 
#        left column (e.g. f)
outDT[is.na(f), f:=f.1]
outDT[is.na(g), g:=g.1]

# Just keep the four columns ultimately needed   
outDT <- outDT[,list(a,b,g,f)]
#       a b  g   f
#  [1,] 1 3  0   0
#  [2,] 1 3 11   0
#  [3,] 1 3  0   0
#  [4,] 1 3 11   0
#  [5,] 1 3 11   0
# First 5 rows of 200 printed.