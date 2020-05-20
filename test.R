
if (!require("data.table")) {
  install.packages("data.table")
  library("data.table")
}

DT <- data.table(airquality)

DT
#      Ozone Solar.R Wind Temp Month Day
#   1:    41     190  7.4   67     5   1
#   2:    36     118  8.0   72     5   2
#   3:    12     149 12.6   74     5   3
#   4:    18     313 11.5   62     5   4
#   5:    NA      NA 14.3   56     5   5
#  ---                                  
# 149:    30     193  6.9   70     9  26
# 150:    NA     145 13.2   77     9  27
# 151:    14     191 14.3   75     9  28
# 152:    18     131  8.0   76     9  29
# 153:    20     223 11.5   68     9  30

setkey(DT, Month, Day)

DT[.(5)]
#     Ozone Solar.R Wind Temp Month Day
#  1:    41     190  7.4   67     5   1
#  2:    36     118  8.0   72     5   2
#  3:    12     149 12.6   74     5   3
#  4:    18     313 11.5   62     5   4
#  5:    NA      NA 14.3   56     5   5
# ---  
# 27:    NA      NA  8.0   57     5  27
# 28:    23      13 12.0   67     5  28
# 29:    45     252 14.9   81     5  29
# 30:   115     223  5.7   79     5  30
# 31:    37     279  7.4   76     5  31
#     Ozone Solar.R Wind Temp Month Day

DT[.(5, 3)]
#     Ozone Solar.R Wind Temp Month Day
#  1:    12     149 12.6   74     5   3