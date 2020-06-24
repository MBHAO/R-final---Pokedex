
rm(list=ls())

# 警告！　同時需要下載大量其他packages
# also installing the dependencies ‘tinytex’, ‘xfun’, ‘highr’, 
# ‘markdown’, ‘systemfonts’, ‘rlang’, ‘rmarkdown’, ‘knitr’, ‘base64enc’, 
# ‘nortest’, ‘gdtools’, ‘uuid’, ‘insight’, ‘tidyselect’, ‘vctrs’, 
# ‘tweenr’, ‘polyclip’, ‘RcppEigen’, ‘flextable’, ‘officer’, 
# ‘moonBook’, ‘ggiraph’, ‘reshape2’, ‘mycor’, ‘ppcor’, ‘sjlabelled’, 
# ‘sjmisc’, ‘webshot’, ‘tidyr’, ‘ggforce’, ‘ztable’

if (!require(ggiraphExtra)) {
  install.packages("ggiraphExtra")
  library(ggiraphExtra)
}

if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

if (!require(readr)) {
  install.packages("readr")
  library(readr)
}

if (!require(DT)) {
  install.packages("DT")
  library(DT)
}


url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRNNywFlctSPG5Gi_q254S8VduxnTDiKtIxZUKUeDjW_2E5Lz1D4rgXs_I9d5WdVmqZb5Bd_wrWtIi3/pub?output=csv"

poke_data <- read_csv(url)

# 除去不需要的欄位, 共18個
# 3:german_name 德文名字, 7:species 種族, 8:type_number 屬性數, 11:height_m 高度, 12:weight_kg 重量
# 13:abilities_number 能力數, 14:ability_1 能力一, 15:ability_2 能力二, 16:ability_hidden 隱藏能力, 24:catch_rate 捕獲率
# 25:base_friendship 基礎好感度, 26:base_experience 基礎經驗值, 27:growth_rate 成長速度, 28:egg_type_number 蛋屬性數
# 29:egg_type_1 蛋屬性一, 30:egg_type_2 蛋屬性二, 31:percentage_male 男性占比, 32:egg_cycles 蛋孵化週期
poke_data <- poke_data[,-c(3, 7, 8, 11:16, 24:32)]

show_pokedex <- function(data = poke_data, poke_property = "", tag = "", content = "", range = "=="){

  if (poke_property == ""){
    poke_property = 1:dim(data)[2]
  }
  
  if (tag != ""){
    if (range == "=="){
      condition <- which(data[,tag] == content)
    }else if (range == ">="){
      condition <- which(data[,tag] >= content)
    }else if (range == ">="){
      condition <- which(data[,tag] >= content)
    }else if (range == ">"){
      condition <- which(data[,tag] > content)
    }else if (range == "<"){
      condition <- which(data[,tag] < content)
    }else{
      print("Error in searching")
    }
  }
  
  data <- data[condition, poke_property]
  
  datatable(data, filter = "top", rownames = NULL)
}



a <- names(poke_data)[c(2,9:14)]


origin <- poke_data[1, c(2, 9:14)]
origin[1] <- ""
origin[2:7] <- 0

temp2 <- rbind(
  origin,
  poke_data[which(poke_data["name"] == "Mega Charizard X"), c(2, 9:14)],
  poke_data[which(poke_data["name"] == "Mega Charizard Y"), c(2, 9:14)]
  )

View(temp2)

ggRadar(data=iris, aes(group=Species))
ggRadar(data=temp2, aes(group=name), rescale = F, interactive = T, size = 2)









