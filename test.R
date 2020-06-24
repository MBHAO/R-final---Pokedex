
rm(list=ls())

# 下載ggiraphExtra時, 同時需要下載大量的其他packages
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


# tag 篩選的欄位(建議只用name抓, 但我沒擋其他欄位), content 篩選的內容
get_pokemon <- function(tag, content){
  return(poke_data[which(poke_data[,tag] == content),])
}


# data 資料, properties 想秀出的屬性, tag 篩選的欄位, content 篩選的內容, range 範圍
show_pokedex <- function(data = poke_data, properties = "", tag = "", content = "", range = "=="){

  suppressWarnings( if (properties == "") properties <- 1:dim(data)[2] )
  condition = 1:dim(data)[1]
  
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
  
  data <- data[condition, properties]
  
  datatable(data, filter = "top", rownames = NULL)
}


# poke_1&2 兩隻寶可夢所有的屬性, properties 想秀出的屬性
compare_two <- function(poke_1, poke_2, properties = 9:14){
  
  origin <- poke_data[1, c(2, properties)]
  origin[1:length(origin)] <- 0
  origin["name"] <- ""
  
  comp_data <- rbind(
    origin,
    poke_1[,c(2, properties)],
    poke_2[,c(2, properties)]
  )
  
  ggRadar(data=comp_data, aes(group=name), rescale = F, interactive = T)
}




# 測試結果

show_pokedex(properties = c(1:14))

poke_1 <- get_pokemon(tag = "name", content = "Mega Venusaur")
poke_2 <- get_pokemon(tag = "name", content = "Mega Charizard X")
compare_two(poke_1, poke_2)








