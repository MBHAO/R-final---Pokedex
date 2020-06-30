
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


# 回傳搜索到的寶可夢的所有資訊 === tag 篩選的欄位(建議只用name抓, 但我沒擋其他欄位), content 篩選的內容
get_pokemon <- function(tag, content){
  return(poke_data[which(poke_data[,tag] == content),])
}


# 用datatable展示圖鑑 === data 資料, properties 想秀出的資訊, by_type 搜索的屬性
show_pokedex <- function(data = poke_data, properties = "", by_type = ""){
  
  suppressWarnings( if (properties == "") properties <- 1:dim(data)[2] )
  
  # 留下欲展示的欄位
  data <- data[, properties]
  
  # 是否要透過屬性篩選
  if (by_type != ""){
    data <- data %>% filter(type_1 == by_type | type_2 == by_type)
  }
  
  # 用DT展示圖鑑
  datatable(data, filter = "top", rownames = NULL)
}


# 用雷達圖比較兩隻寶可夢 === poke_1&2 兩隻寶可夢所有的資訊, properties 想秀出的資訊
compare_two <- function(poke_1, poke_2, properties = default_properties){
  
  # 原點, 用來當指標
  origin <- poke_data[1, c("name", properties)]
  origin[1:length(origin)] <- 0
  origin["name"] <- ""
  
  comp_data <- rbind(
    origin,
    poke_1[,c("name", properties)],
    poke_2[,c("name", properties)]
  )
  
  ggRadar(data=comp_data, aes(group=name), rescale = F, interactive = T)
}


# 透過等級隨機讓寶可夢六大數值成長 === pokemon 寶可夢所有的資訊, lv 等級
give_status <- function(pokemon, lv){
  
  # 隨機使六大數值成長
  poke_hp <- as.integer(pokemon["hp"] + sum( sample(3:5, lv, replace = T) ))
  poke_attack <- as.integer(pokemon["attack"] + sum( sample(1:2, lv, replace = T) ))
  poke_defense <- as.integer(pokemon["defense"] + sum( sample(1:3, lv, replace = T) ))
  poke_sp_attack <- as.integer(pokemon["sp_attack"] + sum( sample(1:2, lv, replace = T) ))
  poke_sp_defense <- as.integer(pokemon["sp_defense"] + sum( sample(1:3, lv, replace = T) ))
  poke_speed <- as.integer(pokemon["speed"] + sum( sample(1:3, lv, replace = T) ))
  
  # 留下需要的欄位
  temp <- data.frame(
    poke_num = pokemon["pokedex_number"],
    name = pokemon["name"],
    type_1 = pokemon["type_1"],
    type_2 = pokemon["type_2"],
    level = lv,
    
    hp = poke_hp,
    attack = poke_attack,
    defense = poke_defense,
    sp_attack = poke_sp_attack,
    sp_defense = poke_sp_defense,
    speed = poke_speed,
    stringsAsFactors = F
  )
  
  temp <- as.data.frame(append(temp, pokemon[15:32]))
  
  return(temp)
}


# 可選擇的攻擊屬性, 自己擁有的屬性機率更高
give_attack_types <- function(pokemon){
  
  # 本身屬性一
  add_type_1 <- tolower(pokemon[1, "type_1"])
  if (add_type_1 == "fighting"){
    add_type_1 <- "fight"
  }
  choosen_types <- append(all_types, rep(add_type_1, 4))
  
  # 檢查是否有屬性二
  if (!is.na(pokemon["type_2"])){
    add_type_2 <- tolower(pokemon[1, "type_2"])
    if (add_type_2 == "fighting"){
      add_type_2 <- "fight"
    }
    choosen_types <- append(choosen_types, rep(add_type_2, 4))
  }
  
  return(choosen_types)
}


# 執行動作
make_move <- function(poke_1_info, poke_2_info, move){
  
  if (move[1, "move_type"] == "attack"){
    
    # 設定防禦的方式與屬性
    attack_way <- as.character(move[1, "attack_way"])
    against_type <- paste0("against_", as.character(move["attack_type"]))
    if (attack_way == "attack"){
      defense_way <- "defense"
    }else{
      defense_way <- "sp_defense"
    }
    
    # 攻擊力基礎值
    damage <- poke_1_info[attack_way]
    
    # 傷害減去防禦力
    damage <- damage - poke_2_info[defense_way]
    
    # 計算攻擊的屬性優劣
    damage <- damage * poke_2_info[against_type]
    
    damage <- as.integer(damage)
    if (damage <= 0){
      damage <- 1
    }
    
    # 是否爆擊(爆擊為1.5倍傷害)
    if ( as.logical(move["is_critical"]) ){
      damage <- as.integer(poke_1_info[attack_way] * 1.5)
    }
    
    # 造成傷害, 避免過度斬殺
    attacked_hp <- poke_2_info["hp"] - damage
    if (attacked_hp < 0){
      poke_2_info["hp"] <- 0
    }else{
      poke_2_info["hp"] <- attacked_hp
    }
    
    # 用文字顯示過程
    msg <- paste0("[", poke_1_info[1, "name"], "] use ", move[1, "attack_type"], " type")
    if (attack_way == "attack"){
      msg <- paste0(msg, " physical")
    }else{
      msg <- paste0(msg, " special")
    }
    msg <- paste0(msg, " attack to [", poke_2_info[1, "name"], "]" )
    print(msg)
    
    if ( as.logical(move["is_critical"]) ){
      print("Critical Attack !")
    }
    msg <- paste0("[", poke_1_info[1, "name"], "] deals ", damage, " damage to [", poke_2_info[1, "name"], "]")
    print(msg)
    
    
    return(poke_2_info)
    
  }else{
    
    # 回血, 並用文字顯示過程
    poke_1_info["hp"] <- poke_1_info["hp"] + move["heal_value"]
    msg <- paste0("[", poke_1_info[1, "name"], "] heals ", move["heal_value"], " health.")
    print(msg)
    
    return(poke_1_info)
  }
}


# 選擇動作(攻擊或使用回血道具)
choose_move <- function(poke_1_info, poke_2_info){
  
  # 選擇動作, 攻擊的權重比較高
  moves <- c(rep("attack", 9), "heal")
  move <- sample(moves, 1)
  
  if (move == "attack"){
    choosen_attack_types <- give_attack_types(poke_1_info)
    
    # 選擇攻擊的屬性, 方式(物攻or特攻), 是否爆擊
    attack_move <- data.frame(
      move_type = "attack",
      attack_type = sample(choosen_attack_types, 1),
      attack_way = sample(c("attack", "sp_attack"), 1),
      is_critical = sample(1:100, 1) == 1,
      stringsAsFactors = F
    )
    
    # 執行攻擊, 回傳被攻擊寶可夢的資訊
    poke_2_info <- make_move(poke_1_info, poke_2_info, attack_move)
    return(poke_2_info)
    
  }else{
    
    # 選擇回血的數值
    heal_move <- data.frame(
      move_type = "heal",
      heal_value = sample(heal_items, 1),
      stringsAsFactors = F
    )
    
    # 執行回血, 回傳回血過後的寶可夢的資訊
    poke_1_info <- make_move(poke_1_info, poke_2_info, heal_move)
    return(poke_1_info)
  }
  
}


# 模擬寶可夢對戰, 回合制戰鬥 === poke_1 2 兩隻寶可夢所有的資訊, lv_1 2 兩隻寶可夢的等級
pokemon_battle <- function(poke_1, lv_1, poke_2, lv_2){
  
  # 根據兩者等級讓六大數值隨機成長
  poke_1_info <- give_status(poke_1, lv_1)
  poke_2_info <- give_status(poke_2, lv_2)
  
  pokemons <- rbind(poke_1_info, poke_2_info)
  
  
  # 計算行動速度
  move_priority <- as.integer(poke_1_info["speed"] - poke_2_info["speed"])
  if (move_priority >= 0){
    move_priority <- as.integer(-move_priority / 10 - 1) : move_priority
  }else{
    move_priority <- move_priority : as.integer(-move_priority / 10 + 1)
  }
  
  # 初始化
  round_num <- 0
  round_end <- TRUE
  change_turn <- FALSE
  second_move <- FALSE
  while(poke_1_info["hp"] > 0 & poke_2_info["hp"] > 0 ){
    if (round_num >= 999){
      break
    }
    
    # 新的回合開始重新抽一個誰先行動
    if (round_end){
      round_end <- FALSE
      round_num <- round_num + 1
      print("")
      print(paste0(" ***** Round ", round_num, " ***** "))
      first_move <- sample(move_priority, 1)
    }
    
    # 每次行動前印出兩者的血量
    print(paste0("[", poke_1_info[1, "name"], "] hp: ", poke_1_info["hp"], "  |||||  [",
                      poke_2_info[1, "name"], "] hp: ", poke_2_info["hp"]))
    print("")
    
    # poke_1 行動, first_move 大於0時先行動
    if ((first_move >= 0 & !second_move) | ( first_move < 0 & second_move)){
      
      change_poke_info <- choose_move(poke_1_info, poke_2_info)
      # 準備輪到下一隻寶可夢行動
      if (first_move >= 0 & !second_move){
        change_turn <- TRUE
      }
    }
    
    
    # poke_2 行動, first_move 小於0時先行動
    if ((first_move < 0 & !second_move) | ( first_move >= 0 & second_move)){
      
      change_poke_info <- choose_move(poke_2_info, poke_1_info)
      # 準備輪到下一隻寶可夢行動
      if (first_move < 0 & !second_move){
        change_turn <- TRUE
      }
    }
    
    
    # 把結果更新到原本的兩隻寶可夢上
    if (change_poke_info["name"] == poke_1_info["name"]){
      poke_1_info <- change_poke_info
    }else{
      poke_2_info <- change_poke_info
    }
    
    # 第二隻寶可夢行動完了, 這個回合結束, 準備開始新的回合
    if (second_move){
      second_move <- FALSE
      round_end <- TRUE
    }
    
    # 第一隻寶可夢行動完了, 同一回合輪到第二隻寶可夢行動
    if (change_turn){
      change_turn <- FALSE
      second_move <- TRUE
    }
  }
  
  # 跳出迴圈代表有其中一方贏了
  if (poke_1_info["hp"] == 0){
    print("")
    print(paste0("[", poke_2_info[1, "name"], "] ", "WIN !!!"))
  }else if (poke_2_info["hp"] == 0){
    print("")
    print(paste0("[", poke_1_info[1, "name"], "] ", "WIN !!!"))
  }else{
    print("")
    print("Draw !!!")
  }
  
  return(pokemons)
}


all_types <- c( "normal", "fire", "water", "electric", "grass", "ice",
                "fight", "poison", "ground", "flying", "psychic", "bug",
                "rock", "ghost", "dragon", "dark", "steel", "fairy" )

heal_items <- c(rep(5, 90), rep(10, 9), 20)

default_properties <- c("hp", "attack", "defense", "sp_attack", "sp_defense", "speed")






# 測試結果


# 用datatable展示圖鑑 === data 資料, properties 想秀出的資訊, by_type 搜索的屬性
# show_pokedex(data = poke_data, properties = "", by_type = "")
show_pokedex(properties = c(1:14))
show_pokedex(properties = c(1:14), by_type = "Fire")


# 回傳搜索到的寶可夢的所有資訊 === tag 篩選的欄位(建議只用name抓, 但我沒擋其他欄位), content 篩選的內容
# get_pokemon(tag, content)
# 用雷達圖比較兩隻寶可夢 === poke_1&2 兩隻寶可夢所有的資訊, properties 想秀出的資訊
# compare_two(poke_1, poke_2, properties = 9:14)
poke_1 <- get_pokemon(tag = "name", content = "Mega Blastoise")
poke_2 <- get_pokemon(tag = "name", content = "Mega Charizard X")
compare_two(poke_1, poke_2)


# 模擬寶可夢對戰, 回合制戰鬥 === poke_1 2 兩隻寶可夢所有的資訊, lv_1 2 兩隻寶可夢的等級
# pokemon_battle(poke_1, lv_1, poke_2, lv_2)
pokemons <- pokemon_battle(poke_1, 10, poke_2, 10)
View(pokemons)
compare_two(pokemons[1,], pokemons[2,])




