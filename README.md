# R-final---Pokedex

## 寶可夢圖鑑

**完成:**
  * 讀取資料 來源:https://www.kaggle.com/mariotormo/complete-pokemon-dataset-updated-090420?select=pokedex_%28Update_05.20%29.csv
    * 先從kaggle把csv檔抓到goole雲端上，公開後使用readr套件中的read_csv讀取檔案
  * 搜尋功能
    * 可以選擇要顯示的欄位
    * 可以透過屬性先篩選一波(不過必須要留下屬性欄位，不然也無法篩)
    * 最後使用DT的datatable展示圖鑑，同時在上方提供filter功能
  * 雷達圖展示兩隻寶可夢基本數值
    * 使用ggiraphExtra中的ggRadar畫出雷達圖，同時透過ggplot2中的aes來做分組
  * 對戰功能，雖然平衡很有問題
    * 等級，隨機成長六大屬性
    * 回合制對戰，speed越大越快行動，不論速度差距多大，速度較小的那一方都有機率搶先行動，速度差會影響這個機率
      * 補充: 內含一個防止打到天荒地老的停止點，預設是1000回合未分出結果設為平手
    * 選擇行動，目前有兩種行動模式，攻擊或使用回血道具
      * 攻擊，隨機選擇所有屬性的攻擊，使用自己擁有的屬性攻擊的機率較高，隨機選擇是物理攻擊或特殊攻擊，有低機率爆擊
      * 回血，自己要設定好每個回血道具的數值與機率，因為平衡很難搞
    * 執行行動
      * 攻擊，傷害值的平衡也是個大問題，為了避免傷害太高一下就打死人，或是打了1000回合打不完
        * 目前傷害計算公式: 攻擊方的攻擊力當基礎值，先減去防守方的防禦力 (會根據前面選擇個攻擊方式調整物理或特殊)
        * 　　　　　　　　  乘上屬性優劣的加成，這時會保底至少一點傷害 (雖然沒看出什麼效果)
        * 　　　　　　　　  判斷是否爆擊，爆擊設為1.5倍傷害，結果一定會是整數 (無條件捨去小數點)
        * 防守方血量減去傷害值，避免過度擊殺，血量最低為0
      * 回血，隨機選擇一個回血數值，回血可超出初始最大血量
