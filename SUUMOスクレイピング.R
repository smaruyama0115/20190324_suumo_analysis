setwd("/Users/maruyamashouhei/R/20190320_SUUMO分析/")

#install.packages("geosphere", dependencies = c("Depends", "Suggests"))

library(magrittr)
library(tidyverse)
library(rvest)
library(leaflet)
library(geosphere)
library(mlr)
library(DataExplorer)

#purrrの優先度を上げる----
unload_package <- function(pkg_name) {
  packages <- Filter(function(x) stringr::str_detect(x, "^package:"), search())
  packages <- Map(function(x) stringr::str_replace(x, "^package:", ""), packages)
  packages <- unlist(unname(packages))
  
  if(!(pkg_name %in% packages)) {
    return(pkg_name)
  }
  
  result_packages <- pkg_name
  while(TRUE) {
    tryCatch({
      detach(paste0("package:", pkg_name), character.only = TRUE)
      break
    }, error = function(e) {
      required_package <- stringr::str_match(e$message, pattern = "required by ‘(.+?)’")[1, 2]
      required_packages <- unload_package(required_package)
      result_packages <<- c(result_packages, required_packages)
    })
  }
  unique(result_packages)
}

prior_package <- function(pkg_name) {
  pkg_name <- as.character(substitute(pkg_name))
  pkg_names <- unload_package(pkg_name)
  for (pkg_name in pkg_names) {
    suppressPackageStartupMessages(library(pkg_name, character.only = TRUE))
  }
}

prior_package(purrr)

# # 家賃リストの取得 ----
# read_html ("https://suumo.jp/chintai/tokyo/ek_27320/") %>% 
#   html_nodes(xpath="//span[@class = 'cassetteitem_other-emphasis ui-text--bold']") %>% 
#   html_text
#   #html_nodes(xpath="//span")

input_html <- read_html("https://suumo.jp/chintai/tokyo/ek_27320/")

# 賃貸マンションのdivを取得 ----
list_chintai <-
  input_html %>% 
  html_nodes(xpath="//div[@class='cassetteitem']")

# マンション名の取得 ----
list_name <-
  list_chintai %>% 
  html_nodes(xpath="//div[@class='cassetteitem_content-title']") %>% 
  html_text

# # マンションの家賃リストの取得 ----
# list_yachin <-
#   list_chintai %>% 
#   html_nodes(x=.,xpath="//span[@class = 'cassetteitem_other-emphasis ui-text--bold']")

# マンション毎のテーブルを取得 ----
list_table <-
  input_html %>% 
  html_nodes(".cassetteitem_other") %>% 
  html_table

# テーブルの整形----
list_table %>%
  map(
    ~ .x %>% 
      .[,c(3,4,5,6)] %>% 
      rename(
        floor = "階"
      ) %>% 
      separate(
        col  = "賃料/管理費",
        into = c("rent","admin"),
        sep  = "\r\n\t\t\t\t\t\t\t\t\t\t\t\t"
      ) %>% 
      separate(
        col  = "敷金/礼金",
        into = c("deposit","key_money"),
        sep  = "\r\n\t\t\t\t\t\t\t\t\t\t\t\t"
      ) %>% 
      separate(
        col  = "間取り/専有面積",
        into = c("floor_plan","area"),
        sep  = "\r\n\t\t\t\t\t\t\t\t\t\t\t\t"
      ) 
  ) 

# ここから本格的に作る----

# suumoのルートアドレス ----
url_root = "https://suumo.jp"

# output用のデータフレームを用意
df_output <- data.frame(
  name=NA,
  url=NA,
  rent=NA,
  admin=NA,
  deposit=NA,
  key_money=NA,
  shozaichi=NA,
  ekitoho_1=NA,
  ekitoho_2=NA,
  ekitoho_3=NA,
  floor_plan=NA,
  floor_plan_detail=NA,
  area=NA,
  age=NA,
  floor=NA,
  muki=NA,
  kouzou=NA,
  lat=NA,
  lng=NA,
  details=NA
)

# 賃貸一覧のforループを回す(中野坂上はmax193)----
for(tmp_pnz in seq(1,193)){
    
  # 賃貸一覧のループが何周目かを表示
  str_c("tmp_pnz = ",tmp_pnz," ------------------------------------------------------------------------") %>% print
  
  # 賃貸一覧のurlを取得----
  chintai_ichiran_url <- str_c(url_root,"/chintai/tokyo/ek_27320/pnz1",tmp_pnz,".html?rn=0015")
  
  # 賃貸一覧のhtmlを取得----
  chintai_ichiran_html <- read_html(chintai_ichiran_url)
  
  # 賃貸一覧から賃貸詳細のurlをリスト化----
  list_chintai_details_url <-
    chintai_ichiran_html %>% 
    html_nodes(xpath = "//a[contains(text(),'詳細を見る')]") %>% 
    html_attr("href") %>% 
    map(~str_c(url_root,.x))
  
  # 賃貸の詳細を確認----
  for(tmp_chintai_details_url in list_chintai_details_url){
  
    # テスト用----
    #tmp_chintai_details_url = "https://suumo.jp/chintai/jnc_000047813144/?bc=100150429352"
    
    # 読み込んでいるurlをプリント----
    print(str_c("reading : ",tmp_chintai_details_url))
    
    # 賃貸詳細のhtml読み込み
    chintai_details_html <- tryCatch(
        expr  = read_html(tmp_chintai_details_url)
      , error = function(e){print(e)}
    ) 
    if(inherits(chintai_details_html,"error")) next
    
    # 名前----
    name <-
      chintai_details_html %>% 
      html_nodes(".section_h1-header-title") %>% 
      html_text  
      
    # 家賃----
    rent <-
      chintai_details_html %>% 
      html_nodes(".property_view_note-emphasis") %>% 
      html_text
  
    # 管理費・共益費----
    admin <-
      chintai_details_html %>% 
      html_nodes(xpath="//div[@class='property_view_note-list']/span[contains(text(),'管理費')]") %>% 
      html_text %>% 
      str_remove("管理費・共益費:\\s")
  
    # 敷金----
    deposit <-
      chintai_details_html %>% 
      html_nodes(xpath="//div[@class='property_view_note-list']/span[contains(text(),'敷金')]") %>% 
      html_text %>% 
      str_remove("敷金:\\s")   
  
    # 礼金----
    key_money <-
      chintai_details_html %>% 
      html_nodes(xpath="//div[@class='property_view_note-list']/span[contains(text(),'礼金')]") %>% 
      html_text %>% 
      str_remove("礼金:\\s")   

    # 所在地----
    shozaichi <-
      chintai_details_html %>% 
      html_nodes(xpath="//th[text()='所在地']/following-sibling::td[position()=1]") %>% 
      html_text
    
    # 駅徒歩----
    ekitoho_list <-
      chintai_details_html %>% 
      html_nodes(xpath="//th[text()='駅徒歩']/following-sibling::td[position()=1]/div") %>% 
      html_text
    
    ekitoho_1 <- ekitoho_list[1]
    ekitoho_2 <- ekitoho_list[2]
    ekitoho_3 <- ekitoho_list[3]

    # 間取り----
    floor_plan <-
      chintai_details_html %>% 
      html_nodes(xpath="//th[text()='間取り']/following-sibling::td[position()=1]") %>% 
      html_text
  
    # 専有面積----
    area <-
      chintai_details_html %>% 
      html_nodes(xpath="//th[text()='専有面積']/following-sibling::td[position()=1]") %>% 
      html_text    
  
    # 築年数----
    age <-
      chintai_details_html %>% 
      html_nodes(xpath="//th[text()='築年数']/following-sibling::td[position()=1]") %>% 
      html_text    
  
    # 階----
    floor <-
      chintai_details_html %>% 
      html_nodes(xpath="//th[text()='階']/following-sibling::td[position()=1]") %>% 
      html_text  
    
    # 向き----
    muki <-
      chintai_details_html %>% 
      html_nodes(xpath="//th[text()='向き']/following-sibling::td[position()=1]") %>% 
      html_text      
 
    # 間取り詳細----
    floor_plan_detail <-
      chintai_details_html %>% 
      html_nodes(xpath="//table[@class='data_table table_gaiyou']/tr/th[contains(text(),'間取り詳細')]/following-sibling::td[position()=1]") %>% 
      html_text %>%
      str_remove_all("(\r|\n|\t)+")
    
    # 構造----
    kouzou <-
      chintai_details_html %>% 
      html_nodes(xpath="//table[@class='data_table table_gaiyou']/tr/th[contains(text(),'構造')]/following-sibling::td[position()=1]") %>% 
      html_text %>% 
      str_remove_all("(\r|\n|\t)+")
    
    # 物件の詳細----
    details <-
      chintai_details_html %>% 
      html_nodes(xpath= "//div[@class='bgc-wht ol-g']/ul/li") %>% 
      html_text
    
    # マップのURL読み込み----
    map_html <- tryCatch(
        expr  = {
          chintai_details_html %>% 
            html_nodes(xpath="//img[contains(@alt,'地図・周辺環境') and contains(@src,'tab_bkdt-around.gif')]/parent::a") %>% 
            html_attr("href") %>%
            str_c(url_root,.) %>% 
            read_html
        }
      , error = function(e){print(e)}
    ) 
    if(inherits(map_html,"error")) next
       
    # マップの緯度経度情報の読み込み----
    lat_and_lng <-
      map_html %>% 
      html_nodes("#js-gmapData")
    
    lat <-
      lat_and_lng %>%
      as.character %>% 
      str_extract("\"lat\": \\d*\\.\\d*") %>% 
      str_remove("\"lat\": ")
    
    lng <-
      lat_and_lng %>%
      as.character %>% 
      str_extract("\"lng\": \\d*\\.\\d*") %>% 
      str_remove("\"lng\": ")
    
    if(length(details)==0) details = NA
    if(length(lat)==0) lat = NA
    if(length(lng)==0) lng = NA
    
    # テスト用プリント
    # print(name)
    # print(tmp_chintai_details_url)
    # print(rent)
    # print(admin)
    # print(deposit)
    # print(key_money)
    # print(shozaichi)
    # print(ekitoho_1)
    # print(ekitoho_2)
    # print(ekitoho_3)
    # print(floor_plan)
    # print(floor_plan_detail)
    # print(area)
    # print(age)
    # print(floor)
    # print(muki)
    # print(kouzou)
    # print(lat)
    # print(lng)
    # print(details)
  
    # output用データフレームに情報を追加
    df_output %<>%
      rbind(
        list(
          name,
          tmp_chintai_details_url,
          rent,
          admin,
          deposit,
          key_money,
          shozaichi,
          ekitoho_1,
          ekitoho_2,
          ekitoho_3,
          floor_plan,
          floor_plan_detail,
          area,
          age,
          floor,
          muki,
          kouzou,
          lat,
          lng,
          details
        )
      )
    # サーバーに負荷をかけすぎないためのタイマー
    Sys.sleep(2)
  }
}

# 間違って消さないようにコピーを作っておく
df_output2 <- df_output

# 中野坂上の緯度経度の情報を入れた変数を作る
lnglat_nakanosakaue = c(139.682223,35.697064)

# 前処理
df_output3 <-
  df_output2 %>% 
  filter(!is.na(name)) %>% 
  mutate(
      rent       = rent    %>% str_remove("万円") %>% as.numeric
    , rent       = rent*10000
    , flag_teikishakuya = admin %>% str_detect("\\s※定期借家") %>% as.numeric
    , admin      = admin %>% str_remove("\\s※定期借家") %>% str_remove("円") %>% str_replace("-","0") %>% as.numeric
    , deposit    = deposit %>% str_replace("-","0") %>% str_remove("万円") %>% as.numeric
    , deposit    = deposit*10000
    , key_money  = key_money %>% str_replace("-","0") %>% str_remove("万円") %>% as.numeric
    , key_money  = key_money*10000
    , area       = area %>% str_remove("m2") %>% as.numeric
    , age        = age %>% str_replace("新築","0") %>% str_remove_all("(築|年)") %>% as.numeric
    , floor      = floor %>% str_remove("(B|\\d)*-") %>% str_remove("階") %>% str_replace("B","-") %>% as.integer
    , rent_total = rent + admin
    , lng = lng %>% as.numeric
    , lat = lat %>% as.numeric
    , dist_nakanosakaue = 
        map2_dbl(
          .x = lng,
          .y = lat,
          .f = ~distGeo(lnglat_nakanosakaue,c(.x,.y))
        )
    , flag_not_unitbath = details %>% str_detect("バストイレ別") %>% as.numeric
    , flag_kado         = details %>% str_detect("角")           %>% as.numeric
    , flag_yokushitsu_kansou = details %>% str_detect("浴室乾燥") %>% as.numeric
    , flag_autolock = details %>% str_detect("オートロック") %>% as.numeric
    , flag_takuhai  = details %>% str_detect("宅配ボックス") %>% as.numeric
    , flag_3_konro  = details %>% str_detect("3口以上コンロ") %>% as.numeric 
    , flag_2_konro  = details %>% str_detect("2口コンロ") %>% as.numeric
    , flag_2_konro  = ifelse(flag_3_konro,0,flag_2_konro)
    , flag_onsui    = details %>% str_detect("温水洗浄便座") %>% as.numeric
  ) %>% 
  #家賃の高すぎる物件・駅から遠すぎる物件・詳細の乗っていない物件は除く
  filter(
      rent_total <=200000
    , dist_nakanosakaue <= 1000
    , !is.na(details)
  )

df_output3 %>% select(
  -rent,
  -admin,
  -deposit,
  -key_money,
  -kouzou,
  -muki,
  -lat,
  -lng
  ) %>% 
  select(rent_total,everything()) %>% 
  plot_correlation()

#家賃の範囲確認
df_output3 %>% esquisser

#部屋の向き別家賃----
df_output3$muki %>% table

df_output3 %>%
  filter(rent_total<=200000)%$% 
  ggplot(data = .) +
  aes(x = muki, y = rent_total) +
  geom_boxplot() +
  theme_bw(base_family = "HiraKakuProN-W3")

#構造別家賃----
df_output3$kouzou %>% table

df_output3 %>%
  filter(rent_total<=200000)%$% 
  ggplot(data = .) +
  aes(x = kouzou, y = rent_total) +
  geom_boxplot() +
  theme_bw(base_family = "HiraKakuProN-W3")

#機械学習用の前処理----
df_output_ml <-
  df_output3 %>% 
  select(
      -name
    , -url
    , -rent
    , -admin
    , -deposit
    , -key_money
    , -shozaichi
    , -ekitoho_1
    , -ekitoho_2
    , -ekitoho_3
    , -floor_plan
    , -floor_plan_detail
    , -lat
    , -lng
    , -details
  ) %>% 
  mutate_if(
    .predicate = is.character,
    .funs = ~ ifelse(.=="-",NA,.) %>% factor
  ) %>% 
  createDummyFeatures(target="rent_total")

#相関係数の確認----
df_output_ml %>% plot_correlation

#回帰分析の実施----
kaiki <- lm(rent_total ~., data=df_output_ml)
summary(kaiki)

df_output_ml %>% map(class)
df_output3 %>% head
df_output3$age %>% table
df_output3 %>% tail(100)
df_output3 %>% View
df_output3 %>% write.csv("output.csv",fileEncoding = "CP932")
df_output %>% write_excel_csv("output.csv")
df_output3

# 地図上に賃貸をプロット----

#家賃を絞る
df_lnglat <-
  df_output3 %>% 
  filter(!is.na(lng),!is.na(lat)) %>% 
  #条件をフィルタに入れる
  filter(
    rent_total <= 100000,
    rent_total >= 70000,
    dist_nakanosakaue <= 1000,
    flag_not_unitbath == 1,
    kouzou != "木造"
    ) %>% 
  mutate(
    label = name,
    popup = str_c(
      "<table>",
      "<tr><td>家賃</td><td>",rent_total,"</td></tr>",
      "<tr><td>間取り</td><td>",floor_plan,"</td></tr>",
      "<tr><td>間取り詳細</td><td>",floor_plan_detail,"</td></tr>",
      "<tr><td>階</td><td>",floor,"階</td></tr>",
      "<tr><td>向き</td><td>",muki,"</td></tr>",
      "<tr><td>築年数</td><td>",age,"</td></tr>",
      "<tr><td>構造</td><td>",kouzou,"</td></tr>",
      "<tr><td>直線距離</td><td>",round(dist_nakanosakaue),"m</td></tr>",
      "</table><br>",
      "詳細 : <br>",details,"<br>",
      "<a href=",url," target=”_blank”>",url,"</a>"
    )
  )

#パレットの作成
pal <- colorNumeric(palette="RdYlBu", domain=df_lnglat$rent_total, reverse=TRUE)

# 地図の作成
leaflet(df_lnglat) %>% 
  addTiles() %>% 
  setView(lng=139.682223,lat=35.697064,zoom=16) %>% 
  addCircleMarkers(lng=~lng,lat=~lat,color=~pal(rent_total),label=~label,popup=~popup) %>% 
  addLegend(position='topright', pal=pal, values=~rent_total) %>% 
  addScaleBar(position="bottomleft",options = scaleBarOptions(imperial=FALSE))

# rakugaki----

tryCatch{test <- read_html("https://suumo.jp/chintai/jnc_000046651683/?bc=100145776322")}
1:3
for(i in 1:3){
  print(i)
  aiueo <- tryCatch(
      expr  = {
        test <- read_html("https://suumo.jp/chintai/jnc_000046651683/?bc=100145776322")
        }
    , error = function(e){print(e)}
  ) 
  print(aiueo)
}

for(i in 1:3){
  print(i)
  flag_error <- tryCatch(
    expr  = {
      read_html("https://suumo.jp/chintai/jnc_000030395778/")
    }
    , error = function(e){print(e)}
  ) 
  if(inherits(flag_error,"error")) next
  print("OK")
}
test
flag_error
aiueo %>% class
aiueo2 %>% class
inherits(aiueo,"error")
# rakugaki----
test <- data.frame(url=NA,lat=NA,lng=NA)
test <- data.frame()  %>% mutate(url=NA,lat=NA,lng=NA)
test %>% rbind(c(1,2,3)) %>% rbind(c(1,2,3))

result <- data.frame(matrix(rep(NA, 2), nrow=1))[numeric(0), ]
colnames(result ) <- c("No", "Group")
result %>% rbind(c(1,2))

test
?data.frame
# 賃貸詳細のurl読み込み----
chintai_html <- read_html("https://suumo.jp/chintai/jnc_000030395778/")

# マップのURL読み込み----
map_html <-
  chintai_html %>% 
  html_nodes(xpath="//img[contains(@alt,'地図・周辺環境') and contains(@src,'tab_bkdt-around.gif')]/parent::a") %>% 
  html_attr("href") %>%
  str_c(url_root,.) %>% 
  read_html
# マップの緯度経度情報の読み込み----
lat_and_lng <-
  map_html %>% 
  html_nodes("#js-gmapData")

lat <-
  lat_and_lng %>%
  as.character %>% 
  str_extract("\"lat\": \\d*\\.\\d*") %>% 
  str_remove("\"lat\": ")

lng <-
  lat_and_lng %>%
  as.character %>% 
  str_extract("\"lng\": \\d*\\.\\d*") %>% 
  str_remove("\"lng\": ")

lng
lat_and_lng %>% as.character
  
  #"//div[contains(text(), '品番')]
#https://suumo.jp
html_
?html_nodes
# 以下らくがき----
?map
#<a href="/chintai/tokyo/ek_27320/pnz1193.html?rn=0015">193</a>
separate(data = df, col = colll, into = c("Sepal.Length","Sepal.Width"), sep = "\r\n\t\t\t\t\t\t\t\t\t\t\t\t")
list_yachin
list_name

list_chintai %>% map(class)

?html_nodes()
?html_attr
