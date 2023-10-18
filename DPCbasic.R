# 最初に施すべき関数---------------------------------------------
# SPSSの欠損値が空白として読み込まれるため、すべてNAに変換
# dtdisc, dtadm, dtbirthを文字型から日付型に変換
# fiscal_year (20100401-20170331までのみ), ageを作成
# hospcd, ptidから一意に定まるuid (uniqueなid) を作成
# 2010年07月1日より昔のデータは特殊な病院からのデータなので除外

dpc_start <- function(df) {
  df_temp <- df %>%
    mutate_all(funs(ifelse(. == "", NA, .))) %>%
    mutate(
      dtdisc_cha = dtdisc,
      dtadm_cha = dtadm,
      dtbirth_cha = dtbirth,
      dtdisc = ymd(dtdisc),
      dtadm = ymd(dtadm),
      dtbirth = ymd(dtbirth),
      fyear = case_when(  
        ymd(20100401) <= dtdisc & dtdisc < ymd(20110401) ~ 2010,  #　2010.04.01～2011.03.31までは2010年度とする
        ymd(20110401) <= dtdisc & dtdisc < ymd(20120401) ~ 2011,
        ymd(20120401) <= dtdisc & dtdisc < ymd(20130401) ~ 2012,
        ymd(20130401) <= dtdisc & dtdisc < ymd(20140401) ~ 2013,
        ymd(20140401) <= dtdisc & dtdisc < ymd(20150401) ~ 2014,
        ymd(20150401) <= dtdisc & dtdisc < ymd(20160401) ~ 2015,
        ymd(20160401) <= dtdisc & dtdisc < ymd(20170401) ~ 2016,
        ymd(20170401) <= dtdisc & dtdisc < ymd(20180401) ~ 2017,
        ymd(20180401) <= dtdisc & dtdisc < ymd(20190401) ~ 2018,
        ymd(20190401) <= dtdisc & dtdisc < ymd(20200401) ~ 2019,
        ymd(20200401) <= dtdisc & dtdisc < ymd(20210401) ~ 2020,
        ymd(20210401) <= dtdisc & dtdisc < ymd(20220401) ~ 2021,
        ymd(20220401) <= dtdisc & dtdisc < ymd(20230401) ~ 2022,
        ymd(20230401) <= dtdisc & dtdisc < ymd(20240401) ~ 2023
        
      ),
      
      # 年齢をlubridateで計算
      age = (dtbirth %--% dtadm) / years(1),
      uid = paste0(hospcd, ptid),
      sex = case_when(sex == 1 ~ 1,
                      sex == 2 ~ 0)
    ) %>%
    filter(dtdisc >= ymd(20100701)) %>%      # 退院日付が2010.07.01より前のものは特殊な病院のため除外
    select(uid, hospcd, ptid, age, fyear, dtadm, dtdisc, LEN, everything())
  return(df_temp)
}


#  Kコードからオペを拾う関数 (左右考慮なし)------------------------------------------------------------
#　Kコードは完全一致
#　1回の入院で、同一眼に同一手術を2回以上している場合は、最後の日付を取り出す。
#　三連ドットを使って、複数の引数をとれるようにする　(K2683, K2684, ...と入る)。 OR検索となる。
dpc_opecode_all <- function(df, new_colname, ...) {
  quosures <- quos(...)
  n_dots <- length(quosures)
  quo_new_colname  <- enquo(new_colname)
  
  # dfの列数
  n_cols <- ncol(df)  
  
  # あらかじめdfと同じデータフレームを作っておく
  df_temp_1 <- df
  
  #　注文している変数を仮にjとした
  # ...の引数の最初を文字列とした
  for(i in 1:n_dots){
    j <- quo_name(quosures[[i]])  
    ope_date <- paste0(j, "_date")
    ope_1or0 <- paste0(j, "_1or0")
    
    df_temp_2 <- df %>% 
      mutate(
        opek1_j = if_else(is.na(.$opek1), 0, if_else(opek1 == j, 1, 0)),  #　最初にNAの条件の場合を0にかえてしまう。
        opek2_j = if_else(is.na(.$opek2), 0, if_else(opek2 == j, 1, 0)),
        opek3_j = if_else(is.na(.$opek3), 0, if_else(opek3 == j, 1, 0)),
        opek4_j = if_else(is.na(.$opek4), 0, if_else(opek4 == j, 1, 0)),
        opek5_j = if_else(is.na(.$opek5), 0, if_else(opek5 == j, 1, 0)),
        
        opek1_j_date = if_else(opek1_j == 1, opedt1, "0"), # opeK1==2683 の日付
        opek2_j_date = if_else(opek2_j == 1, opedt2, "0"),
        opek3_j_date = if_else(opek3_j == 1, opedt3, "0"),
        opek4_j_date = if_else(opek4_j == 1, opedt4, "0"),
        opek5_j_date = if_else(opek5_j == 1, opedt5, "0"),
        
        ope_j_date = pmap_dbl(
          list(as.integer(opek1_j_date),  # opek1_j_date, opek2_j_date,…の中で最大値のものをオペ日とする。同じ手術を2回している場合は、後の日付を採用することにした。
               as.integer(opek2_j_date),  
               as.integer(opek3_j_date),
               as.integer(opek4_j_date),
               as.integer(opek5_j_date)),
          max
        ),
        ope_j = if_else(!ope_j_date == 0, 1, 0)) %>%
      select(ope_j_date,
             ope_j) %>%
      rename(!!ope_date := ope_j_date,
             !!ope_1or0 := ope_j)
    df_temp_1 <- bind_cols(df_temp_1, df_temp_2)
  }
  
  # 引数に複数個のKコードを入れた場合、どれか1個でも当てはまるなら1という列を作成する
  result <- df_temp_1 %>%
    select((ncol(.) - 2 * n_dots + 1) : ncol(.)) %>% # 各opeKコードごとに2列が加わるので、df_temp_1の後ろ2n列をとってくる
    select(ends_with("1or0")) %>%
    pmap_dbl(max)  # どれか1だったら1とする
  df_temp_4 <- df_temp_1 %>%
    mutate(!!quo_name(quo_new_colname) := result)
  return(df_temp_4)
}




#  日本語からオペを拾う関数 (左右考慮なし)------------------------------------------------------------
#　日本語は部分一致
#　1回の入院で、同一眼に同一手術を2回以上している場合は、最後の日付を取り出す。
#　三連ドットを使って、複数の引数をとれるようにする　(緑内障, 白内障, ...と入る)。 OR検索となる。
dpc_openm_all <- function(df, new_colname, ...) {
  quosures <- quos(...)
  n_dots <- length(quosures)
  quo_new_colname  <- enquo(new_colname)
  
  # dfの列数
  n_cols <- ncol(df)  
  
  # あらかじめdfと同じデータフレームを作っておく
  df_temp_1 <- df
  
  #　注文している変数を仮にjとした
  # ...の引数の最初を文字列とした
  for(i in 1:n_dots){
    j <- quo_name(quosures[[i]])  
    ope_date <- paste0(j, "_date")
    ope_1or0 <- paste0(j, "_1or0")
    
    df_temp_2 <- df %>% 
      mutate(
        openm1_j = if_else(is.na(.$openm1), 0, if_else(str_detect(openm1, j), 1, 0)),  #　最初にNAの条件の場合を0にかえてしまう。
        openm2_j = if_else(is.na(.$openm2), 0, if_else(str_detect(openm2, j), 1, 0)),
        openm3_j = if_else(is.na(.$openm3), 0, if_else(str_detect(openm3, j), 1, 0)),
        openm4_j = if_else(is.na(.$openm4), 0, if_else(str_detect(openm4, j), 1, 0)),
        openm5_j = if_else(is.na(.$openm5), 0, if_else(str_detect(openm5, j), 1, 0)),
        
        openm1_j_date = if_else(openm1_j == 1, opedt1, "0"), # openm1に緑内障が入っている 日付
        openm2_j_date = if_else(openm2_j == 1, opedt2, "0"),
        openm3_j_date = if_else(openm3_j == 1, opedt3, "0"),
        openm4_j_date = if_else(openm4_j == 1, opedt4, "0"),
        openm5_j_date = if_else(openm5_j == 1, opedt5, "0"),
        
        ope_j_date = pmap_dbl(
          list(as.integer(openm1_j_date),  # openm1_j_date, openm2_j_date,…の中で最大値のものをオペ日とする。同じ手術を2回している場合は、後の日付を採用することにした。
               as.integer(openm2_j_date),  
               as.integer(openm3_j_date),
               as.integer(openm4_j_date),
               as.integer(openm5_j_date)),
          max
        ),
        ope_j = if_else(!ope_j_date == 0, 1, 0)) %>%
      select(ope_j_date,
             ope_j) %>%
      rename(!!ope_date := ope_j_date,
             !!ope_1or0 := ope_j)
    df_temp_1 <- bind_cols(df_temp_1, df_temp_2)
  }
  
  # 引数に複数個のKコードを入れた場合、どれか1個でも当てはまるなら1という列を作成する
  result <- df_temp_1 %>%
    select((ncol(.) - 2 * n_dots + 1) : ncol(.)) %>% # 各openmごとに2列が加わるので、df_temp_1の後ろ2n列をとってくる
    select(ends_with("1or0")) %>%
    pmap_dbl(max)  # どれか1だったら1とする
  df_temp_4 <- df_temp_1 %>%
    mutate(!!quo_name(quo_new_colname) := result)
  return(df_temp_4)
}

#  Kコードからオペを拾う関数 (左右考慮)------------------------------------------------------------
#  右眼or左眼に(ex.K2683)のオペしているかの新たな列を作成する
#　Kコードは完全一致
#　両眼オペも含む
#　オペしているけど、左右の記載がない症例を数えた列も加える
#　1回の入院で、同一眼に同一手術を2回以上している場合は、最後の日付を取り出す。
#　三連ドットを使って、複数の引数をとれるようにする　(K2683, K2684, ...と入る)　→　f_dpc_dxcode_allと違ってOR条件とかではなく、各手術毎に新しい列が出てくる
dpc_opecode_all_rl <- function(df, ...) {
  quosures <- quos(...)
  n_dots <- length(quosures)
  
  # dfの列数
  n_cols <- ncol(df)  
  
  # あらかじめdfと同じデータフレームを作っておく
  df_temp1 <- df
  
  #　注文している変数を仮にjとした
  # ...の引数の最初を文字列とした
  for(i in 1:n_dots){
    j <- quo_name(quosures[[i]])  
    ope_date_r <- paste0(j, "_date_r")
    ope_1or0_r <- paste0(j, "_1or0_r")
    ope_date_l <- paste0(j, "_date_l")
    ope_1or0_l <- paste0(j, "_1or0_l")
    ope_1or0_b <- paste0(j, "_1or0_b")
    ope_noside <- paste0(j, "_noside")
    
    df_temp2 <- df %>% 
      mutate(
        #　右眼
        opek1_j = if_else(is.na(.$opek1), 0, if_else(opek1 == j, 1, 0)),  #　最初にNAの条件の場合を0にかえてしまう。
        opek2_j = if_else(is.na(.$opek2), 0, if_else(opek2 == j, 1, 0)),
        opek3_j = if_else(is.na(.$opek3), 0, if_else(opek3 == j, 1, 0)),
        opek4_j = if_else(is.na(.$opek4), 0, if_else(opek4 == j, 1, 0)),
        opek5_j = if_else(is.na(.$opek5), 0, if_else(opek5 == j, 1, 0)),
        
        opek1_j_r = if_else(is.na(.$opeside1), 0, if_else((opek1_j== 1 & (opeside1 == "1"|opeside1 == "3")), 1, 0)), # opek1にK2683を含み、右側のopeをしたものを1とする. もしlec_startの日付が入っている人で判断すると、たとえば同一入院で右LECかつ左Expressやったひとの日付がどちらかわからなくなってしまう。
        opek2_j_r = if_else(is.na(.$opeside2), 0, if_else((opek2_j== 1 & (opeside2 == "1"|opeside2 == "3")), 1, 0)),
        opek3_j_r = if_else(is.na(.$opeside3), 0, if_else((opek3_j== 1 & (opeside3 == "1"|opeside3 == "3")), 1, 0)),
        opek4_j_r = if_else(is.na(.$opeside4), 0, if_else((opek4_j== 1 & (opeside4 == "1"|opeside4 == "3")), 1, 0)),
        opek5_j_r = if_else(is.na(.$opeside5), 0, if_else((opek5_j== 1 & (opeside5 == "1"|opeside5 == "3")), 1, 0)),
        
        opek1_j_r_date = if_else(opek1_j_r == 1, opedt1, "0"), # opeK1==2683 かつ 右眼のオペをした日付
        opek2_j_r_date = if_else(opek2_j_r == 1, opedt2, "0"),
        opek3_j_r_date = if_else(opek3_j_r == 1, opedt3, "0"),
        opek4_j_r_date = if_else(opek4_j_r == 1, opedt4, "0"),
        opek5_j_r_date = if_else(opek5_j_r == 1, opedt5, "0"),
        
        ope_j_r_date = pmap_dbl(
          list(as.integer(opek1_j_r_date),  # opek1_j_r_date, opek2_j_r_date,…の中で最大値のものをオペ日とする。万が一同一眼に2回LECしている場合を考えて、最大値を採用することにした。
               as.integer(opek2_j_r_date),  #　単純にmax(as.integer(opek1_jr_date), as.integer(opek2_jr_date),…としてもベクトル計算になってしまうため×)
               as.integer(opek3_j_r_date),
               as.integer(opek4_j_r_date),
               as.integer(opek5_j_r_date)),
          max
        ),
        ope_j_r = if_else(!ope_j_r_date == 0, 1, 0),
        
        # 左眼
        opek1_j_l = if_else(is.na(.$opeside1), 0, if_else((opek1_j== 1 & (opeside1 == "2"|opeside1 == "3")), 1, 0)), # opek1にK2683を含み、右側のopeをしたものを1とする. もしlec_startの日付が入っている人で判断すると、たとえば同一入院で右LECかつ左Expressやったひとの日付がどちらかわからなくなってしまう。
        opek2_j_l = if_else(is.na(.$opeside2), 0, if_else((opek2_j== 1 & (opeside2 == "2"|opeside2 == "3")), 1, 0)),
        opek3_j_l = if_else(is.na(.$opeside3), 0, if_else((opek3_j== 1 & (opeside3 == "2"|opeside3 == "3")), 1, 0)),
        opek4_j_l = if_else(is.na(.$opeside4), 0, if_else((opek4_j== 1 & (opeside4 == "2"|opeside4 == "3")), 1, 0)),
        opek5_j_l = if_else(is.na(.$opeside5), 0, if_else((opek5_j== 1 & (opeside5 == "2"|opeside5 == "3")), 1, 0)),
        
        opek1_j_l_date = if_else(opek1_j_l == 1, opedt1, "0"), # opeK1==2683 かつ 右眼のオペをした日付
        opek2_j_l_date = if_else(opek2_j_l == 1, opedt2, "0"),
        opek3_j_l_date = if_else(opek3_j_l == 1, opedt3, "0"),
        opek4_j_l_date = if_else(opek4_j_l == 1, opedt4, "0"),
        opek5_j_l_date = if_else(opek5_j_l == 1, opedt5, "0"),
        
        ope_j_l_date = pmap_dbl(
          list(as.integer(opek1_j_l_date),  # opek1_j_l_date, opek2_j_l_date,…の中で最大値のものをオペ日とする。万が一同一眼に2回LECしている場合を考えて、最大値を採用することにした。
               as.integer(opek2_j_l_date),  #　単純にmax(as.integer(opek1_jl_date), as.integel(opek2_jl_date),…としてもベクトル計算になってしまうため×)
               as.integer(opek3_j_l_date),
               as.integer(opek4_j_l_date),
               as.integer(opek5_j_l_date)),
          max
        ),
        ope_j_l = if_else(!ope_j_l_date == 0, 1, 0),
        
        # 両眼のオペ回数の合計　(両眼オペなら2、片眼オペなら1、オペなしなら0を返す)
        ope_j_b = ope_j_r + ope_j_l,
        
        # Kコードにオペの記載がある（つまりK2683と書いてある）が、どちら側の眼かわからないものを数える
        opek1_j_noside = if_else((opek1_j == 1 & is.na(opeside1)), 1, 0),  # ope_jをやったが、opesideの記載がない人を取り出す
        opek2_j_noside = if_else((opek2_j == 1 & is.na(opeside1)), 1, 0),
        opek3_j_noside = if_else((opek3_j == 1 & is.na(opeside1)), 1, 0),
        opek4_j_noside = if_else((opek4_j == 1 & is.na(opeside1)), 1, 0),
        opek5_j_noside = if_else((opek5_j == 1 & is.na(opeside1)), 1, 0),
        
        ope_j_noside = pmap_dbl(list(opek1_j_noside,
                                     opek2_j_noside,
                                     opek3_j_noside,
                                     opek4_j_noside,
                                     opek5_j_noside),sum)
      ) %>%
      select(ope_j_r_date,
             ope_j_r,
             ope_j_l_date, 
             ope_j_l,
             ope_j_b,
             ope_j_noside) %>%
      rename(!!ope_date_r := ope_j_r_date,
             !!ope_1or0_r := ope_j_r,
             !!ope_date_l := ope_j_l_date,
             !!ope_1or0_l := ope_j_l,
             !!ope_1or0_b := ope_j_b,
             !!ope_noside := ope_j_noside)
    
    
    df_temp1 <- bind_cols(df_temp1, df_temp2)
  }
  return(df_temp1)
}






#  日本語からオペを拾う関数 (左右考慮) ------------------------------------------------------------
#  右眼or左眼に(ex.緑内障)のオペしているかの新たな列を作成する
#　日本語は部分一致
#　両眼オペも含む
#　オペしているけど、左右の記載がない症例を数えた列も加える
#　1回の入院で、同一眼に同一手術を2回以上している場合は、最後の日付を取り出す。
#　三連ドットを使って、複数の引数をとれるようにする　(緑内障,　硝子体,  ...と入る)
dpc_openm_all_rl <- function(df, ...) {
  quosures <- quos(...)
  n_dots <- length(quosures)
  
  # dfの列数
  n_cols <- ncol(df)  
  
  # あらかじめdfと同じデータフレームを作っておく
  df_temp1 <- df
  
  #　注文している変数を仮にjとした
  # ...の引数の最初を文字列とした
  for(i in 1:n_dots){
    j <- quo_name(quosures[[i]])  
    ope_date_r <- paste0(j, "_date_r")
    ope_1or0_r <- paste0(j, "_1or0_r")
    ope_date_l <- paste0(j, "_date_l")
    ope_1or0_l <- paste0(j, "_1or0_l")
    ope_1or0_b <- paste0(j, "_1or0_b")
    ope_noside <- paste0(j, "_noside")
    
    df_temp2 <- df %>% 
      mutate(
        #　右眼
        openm1_j = if_else(is.na(.$openm1), 0, if_else(str_detect(openm1, j), 1, 0)),  #　最初にNAの条件の場合を0にかえてしまう。
        openm2_j = if_else(is.na(.$openm2), 0, if_else(str_detect(openm2, j), 1, 0)),
        openm3_j = if_else(is.na(.$openm3), 0, if_else(str_detect(openm3, j), 1, 0)),
        openm4_j = if_else(is.na(.$openm4), 0, if_else(str_detect(openm4, j), 1, 0)),
        openm5_j = if_else(is.na(.$openm5), 0, if_else(str_detect(openm5, j), 1, 0)),
        
        openm1_j_r = if_else(is.na(.$opeside1), 0, if_else((openm1_j== 1 & (opeside1 == "1"|opeside1 == "3")), 1, 0)), # openm1に緑内障を含み、右側のopeをしたものを1とする. もしlec_startの日付が入っている人で判断すると、たとえば同一入院で右LECかつ左Expressやったひとの日付がどちらかわからなくなってしまう。
        openm2_j_r = if_else(is.na(.$opeside2), 0, if_else((openm2_j== 1 & (opeside2 == "1"|opeside2 == "3")), 1, 0)),
        openm3_j_r = if_else(is.na(.$opeside3), 0, if_else((openm3_j== 1 & (opeside3 == "1"|opeside3 == "3")), 1, 0)),
        openm4_j_r = if_else(is.na(.$opeside4), 0, if_else((openm4_j== 1 & (opeside4 == "1"|opeside4 == "3")), 1, 0)),
        openm5_j_r = if_else(is.na(.$opeside5), 0, if_else((openm5_j== 1 & (opeside5 == "1"|opeside5 == "3")), 1, 0)),
        
        openm1_j_r_date = if_else(openm1_j_r == 1, opedt1, "0"), # openm1に緑内障が含まれる かつ 右眼のオペをした日付
        openm2_j_r_date = if_else(openm2_j_r == 1, opedt2, "0"),
        openm3_j_r_date = if_else(openm3_j_r == 1, opedt3, "0"),
        openm4_j_r_date = if_else(openm4_j_r == 1, opedt4, "0"),
        openm5_j_r_date = if_else(openm5_j_r == 1, opedt5, "0"),
        
        ope_j_r_date = pmap_dbl(
          list(as.integer(openm1_j_r_date),  # openm1_j_r_date, openm2_j_r_date,…の中で最大値のものをオペ日とする。万が一同一眼に2回LECしている場合を考えて、最大値を採用することにした。
               as.integer(openm2_j_r_date),  #　単純にmax(as.integer(openm1_jr_date), as.integer(openm2_jr_date),…としてもベクトル計算になってしまうため×)
               as.integer(openm3_j_r_date),
               as.integer(openm4_j_r_date),
               as.integer(openm5_j_r_date)),
          max
        ),
        ope_j_r = if_else(!ope_j_r_date == 0, 1, 0),
        
        # 左眼
        openm1_j_l = if_else(is.na(.$opeside1), 0, if_else((openm1_j== 1 & (opeside1 == "2"|opeside1 == "3")), 1, 0)), 
        openm2_j_l = if_else(is.na(.$opeside2), 0, if_else((openm2_j== 1 & (opeside2 == "2"|opeside2 == "3")), 1, 0)),
        openm3_j_l = if_else(is.na(.$opeside3), 0, if_else((openm3_j== 1 & (opeside3 == "2"|opeside3 == "3")), 1, 0)),
        openm4_j_l = if_else(is.na(.$opeside4), 0, if_else((openm4_j== 1 & (opeside4 == "2"|opeside4 == "3")), 1, 0)),
        openm5_j_l = if_else(is.na(.$opeside5), 0, if_else((openm5_j== 1 & (opeside5 == "2"|opeside5 == "3")), 1, 0)),
        
        openm1_j_l_date = if_else(openm1_j_l == 1, opedt1, "0"), 
        openm2_j_l_date = if_else(openm2_j_l == 1, opedt2, "0"),
        openm3_j_l_date = if_else(openm3_j_l == 1, opedt3, "0"),
        openm4_j_l_date = if_else(openm4_j_l == 1, opedt4, "0"),
        openm5_j_l_date = if_else(openm5_j_l == 1, opedt5, "0"),
        
        ope_j_l_date = pmap_dbl(
          list(as.integer(openm1_j_l_date),  # openm1_j_l_date, openm2_j_l_date,…の中で最大値のものをオペ日とする。万が一同一眼に2回LECしている場合を考えて、最大値を採用することにした。
               as.integer(openm2_j_l_date),  #　単純にmax(as.integer(openm1_jl_date), as.integel(openm2_jl_date),…としてもベクトル計算になってしまうため×)
               as.integer(openm3_j_l_date),
               as.integer(openm4_j_l_date),
               as.integer(openm5_j_l_date)),
          max
        ),
        ope_j_l = if_else(!ope_j_l_date == 0, 1, 0),
        
        # 両眼のオペ回数の合計　(両眼オペなら2、片眼オペなら1、オペなしなら0を返す)
        ope_j_b = ope_j_r + ope_j_l,
        
        # Kコードにオペの記載がある（つまり緑内障と書いてある）が、どちら側の眼かわからないものを数える
        openm1_j_noside = if_else((openm1_j == 1 & is.na(opeside1)), 1, 0),  # ope_jをやったが、opesideの記載がない人を取り出す
        openm2_j_noside = if_else((openm2_j == 1 & is.na(opeside1)), 1, 0),
        openm3_j_noside = if_else((openm3_j == 1 & is.na(opeside1)), 1, 0),
        openm4_j_noside = if_else((openm4_j == 1 & is.na(opeside1)), 1, 0),
        openm5_j_noside = if_else((openm5_j == 1 & is.na(opeside1)), 1, 0),
        
        ope_j_noside = pmap_dbl(list(openm1_j_noside,
                                     openm2_j_noside,
                                     openm3_j_noside,
                                     openm4_j_noside,
                                     openm5_j_noside),sum)
      ) %>%
      select(ope_j_r_date,
             ope_j_r,
             ope_j_l_date, 
             ope_j_l,
             ope_j_b,
             ope_j_noside) %>%
      rename(!!ope_date_r := ope_j_r_date,
             !!ope_1or0_r := ope_j_r,
             !!ope_date_l := ope_j_l_date,
             !!ope_1or0_l := ope_j_l,
             !!ope_1or0_b := ope_j_b,
             !!ope_noside := ope_j_noside)
    
    
    df_temp1 <- bind_cols(df_temp1, df_temp2)
  }
  return(df_temp1)
}




# 同時手術をしているかの列を加える関数------------------------------------------------------------------------------------------------------------
# dpc_opecode_all_rlもしくはdpc_opedx_all_rlを施したうえで機能する
#　K2683, K2684という引数をいれれば、左右の同時手術したかどうかが一気に出てくる
# 複数個の指定はできない　(1組２つまで、たとえばK2683とK2684とか)
# opeAにはK2683などを入れる
dpc_simultaneous <- function(df, opeA, opeB) {
  
  # enquo()でquosureにする
  quo_opeA <- enquo(opeA)
  quo_opeB <- enquo(opeB)
  # quo_rl <- enquo(rl)
  
  # quo_name()で文字列にする
  # 右眼
  opeA_date_r <- paste0(quo_name(quo_opeA), "_date_r")
  opeA_1or0_r <- paste0(quo_name(quo_opeA), "_1or0_r")
  opeB_date_r <- paste0(quo_name(quo_opeB), "_date_r")
  opeB_1or0_r <- paste0(quo_name(quo_opeB), "_1or0_r")
  #　左眼
  opeA_date_l <- paste0(quo_name(quo_opeA), "_date_l")
  opeA_1or0_l <- paste0(quo_name(quo_opeA), "_1or0_l")
  opeB_date_l <- paste0(quo_name(quo_opeB), "_date_l")
  opeB_1or0_l <- paste0(quo_name(quo_opeB), "_1or0_l")
  
  result_colname_r <- paste0("simul_", quo_name(quo_opeA), "_", quo_name(quo_opeB), "_r")
  result_colname_l <- paste0("simul_", quo_name(quo_opeA), "_", quo_name(quo_opeB), "_l")
  
  # 関心のある列だけ選択し、同時手術しているか判断する
  df_temp <- df %>%
    select(A_date_r = !!opeA_date_r,   # 以降、列名をうまく使うために仮の列名をつける
           A_1or0_r = !!opeA_1or0_r,
           B_date_r = !!opeB_date_r,
           B_1or0_r = !!opeB_1or0_r,
           A_date_l = !!opeA_date_l,
           A_1or0_l = !!opeA_1or0_l,
           B_date_l = !!opeB_date_l,
           B_1or0_l = !!opeB_1or0_l
    ) %>%
    mutate(simul_opeA_opeB_r = (A_1or0_r == 1 & (A_date_r == B_date_r)) %>% as.integer(),
           simul_opeA_opeB_l = (A_1or0_l == 1 & (A_date_l == B_date_l)) %>% as.integer()) %>%
    select(simul_opeA_opeB_r, simul_opeA_opeB_l) %>%
    rename(!!result_colname_r := simul_opeA_opeB_r,
           !!result_colname_l := simul_opeA_opeB_l)
  
  df <- bind_cols(df, df_temp)
  return(df)
}



# ICD10コード24個すべてから、病名を拾う関数--------------------------------------------------------------------------------------------------------------------
# new_colnameは、新たな列名(ex. DM)
# ...は、拾ってきたい病名ICD10コード(ex. E11, E12,... )
# 前方一致(E11だったら^E11.*として認識される)
# 複数検索可能(OR条件)である。

dpc_dxcode_all <- function(df, new_colname, ...) {
  
  df_temp <- df %>%
    select(starts_with("d")) %>%  #dからはじまる列だけ取り出す
    mutate(   
      # 疑い病名がついていないものは0、疑い病名がついているものは1を返す
      dmainnm_suspect = str_detect(.$dmainnm, "疑い") %>% as.integer(),
      dadmnm_suspect = str_detect(.$dadmnm, "疑い") %>% as.integer(),
      dres1nm_suspect = str_detect(.$dres1nm, "疑い") %>% as.integer(),
      dres2nm_suspect = str_detect(.$dres2nm, "疑い") %>% as.integer(),
      
      dcin1nm_suspect = str_detect(.$dcin1nm, "疑い") %>% as.integer(),
      dcin2nm_suspect = str_detect(.$dcin2nm, "疑い") %>% as.integer(),
      dcin3nm_suspect = str_detect(.$dcin3nm, "疑い") %>% as.integer(),
      dcin4nm_suspect = str_detect(.$dcin4nm, "疑い") %>% as.integer(),
      dcin5nm_suspect = str_detect(.$dcin5nm, "疑い") %>% as.integer(),
      dcin6nm_suspect = str_detect(.$dcin6nm, "疑い") %>% as.integer(),
      dcin7nm_suspect = str_detect(.$dcin7nm, "疑い") %>% as.integer(),
      dcin8nm_suspect = str_detect(.$dcin8nm, "疑い") %>% as.integer(),
      dcin9nm_suspect = str_detect(.$dcin9nm, "疑い") %>% as.integer(),
      dcin10nm_suspect = str_detect(.$dcin10nm, "疑い") %>% as.integer(),
      
      dcc1nm_suspect = str_detect(.$dcc1nm, "疑い") %>% as.integer(),
      dcc2nm_suspect = str_detect(.$dcc2nm, "疑い") %>% as.integer(),
      dcc3nm_suspect = str_detect(.$dcc3nm, "疑い") %>% as.integer(),
      dcc4nm_suspect = str_detect(.$dcc4nm, "疑い") %>% as.integer(),
      dcc5nm_suspect = str_detect(.$dcc5nm, "疑い") %>% as.integer(),
      dcc6nm_suspect = str_detect(.$dcc6nm, "疑い") %>% as.integer(),
      dcc7nm_suspect = str_detect(.$dcc7nm, "疑い") %>% as.integer(),
      dcc8nm_suspect = str_detect(.$dcc8nm, "疑い") %>% as.integer(),
      dcc9nm_suspect = str_detect(.$dcc9nm, "疑い") %>% as.integer(),
      dcc10nm_suspect = str_detect(.$dcc10nm, "疑い") %>% as.integer(),
      
      # 日本語病名に疑いがついていないものはdefiniteとしてICD10-codeを返す
      dmain_definite = if_else(dmainnm_suspect == 0, dmain, NA_character_),
      dadm_definite = if_else(dadmnm_suspect == 0, dadm, NA_character_),
      dres1_definite = if_else(dres1nm_suspect == 0, dres1, NA_character_),
      dres2_definite = if_else(dres2nm_suspect == 0, dres2, NA_character_),
      
      
      dcin1_definite = if_else(dcin1nm_suspect == 0, dcin1, NA_character_),
      dcin2_definite = if_else(dcin2nm_suspect == 0, dcin2, NA_character_),
      dcin3_definite = if_else(dcin3nm_suspect == 0, dcin3, NA_character_),
      dcin4_definite = if_else(dcin4nm_suspect == 0, dcin4, NA_character_),
      dcin5_definite = if_else(dcin5nm_suspect == 0, dcin5, NA_character_),
      dcin6_definite = if_else(dcin6nm_suspect == 0, dcin6, NA_character_),
      dcin7_definite = if_else(dcin7nm_suspect == 0, dcin7, NA_character_),
      dcin8_definite = if_else(dcin8nm_suspect == 0, dcin8, NA_character_),
      dcin9_definite = if_else(dcin9nm_suspect == 0, dcin9, NA_character_),
      dcin10_definite = if_else(dcin10nm_suspect == 0, dcin10, NA_character_),
      
      dcc1_definite = if_else(dcc1nm_suspect == 0, dcc1, NA_character_),
      dcc2_definite = if_else(dcc2nm_suspect == 0, dcc2, NA_character_),
      dcc3_definite = if_else(dcc3nm_suspect == 0, dcc3, NA_character_),
      dcc4_definite = if_else(dcc4nm_suspect == 0, dcc4, NA_character_),
      dcc5_definite = if_else(dcc5nm_suspect == 0, dcc5, NA_character_),
      dcc6_definite = if_else(dcc6nm_suspect == 0, dcc6, NA_character_),
      dcc7_definite = if_else(dcc7nm_suspect == 0, dcc7, NA_character_),
      dcc8_definite = if_else(dcc8nm_suspect == 0, dcc8, NA_character_),
      dcc9_definite = if_else(dcc9nm_suspect == 0, dcc9, NA_character_),
      dcc10_definite = if_else(dcc10nm_suspect == 0, dcc10, NA_character_)
    ) %>%
    mutate_all(funs(ifelse(is.na(.), 0, .)))  # NAを0にしておかないと、下の|またはの条件でうまくいかない。。
  
  quo_new_colname <- enquo(new_colname)
  quosures <- quos(...)
  n_quosures <- length(quosures)
  
  for(i in 1:n_quosures) {
    regular_exp <- paste0("^", quo_name(quosures[[i]]))
    colnames_temp <- paste0(quo_name(quo_new_colname), "_", quo_name(quosures[[i]]))  # forループをまわすたびに、新たな列名をつける必要がある
    df_temp_2 <- df_temp %>%
      mutate(   
        !!colnames_temp := ifelse(
          str_detect(dmain_definite, regular_exp)|
            str_detect(dadm_definite, regular_exp)|
            str_detect(dres1_definite, regular_exp)|
            str_detect(dres2_definite, regular_exp)|
            str_detect(dcin1_definite, regular_exp)|
            str_detect(dcin2_definite, regular_exp)|
            str_detect(dcin3_definite, regular_exp)|
            str_detect(dcin4_definite, regular_exp)|
            str_detect(dcin5_definite, regular_exp)|
            str_detect(dcin6_definite, regular_exp)|
            str_detect(dcin7_definite, regular_exp)|
            str_detect(dcin8_definite, regular_exp)|
            str_detect(dcin9_definite, regular_exp)|
            str_detect(dcin10_definite, regular_exp)|
            str_detect(dcc1_definite, regular_exp)|
            str_detect(dcc2_definite, regular_exp)|
            str_detect(dcc3_definite, regular_exp)|
            str_detect(dcc4_definite, regular_exp)|
            str_detect(dcc5_definite, regular_exp)|
            str_detect(dcc6_definite, regular_exp)|
            str_detect(dcc7_definite, regular_exp)|
            str_detect(dcc8_definite, regular_exp)|
            str_detect(dcc9_definite, regular_exp)|
            str_detect(dcc10_definite, regular_exp)
          , 1, 0)
      ) %>%
      select(ncol(.))
    df_temp <- bind_cols(df_temp, df_temp_2)
  }
  
  #　各ループの結果（ex. DM_E11, DM_E12, ...の合計）を統合する
  result <- df_temp %>%
    select((ncol(.) - n_quosures + 1) : ncol(.)) %>%
    pmap_dbl(sum)
  # どれか1つでも満たしていればOKなので、合計が2以上のものはすべて1としてカウントしてよい。
  result_2 <- if_else(result >= 1, 1, 0)
  
  # 最後に、自分のつけたい列名に、病名ありなしの結果をいれればよい。
  df_temp_4 <- df %>%
    mutate(!!quo_name(quo_new_colname) := result_2)
  return(df_temp_4)
  
}

# ICD10コード主要4個から、病名を拾う関数--------------------------------------------------------------------------------------------------------------------
# new_colnameは、新たな列名(ex. DM)
# ...は、拾ってきたい病名ICD10コード(ex. E11, E12,... )
# 前方一致(E11だったら^E11.*として認識される)
# 複数検索可能(OR条件)である。

dpc_dxcode_main <- function(df, new_colname, ...) {
  
  df_temp <- df %>%
    select(starts_with("d")) %>%  #dからはじまる列だけ取り出す
    mutate(   
      # 疑い病名がついていないものは0、疑い病名がついているものは1を返す
      dmainnm_suspect = str_detect(.$dmainnm, "疑い") %>% as.integer(),
      dadmnm_suspect = str_detect(.$dadmnm, "疑い") %>% as.integer(),
      dres1nm_suspect = str_detect(.$dres1nm, "疑い") %>% as.integer(),
      dres2nm_suspect = str_detect(.$dres2nm, "疑い") %>% as.integer(),
      
      # 日本語病名に疑いがついていないものはdefiniteとしてICD10-codeを返す
      dmain_definite = if_else(dmainnm_suspect == 0, dmain, NA_character_),
      dadm_definite = if_else(dadmnm_suspect == 0, dadm, NA_character_),
      dres1_definite = if_else(dres1nm_suspect == 0, dres1, NA_character_),
      dres2_definite = if_else(dres2nm_suspect == 0, dres2, NA_character_)
    ) %>%
    mutate_all(funs(ifelse(is.na(.), 0, .)))  # NAを0にしておかないと、下の|またはの条件でうまくいかない。。
  
  quo_new_colname <- enquo(new_colname)
  quosures <- quos(...)
  n_quosures <- length(quosures)
  
  for(i in 1:n_quosures) {
    regular_exp <- paste0("^", quo_name(quosures[[i]]))
    colnames_temp <- paste0(quo_name(quo_new_colname), "_", quo_name(quosures[[i]]))  # forループをまわすたびに、新たな列名をつける必要がある
    df_temp_2 <- df_temp %>%
      mutate(   
        !!colnames_temp := ifelse(
          str_detect(dmain_definite, regular_exp)|
            str_detect(dadm_definite, regular_exp)|
            str_detect(dres1_definite, regular_exp)|
            str_detect(dres2_definite, regular_exp)
          , 1, 0)
      ) %>%
      select(ncol(.))
    df_temp <- bind_cols(df_temp, df_temp_2)
  }
  
  #　各ループの結果（ex. DM_E11, DM_E12, ...の合計）を統合する
  result <- df_temp %>%
    select((ncol(.) - n_quosures + 1) : ncol(.)) %>%
    pmap_dbl(sum)
  # どれか1つでも満たしていればOKなので、合計が2以上のものはすべて1としてカウントしてよい。
  result_2 <- if_else(result >= 1, 1, 0)
  
  # 最後に、自分のつけたい列名に、病名ありなしの結果をいれればよい。
  df_temp_4 <- df %>%
    mutate(!!quo_name(quo_new_colname) := result_2)
  return(df_temp_4)
  
}

# ICD10コード併存10個から、病名を拾う関数--------------------------------------------------------------------------------------------------------------------
# new_colnameは、新たな列名(ex. DM)
# ...は、拾ってきたい病名ICD10コード(ex. E11, E12,... )
# 前方一致(E11だったら^E11.*として認識される)
# 複数検索可能(OR条件)である。

dpc_dxcode_pre <- function(df, new_colname, ...) {
  
  df_temp <- df %>%
    select(starts_with("d")) %>%  #dからはじまる列だけ取り出す
    mutate(   
      # 疑い病名がついていないものは0、疑い病名がついているものは1を返す
      dcin1nm_suspect = str_detect(.$dcin1nm, "疑い") %>% as.integer(),
      dcin2nm_suspect = str_detect(.$dcin2nm, "疑い") %>% as.integer(),
      dcin3nm_suspect = str_detect(.$dcin3nm, "疑い") %>% as.integer(),
      dcin4nm_suspect = str_detect(.$dcin4nm, "疑い") %>% as.integer(),
      dcin5nm_suspect = str_detect(.$dcin5nm, "疑い") %>% as.integer(),
      dcin6nm_suspect = str_detect(.$dcin6nm, "疑い") %>% as.integer(),
      dcin7nm_suspect = str_detect(.$dcin7nm, "疑い") %>% as.integer(),
      dcin8nm_suspect = str_detect(.$dcin8nm, "疑い") %>% as.integer(),
      dcin9nm_suspect = str_detect(.$dcin9nm, "疑い") %>% as.integer(),
      dcin10nm_suspect = str_detect(.$dcin10nm, "疑い") %>% as.integer(),
      
      # 日本語病名に疑いがついていないものはdefiniteとしてICD10-codeを返す
      
      dcin1_definite = if_else(dcin1nm_suspect == 0, dcin1, NA_character_),
      dcin2_definite = if_else(dcin2nm_suspect == 0, dcin2, NA_character_),
      dcin3_definite = if_else(dcin3nm_suspect == 0, dcin3, NA_character_),
      dcin4_definite = if_else(dcin4nm_suspect == 0, dcin4, NA_character_),
      dcin5_definite = if_else(dcin5nm_suspect == 0, dcin5, NA_character_),
      dcin6_definite = if_else(dcin6nm_suspect == 0, dcin6, NA_character_),
      dcin7_definite = if_else(dcin7nm_suspect == 0, dcin7, NA_character_),
      dcin8_definite = if_else(dcin8nm_suspect == 0, dcin8, NA_character_),
      dcin9_definite = if_else(dcin9nm_suspect == 0, dcin9, NA_character_),
      dcin10_definite = if_else(dcin10nm_suspect == 0, dcin10, NA_character_)
    ) %>%
    mutate_all(funs(ifelse(is.na(.), 0, .)))  # NAを0にしておかないと、下の|またはの条件でうまくいかない。。
  
  quo_new_colname <- enquo(new_colname)
  quosures <- quos(...)
  n_quosures <- length(quosures)
  
  for(i in 1:n_quosures) {
    regular_exp <- paste0("^", quo_name(quosures[[i]]))
    colnames_temp <- paste0(quo_name(quo_new_colname), "_", quo_name(quosures[[i]]))  # forループをまわすたびに、新たな列名をつける必要がある
    df_temp_2 <- df_temp %>%
      mutate(   
        !!colnames_temp := ifelse(
          str_detect(dcin1_definite, regular_exp)|
            str_detect(dcin2_definite, regular_exp)|
            str_detect(dcin3_definite, regular_exp)|
            str_detect(dcin4_definite, regular_exp)|
            str_detect(dcin5_definite, regular_exp)|
            str_detect(dcin6_definite, regular_exp)|
            str_detect(dcin7_definite, regular_exp)|
            str_detect(dcin8_definite, regular_exp)|
            str_detect(dcin9_definite, regular_exp)|
            str_detect(dcin10_definite, regular_exp)
          , 1, 0)
      ) %>%
      select(ncol(.))
    df_temp <- bind_cols(df_temp, df_temp_2)
  }
  
  #　各ループの結果（ex. DM_E11, DM_E12, ...の合計）を統合する
  result <- df_temp %>%
    select((ncol(.) - n_quosures + 1) : ncol(.)) %>%
    pmap_dbl(sum)
  # どれか1つでも満たしていればOKなので、合計が2以上のものはすべて1としてカウントしてよい。
  result_2 <- if_else(result >= 1, 1, 0)
  
  # 最後に、自分のつけたい列名に、病名ありなしの結果をいれればよい。
  df_temp_4 <- df %>%
    mutate(!!quo_name(quo_new_colname) := result_2)
  return(df_temp_4)
  
}

# ICD10コード入院後発症10個から、病名を拾う関数--------------------------------------------------------------------------------------------------------------------
dpc_dxcode_post <- function(df, new_colname, ...) {
  
  df_temp <- df %>%
    select(starts_with("d")) %>%  #dからはじまる列だけ取り出す
    mutate(   
      # 疑い病名がついていないものは0、疑い病名がついているものは1を返す
      dcc1nm_suspect = str_detect(.$dcc1nm, "疑い") %>% as.integer(),
      dcc2nm_suspect = str_detect(.$dcc2nm, "疑い") %>% as.integer(),
      dcc3nm_suspect = str_detect(.$dcc3nm, "疑い") %>% as.integer(),
      dcc4nm_suspect = str_detect(.$dcc4nm, "疑い") %>% as.integer(),
      dcc5nm_suspect = str_detect(.$dcc5nm, "疑い") %>% as.integer(),
      dcc6nm_suspect = str_detect(.$dcc6nm, "疑い") %>% as.integer(),
      dcc7nm_suspect = str_detect(.$dcc7nm, "疑い") %>% as.integer(),
      dcc8nm_suspect = str_detect(.$dcc8nm, "疑い") %>% as.integer(),
      dcc9nm_suspect = str_detect(.$dcc9nm, "疑い") %>% as.integer(),
      dcc10nm_suspect = str_detect(.$dcc10nm, "疑い") %>% as.integer(),
      
      # 日本語病名に疑いがついていないものはdefiniteとしてICD10-codeを返す
      dcc1_definite = if_else(dcc1nm_suspect == 0, dcc1, NA_character_),
      dcc2_definite = if_else(dcc2nm_suspect == 0, dcc2, NA_character_),
      dcc3_definite = if_else(dcc3nm_suspect == 0, dcc3, NA_character_),
      dcc4_definite = if_else(dcc4nm_suspect == 0, dcc4, NA_character_),
      dcc5_definite = if_else(dcc5nm_suspect == 0, dcc5, NA_character_),
      dcc6_definite = if_else(dcc6nm_suspect == 0, dcc6, NA_character_),
      dcc7_definite = if_else(dcc7nm_suspect == 0, dcc7, NA_character_),
      dcc8_definite = if_else(dcc8nm_suspect == 0, dcc8, NA_character_),
      dcc9_definite = if_else(dcc9nm_suspect == 0, dcc9, NA_character_),
      dcc10_definite = if_else(dcc10nm_suspect == 0, dcc10, NA_character_)
    ) %>%
    mutate_all(funs(ifelse(is.na(.), 0, .)))  # NAを0にしておかないと、下の|またはの条件でうまくいかない。。
  
  quo_new_colname <- enquo(new_colname)
  quosures <- quos(...)
  n_quosures <- length(quosures)
  
  for(i in 1:n_quosures) {
    regular_exp <- paste0("^", quo_name(quosures[[i]]))
    colnames_temp <- paste0(quo_name(quo_new_colname), "_", quo_name(quosures[[i]]))  # forループをまわすたびに、新たな列名をつける必要がある
    df_temp_2 <- df_temp %>%
      mutate(   
        !!colnames_temp := ifelse(
          str_detect(dcc1_definite, regular_exp)|
            str_detect(dcc2_definite, regular_exp)|
            str_detect(dcc3_definite, regular_exp)|
            str_detect(dcc4_definite, regular_exp)|
            str_detect(dcc5_definite, regular_exp)|
            str_detect(dcc6_definite, regular_exp)|
            str_detect(dcc7_definite, regular_exp)|
            str_detect(dcc8_definite, regular_exp)|
            str_detect(dcc9_definite, regular_exp)|
            str_detect(dcc10_definite, regular_exp)
          , 1, 0)
      ) %>%
      select(ncol(.))
    df_temp <- bind_cols(df_temp, df_temp_2)
  }
  
  #　各ループの結果（ex. DM_E11, DM_E12, ...の合計）を統合する
  result <- df_temp %>%
    select((ncol(.) - n_quosures + 1) : ncol(.)) %>%
    pmap_dbl(sum)
  # どれか1つでも満たしていればOKなので、合計が2以上のものはすべて1としてカウントしてよい。
  result_2 <- if_else(result >= 1, 1, 0)
  
  # 最後に、自分のつけたい列名に、病名ありなしの結果をいれればよい。
  df_temp_4 <- df %>%
    mutate(!!quo_name(quo_new_colname) := result_2)
  return(df_temp_4)
  
}

# 日本語病名24個すべてから病名を拾う関数----------------------------------------------------------------------------
# new_colnameは、新たな列名(ex. 糖尿病)
# ...は、拾ってきたい日本語病名(ex. 糖尿病足壊疽, 糖尿病網膜症,... )
# 部分一致(糖尿病だったら.*糖尿病.*として認識される)
# 複数検索可能(OR条件)である。

dpc_dxnm_all <- function(df, new_colname, ...) {
  
  df_temp <- df %>%
    select(starts_with("d")) %>% #dからはじまる列だけ取り出す
    mutate(   
      # 疑い病名がついていないものは0、疑い病名がついているものは1を返す
      dmainnm_suspect = str_detect(.$dmainnm, "疑い") %>% as.integer(),
      dadmnm_suspect = str_detect(.$dadmnm, "疑い") %>% as.integer(),
      dres1nm_suspect = str_detect(.$dres1nm, "疑い") %>% as.integer(),
      dres2nm_suspect = str_detect(.$dres2nm, "疑い") %>% as.integer(),
      
      dcin1nm_suspect = str_detect(.$dcin1nm, "疑い") %>% as.integer(),
      dcin2nm_suspect = str_detect(.$dcin2nm, "疑い") %>% as.integer(),
      dcin3nm_suspect = str_detect(.$dcin3nm, "疑い") %>% as.integer(),
      dcin4nm_suspect = str_detect(.$dcin4nm, "疑い") %>% as.integer(),
      dcin5nm_suspect = str_detect(.$dcin5nm, "疑い") %>% as.integer(),
      dcin6nm_suspect = str_detect(.$dcin6nm, "疑い") %>% as.integer(),
      dcin7nm_suspect = str_detect(.$dcin7nm, "疑い") %>% as.integer(),
      dcin8nm_suspect = str_detect(.$dcin8nm, "疑い") %>% as.integer(),
      dcin9nm_suspect = str_detect(.$dcin9nm, "疑い") %>% as.integer(),
      dcin10nm_suspect = str_detect(.$dcin10nm, "疑い") %>% as.integer(),
      
      dcc1nm_suspect = str_detect(.$dcc1nm, "疑い") %>% as.integer(),
      dcc2nm_suspect = str_detect(.$dcc2nm, "疑い") %>% as.integer(),
      dcc3nm_suspect = str_detect(.$dcc3nm, "疑い") %>% as.integer(),
      dcc4nm_suspect = str_detect(.$dcc4nm, "疑い") %>% as.integer(),
      dcc5nm_suspect = str_detect(.$dcc5nm, "疑い") %>% as.integer(),
      dcc6nm_suspect = str_detect(.$dcc6nm, "疑い") %>% as.integer(),
      dcc7nm_suspect = str_detect(.$dcc7nm, "疑い") %>% as.integer(),
      dcc8nm_suspect = str_detect(.$dcc8nm, "疑い") %>% as.integer(),
      dcc9nm_suspect = str_detect(.$dcc9nm, "疑い") %>% as.integer(),
      dcc10nm_suspect = str_detect(.$dcc10nm, "疑い") %>% as.integer(),
      
      # 日本語病名に疑いがついていないものはdefiniteとしてICD10-codeを返す
      dmainnm_definite = if_else(dmainnm_suspect == 0, dmainnm, NA_character_),
      dadmnm_definite = if_else(dadmnm_suspect == 0, dadmnm, NA_character_),
      dres1nm_definite = if_else(dres1nm_suspect == 0, dres1nm, NA_character_),
      dres2nm_definite = if_else(dres2nm_suspect == 0, dres2nm, NA_character_),
      
      
      dcin1nm_definite = if_else(dcin1nm_suspect == 0, dcin1nm, NA_character_),
      dcin2nm_definite = if_else(dcin2nm_suspect == 0, dcin2nm, NA_character_),
      dcin3nm_definite = if_else(dcin3nm_suspect == 0, dcin3nm, NA_character_),
      dcin4nm_definite = if_else(dcin4nm_suspect == 0, dcin4nm, NA_character_),
      dcin5nm_definite = if_else(dcin5nm_suspect == 0, dcin5nm, NA_character_),
      dcin6nm_definite = if_else(dcin6nm_suspect == 0, dcin6nm, NA_character_),
      dcin7nm_definite = if_else(dcin7nm_suspect == 0, dcin7nm, NA_character_),
      dcin8nm_definite = if_else(dcin8nm_suspect == 0, dcin8nm, NA_character_),
      dcin9nm_definite = if_else(dcin9nm_suspect == 0, dcin9nm, NA_character_),
      dcin10nm_definite = if_else(dcin10nm_suspect == 0, dcin10nm, NA_character_),
      
      dcc1nm_definite = if_else(dcc1nm_suspect == 0, dcc1nm, NA_character_),
      dcc2nm_definite = if_else(dcc2nm_suspect == 0, dcc2nm, NA_character_),
      dcc3nm_definite = if_else(dcc3nm_suspect == 0, dcc3nm, NA_character_),
      dcc4nm_definite = if_else(dcc4nm_suspect == 0, dcc4nm, NA_character_),
      dcc5nm_definite = if_else(dcc5nm_suspect == 0, dcc5nm, NA_character_),
      dcc6nm_definite = if_else(dcc6nm_suspect == 0, dcc6nm, NA_character_),
      dcc7nm_definite = if_else(dcc7nm_suspect == 0, dcc7nm, NA_character_),
      dcc8nm_definite = if_else(dcc8nm_suspect == 0, dcc8nm, NA_character_),
      dcc9nm_definite = if_else(dcc9nm_suspect == 0, dcc9nm, NA_character_),
      dcc10nm_definite = if_else(dcc10nm_suspect == 0, dcc10nm, NA_character_)
    ) %>%
    mutate_all(funs(ifelse(is.na(.), 0, .)))  # NAを0にしておかないと、下の|またはの条件でうまくいかない。。
  
  quo_new_colname <- enquo(new_colname)
  quosures <- quos(...)
  n_quosures <- length(quosures)
  
  for(i in 1:n_quosures) {
    regular_exp <- paste0(quo_name(quosures[[i]]))
    colnames_temp <- paste0(quo_name(quo_new_colname), "_", quo_name(quosures[[i]]))  # forループをまわすたびに、新たな列名をつける必要がある
    df_temp_2 <- df_temp %>%
      mutate(   
        !!colnames_temp := ifelse(
          str_detect(dmainnm_definite, regular_exp)|
            str_detect(dadmnm_definite, regular_exp)|
            str_detect(dres1nm_definite, regular_exp)|
            str_detect(dres2nm_definite, regular_exp)|
            str_detect(dcin1nm_definite, regular_exp)|
            str_detect(dcin2nm_definite, regular_exp)|
            str_detect(dcin3nm_definite, regular_exp)|
            str_detect(dcin4nm_definite, regular_exp)|
            str_detect(dcin5nm_definite, regular_exp)|
            str_detect(dcin6nm_definite, regular_exp)|
            str_detect(dcin7nm_definite, regular_exp)|
            str_detect(dcin8nm_definite, regular_exp)|
            str_detect(dcin9nm_definite, regular_exp)|
            str_detect(dcin10nm_definite, regular_exp)|
            str_detect(dcc1nm_definite, regular_exp)|
            str_detect(dcc2nm_definite, regular_exp)|
            str_detect(dcc3nm_definite, regular_exp)|
            str_detect(dcc4nm_definite, regular_exp)|
            str_detect(dcc5nm_definite, regular_exp)|
            str_detect(dcc6nm_definite, regular_exp)|
            str_detect(dcc7nm_definite, regular_exp)|
            str_detect(dcc8nm_definite, regular_exp)|
            str_detect(dcc9nm_definite, regular_exp)|
            str_detect(dcc10nm_definite, regular_exp)
          , 1, 0)
      ) %>%
      select(ncol(.))
    df_temp <- bind_cols(df_temp, df_temp_2)
  }
  
  #　各ループの結果（ex. DM_糖尿病足壊疽, DM_糖尿病網膜症, ...の合計）を統合する
  result <- df_temp %>%
    select((ncol(.) - n_quosures + 1) : ncol(.)) %>%
    pmap_dbl(sum)
  # どれか1つでも満たしていればOKなので、合計が2以上のものはすべて1としてカウントしてよい。
  result_2 <- if_else(result >= 1, 1, 0)
  
  # 最後に、自分のつけたい列名に、病名ありなしの結果をいれればよい。
  df_temp_4 <- df %>%
    mutate(!!quo_name(quo_new_colname) := result_2)
  return(df_temp_4)
}




# 日本語病名の主要4つから拾う関数----------------------------------------------------------------------------
# new_colnameは、新たな列名(ex. 糖尿病)
# ...は、拾ってきたい日本語病名(ex. 糖尿病足壊疽, 糖尿病網膜症,... )
# 部分一致(糖尿病だったら.*糖尿病.*として認識される)
# 複数検索可能(OR条件)である。

dpc_dxnm_main <- function(df, new_colname, ...) {
  
  df_temp <- df %>%
    select(starts_with("d")) %>% #dからはじまる列だけ取り出す
    mutate(   
      # 疑い病名がついていないものは0、疑い病名がついているものは1を返す
      dmainnm_suspect = str_detect(.$dmainnm, "疑い") %>% as.integer(),
      dadmnm_suspect = str_detect(.$dadmnm, "疑い") %>% as.integer(),
      dres1nm_suspect = str_detect(.$dres1nm, "疑い") %>% as.integer(),
      dres2nm_suspect = str_detect(.$dres2nm, "疑い") %>% as.integer(),
      
      # 日本語病名に疑いがついていないものはdefiniteとしてICD10-codeを返す
      dmainnm_definite = if_else(dmainnm_suspect == 0, dmainnm, NA_character_),
      dadmnm_definite = if_else(dadmnm_suspect == 0, dadmnm, NA_character_),
      dres1nm_definite = if_else(dres1nm_suspect == 0, dres1nm, NA_character_),
      dres2nm_definite = if_else(dres2nm_suspect == 0, dres2nm, NA_character_)
    ) %>%
    mutate_all(funs(ifelse(is.na(.), 0, .)))  # NAを0にしておかないと、下の|またはの条件でうまくいかない。。
  
  quo_new_colname <- enquo(new_colname)
  quosures <- quos(...)
  n_quosures <- length(quosures)
  
  for(i in 1:n_quosures) {
    regular_exp <- paste0(quo_name(quosures[[i]]))
    colnames_temp <- paste0(quo_name(quo_new_colname), "_", quo_name(quosures[[i]]))  # forループをまわすたびに、新たな列名をつける必要がある
    df_temp_2 <- df_temp %>%
      mutate(   
        !!colnames_temp := ifelse(
          str_detect(dmainnm_definite, regular_exp)|
            str_detect(dadmnm_definite, regular_exp)|
            str_detect(dres1nm_definite, regular_exp)|
            str_detect(dres2nm_definite, regular_exp)
          , 1, 0)
      ) %>%
      select(ncol(.))
    df_temp <- bind_cols(df_temp, df_temp_2)
  }
  
  #　各ループの結果（ex. DM_糖尿病足壊疽, DM_糖尿病網膜症, ...の合計）を統合する
  result <- df_temp %>%
    select((ncol(.) - n_quosures + 1) : ncol(.)) %>%
    pmap_dbl(sum)
  # どれか1つでも満たしていればOKなので、合計が2以上のものはすべて1としてカウントしてよい。
  result_2 <- if_else(result >= 1, 1, 0)
  
  # 最後に、自分のつけたい列名に、病名ありなしの結果をいれればよい。
  df_temp_4 <- df %>%
    mutate(!!quo_name(quo_new_colname) := result_2)
  return(df_temp_4)
}




# 日本語病名の併存10個から病名を拾う関数----------------------------------------------------------------------------
# new_colnameは、新たな列名(ex. 糖尿病)
# ...は、拾ってきたい日本語病名(ex. 糖尿病足壊疽, 糖尿病網膜症,... )
# 部分一致(糖尿病だったら.*糖尿病.*として認識される)
# 複数検索可能(OR条件)である。

dpc_dxnm_pre <- function(df, new_colname, ...) {
  
  df_temp <- df %>%
    select(starts_with("d")) %>% #dからはじまる列だけ取り出す
    mutate(   
      # 疑い病名がついていないものは0、疑い病名がついているものは1を返す
      
      dcin1nm_suspect = str_detect(.$dcin1nm, "疑い") %>% as.integer(),
      dcin2nm_suspect = str_detect(.$dcin2nm, "疑い") %>% as.integer(),
      dcin3nm_suspect = str_detect(.$dcin3nm, "疑い") %>% as.integer(),
      dcin4nm_suspect = str_detect(.$dcin4nm, "疑い") %>% as.integer(),
      dcin5nm_suspect = str_detect(.$dcin5nm, "疑い") %>% as.integer(),
      dcin6nm_suspect = str_detect(.$dcin6nm, "疑い") %>% as.integer(),
      dcin7nm_suspect = str_detect(.$dcin7nm, "疑い") %>% as.integer(),
      dcin8nm_suspect = str_detect(.$dcin8nm, "疑い") %>% as.integer(),
      dcin9nm_suspect = str_detect(.$dcin9nm, "疑い") %>% as.integer(),
      dcin10nm_suspect = str_detect(.$dcin10nm, "疑い") %>% as.integer(),
      
      # 日本語病名に疑いがついていないものはdefiniteとしてICD10-codeを返す
      
      dcin1nm_definite = if_else(dcin1nm_suspect == 0, dcin1nm, NA_character_),
      dcin2nm_definite = if_else(dcin2nm_suspect == 0, dcin2nm, NA_character_),
      dcin3nm_definite = if_else(dcin3nm_suspect == 0, dcin3nm, NA_character_),
      dcin4nm_definite = if_else(dcin4nm_suspect == 0, dcin4nm, NA_character_),
      dcin5nm_definite = if_else(dcin5nm_suspect == 0, dcin5nm, NA_character_),
      dcin6nm_definite = if_else(dcin6nm_suspect == 0, dcin6nm, NA_character_),
      dcin7nm_definite = if_else(dcin7nm_suspect == 0, dcin7nm, NA_character_),
      dcin8nm_definite = if_else(dcin8nm_suspect == 0, dcin8nm, NA_character_),
      dcin9nm_definite = if_else(dcin9nm_suspect == 0, dcin9nm, NA_character_),
      dcin10nm_definite = if_else(dcin10nm_suspect == 0, dcin10nm, NA_character_)
      
    ) %>%
    mutate_all(funs(ifelse(is.na(.), 0, .)))  # NAを0にしておかないと、下の|またはの条件でうまくいかない。。
  
  quo_new_colname <- enquo(new_colname)
  quosures <- quos(...)
  n_quosures <- length(quosures)
  
  for(i in 1:n_quosures) {
    regular_exp <- paste0(quo_name(quosures[[i]]))
    colnames_temp <- paste0(quo_name(quo_new_colname), "_", quo_name(quosures[[i]]))  # forループをまわすたびに、新たな列名をつける必要がある
    df_temp_2 <- df_temp %>%
      mutate(   
        !!colnames_temp := ifelse(
          str_detect(dcin1nm_definite, regular_exp)|
            str_detect(dcin2nm_definite, regular_exp)|
            str_detect(dcin3nm_definite, regular_exp)|
            str_detect(dcin4nm_definite, regular_exp)|
            str_detect(dcin5nm_definite, regular_exp)|
            str_detect(dcin6nm_definite, regular_exp)|
            str_detect(dcin7nm_definite, regular_exp)|
            str_detect(dcin8nm_definite, regular_exp)|
            str_detect(dcin9nm_definite, regular_exp)|
            str_detect(dcin10nm_definite, regular_exp)
          , 1, 0)
      ) %>%
      select(ncol(.))
    df_temp <- bind_cols(df_temp, df_temp_2)
  }
  
  #　各ループの結果（ex. DM_糖尿病足壊疽, DM_糖尿病網膜症, ...の合計）を統合する
  result <- df_temp %>%
    select((ncol(.) - n_quosures + 1) : ncol(.)) %>%
    pmap_dbl(sum)
  # どれか1つでも満たしていればOKなので、合計が2以上のものはすべて1としてカウントしてよい。
  result_2 <- if_else(result >= 1, 1, 0)
  
  # 最後に、自分のつけたい列名に、病名ありなしの結果をいれればよい。
  df_temp_4 <- df %>%
    mutate(!!quo_name(quo_new_colname) := result_2)
  return(df_temp_4)
}




# 日本語病名の入院後発症10個から病名を拾う関数----------------------------------------------------------------------------
# new_colnameは、新たな列名(ex. 糖尿病)
# ...は、拾ってきたい日本語病名(ex. 糖尿病足壊疽, 糖尿病網膜症,... )
# 部分一致(糖尿病だったら.*糖尿病.*として認識される)
# 複数検索可能(OR条件)である。

dpc_dxnm_post <- function(df, new_colname, ...) {
  
  df_temp <- df %>%
    select(starts_with("d")) %>% #dからはじまる列だけ取り出す
    mutate(   
      # 疑い病名がついていないものは0、疑い病名がついているものは1を返す
      
      dcc1nm_suspect = str_detect(.$dcc1nm, "疑い") %>% as.integer(),
      dcc2nm_suspect = str_detect(.$dcc2nm, "疑い") %>% as.integer(),
      dcc3nm_suspect = str_detect(.$dcc3nm, "疑い") %>% as.integer(),
      dcc4nm_suspect = str_detect(.$dcc4nm, "疑い") %>% as.integer(),
      dcc5nm_suspect = str_detect(.$dcc5nm, "疑い") %>% as.integer(),
      dcc6nm_suspect = str_detect(.$dcc6nm, "疑い") %>% as.integer(),
      dcc7nm_suspect = str_detect(.$dcc7nm, "疑い") %>% as.integer(),
      dcc8nm_suspect = str_detect(.$dcc8nm, "疑い") %>% as.integer(),
      dcc9nm_suspect = str_detect(.$dcc9nm, "疑い") %>% as.integer(),
      dcc10nm_suspect = str_detect(.$dcc10nm, "疑い") %>% as.integer(),
      
      # 日本語病名に疑いがついていないものはdefiniteとしてICD10-codeを返す
      
      dcc1nm_definite = if_else(dcc1nm_suspect == 0, dcc1nm, NA_character_),
      dcc2nm_definite = if_else(dcc2nm_suspect == 0, dcc2nm, NA_character_),
      dcc3nm_definite = if_else(dcc3nm_suspect == 0, dcc3nm, NA_character_),
      dcc4nm_definite = if_else(dcc4nm_suspect == 0, dcc4nm, NA_character_),
      dcc5nm_definite = if_else(dcc5nm_suspect == 0, dcc5nm, NA_character_),
      dcc6nm_definite = if_else(dcc6nm_suspect == 0, dcc6nm, NA_character_),
      dcc7nm_definite = if_else(dcc7nm_suspect == 0, dcc7nm, NA_character_),
      dcc8nm_definite = if_else(dcc8nm_suspect == 0, dcc8nm, NA_character_),
      dcc9nm_definite = if_else(dcc9nm_suspect == 0, dcc9nm, NA_character_),
      dcc10nm_definite = if_else(dcc10nm_suspect == 0, dcc10nm, NA_character_)
    ) %>%
    mutate_all(funs(ifelse(is.na(.), 0, .)))  # NAを0にしておかないと、下の|またはの条件でうまくいかない。。
  
  quo_new_colname <- enquo(new_colname)
  quosures <- quos(...)
  n_quosures <- length(quosures)
  
  for(i in 1:n_quosures) {
    regular_exp <- paste0(quo_name(quosures[[i]]))
    colnames_temp <- paste0(quo_name(quo_new_colname), "_", quo_name(quosures[[i]]))  # forループをまわすたびに、新たな列名をつける必要がある
    df_temp_2 <- df_temp %>%
      mutate(   
        !!colnames_temp := ifelse(
          str_detect(dcc1nm_definite, regular_exp)|
            str_detect(dcc2nm_definite, regular_exp)|
            str_detect(dcc3nm_definite, regular_exp)|
            str_detect(dcc4nm_definite, regular_exp)|
            str_detect(dcc5nm_definite, regular_exp)|
            str_detect(dcc6nm_definite, regular_exp)|
            str_detect(dcc7nm_definite, regular_exp)|
            str_detect(dcc8nm_definite, regular_exp)|
            str_detect(dcc9nm_definite, regular_exp)|
            str_detect(dcc10nm_definite, regular_exp)
          , 1, 0)
      ) %>%
      select(ncol(.))
    df_temp <- bind_cols(df_temp, df_temp_2)
  }
  
  #　各ループの結果（ex. DM_糖尿病足壊疽, DM_糖尿病網膜症, ...の合計）を統合する
  result <- df_temp %>%
    select((ncol(.) - n_quosures + 1) : ncol(.)) %>%
    pmap_dbl(sum)
  # どれか1つでも満たしていればOKなので、合計が2以上のものはすべて1としてカウントしてよい。
  result_2 <- if_else(result >= 1, 1, 0)
  
  # 最後に、自分のつけたい列名に、病名ありなしの結果をいれればよい。
  df_temp_4 <- df %>%
    mutate(!!quo_name(quo_new_colname) := result_2)
  return(df_temp_4)
}



