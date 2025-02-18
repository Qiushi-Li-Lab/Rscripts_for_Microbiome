
# venn图矩阵转换
venn_trans <- function(df) {
  
  df_ncol <- ncol(df) # 获取列数
  df_name <- colnames(df) # 获取列名
  
  # 创建索引列
  df_index <- df %>% 
    gather(key = "state", value = "index", 1:all_of(df_ncol)) %>% 
    select("index") %>% unique() %>%  drop_na()
  
  # 创建字典列表
  df_index_sub <- vector(mode = "list") 
  # 构建字典列表
  for (i in 1:all_of(df_ncol)) {
    
    df_index_sub[[i]] <- data.frame(
      index = df[i],
      exi = 1
    ) %>% drop_na()
    colnames(df_index_sub[[i]]) <- c("index", df_name[i])
    
  }
  
  # 备份数据
  dyd_upset_df <- df_index
  
  # 匹配构建
  for(i in 1:all_of(df_ncol)) {
    
    dyd_upset_df <- dyd_upset_df %>% 
      left_join(
        df_index_sub[[i]], by = "index"
      )
  }
  
  # 转换为数据框格式
  dyd_upset_df <- as.data.frame(dyd_upset_df)
  
  # NA的处理
  dyd_upset_df[is.na(dyd_upset_df)] <- 0
  
  # 返回目标矩阵
  return(dyd_upset_df)
  
}

Venn_index <- function(lst) {
  lst_name <- names(lst)
  leg <- vector()
  lst_leg <- length(lst)
  venn_lst <- list()
  for(i in 1:lst_leg) {
    leg[i] <- length(lst[[i]])
  }
  leg_max <- max(leg)
  for(i in 1:lst_leg) {
    venn_lst[[i]] <- c(lst[[i]], rep(NA, leg_max - length(lst[[i]])))
  }
  names(venn_lst) <- lst_name
  venn_df <- as.data.frame(venn_lst)
  return(venn_df)
}