

##### krona plot
##### by Qiushi-Li, IM-CAS
##### 2024.11.18


# packages -----------------
library(tidyverse)
library(ggsci)


# data from step 2 ---------------
rCNV_rlt_taxa_spl


# subdata for krona plot
rCNV_krona_df <- rCNV_rlt_taxa_spl %>% select(project, counts, phy, cla, ord, fam)
rCNV_krona_df


# width to long
rCNV_krona_df1 <- 
  rCNV_krona_df %>% gather(key = "grp", value = "val", 3:6)

# fulltaxa for count level
rCNV_krona_fulltaxa <- rCNV_krona_df %>% mutate(
  fulltaxa = str_c(phy, cla, ord, fam, sep = "000")
) %>% select(project, fulltaxa)

rCNV_krona_df1_fulltaxa <- rCNV_krona_df1 %>% left_join(rCNV_krona_fulltaxa, by = "project")

# levels
rCNV_krona_df1_fff <- 
  rCNV_krona_df1_fulltaxa %>% mutate(
  kro_ind = case_when(
    grp == "phy" ~ str_split_i(fulltaxa, "000", 1),
    grp == "cla" ~ str_c(str_split_i(fulltaxa, "000", 1), str_split_i(fulltaxa, "000",2), sep = "000"),
    grp == "ord" ~ str_c(str_split_i(fulltaxa, "000", 1), str_split_i(fulltaxa, "000",2), str_split_i(fulltaxa, "000",3), sep = "000"),
    grp == "fam" ~ fulltaxa
  )
) %>% select(-fulltaxa)

# prepare krona plot input data
rCNV_krona_df2 <- 
  rCNV_krona_df1_fff %>% mutate(level = str_count(kro_ind, "000"))

rCNV_krona_df2_phy <- 
  rCNV_krona_df2 %>% 
  filter(grp == "phy") %>% 
  group_by(kro_ind) %>% count(kro_ind) %>% ungroup() %>% 
  mutate(prp = n/sum(n)) %>% 
  mutate(grp = "phy") %>% 
  mutate(lab = str_split_i(kro_ind, "000", 1)) %>% 
  mutate(fil = str_sub(kro_ind, start = 1, end = 5)) %>% 
  mutate(alp = 1) %>% 
  mutate(lab = str_sub(lab, start = 1, end = 5)) %>% 
  mutate(lab = str_c(lab, "\n", str_c(round(prp, 4)*100, "%"))) %>% 
  mutate(lab = if_else(prp > 0.05, lab, NA)) %>% 
  mutate(posi = 1 - cumsum(prp) + 0.5*prp) %>% 
  mutate(ang = (cumsum(prp) - 0.5 * prp) / sum(prp) * 270 - 90) %>% 
  group_by(fil) %>% arrange(desc(kro_ind)) %>% ungroup()


rCNV_krona_df2_cla <- 
  rCNV_krona_df2 %>% 
  filter(grp == "cla") %>% 
  group_by(kro_ind) %>% count(kro_ind) %>% ungroup() %>% 
  mutate(prp = n/sum(n)) %>% 
  mutate(grp = "cla") %>% 
  mutate(lab = str_split_i(kro_ind, "000", 2)) %>% 
  mutate(fil = str_sub(kro_ind, start = 1, end = 5)) %>% 
  mutate(alp = 0.7) %>% 
  mutate(lab = str_sub(lab, start = 1, end = 5)) %>% 
  mutate(lab = str_c(lab, "\n", str_c(round(prp, 4)*100, "%"))) %>% 
  mutate(lab = if_else(prp > 0.05, lab, NA)) %>% 
  mutate(posi = 1 - cumsum(prp) + 0.5*prp) %>% 
  mutate(ang = (cumsum(prp) - 0.5 * prp) / sum(prp) * 270 - 90) %>% 
  group_by(fil) %>% arrange(desc(kro_ind)) %>% ungroup()


rCNV_krona_df2_Ord <- 
  rCNV_krona_df2 %>% 
  filter(grp == "ord") %>% 
  group_by(kro_ind) %>% count(kro_ind)  %>% ungroup() %>% 
  mutate(prp = n/sum(n)) %>% 
  mutate(grp = "ord") %>% 
  mutate(lab = str_split_i(kro_ind, "000", 3)) %>% 
  mutate(fil = str_sub(kro_ind, start = 1, end = 5)) %>% 
  mutate(alp = 0.5) %>% 
  mutate(lab = str_sub(lab, start = 1, end = 5)) %>% 
  mutate(lab = str_c(lab, "\n", str_c(round(prp, 4)*100, "%"))) %>% 
  mutate(lab = if_else(prp > 0.04, lab, NA)) %>%  
  mutate(posi = 1 - cumsum(prp) + 0.5*prp) %>% 
  mutate(ang = (cumsum(prp) - 0.5 * prp) / sum(prp) * 270 - 90) %>% 
  group_by(fil) %>% arrange(desc(kro_ind)) %>% ungroup()

# is.na ?  
is.na(rCNV_krona_df2_phy$fil) %>% table()
is.na(rCNV_krona_df2_cla$fil) %>% table()
is.na(rCNV_krona_df2_Ord$fil) %>% table()

# cbind
rCNV_krona_df2_cbind <- rbind(rCNV_krona_df2_phy, rCNV_krona_df2_cla, rCNV_krona_df2_Ord)


# factors
rCNV_krona_df2_cbind$grp <- factor(rCNV_krona_df2_cbind$grp, levels = c("phy", "cla", "ord"))

rCNV_krona_df2_phy$kro_ind
fig2_2 <- 
  ggplot(rCNV_krona_df2_cbind, aes(grp, n, fill = fil)) +
  geom_col(aes(alpha = alp), colour = "black", position = "fill") +
  geom_text(aes(grp, posi, label = lab,
                angle = ang), vjust = 0.5) +
  scale_fill_d3(palette = "category20", label = rev(rCNV_krona_df2_phy$kro_ind)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  guides(alpha = "none",
         fill = guide_legend(title = "Phylum",
                             ncol = 1
                             )) + 
  coord_radial("y", start = pi, end = 2.5 * pi) +
  labs(x = NULL, y = NULL) +
  theme_void() +
  theme(legend.title = element_text(face = "bold", size = rel(1.2)),
        legend.box.background = element_rect(),
        legend.margin = margin(6, 6, 6, 6),
        legend.position = "inside",
        legend.position.inside = c(0.70, 0.30),
        legend.key.size = unit(0.62, "cm"),
        legend.text = element_text(size = rel(1), face = "italic"),
        plot.background = element_blank()
  )
fig2_2

ggsave("krona.jpg", fig2_2)

# done.
