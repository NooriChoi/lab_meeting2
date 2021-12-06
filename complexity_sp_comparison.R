# Noori Choi
# schiz compelxity comparison
# 2021-10-14
# comparison of complexity metrics among species

# import packages ---------------------------------------------------------
library(lme4)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(broom)
library(ggcorrplot)
library(emmeans)
library(multcomp)
library(multcompView)

# classification accuracy ---------------------------------------------------------
## import datasets
bil_df <- read.csv('bilineata_classification_class2_211130.csv', header=T, na.strings=c('NA'))
crp_df <- read.csv('crassipalpata_classification_class2_211130.csv', header=T, na.strings=c('NA'))
dup_df <- read.csv('duplex_classification_class2_211130.csv', header=T, na.strings=c('NA'))
str_df <- read.csv('stridulans_classification_class2_211130.csv', header=T, na.strings=c('NA'))
uet_df <- read.csv('uetzi_classification_class2_211130.csv', header=T, na.strings=c('NA'))
all_df <- read.csv('allsp_classification_211129.csv', header=T, na.strings=c('NA'))

bil_df %>%
  mutate('species'='bil') -> bil_df
crp_df %>%
  mutate('species'='crp') -> crp_df
dup_df %>%
  mutate('species'='dup') -> dup_df
str_df %>%
  mutate('species'='str') -> str_df
uet_df %>%
  mutate('species'='uet') -> uet_df
all_df %>% 
  mutate('species'='all') -> all_df

bind_rows(list(bil_df, crp_df, dup_df, str_df, uet_df, all_df)) -> accuracy_df

## wilcox test
combos <- unique(accuracy_df$metric) %>%
  combn(2, simplify = F) %>%
  set_names(map_chr(., ~ paste(., collapse = "_")))

accuracy_df %>% 
  group_split(species) %>%
  set_names(map_chr(., ~ unique(.$species))) %>% 
  map_df(function(x) {
    map_df(combos, function(y) {
      filter(x, metric %in% y) %>% 
        wilcox.test(accuracy ~ metric, exact=FALSE, data = .) %>% 
        broom::tidy()
    }, .id = "contrast")
  }, .id = "trial") %>% 
  write.csv(., file='wilcox_result_211130.csv')

## plot
accuracy_df %>%
  mutate(species = factor(species, levels=c("dup", "bil", "crp", "str", "uet", "all")),
         metric = factor(metric, levels=c("chr_euc", "mfcc_dtw", "spcc", "pca_dtw",
                                          "sax_ld", "sax_twed"))) %>% 
  ggplot(aes(x=species, y=accuracy, fill=metric)) +
  geom_boxplot() +
  geom_jitter(position=position_dodge(0.8))+
  labs(x = "Species", y = "Accuracy", fill='1NN Classifiers') +
  scale_x_discrete(labels=c("dup" = "S. duplex", "bil" = "S. bilineata", "crp"='S. crassipalpata',
                            "str"="S. stridulans", "uet" = "S. uetzi")) +
  scale_fill_discrete(labels=c("chr_euc" = "AF", "mfcc_dtw" = "MFCCs", 
                               "spcc"="SPCC", "pca_dtw"='Time series PCs',
                               "sax_ld"="MSAX-PCA+DL", "sax_twed" = "MSAX-PCA+TWED")) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        axis.text.y =  element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold.italic"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, face = 'bold'))
### add explanation about figures
### changed colors line 76
### changed lines 
### add few lines
