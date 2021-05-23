# Data visualization
# Template:
# ggplot(data = <DATA>) +
#   <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>),
#                   stat = <STAT>,
#                   position = <POSITION>) +
#   <COORDINATE_FUNCTION> +
#   <FACET_FUNCTION>

# data preparation
library(tidyverse)
library(readxl)
cjb<-read_excel('cjb.xlsx')

# 1D----
# stem plot----
cjb %>%
  dplyr::filter(bj == "1110") %>%
  select(sx) %>%
  as_vector() %>%
  stem(scale = 2)

# histogram----
sx_hist_results <- hist(cjb$sx,
                        plot = TRUE)
names(sx_hist_results)
sx_hist_results$breaks

ggplot(data = cjb, mapping = aes(sx)) +
  geom_histogram(breaks = sx_hist_results$breaks,
                 color = "darkgray",
                 fill = "pink") +
  stat_bin(breaks = sx_hist_results$breaks,
           geom = "text",
           aes(label = ..count..)) 
ggsave("histogram1.png", dpi = 600)
#dpi一般设为300，就可以达到印刷要求了
#换言之，发表论文时，dpi设置为300也就足够了
#当然，dpi值越高、质量越好

# box-plot----
# Q1-1.5IQR——|Q1-Median-Q3|——Q3+1.5IQR
# lower whisker: max(min(x),Q1-1.5IQR)
# upper whisker: min(max(x),Q3+1.5IQR)
# combined with violin:
cjb %>%
  ggplot(aes(x = factor(0), y = sx))+
  geom_violin(fill = '#FFC0CB',width = 0.75)+
  geom_boxplot(width = 0.25,
               fill = '#B0C4DE',
               outlier.color = 'Red',
               outlier.shape = 1,
               outlier.size = 2)+
  geom_rug(position = 'jitter',
           size = 0.1,
           sides = 'b')+
  coord_flip()
# other plot----
cjb %>%
  dplyr::filter(bj == "1110" & xb == "男") %>%
  select(xm, sx) %>%
  mutate(sx_z = (sx - mean(sx)) / sd(sx),
         sx_type = ifelse(sx_z >= 0, "above", "below")) %>%
  arrange(sx_z) %>%
  ggplot(aes(x = fct_inorder(xm), y = sx_z, label = sx_z)) +
  geom_bar(stat = 'identity', aes(fill = sx_type), width = 0.5)  +
  scale_fill_manual(
    name = "Math Score",
    labels = c("Above Average", "Below Average"),
    values = c("above" = "#00ba38", "below" = "#f8766d")
  ) +
  coord_flip() +
  theme_bw()


g <- ggplot(cjb, aes(sx))
sx_hist_results <- hist(cjb$sx,
                        plot = FALSE)
g + geom_histogram(aes(fill = bj),
                   breaks = sx_hist_results$breaks,
                   col = "black",
                   size = .1) + 
  scale_fill_brewer(palette = "Spectral")

# 2D----
# treemap (2 discrete variables)----
library(treemap)
cjb %>%
  group_by(wlfk,bj,xb)%>%
  summarise(count = n())%>%
  as.data.frame()%>%
  treemap(
    index = c('wlfk','bj','xb'),
    vSize = 'count',
    vColor = 'count',
    type = 'value'
  )

# scatter plot (2 continuous variables)----
ggplot(cjb,
       aes(x=sx,
           y=sw,
           shape=wlfk,
           colour=wlfk))+
  geom_point(size = 2)+
  labs(x='math',
       y='biology',
       colour='category',
       shape='category')

# scatter matrix (multiple variables)
GGally::ggpairs(cjb,columns = 4:12)

# correlation coefficient
(cor_coef<-cor(cjb[,4:12]))
cor_coef %>%
  as.data.frame()%>%
  rownames_to_column(var = 'km1') %>% #将行名称汇总为一列
  gather(key = km2, value = cor_num, -km1) %>% #宽数据转化为长数据
  mutate(cor_level = cut(cor_num,
                         breaks = c(0,0.3,0.5,0.8,1),
                         right = FALSE)) %>%
  ggplot(aes(x = km1,y=km2,fill = cor_level))+
  geom_tile(colour = 'white',size = 1.5)+
  geom_text(aes(label = format(cor_num,digits = 2)))+
  scale_fill_brewer(palette = 'Y1Gn',name='cor CI')

# grouping plot
cjb$bj<-as.factor(cjb$bj)
ggplot(cjb,aes(group=bj,
               x=bj,
               y=sx,
               fill=bj))+
  geom_boxplot(outlier.colour = 'red',
               outlier.shape = 3,
               outlier.size = 1)+
  labs(x='class',y='math score')+
  theme(legend.position = 'none')

# ridges plot
install.packages(c('ggridges','viridis'))
library(ggridges)#绘制层峦叠嶂图
library(viridis)#采用其中的颜色
ggplot(cjb, aes(x = sx, y = bj, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 2,
                               rel_min_height = 0.01,
                               gradient_lwd = 1) +
  scale_fill_viridis(name = "math score",
                     option = "C") +
  labs(x = "math", y = "class")

# Y vs X
# install.packages('caret')
# library(caret)
# featurePlot(
#   x = cjb[, 4:12],
#   y = cjb$wlfk,
#   plot = "density",
#   scales = list(
#     x = list(relation = "free"),
#     y = list(relation = "free")
#   ),
#   adjust = 1.5,
#   pch = "|",
#   auto.key = list(columns = 2))

# 3D ----

