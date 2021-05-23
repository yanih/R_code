# Data description----
cjb %>%
  group_by(wlfk) %>%
  summarise(
    count = n(),
    sx_median = median(sx),
    sx_mean = mean(sx),
    sx_range = max(sx)-min(sx),
    sx_IQR = IQR(sx)#四分位距
  )
# equal to following code which uses apply()
round(apply(cjb[,4:12],2,function(x){
  c(
    mean = mean(x),
    median = median(x),
    range = diff(range(x)),#diff=max-min
    IQR = IQR(x)
  )
}))
