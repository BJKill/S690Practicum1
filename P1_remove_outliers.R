#### Code nice way to identify and remove outliers

library(tidyverse)

# color blind pallet - we were told to use it in EDA
cb_palette = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(na.omit(p1.Data), aes(x=shift, y=lbs_gained, color=gender)) + geom_boxplot() +
  scale_color_manual(values  =  cb_palette)

p1.Data1 <- na.omit(p1.Data)


is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

p1.Data1 %>%
  group_by(shift, gender) %>%
  mutate(outlier = ifelse(is_outlier(lbs_gained), 
                          lbs_gained, 
                          as.numeric(NA))) %>%
  ggplot(., aes(x = shift, y = lbs_gained, color = gender)) +
  geom_boxplot() +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.6) +
  scale_color_manual(values  =  cb_palette[2:3])

p1.Data.out <- p1.Data1 %>%
  group_by(shift) %>%
  mutate(outlier = ifelse(is_outlier(lbs_gained), "Yes", "No"))

p1.Data2 <- p1.Data.out %>%
  filter(outlier == "No")




p1.Data2 <- p1.Data2[,-ncol(p1.Data2)]

p1.Data2 %>%
  group_by(shift, gender) %>%
  mutate(outlier = ifelse(is_outlier(lbs_gained), 
                          lbs_gained, 
                          as.numeric(NA))) %>%
  ggplot(., aes(x = shift, y = lbs_gained, color = gender)) +
  geom_boxplot() +
  geom_text(aes(label = outlier, na.rm = TRUE, hjust = -0.3)) +
  scale_color_manual(values  =  cb_palette[2:3])



p1.Data.out2 <- p1.Data2 %>%
  group_by(shift, gender) %>%
  mutate(outlier = ifelse(is_outlier(lbs_gained), "Yes", "No"))

p1.Data3 <- p1.Data.out2 %>% filter(outlier == "No")

p1.Data3 %>%
  group_by(shift, gender) %>%
  mutate(outlier = ifelse(is_outlier(lbs_gained), lbs_gained, as.numeric(NA))) %>%
  ggplot(., aes(x = shift, y = lbs_gained, color = gender)) +
  geom_boxplot() +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  scale_color_manual(values  =  cb_palette[2:3])



zipmod <- zeroinfl(round(lbs_gained,0)~oshift+I(Total_Met_Min/100)+
                     beg_weight*gender+Age*gender, data = p1.Data1)
zipmod2 <- zeroinfl(round(lbs_gained,0)~oshift+I(Total_Met_Min/100)+
                      beg_weight*gender+Age*gender, data = p1.Data2)
zipmod3 <- zeroinfl(round(lbs_gained,0)~oshift+I(Total_Met_Min/100)+
                      beg_weight*gender+Age*gender, data = p1.Data3)
summary(zipmod)
summary(zipmod2)
summary(zipmod3)
