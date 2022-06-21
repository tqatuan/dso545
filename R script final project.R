
library(tidyverse)
vaccine <- read.csv("vaccine.csv")
library(ggplot2)


vaccine$subregion=vaccine$County.Name

vaccine$subregion=tolower(vaccine$County.Name)

USA <- map_data("county")

merged_df=left_join(USA, vaccine, by="subregion")


installed.packages("corrplot")
library(corrplot)
library(psych)


pairs.panels(merged_df[1:4])

ggplot(merged_df, aes(long, lat,group=group, fill= (COVID.Cases/Population)))+
  geom_polygon( colour = "gray", size = 0.01)+ 
  scale_fill_gradient2(low = "white", mid = "orange3", high="darkred")+ theme_void()+ labs( fill = "Covid cases per cap")+
  ggtitle("COVID Cases per capita by county")


ggplot(merged_df, aes(long, lat,group=group, fill= COVID.Deaths/Population))+
  geom_polygon( colour = "grey", size = 0.01)+ 
  scale_fill_gradient2(low = "darkcyan", mid = "beige", high="yellow4")+ theme_void() + labs( fill = "Covid deaths per cap")+
  ggtitle("COVID Deaths per capita by county")


  
rm <- lm(Estimated.strongly.hesitant ~ pct_hispanic + 
           pct_white_.non_hispanic +pct_black_.non_hispanic+
           pct_asian_non_hispanic, data = merged_df)

library(car)
avPlots(rm)




