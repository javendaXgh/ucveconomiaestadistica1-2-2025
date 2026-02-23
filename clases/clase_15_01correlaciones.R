library(openintro)
shinyWidgets::shinyWidgetsGallery()
shinyWidgets::updateSele
openintro
write.csv(openintro::ames,'data/ames.csv')
burger
corr_match
write.csv(corr_match,'data/corr_match.csv')
library(anscombiser)
install.packages('anscombiser')
anscombiser::anscombe2

library(ggplot2)
ggplot2::ggplot(anscombe2)+
  geom_point(aes(x=x2,y=y2))

anscombi <- bind_cols(anscombe1,
                      anscombe2,
                      anscombe3,
                      anscombe4)%>%
  select(-x2,-x3,-x4)


write.csv(anscombi,'data/anscombi.csv')