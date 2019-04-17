library(dplyr)
library(CausalImpact)
library(babynames)
library(ggplot2)
library(tidyr)
library(xts)

fluxo <- read.csv("/Users/renato.sousa/Desktop/2019/04_Abril/hackathon/extra_protesto_fluxo.csv") %>% dplyr::select(dt, id, total_visits)

fluxo<- fluxo%>%
  group_by(id,dt)%>%
  summarise(
    visits = sum(total_visits)
  )


fluxo_2 <-
  fluxo %>%
  mutate(dt = as.Date(dt, "%Y-%m-%d")) %>%
  spread(id, visits)

fluxo_3 <-
  fluxo_2 %>% 
  dplyr::select(dt, "ExtraProtesto",  "Extra")

fluxo_4 <- 
  fluxo_3 %>%
  dplyr::select(-dt) %>%
  as.xts(order.by = fluxo_3$dt)

pre_period <- as.Date(c("2018-12-01", "2019-02-13"))
post_period <- as.Date(c("2019-02-14", "2019-02-19"))

fluxo_causal <- CausalImpact(fluxo_4, 
                             pre.period = pre_period, 
                             post.period = post_period,
                             alpha=0.01)

plot(fluxo_causal)
summary(fluxo_causal, "report")
