library(dplyr)
library(CausalImpact)
library(babynames)
library(ggplot2)
library(tidyr)
library(xts)

fluxo <- read.csv("/Users/renato.sousa/Desktop/2019/04_Abril/hackathon/extra_fluxo.csv") %>% dplyr::select(dt, id, total_visits)

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
  dplyr::select(dt, "EXTRA","extraControle" = "EXTRA CONTROLE")

fluxo_4 <- 
  fluxo_3 %>%
  dplyr::select(-dt) %>%
  as.xts(order.by = fluxo_3$dt)

pre_period <- as.Date(c("2018-08-01", "2018-10-02"))
post_period <- as.Date(c("2018-10-03", "2018-11-01"))

fluxo_causal <- CausalImpact(fluxo_4, 
                             pre.period = pre_period, 
                             post.period = post_period,
                             alpha=0.01)

plot(fluxo_causal)
summary(fluxo_causal, "report")
