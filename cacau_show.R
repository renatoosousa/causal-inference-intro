library(dplyr)
library(CausalImpact)
library(babynames)
library(ggplot2)
library(tidyr)
library(xts)

fluxo <- read.csv("/Users/renato.sousa/Desktop/2019/04_Abril/hackathon/cacau_show_fluxo.csv") %>% dplyr::select(dt, id, total_visits)


fluxo_2 <-
  fluxo %>%
  mutate(dt = as.Date(dt, "%Y-%m-%d")) %>%
  spread(id, total_visits)

fluxo_3 <-
  fluxo_2 %>% 
  dplyr::select(dt, "cacauShow" = "Cacau Show", "brasilCacau" = "BRASIL CACAU", "LINDT", "Ofner")

fluxo_4 <- 
  fluxo_3 %>%
  dplyr::select(-dt) %>%
  as.xts(order.by = fluxo_3$dt)

pre_period <- as.Date(c("2018-08-01", "2018-09-28"))
post_period <- as.Date(c("2018-09-29", "2018-11-01"))

fluxo_causal <- CausalImpact(fluxo_4, 
                             pre.period = pre_period, 
                             post.period = post_period,
                             model.args = list(nseasons = 7, season.duration = 1),
                             alpha = 0.01)

plot(fluxo_causal)
summary(fluxo_causal, "report")
