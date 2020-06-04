if (!require(tidyverse)) {
  install.packages("tidyverse", dependencies=TRUE, repos = 'http://cran.us.r-project.org')
  library(tidyverse)
}else{library(tidyverse)}

if (!require(viridis)) {
  install.packages("viridis", dependencies=TRUE, repos = 'http://cran.us.r-project.org')
  library(viridis)
}else{library(viridis)}

rawdat <- 
  read.table(file = "D:/Research/NF_GzGr/NOR3Cr003_Bac/NOR3Cr003_Bac.csv", 
             sep = ",", header = TRUE, stringsAsFactors = FALSE, fill = TRUE) %>%
  filter(!grepl("blank", Tube, fixed  = FALSE)) %>%
  mutate(
    Bottle = substr(Tube, 5, 5),
    Trmt = substr(Tube, 7, 9),
    Rep = substr(Tube, 11, 12),
    Sub = substr(Tube, 14, 14)
  )

Sdat <- rawdat %>%
  filter(Bottle == "S") %>%
  group_by(Trmt) %>%
  summarize(
    HB = mean(HB),
    HiNA = mean(HiNA),
    MedNA = mean(MedNA),
    LowNA = mean(LowNA)
  ) %>%
  mutate(
    HB_T0 = HB[which(Trmt == "T00")],
    HiNA_T0 = HiNA[which(Trmt == "T00")],
    MedNA_T0 = MedNA[which(Trmt == "T00")],
    LowNA_T0 = LowNA[which(Trmt == "T00")],
    frac = c(seq(from = 0, to = 1, by = 0.2), rep(1, 4))
  ) %>%
  mutate(
    HB_r = (log(HB) / log(HB_T0 * frac)) / 0.5,
    HiNA_r = (log(HiNA) / log(HiNA * frac)) / 0.5,
    MedNA_r = (log(MedNA) / log(MedNA * frac)) / 0.5,
    LowNA_r = (log(LowNA) / log(LowNA * frac)) / 0.5
  )

summary(lm(HB_r ~ frac, data = Sdat[2:6, ]))
summary(lm(HiNA_r ~ frac, data = Sdat[2:6, ]))
summary(lm(MedNA_r ~ frac, data = Sdat[2:6, ]))
summary(lm(LowNA_r ~ frac, data = Sdat[2:6, ]))

SBacGz_p <- Sdat[2:6,] %>%
  select(frac, HB_r, HiNA_r, MedNA_r, LowNA_r) %>%
  gather(key = "Bacteria_group", value = "Net_growth_rate", -frac) %>%
  mutate(Sig = ifelse(Bacteria_group == "HiNA_r" | Bacteria_group =="LowNA_r", "1", "2")) %>%
  ggplot(aes(x = frac, y = Net_growth_rate, color = Bacteria_group)) + 
    geom_point(size = 8) + 
    scale_colour_viridis(alpha = 1, discrete=TRUE) +
    geom_smooth(formula = y ~ x, method = "lm", se = FALSE, aes(linetype = Sig)) + 
    #geom_smooth(method = mgcv::gam, formula = y ~ s(x), se = TRUE, color = "red", linetype = "solid") + 
    labs(x = bquote("Dilution factor"),
         y = bquote(paste("Net growth rate (day " ^ "-1", ")"))) + 
    #annotate("text", x = 0.18, y = 1, label = "paste( \"conditional \", italic(R) ^ 2, \" = 0.18\")", parse = TRUE, size = 6) + 
    theme(
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.title = element_text(size = 32),
      axis.text = element_text(size = 24),
      legend.title = element_text(size = 24),
      legend.text = element_text(size = 24)
    )
SBacGz_p

SBacGz_p <- Sdat[2:6,] %>%
  select(frac, HB_r, HiNA_r, MedNA_r, LowNA_r) %>%
  gather(key = "Bacteria_group", value = "Net_growth_rate", -frac) %>%
  mutate(Sig = ifelse(Bacteria_group == "HiNA_r" | Bacteria_group =="LowNA_r", "1", "2")) %>%
  ggplot(aes(x = frac, y = Net_growth_rate, color = Bacteria_group)) + 
  geom_point(size = 8) + 
  scale_colour_viridis(alpha = 1, discrete=TRUE) +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE, aes(linetype = Sig)) + 
  #geom_smooth(method = mgcv::gam, formula = y ~ s(x), se = TRUE, color = "red", linetype = "solid") + 
  labs(x = bquote("Dilution factor"),
       y = bquote(paste("Net growth rate (day " ^ "-1", ")"))) + 
  #annotate("text", x = 0.18, y = 1, label = "paste( \"conditional \", italic(R) ^ 2, \" = 0.18\")", parse = TRUE, size = 6) + 
  theme(
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size = 32),
    axis.text = element_text(size = 24),
    legend.title = element_text(size = 24),
    legend.text = element_text(size = 24)
  )
SBacGz_p
