if (!require(tidyverse)) {
  install.packages("tidyverse", dependencies=TRUE, repos = 'http://cran.us.r-project.org')
  library(tidyverse)
}else{library(tidyverse)}

if (!require(viridis)) {
  install.packages("viridis", dependencies=TRUE, repos = 'http://cran.us.r-project.org')
  library(viridis)
}else{library(viridis)}

T0 <- read.table(file = "https://raw.githubusercontent.com/OscarFHC/NF_GzGr/master/Data/NOR3Cr0015/NOR3Cr0015_NF.csv", 
                  sep = ",", header = TRUE, stringsAsFactors = FALSE, fill = TRUE) %>%
  filter(Time == "T0")

T12 <- read.table(file = "https://raw.githubusercontent.com/OscarFHC/NF_GzGr/master/Data/NOR3Cr0015/NOR3Cr0015_NF.csv", 
                 sep = ",", header = TRUE, stringsAsFactors = FALSE, fill = TRUE) %>%
  filter(Time == "T12") %>%
  mutate(St = substr(Station, 1, 1),
         Seq = substr(Station, 3, 3))

for (i in 1: nrow(T12)){
  id <- which(T0[, "Station"] == T12[i, "Station"] & T0[, "DF"] == T12[i, "DF"])
  T12[i, "NetG"] <- log(T12[i, "HNF"]/T0[id, "HNF"])/0.5
}

T12_p <- T12 %>% 
  ggplot(aes(x = DF, y = NetG)) + 
    geom_point(aes(color = Station), size = 4) + 
    scale_color_viridis_d() + 
    labs(x = bquote("Dilution factor (%)"),
         y = bquote(paste("Net growth rate (day " ^ "-1", ")"))) +   
    facet_grid(rows = vars(St), cols = vars(Seq), scales = "free") + 
    theme(
      #panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.title = element_text(size = 32),
      axis.text = element_text(size = 24),
      legend.title = element_text(size = 24),
      legend.text = element_text(size = 24),
      axis.text.x = element_text(angle = 45), 
      panel.spacing = unit(1, 'lines')
    )
T12_p
ggsave(T12_p, file = "D:/Dropbox/Research/NF_GzGr/Results/NOR3CR0015/NFDilu_T12.png",
       dpi = 600, width = 50, height = 30, units = "cm")

T24 <- read.table(file = "D:/Dropbox/Research/NF_GzGr/Data/NOR3Cr0015/NOR3Cr0015_NF.csv", 
                  sep = ",", header = TRUE, stringsAsFactors = FALSE, fill = TRUE) %>%
  filter(Time == "T24") %>%
  filter(HNF != "NA") %>%
  mutate(St = substr(Station, 1, 1),
         Seq = substr(Station, 3, 3))

for (i in 1: nrow(T24)){
  id <- which(T0[, "Station"] == T12[i, "Station"] & T0[, "DF"] == T12[i, "DF"])
  T24[i, "NetG"] <- log(T24[i, "HNF"]/T0[id, "HNF"])/1
}

T24_p <- T24 %>% 
  ggplot(aes(x = DF, y = NetG)) + 
  geom_point(aes(color = Station), size = 4) + 
  scale_color_viridis_d() + 
  labs(x = bquote("Dilution factor (%)"),
       y = bquote(paste("Net growth rate (day " ^ "-1", ")"))) +   
  facet_grid(rows = vars(St), cols = vars(Seq), scales = "free") + 
  theme(
    #panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size = 32),
    axis.text = element_text(size = 24),
    legend.title = element_text(size = 24),
    legend.text = element_text(size = 24),
    axis.text.x = element_text(angle = 45), 
    panel.spacing = unit(1, 'lines')
  )
T24_p
ggsave(T24_p, file = "D:/Dropbox/Research/NF_GzGr/Results/NOR3CR0015/NFDilu_T24.png",
       dpi = 600, width = 50, height = 30, units = "cm")


rawdat <- 
  read.table(file = "D:/Dropbox/Research/NF_GzGr/NOR3Cr003_Bac/NOR3Cr003_Bac.csv", 
             sep = ",", header = TRUE, stringsAsFactors = FALSE, fill = TRUE) %>%
  filter(!grepl("blank", Tube, fixed  = FALSE)) %>%
  mutate(
    Bottle = substr(Tube, 5, 5),
    Trmt = substr(Tube, 7, 9),
    Rep = substr(Tube, 11, 12),
    Sub = substr(Tube, 14, 14)
  )

rawdat %>% 
  ggplot(aes(x = Trmt, y = HB, color = Sub)) +
    geom_point()
  

Sdata <- rawdat %>%
  filter(Bottle == "S")

Ldata <- rawdat %>%
  filter(Bottle == "L")

dat <- Ldata %>%
  #filter(Trmt %in% c("T00", "T02", "T03", "T04", "T05", "T06")) %>%
  filter(HB > 10) %>%
  inner_join(Sdata, by = c("Trmt" = "Trmt", "Rep" = "Rep", "Sub" = "Sub"))

summary(lm(HB.x ~ HB.y, data = dat))

dat %>%
  ggplot(aes(x = HB.x, y = HB.y)) + 
    geom_point() + 
    geom_smooth(formula = y ~ x, method = "lm", se = FALSE) +
    geom_abline(slope = 1) + 
    labs(x = bquote("2.5 L bottle"),
         y = bquote(paste("1.4 L bottle")))


Sdat <- rawdat %>%
  filter(Bottle == "S") %>%
  #filter(HB > 10) %>%
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
    frac = c(0, seq(from = 0, to = 1, by = 0.2), rep(1, 3))
  ) %>%
  mutate(
    HB_r = (log(HB) / log(HB_T0 * frac)) / 0.5,
    HiNA_r = (log(HiNA) / log(HiNA * frac)) / 0.5,
    MedNA_r = (log(MedNA) / log(MedNA * frac)) / 0.5,
    LowNA_r = (log(LowNA) / log(LowNA * frac)) / 0.5
  )

summary(lm(HB_r ~ frac, data = Sdat[3:7, ]))
summary(lm(HiNA_r ~ frac, data = Sdat[3:7, ]))
summary(lm(MedNA_r ~ frac, data = Sdat[3:7, ]))
summary(lm(LowNA_r ~ frac, data = Sdat[3:7, ]))

SBacGz_p <- Sdat[3:7,] %>%
  select(frac, HB_r) %>%
  #gather(key = "Bacteria_group", value = "Net_growth_rate", -frac) %>%
  #mutate(Sig = ifelse(Bacteria_group == "HiNA_r" | Bacteria_group =="LowNA_r", "1", "2")) %>%
  ggplot(aes(x = frac, y = HB_r)) + #, color = Bacteria_group
    geom_jitter(size = 8, width = 0.01) + 
    scale_colour_viridis(alpha = 1, discrete=TRUE) +
    geom_smooth(formula = y ~ x, method = "lm", se = FALSE) + 
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

Ldat <- rawdat %>%
  filter(Bottle == "L") %>%
  #filter(HB > 10) %>%
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
    frac = c(seq(from = 0, to = 1, by = 0.2), rep(1, 5))
  ) %>%
  mutate(
    HB_r = (log(HB) / log(HB_T0 * frac)) / 0.5,
    HiNA_r = (log(HiNA) / log(HiNA * frac)) / 0.5,
    MedNA_r = (log(MedNA) / log(MedNA * frac)) / 0.5,
    LowNA_r = (log(LowNA) / log(LowNA * frac)) / 0.5
  )

summary(lm(HB_r ~ frac, data = Ldat[2:6, ]))
summary(lm(HiNA_r ~ frac, data = Ldat[2:6, ]))
summary(lm(MedNA_r ~ frac, data = Ldat[2:6, ]))
summary(lm(LowNA_r ~ frac, data = Ldat[2:6, ]))

LBacGz_p <- Ldat[2:6,] %>%
  select(frac, HB_r, HiNA_r, MedNA_r, LowNA_r) %>%
  gather(key = "Bacteria_group", value = "Net_growth_rate", -frac) %>%
  #mutate(Sig = ifelse(Bacteria_group == "HiNA_r" | Bacteria_group =="LowNA_r", "1", "2")) %>%
  ggplot(aes(x = frac, y = Net_growth_rate, color = Bacteria_group)) + 
    geom_jitter(size = 8, width = 0.01) + 
    scale_colour_viridis(alpha = 1, discrete=TRUE) +
    geom_smooth(formula = y ~ x, method = "lm", se = FALSE) + 
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
LBacGz_p

