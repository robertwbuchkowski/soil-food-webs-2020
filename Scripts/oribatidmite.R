# Oribatid mite example

# R version 4.0.2 (2020-06-22) -- "Taking Off Again"

library(diagram) # version 1.6.4
library(tidyverse) # version 1.3.0
library(cowplot) # version 1.1.0
library(gridGraphics) # version 0.5-0

# Load functions
source("Scripts/functions.R")

# Food web example:

nvec = c("Predator", "Orib1", "Orib2", "Fungi1", "Fungi2", "Detritus")

comm0 = list(imat = matrix(c(0,1,0,0,0,0,
                             0,0,0,1,1,1,
                             0,0,0,1,1,1,
                             0,0,0,0,0,1,
                             0,0,0,0,0,1,
                             0,0,0,0,0,0), ncol = 6, nrow = 6, byrow = T,
                           dimnames = list(nvec,nvec)),
             prop = data.frame(ID = nvec,
                               d = c(1.84,3,1,1.2,1.2,0),
                               a = c(0.6,0.65,0.45,1,1,1), # Luxton et al. 1972
                               p = c(0.4,0.4,0.4,0.3,0.3,1),
                               B = c(1.04,6.35,6.35,7122,7122,2500000),
                               CN = c(4.5,4.9,4.7,8,12,25), # Mochnacka
                               isDetritus = c(0,0,0,0,0,1),
                               isPlant = c(0,0,0,0,0,0),
                               canIMM = c(0,0,0,1,1,0)))


orbmult5 = orbmult4 = orbmult = orbmult2 = orbmult3 = vector(mode = "list", length = 500)
comm1 = comm0


for(ih in 1:500){
  
  STABTEST = T
  
  while(STABTEST){
    for(i in 1:5){
      comm1$prop$CN[i] = rlnorm(1, meanlog = log(comm0$prop$CN[i]), sdlog = 0.35)
      comm1$prop$B[i] = rlnorm(1, meanlog = log(comm0$prop$B[i]), sdlog = 0.35)
      
      if(i < 5){
        comm1$prop$p[i] = rlnorm(1, meanlog = log(comm0$prop$p[i]), sdlog = 0.35)
        comm1$prop$d[i] = rlnorm(1, meanlog = log(comm0$prop$d[i]), sdlog = 0.35)
      }
      
      if(i < 4) comm1$prop$a[i] = rlnorm(1, meanlog = log(comm0$prop$a[i]), sdlog = 0.35)
    }
    
    comm1a = corrstoich(comm1)
    
    out1 = comana(comm1a)
    
    comm2 = comtrosp(comm1, selected = T, selected2 = c("Orib1", "Orib2"))
    
    comm2a = corrstoich(comm2)
    
    out2 = comana(comm2a)
    
    comm3 = comtrosp(comm1, selected = T, selected2 = c("Fungi1", "Fungi2"))
    
    comm3a = corrstoich(comm3)
    
    out3 = comana(comm3a)
    
    comm4 = comtrosp(comm3, selected = T, selected2 = c("Orib1", "Orib2"))
    
    comm4a = corrstoich(comm4)
    
    out4 = comana(comm4a)
    
    orbmult3[[ih]] = c(rmax1 = stability(comm1a)$rmax,rmax2 = stability(comm2a)$rmax,rmax3 = stability(comm3a)$rmax,rmax4 = stability(comm4a)$rmax, N = ih)
    
    orbmult[[ih]] = cbind(diffcomana(out1,out2), N = ih)
    orbmult4[[ih]] = cbind(diffcomana(out1,out3), N = ih)
    orbmult5[[ih]] = cbind(diffcomana(out3,out4), N = ih)
    orbmult2[[ih]] = cbind(comm1$prop, N = ih)
    
    if(
      all(c(rmax1 = stability(comm1a)$rmax,rmax2 = stability(comm2a)$rmax,rmax3 = stability(comm3a)$rmax,rmax4 = stability(comm4a)$rmax) < 0)
    ) STABTEST = F
  }
  
  if(ih %% 10 == 0) print(paste0("Done ", ih))
  
}

orbmult1 = do.call("rbind",orbmult)
orbmult2 = do.call("rbind",orbmult2)
orbmult3 = do.call("rbind",orbmult3)
orbmult4 = do.call("rbind",orbmult4)
orbmult5 = do.call("rbind",orbmult5)

# Confirm stability
all(orbmult3[,1:4] <0)

# Demonstrate color choices
plot(rep(1,6)~seq(1,6), col = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00"), pch = 19, cex = 3)

plot(rep(1,4)~seq(1,4), col = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")[c(5,3,2,4)], pch = 19, cex = 3)

# Combining Oribatid Mites ----

# Create temporary copy to prepare for plot
temp_middle = orbmult1 %>% as_tibble() %>%
  select(ID, dNmin, dCmin) %>%
  rename(Nitrogen = dNmin, Carbon = dCmin) %>%
  gather(-ID, key = Element, value = Change) %>%
  mutate(Change = Change*100) %>%
  filter(ID != "Det") %>%
  mutate(sry = ifelse(ID == "Net", "Net", "Individual")) %>%
  filter(!is.na(Change))

# Figure out how many are +/- 50%
temp_middle %>% 
  mutate(Catagory = ifelse(abs(Change > 50), 1,0)) %>%
  group_by(sry) %>%
  summarize(Total = sum(Catagory), N = n()) %>%
  mutate(Total/N)

# Group all points over 100% change together at +/-110 %
temp_outer = temp_middle %>% 
  filter(abs(Change) > 100) %>% 
  mutate(Change = ifelse(Change > 0, 110, -110))

temp_middle = temp_middle %>% filter(abs(Change) <= 100)

temp_middle = temp_middle %>%
  bind_rows(temp_outer)

pdf("Plots/oribatid1.pdf", width = 10, height = 5)
temp_middle %>% 
  ggplot(aes(x = Change, fill = ID)) +
  geom_hline(yintercept = 0, linetype = 1) +
  geom_histogram(size = 0.9, col = "black",breaks = c(seq(-110, 110, 10))) + theme_classic() +
  xlab("Change (%)") + facet_grid(Element~sry) + 
  geom_vline(xintercept = 100, linetype = 3) +
  geom_vline(xintercept = -100, linetype = 3) +
  scale_fill_manual(values = c("darkblue", "#117733", "#56B4E9", "#F0E442","white"),
                    limits = c("Predator", "Orib1/Orib2", "Fungi1", "Fungi2", "Net"),
                    labels = c("Predatory Mite", "Oribatid Mite(s)", "Bacteria", "Fungi", "Net change"), name = "Node") +theme(legend.position = "top") +
  theme(text = element_text(size = 20),
        axis.text.y = element_text(margin = unit(c(t = 0, r = 3.5, b = 0, l = 0), "mm")),
        axis.text.x = element_text(margin = unit(c(t = 3.5, r = 0, b = 0, l = 0), "mm")),
        axis.ticks.length = unit(-0.25, "cm")) +
  scale_x_continuous(breaks = c(-100, 0, 100)) +
  scale_y_continuous(breaks = c(0, 500, 1000))
dev.off()

pdf("Plots/oribatid1a.pdf", width = 6, height = 3)
par(mfrow=c(1,2), mar = c(1,1,1,1))
comana(corrstoich(comm0), mkplot = T, whattoplot = c("web"), outsideplotctrl = T, BOX.SIZE = 0.2, edgepos = c(0.2, 0.7),
       prettynames = c("Predator", "Oribatid 1", "Oribatid 2", "Bacteria", "Fungi", "Detritus"), shuffleTL = F)
arrows(0.95, 0.5, 1.05, 0.5, lwd = 2.5, length = 0.1, xpd =T)
comana(corrstoich(comtrosp(comm0, selected = T, selected2 = c("Orib1", "Orib2"))), mkplot = T, whattoplot = c("web"), outsideplotctrl = T, BOX.SIZE = 0.2, edgepos = c(0.2, 0.7),
       prettynames = c("Predator", "Oribatid 1 & 2", "Bacteria", "Fungi", "Detritus"), shuffleTL = F)
dev.off()

# Combining Microorganisms ----

# Create temporary copy to prepare for plot
temp_middle = orbmult4 %>% as_tibble() %>%
  select(ID, dNmin, dCmin) %>%
  rename(Nitrogen = dNmin, Carbon = dCmin) %>%
  gather(-ID, key = Element, value = Change) %>%
  mutate(Change = Change*100) %>%
  filter(ID != "Det") %>%
  mutate(sry = ifelse(ID == "Net", "Net", "Individual")) %>%
  filter(!is.na(Change))

# Figure out how many are +/- 50%
temp_middle %>% 
  mutate(Catagory = ifelse(abs(Change > 50), 1,0)) %>%
  group_by(sry) %>%
  summarize(Total = sum(Catagory), N = n()) %>%
  mutate(Total/N)

# Group all points over 100% change together at +/-110 %
temp_outer = temp_middle %>% 
  filter(abs(Change) > 100) %>% 
  mutate(Change = ifelse(Change > 0, 110, -110))

temp_middle = temp_middle %>% filter(abs(Change) <= 100)

temp_middle = temp_middle %>%
  bind_rows(temp_outer)

pdf("Plots/oribatid2.pdf", width = 11, height = 5)
temp_middle %>% 
  ggplot(aes(x = Change, fill = ID)) +
  geom_hline(yintercept = 0, linetype = 1) +
  geom_histogram(size = 0.9, col = "black",breaks = c(seq(-110, 110, 10))) + theme_classic() +
  xlab("Change (%)") + facet_grid(Element~sry) + 
  geom_vline(xintercept = 100, linetype = 3) +
  geom_vline(xintercept = -100, linetype = 3) +
  scale_fill_manual(values = c("darkblue", "#117733", "#56B4E9", "#F0E442","white"),
                    limits = c("Predator", "Orib1", "Orib2", "Fungi1/Fungi2", "Net"),
                    labels = c("Predatory Mite", "Oribatid Mite 1", "Oribatid Mite 2", "Microbes", "Net change"), name = "Node") +theme(legend.position = "top") +
  theme(text = element_text(size = 20),
        axis.text.y = element_text(margin = unit(c(t = 0, r = 3.5, b = 0, l = 0), "mm")),
        axis.text.x = element_text(margin = unit(c(t = 3.5, r = 0, b = 0, l = 0), "mm")),
        axis.ticks.length = unit(-0.25, "cm")) +
  scale_x_continuous(breaks = c(-100, 0, 100)) +
  scale_y_continuous(breaks = c(0, 500, 1000))
dev.off() 

pdf("Plots/oribatid2a.pdf", width = 6, height = 3)
par(mfrow=c(1,2), mar = c(1,1,1,1))
comana(corrstoich(comm0), mkplot = T, whattoplot = c("web"), outsideplotctrl = T, BOX.SIZE = 0.2, edgepos = c(0.2, 0.7),
       prettynames = c("Predator", "Oribatid 1", "Oribatid 2", "Bacteria", "Fungi", "Detritus"), shuffleTL = F)
arrows(0.95, 0.5, 1.05, 0.5, lwd = 2.5, length = 0.1, xpd =T)
comana(corrstoich(comtrosp(comm0, selected = T, selected2 = c("Fungi1", "Fungi2"))), mkplot = T, whattoplot = c("web"), outsideplotctrl = T, BOX.SIZE = 0.2, edgepos = c(0.2, 0.7),
       prettynames = c("Predator", "Oribatid 1", "Oribatid 2", "Microbes", "Detritus"), shuffleTL = F)
dev.off()

# Combining Oribatid Mites after microorganisms were combined ----

# Create temporary copy to prepare for plot
temp_middle = orbmult5 %>% as_tibble() %>%
  select(ID, dNmin, dCmin) %>%
  rename(Nitrogen = dNmin, Carbon = dCmin) %>%
  gather(-ID, key = Element, value = Change) %>%
  mutate(Change = Change*100) %>%
  filter(ID != "Det") %>%
  mutate(sry = ifelse(ID == "Net", "Net", "Individual")) %>%
  filter(!is.na(Change))

# Figure out how many are +/- 50%
temp_middle %>% 
  mutate(Catagory = ifelse(abs(Change > 50), 1,0)) %>%
  group_by(sry) %>%
  summarize(Total = sum(Catagory), N = n()) %>%
  mutate(Total/N)

# Group all points over 100% change together at +/-110 %
temp_outer = temp_middle %>% 
  filter(abs(Change) > 100) %>% 
  mutate(Change = ifelse(Change > 0, 110, -110))

temp_middle = temp_middle %>% filter(abs(Change) <= 100)

temp_middle = temp_middle %>%
  bind_rows(temp_outer)

pdf("Plots/oribatid3.pdf", width = 10, height = 5)
temp_middle %>% 
  ggplot(aes(x = Change, fill = ID)) +
  geom_hline(yintercept = 0, linetype = 1) +
  geom_histogram(size = 0.9, col = "black",breaks = c(seq(-110, 110, 10))) + theme_classic() +
  xlab("Change (%)") + facet_grid(Element~sry) + 
  geom_vline(xintercept = 100, linetype = 3) +
  geom_vline(xintercept = -100, linetype = 3) +
  scale_fill_manual(values = c("darkblue", "#56B4E9", "#F0E442","white"),
                    limits = c("Predator", "Orib1/Orib2", "Fungi1/Fungi2", "Net"),
                    labels = c("Predatory Mite", "Oribatid Mite(s)", "Microbes", "Net change"), name = "Node") +theme(legend.position = "top") +
  theme(text = element_text(size = 20),
        axis.text.y = element_text(margin = unit(c(t = 0, r = 3.5, b = 0, l = 0), "mm")),
        axis.text.x = element_text(margin = unit(c(t = 3.5, r = 0, b = 0, l = 0), "mm")),
        axis.ticks.length = unit(-0.25, "cm")) +
  scale_x_continuous(breaks = c(-100, 0, 100)) +
  scale_y_continuous(breaks = c(0, 500, 1000))
dev.off() 

pdf("Plots/oribatid3a.pdf", width = 6, height = 3)
par(mfrow=c(1,2), mar = c(1,1,1,1))
comana(corrstoich(comtrosp(comm0, selected = T, selected2 = c("Fungi1", "Fungi2"))), mkplot = T, whattoplot = c("web"), outsideplotctrl = T, BOX.SIZE = 0.21, edgepos = c(0.21, 0.69),
       prettynames = c("Predator", "Oribatid 1", "Oribatid 2", "Microbes", "Detritus"), shuffleTL = F)
arrows(0.95, 0.5, 1.05, 0.5, lwd = 2.5, length = 0.1, xpd =T)
comana(corrstoich(comtrosp(comtrosp(comm0, selected = T, selected2 = c("Fungi1", "Fungi2")), selected = T, selected2 = c("Orib1", "Orib2"))), mkplot = T, whattoplot = c("web"), outsideplotctrl = T, BOX.SIZE = 0.21, edgepos = c(0.21, 0.69),
       prettynames = c("Predator", "Oribatid 1 & 2", "Microbes", "Detritus"), shuffleTL = F)
dev.off()
