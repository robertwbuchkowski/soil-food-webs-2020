# Starting examples for the manuscript

# R version 4.0.2 (2020-06-22) -- "Taking Off Again"

library(diagram) # version 1.6.4
library(tidyverse) # version 1.3.0
library(cowplot) # version 1.1.0
library(gridGraphics) # version 0.5-0
verbose = F

# Load functions
source("Scripts/functions.R")

# FIGURE 1: Explore the diet and production efficiency shifts ----

comm1 = list(imat = matrix(c(0,1,1,
                             0,0,0,
                             0,0,0), ncol = 3, nrow = 3, byrow = T,
                           dimnames = list(paste0("X", seq(1,3)),paste0("X", seq(1,3)))),
             prop = data.frame(ID = paste0("X", seq(1,3)),
                               d = sort(rlnorm(3, meanlog = log(0.5), sdlog = 0.35)),
                               a = c(0.8, 0.8,0.8),
                               p = c(0.4, 0.4,0.4),
                               B = sort(rlnorm(3, meanlog = log(10), sdlog = 0.35)),
                               CN = c(4, 8, 25),
                               isDetritus = c(0,0,0),
                               isPlant = c(0,0,0),
                               canIMM = c(0,0,0)))

stability(comm1)

pdf("Plots/example_stoichiometricadj.pdf", width = 7, height = 5)
par(mfrow=c(3,3), mar = c(1,1,1,1) + 0.1)

# ROW A .............................

comana(comm1, mkplot = T, whattoplot = c("web"), outsideplotctrl = T, showCN = T, edgepos = c(0.4,0.8), BOX.CEX = 1.5, BOX.SIZE = 0.15)
legend("topleft", legend = "A", bty = "n", cex = 1.4, text.font = 2)

par(mar = c(2,4,1,1) + 0.1, cex.axis = 1.4, cex.lab = 1.4, tck = 0.02)
comana(comm1, mkplot = T, whattoplot = c("Nmin", "Cmin"), outsideplotctrl = T)

# ROW B .............................

par(mar = c(1,1,1,1) + 0.1, cex.axis = 1, cex.lab = 1)
comana(corrstoich(comm1), mkplot = T, whattoplot = c("web"), outsideplotctrl = T, showCN = T, edgepos = c(0.4,0.8), BOX.CEX = 1.5, BOX.SIZE = 0.15)
legend("topleft", legend = "B", bty = "n", cex = 1.4, text.font = 2)

cutoffdiet = corrstoich(comm1)$imat[1,2]

par(mar = c(4,4,1,1) + 0.1, cex.axis = 1.4, cex.lab = 1.4, tck = 0.02)
gradin <- seq(1, 6, 0.1)
answ2 <- answ <- gradin
comm2 = comm1
for(iij in 1: length(gradin)){
  comm2$imat["X1", "X2"] = gradin[iij]
  answ[iij] = sum(comana(comm2)$Nmin["X1",])
  answ2[iij] = comana(comm2)$Cmin["X1"]
}
plot(answ~gradin, type = "l", xlab = "Diet X2:X3", ylab = "N min.", lwd = 3)
points(sum(comana(comm1)$Nmin["X1",])~comm1$imat["X1","X2"], pch = 19, cex = 2)
abline(h = 0, lty =2)
abline(v = cutoffdiet, lty =3, col = "red")

plot(answ2~gradin, type = "l", xlab = "Diet X2:X3", ylab = "C min.", lwd = 3, ylim = c(0, ceiling(max(answ2))))
points(comana(comm1)$Cmin["X1"]~comm1$imat["X1","X2"], pch = 19, cex = 2)
abline(h = 0, lty =2)
abline(v = cutoffdiet, lty =3, col = "red")

# ROW C .............................

par(mar = c(1,1,1,1) + 0.1, cex.axis = 1, cex.lab = 1)
comana(corrstoich(comm1, forceProd = T), mkplot = T, whattoplot = c("web"), outsideplotctrl = T, showCN = T, edgepos = c(0.4,0.8), BOX.CEX = 1.5, BOX.SIZE = 0.15)
legend("topleft", legend = "C", bty = "n", cex = 1.4, text.font = 2)

cutoffprod = corrstoich(comm1, forceProd = T)$prop$p[1]

par(mar = c(4,4,1,1) + 0.1, cex.axis = 1.4, cex.lab = 1.4, tck = 0.02)
gradin <- seq(0.4, 0.2, -0.001)
answ <- gradin
answ2 <- gradin
comm2 = comm1
for(iij in 1: length(gradin)){
  comm2$prop[comm2$prop$ID == "X1","p"] = gradin[iij]
  answ[iij] = sum(comana(comm2)$Nmin["X1",])
  answ2[iij] = comana(comm2)$Cmin["X1"]
}
plot(answ~gradin, type = "l", xlab = "Production efficiency (X1)", ylab = "N min.", lwd = 3)
points(sum(comana(comm1)$Nmin["X1",])~comm1$prop[comm2$prop$ID == "X1","p"], pch = 19, cex = 2)
abline(h = 0, lty =2)
abline(v = cutoffprod, lty =3, col = "red")

plot(answ2~gradin, type = "l", xlab = "Production efficiency (X1)", ylab = "C min.", lwd = 3, ylim = c(0, ceiling(max(answ2))))
points(comana(comm1)$Cmin["X1"]~comm1$prop[comm2$prop$ID == "X1","p"], pch = 19, cex = 2)
abline(h = 0, lty =2)
abline(v = cutoffprod, lty =3, col = "red")

dev.off()

# FIGURE 2: Show a simple example of a combination ----

mediuma = mediumb = mediumc = vector(mode = "list", length = 10000)

for(ih in 1:10000){
  
  comm1 = list(imat = matrix(c(0,1,1,0,0,0,0,0,
                               0,0,0,1,0,0,0,0,
                               0,0,0,1,0,0,0,0,
                               0,0,0,0,1,1,0,0,
                               0,0,0,0,0,1,1,1,
                               0,0,0,0,0,0,0,0,
                               0,0,0,0,0,0,0,0,
                               0,0,0,0,0,0,0,0), ncol = 8, nrow = 8, byrow = T,
                             dimnames = list(paste0("X", seq(1,8)),paste0("X", seq(1,8)))),
               prop = data.frame(ID = paste0("X", seq(1,8)),
                                 d = sort(rlnorm(8, meanlog = log(0.5), sdlog = 0.35)),
                                 a = c(runif(5, min = 0.3, max = 0.8),1,1,1),
                                 p = c(runif(5, min = 0.3, max = 0.5),1,1,1),
                                 B = sort(rlnorm(8, meanlog = log(10), sdlog = 0.35)),
                                 CN = sort(rlnorm(8, meanlog = log(10), sdlog = 0.35)),
                                 isDetritus = c(0,0,0,0,0,0,0,0),
                                 isPlant = c(0,0,0,0,0,0,0,0),
                                 canIMM = c(0,0,0,0,0,0,0,0)))
  
  tempout = loopsubsets(comm1, STEPS = 2, MKPLOT = F)
  
  mediuma[[ih]] = cbind(tempout$different[[2]], WebN = ih)
  mediumb[[ih]] = cbind(comm1$prop, WebN = ih)
  mediumc[[ih]] = c(rmax1 = stability(tempout$USINvec[[1]])$rmax,rmax2 = stability(tempout$USINvec[[2]])$rmax, WebN = ih)
  
}

mediuma = do.call("rbind",mediuma)
mediumb = do.call("rbind",mediumb)
mediumc = do.call("rbind",mediumc)

mediumd = mediuma %>% left_join(data.frame(mediumc)) %>%
  mutate(stable = ifelse(rmax1 < 0 & rmax2 < 0, "Yes", "No"))

par(mfrow=c(1,2), mar = c(2,2,2,2)+0.1)
comana(comm1, mkplot = T, whattoplot = c("web"), outsideplotctrl = T, BOX.SIZE = 0.1, BOX.PROP = 0.5)
comana(comtrosp(comm1, selected = T), mkplot = T, whattoplot = c("web"), outsideplotctrl = T, BOX.SIZE = 0.1, BOX.PROP = 0.5)
p1 = recordPlot()

# Pick 5000 stable webs
IDstable = mediumd %>% 
  filter(stable == "Yes") %>%
  select(WebN) %>%
  distinct() %>% 
  arrange()

p3 = mediumd %>% 
  filter(WebN %in% IDstable[1:5000,]) %>%
  filter(stable == "Yes") %>% 
  select(ID, dNmin, dCmin) %>%
  filter(!is.na(dNmin)) %>% 
  gather(-ID, key = Element, value = Change) %>%
  group_by(ID, Element) %>%
  mutate(Change = Change*100) %>%
  summarize(sd = sd(Change),Change = mean(Change)) %>%
  mutate(lower = Change -sd, upper = Change + sd) %>%
  mutate(lower = ifelse(lower < -100, -15, lower)) %>%
  mutate(upper = ifelse(upper > 100, 15, upper)) %>%
  ggplot(aes(x = ID, y = Change)) + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_pointrange(aes(ymin = lower, ymax = upper, color = Element, linetype = Element, shape = Element)) + theme_classic() +
  xlab("Node") + ylab("Change (%)") + 
  scale_color_manual(labels = c("C mineralization", "N mineralization"), values = c("blue", "orange")) + 
  scale_linetype_discrete(labels = c("C mineralization", "N mineralization")) +
  scale_shape_manual(labels = c("C mineralization", "N mineralization"), values = c(19,1)) +theme(legend.position = "top") +
  ylim(c(-15,15)) +
  geom_text(aes(label = text), data = tibble(ID = c("X1", "X1"),
                          Change = c(-15, 15),
                          text = c("-125", "118")),
            nudge_x = 0.25)

pdf("Plots/example_combine.pdf", width = 7, height = 6)  
plot_grid(p1,p3,labels = "AUTO", ncol = 1, nrow = 2)
dev.off()


# FIGURE 3: Series combine example for one large food web ----
multistepa = multistepb = multistepc = vector(mode = "list", length = 500)

stability_function <- function(NN){
  return(stability(tempout$USINvec[[NN]])$rmax)
}

# Hard code comibations in so they are the same everytime
COMBINATIONS = list(c("X2", "X3"),
                    c("X2/X3", "X4"),
                    c("X2/X3/X4", "X5"),
                    c("X2/X3/X4/X5", "X6"),
                    c("X2/X3/X4/X5/X6", "X7"),
                    c("X2/X3/X4/X5/X6/X7", "X8"),
                    c("X2/X3/X4/X5/X6/X7/X8", "X10"),
                    c("X2/X3/X4/X5/X6/X7/X8/X10", "X9"))

for(ih in 1:500){
  while(TRUE){ # Stability of all webs
    while(TRUE){ # Stability of first web
      comm1 = list(imat = matrix(c(0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,
                                   0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,
                                   0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,
                                   0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,
                                   0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,
                                   0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,
                                   0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,
                                   0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,
                                   0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,
                                   0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,
                                   0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,
                                   0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,
                                   0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,
                                   0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,
                                   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
                                   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
      ), ncol = 16, nrow = 16, byrow = T,
      dimnames = list(paste0("X", seq(1,16)),paste0("X", seq(1,16)))),
      prop = data.frame(ID = paste0("X", seq(1,16)),
                        d = c(sort(rlnorm(15, meanlog = log(0.5), sdlog = 0.35)),0),
                        a = c(runif(12, min = 0.3, max = 0.8),1,1,1,1),
                        p = c(runif(12, min = 0.3, max = 0.5),1,1,1,1),
                        B = sort(rlnorm(16, meanlog = log(10), sdlog = 0.35)),
                        CN = sort(rlnorm(16, meanlog = log(10), sdlog = 0.35)),
                        isDetritus = c(rep(0,15),0),
                        isPlant = c(rep(0,15),0),
                        canIMM = c(rep(0,15),0)))
      
      if(stability(corrstoich(comm1))$rmax < 0) break
      
    }
    
    tempout = loopsubsets(comm1, STEPS = 9, MKPLOT = F, labeldiff = T, selectedCOMBO = T, selected2COMBO = COMBINATIONS)
    
    Qstable = all(sapply(seq(1,9,1), FUN = stability_function) < 0)
    
    if(Qstable) break  
  }
  
  multistepa[[ih]] = cbind(do.call("rbind", tempout$different), WebN = ih)

  multistepb[[ih]] = cbind(comm1$prop, WebN = ih)
  
  multistepc[[ih]] = c(stable = Qstable, WebN = ih)
  
  print(ih)
  # if(ih %% 100 == 0) print(ih)
  
}

multistepa = do.call("rbind",multistepa)
multistepb = do.call("rbind",multistepb)
multistepc = do.call("rbind",multistepc)

multistepd = multistepa %>% left_join(data.frame(multistepc)) %>%
  mutate(stable = ifelse(stable, "Yes", "No"))

# Save data
multistepd %>% write_rds("Data/multistepd.rds")
multistepb %>% write_rds("Data/multistepb.rds")

# Read in save data
multistepd = read_rds("Data/multistepd.rds")
multistepb = read_rds("Data/multistepb.rds")

# Create a figure of food web steps:
pdf("Plots/sequence1.pdf", width = 4, height = 5)
par(mar = c(1,1,1,1))
comana(tempout$USINvec[[1]], mkplot = T, whattoplot = "web", BOX.CEX = 1.4, outsideplotctrl = T)
dev.off()

# Show the final version for demonstration purposes: 
comana(tempout$USINvec[[9]], mkplot = T, whattoplot = "web")

pdf("Plots/sequence.pdf", width = 7, height = 5)
multistepd %>% filter(ID == "Net") %>% filter(stable == "Yes") %>%
  select(dNmin, dCmin, STEP) %>%
  gather(-STEP, key = Element, value = Change) %>%
  mutate(Change = c(Change*100)) %>%
  group_by(STEP, Element) %>%
  summarize(N = n(), sd = sd(Change),Change = mean(Change)) %>%
  mutate(lower = Change -sd, upper = Change + sd) %>% 
  ungroup() %>%
  mutate(STEP = STEP) %>%
  ggplot(aes(x = STEP, y = Change, color = Element, linetype = Element, shape = Element)) + geom_hline(yintercept = 0, linetype = 2) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) + 
  geom_line() +
  theme_classic() +
  xlab("Nodes combined (#)") + ylab("Change (%)") + 
  scale_color_manual(labels = c("C mineralization", "N mineralization"), values = c("blue", "orange")) + 
  scale_linetype_discrete(labels = c("C mineralization", "N mineralization")) +
  scale_shape_manual(labels = c("C mineralization", "N mineralization"), values = c(19,1)) +
  theme(legend.position = "top", 
        text = element_text(size = 20),
        axis.text.y = element_text(margin = unit(c(t = 0, r = 3.5, b = 0, l = 0), "mm")),
        axis.text.x = element_text(margin = unit(c(t = 3.5, r = 0, b = 0, l = 0), "mm")),
        axis.ticks.length = unit(-0.25, "cm")) +
  scale_y_continuous(breaks = c(0, -50, -100))
dev.off()

# FIGURE S1: Compare the different mineralization rates ----

generalfunction1 <- function(runid){
  STABLE = F
  while(!STABLE){
    comm1 = list(imat = matrix(c(0,1,1,
                                 0,0,0,
                                 0,0,0), ncol = 3, nrow = 3, byrow = T,
                               dimnames = list(paste0("X", seq(1,3)),paste0("X", seq(1,3)))),
                 prop = data.frame(ID = paste0("X", seq(1,3)),
                                   d = sort(rlnorm(3, meanlog = log(0.5), sdlog = 0.35)),
                                   a = runif(3, min = 0.3, max = 0.8),
                                   p = runif(3, min = 0.3, max = 0.5),
                                   B = sort(rlnorm(3, meanlog = log(10), sdlog = 0.35)),
                                   CN = c(4, 8, 25),
                                   isDetritus = c(0,0,0),
                                   isPlant = c(0,0,0),
                                   canIMM = c(0,0,0)))
    
    if(stability(corrstoich(comm1, forceProd = F))$rmax <0 &
       stability(corrstoich(comm1, forceProd = T))$rmax <0) STABLE = T
    
  }
  
  o1 = comana(corrstoich(comm1, forceProd = F), mkplot = F)
  o2 = comana(corrstoich(comm1, forceProd = T), mkplot = F)
  
  dNmin = (rowSums(o2$Nmin) - rowSums(o1$Nmin))/rowSums(o1$Nmin)
  dCmin = (o2$Cmin - o1$Cmin)/o1$Cmin
  
  return(data.frame(ID = names(dCmin), dNmin = dNmin, dCmin = dCmin, RUNID = runid))
}

final = do.call("rbind", lapply(seq(1,10000,1), FUN = generalfunction1))


png("Plots/dietVSprod.png", width = 8, height = 4, units = "in", res = 300)
final %>% tibble() %>%
  gather(-ID, -RUNID, key = Element, value = Change) %>%
  filter(!is.na(Change)) %>%
  mutate(Change = Change*100) %>%
  filter(abs(Change) < 200) %>%
  ggplot(aes(x = Change, fill = ID)) + geom_histogram() +
  facet_wrap(.~Element, labeller = as_labeller(c(
    dNmin = "Nitrogen mineralization",
    dCmin = "Carbon mineralization"
  ))) + 
  theme_classic() +
  scale_fill_manual(name = "Node", values = c("#009E73","#E69F00","#56B4E9")) +
  xlab("Change (%; production efficiency change - diet change)")
dev.off()
