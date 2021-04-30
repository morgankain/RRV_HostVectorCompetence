#################################
### ggplot manuscript figures ###
#################################

####
## Figure 1 is the conceptual figure
####

####
## Figure 2 see "manuscript_plots_fig2_panels.R"
####

####
## Figure 3 and supplemental multi-generation plots
####

### Hosts

next_gen.gg.f.f.s.h$Next <- factor(next_gen.gg.f.f.s.h$Next, levels = rev(AUC_titer.gg.s$Host))

ggplot(next_gen.gg.f.f.s.h, aes(gen, est)) + 
  geom_line() + 
  geom_ribbon(aes(x = gen, ymin = lwr, ymax = upr), alpha = 0.2) +
  xlab("Generation") + ylab("Total Number Infected") +
  xlim(c(0, 5.3)) +
  scale_y_continuous(trans = "pseudo_log") +
# geom_dl(aes(label = Next, colour = Next), method = list(dl.trans(x = x + .3), "last.bumpup")) +
  theme(legend.position = "none") + 
  facet_wrap(~Next)

subsamp <- sample(seq(1:1000), 500)
next_gen.gg.f.f.all <- next_gen.gg.f.f %>% filter(samp %in% subsamp) %>% 
  group_by(Next, gen, samp) %>% summarize(est = sum(R))
host.names <- strsplit(as.character(next_gen.gg.f.f.all$Next), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
next_gen.gg.f.f.all$Next <- host.names

ggplot(next_gen.gg.f.f.all, aes(gen, est)) + 
  geom_line(aes(group = interaction(samp, Next)), lwd = 0.2, alpha = 0.5, colour = "grey60") + 
  geom_line(data = next_gen.gg.f.f.s.h, aes(gen, est), lwd = 1) +
  xlab("Generation") +
  ylab("Total Number Infected") +
  xlim(c(0, 5.3)) +
  scale_y_continuous(trans = "pseudo_log", breaks = c(0, 4, 16, 64, 256, 1024)) +
  theme(legend.position = "none") + 
  facet_wrap(~Next)

next_gen.gg.f.f.s2$Next <- factor(next_gen.gg.f.f.s2$Next, levels = AUC_titer.gg.s$Host)
next_gen.gg.f.f.s2$Current <- factor(next_gen.gg.f.f.s2$Current, levels = AUC_titer.gg.s$Host)

next_gen.gg.f.f.s2 <- next_gen.gg.f.f.s2 %>% 
  mutate(gen = mapvalues(
    gen, from = c(1, 3, 5), to = c("Generation 1", "Generation 3", "Generation 5")
  ))

## 13.78 x 5.13
(gg.multigen.host <- ggplot(
  (next_gen.gg.f.f.s2 %>% filter(gen %in% c("Generation 1", "Generation 3", "Generation 5")))
  , aes(Next, Current)) + 
  geom_tile(aes(fill = est), colour = "white") + 
    facet_wrap(~gen) +
    scale_fill_gradient(low = "grey97", high = "#4472C4"
  , name = "New Host Infections 
(Median)") +
    theme(
    axis.text.x     = element_text(angle = 300, hjust = 0, colour = "grey50", size = 16)
  , axis.text.y     = element_text(size = 16)
  , legend.key.size = unit(.55, "cm")
  , legend.text     = element_text(size = 14)
  , legend.title    = element_text(size = 16)
  ) +
  ylab("Current Host Generation") +
  xlab("Next Host Generation")
)

ggsave(filename = "/Users/Morgan/Desktop/host_multigen.pdf", plot = last_plot(), width = 13.78,  height = 5.13, units = c("in"))

### Mosquitoes

ggplot(next_gen.gg.f.f.s.m, aes(gen, est)) + 
  geom_line(aes(colour = Next)) + 
  geom_ribbon(aes(x = gen, ymin = lwr, ymax = upr,fill = Next), alpha = 0.2) +
  xlab("Generation") + ylab("Total Number Infected") +
  xlim(c(0, 5.3)) +
  scale_y_continuous(trans = "pseudo_log") +
# geom_dl(aes(label = Next, colour = Next), method = list(dl.trans(x = x + .3), "last.bumpup")) +
  theme(legend.position = "none") + 
  facet_wrap(~Next)

next_gen.gg.f.f.all.m <- next_gen.gg.f.f.m %>% filter(samp %in% subsamp) %>% 
  group_by(Next, gen, samp) %>% summarize(est = sum(R))
mosq.names <- strsplit(as.character(next_gen.gg.f.f.all.m$Next), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
next_gen.gg.f.f.all.m$Next <- mosq.names

ggplot(next_gen.gg.f.f.all.m, aes(gen, est)) + 
  geom_line(aes(group = interaction(samp, Next)), lwd = 0.2, alpha = 0.5, colour = "grey60") + 
  geom_line(data = next_gen.gg.f.f.s.m, aes(gen, est), lwd = 1) +
  xlab("Generation") +
  ylab("Total Number Infected") +
  xlim(c(0, 5.3)) +
  scale_y_continuous(trans = "pseudo_log", breaks = c(0, 4, 16, 64, 256, 1024, 4096)) +
  theme(legend.position = "none") + 
  facet_wrap(~Next)

next_gen.gg.f.f.s2.m$Next <- factor(next_gen.gg.f.f.s2.m$Next, levels = mosq_inf_AUC_all_samps_adj_to_com.gg.s$mosq)
next_gen.gg.f.f.s2.m$Current <- factor(next_gen.gg.f.f.s2.m$Current, levels = mosq_inf_AUC_all_samps_adj_to_com.gg.s$mosq)

next_gen.gg.f.f.s2.m <- next_gen.gg.f.f.s2.m %>% 
  mutate(gen = mapvalues(
    gen, from = c(1, 3, 5), to = c("Generation 1", "Generation 3", "Generation 5")
  ))

## 19.97 x 8.00
(gg.multigen.mosq <- ggplot(
  (next_gen.gg.f.f.s2.m %>% filter(gen %in% c("Generation 1", "Generation 3", "Generation 5")))
  , aes(Next, Current)) + 
  geom_tile(aes(fill = est), colour = "white") + 
    facet_wrap(~gen) +
    scale_fill_gradient(low = "grey97", high = "orangered4"
  , name = "New Mosquito Infections 
(Median)") +
    theme(
    axis.text.x     = element_text(angle = 300, hjust = 0, colour = "grey50", size = 20)
  , axis.text.y     = element_text(size = 18)
  , axis.title.y    = element_text(size = 19)
  , strip.text.x    = element_text(size = 18)  
  , legend.key.size = unit(.85, "cm")
  , legend.text     = element_text(size = 18)
  , legend.title    = element_text(size = 16)
  ) +
  ylab("Current Mosquito Generation") +
  xlab("Next Mosquito Generation")
)

ggsave(filename = "/Users/Morgan/Desktop/mosq_multigen.pdf", plot = last_plot(), width = 19.97,  height = 8.00, units = c("in"))

# gridExtra::grid.arrange(gg.multigen.host, gg.multigen.mosq, nrow = 2)

####
## All of the supplemental methods figures are created in the scripts where the statistical models are run
####

### Except for this results summary figure showing the densities of hosts responses

physiol_mat.s.all %>% filter(G1 == "human" | G1 == "bird" | G1 == "macropod" | G1 == "horse" | G1 == "possum") %>%
  mutate(G1 = plyr::mapvalues(G1, from = c("human", "bird", "macropod", "horse", "possum"), to = c("Human", "Bird", "Macropod", "Horse", "Possum"))) %>% {
    ggplot(., aes(x = sum)) +
    geom_density(aes(colour = G1, fill = G1), lwd = 1, alpha = 0.3) +
    scale_color_discrete(name = "Source Infection") + 
    scale_fill_discrete(name = "Source Infection") + 
    xlab("Total Number of Second Generation Infections") +
    ylab("Density") + 
    geom_vline(data = 
        (physiol_mat.s.all.gg %>% filter(G1 == "Human" | G1 == "Bird" | G1 == "Macropod" | G1 == "Horse" | G1 == "Possum"))
    ,  aes(xintercept = est_tot, colour = G1), lwd = 0.75, linetype = "dashed"
        ) +
    theme(
      legend.key.size = unit(.65, "cm")
    , legend.text = element_text(size = 14)
    , legend.title = element_text(size = 16)
    )
  }

######
## A few pieces for the conceptual figure (put together in keynote)
######
mosq.names <- strsplit(mosq_prop$mos_species, "_")
mosq_prop.gg.cf <- mosq_prop

names   <- strsplit(as.character(mosq_prop.gg.cf$mos_species), "_") %>% 
  sapply(., FUN = function(x) x[1]) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
mosq_prop.gg.cf$mos_gen <- names
mosq_prop.gg.cf <- mosq_prop.gg.cf %>% group_by(mos_gen) %>% summarize(sum_prop = sum(prop))
mosq_prop.gg.cf <- data.frame(Culex = 0.390, Aedes = 0.460, Coquillettidia = 0.111, Other = (1 - (0.390 + 0.460 + 0.111)))
mosq_prop.gg.cf <- melt(mosq_prop.gg.cf)
mosq_prop.gg.cf <- mosq_prop.gg.cf %>% mutate(
  ymax = cumsum(value)
)
mosq_prop.gg.cf$ymin      <- c(0, head(mosq_prop.gg.cf$ymax, n=-1))
names(mosq_prop.gg.cf)[1] <- "Mosquito"

ggplot(mosq_prop.gg.cf
  , aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = Mosquito)) +
     geom_rect() +
     scale_fill_manual(values = c("aquamarine4", "purple3", "firebrick3", "deeppink1")) +
     coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
     xlim(c(2, 4)) + 
  theme_void() 


mosq_blood.gg.cf <- mosq_blood %>% filter(mos_species == "ae_vigilax")
mosq_blood.gg.cf <- data.frame(Bird = 40, Human = 27, Horse = 20, Other = 2 + 12 + 2 + 1 + 33 + 2)

mosq_blood.gg.cf <- melt(mosq_blood.gg.cf)
mosq_blood.gg.cf <- mosq_blood.gg.cf %>% mutate(
  frac = value / sum(value)
, ymax = cumsum(frac)
)
mosq_blood.gg.cf$ymin      <- c(0, head(mosq_blood.gg.cf$ymax, n=-1))
names(mosq_blood.gg.cf)[1] <- "Host"

ggplot(mosq_blood.gg.cf
  , aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = Host)) +
     geom_rect() +
     coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
     xlim(c(2, 4)) + 
  theme_void()

ggplot(
  (host_sero %>% filter(host_species %in% c("human", "horse", "bird"))) 
, aes(x = host_species, y = (1 - prop_positive))) +
  geom_bar(stat="identity", width=0.5) + 
  xlab("") + ylab("Proportion
Seronegative") + theme(
  axis.text.x = element_text(size = 0)
)
