###################################################################
### various individual plots for the various panels of figure 2 ###
###################################################################

####
## Host panel 1, just AUC of the hosts in the community
####

ggplot(AUC_titer.gg.s, aes(est, Host)) + 
  geom_point(lwd = 0.50) +
  geom_errorbarh(aes(xmin = lwr, xmax = upr, y = Host), height = 0.5, lwd = 0.3) +
  ylab("Host Species") +
  xlab("AUC Titer Profile") +
  theme(
    panel.grid = element_blank()
  , panel.border = element_rect(colour = "black", fill = NA, size = 0)
  , axis.line = element_line()
  , axis.text.y = element_text(size = 10)
  , axis.text.x = element_text(size = 10)
  , axis.title.x = element_text(size = 12)
  , axis.title.y = element_text(size = 12)
  )
ggsave(filename = "/Users/Morgan/Desktop/host_A.pdf", plot = last_plot(), width = 2.2,  height = 2.24, units = c("in"))

####
## Mosquito panel 1, just AUC of the mosquitoes in the community
####

ggplot(mosq_inf_AUC_all_samps_adj_to_com.gg.s, aes(est, mosq)) + 
  geom_point(lwd = 0.50) +
  geom_errorbarh(aes(xmin = lwr, xmax = upr, y = mosq), height = 0.5, lwd = 0.3) +
  scale_x_continuous(breaks = c(0, 30, 60, 120)) +
  ylab("Mosquito Species") +
  xlab("AUC Infection
Probability * 
AUC Transmission 
Probability") +
  theme(
    panel.grid = element_blank()
  , panel.border = element_rect(colour = "black", fill = NA, size = 0)
  , axis.line = element_line()
  , axis.text.y = element_text(size = 8)
  , axis.text.x = element_text(size = 10)
  , axis.title.x = element_text(size = 10)
  , axis.title.y = element_text(size = 12)
  )
ggsave(filename = "/Users/Morgan/Desktop/mosq_A.pdf", plot = last_plot(), width = 2.3,  height = 2.65, units = c("in"))

####
## Host panel 2, Host to Mosquito transmission matrix and summary CI plot
####

## The summary part of the panel
ggplot(host_competence.gg.heat.s.p) +
  geom_point(aes(est, host), lwd = 0.5) + 
  geom_errorbarh(aes(xmin = lwr, xmax = upr, y = host), height = 0.5, lwd = 0.30, linetype = "solid") +
  scale_x_continuous(breaks = c(0, 1.5, 3.0), labels = c("0.0", "1.5", "3.0")) +
  theme(
    panel.grid = element_blank()
  , legend.key.size = unit(.65, "cm")
  , legend.title = element_text(size = 12)
  , legend.text = element_text(size = 10) 
  , panel.border = element_rect(colour = "black", fill = NA, size = 0)
  , axis.line = element_line()
  , axis.text.y = element_text(size = 10)
  , axis.text.x = element_text(size = 8)
  , axis.title.x = element_text(size = 10)
  , axis.title.y = element_text(size = 12)
  ) +
  xlab("Total Host to 
Mosquito 
Infections") + ylab("")
ggsave(filename = "/Users/Morgan/Desktop/host_B1.pdf", plot = last_plot(), width = 1.8,  height = 2.5, units = c("in"))

## The matrix part of the panel
ggplot(host_competence.mat.gg.s
  , aes(mosq, host)) + 
  geom_tile(aes(fill = est)
    , colour = "white") + 
  scale_fill_gradient(low = "grey95", high = "#4472C4"
  , name = "Infected 
mosquitoes") +
  theme(
    axis.text.x     = element_text(angle = 300, hjust = 0, colour = "grey50", size = 9)
  , legend.key.size = unit(.35, "cm")
  , legend.text     = element_text(size = 9)
  , legend.title    = element_text(size = 10)
  , axis.text.y     = element_text(size = 0)
  , axis.title.x = element_text(size = 10)
  , axis.title.y = element_text(size = 10)
  ) +
  ylab("") +
  xlab("Newly Infected Mosquitoes")
ggsave(filename = "/Users/Morgan/Desktop/host_B2.pdf", plot = last_plot(), width = 2.65,  height = 3.13, units = c("in"))
  
####
## Mosquito panel 2, Mosquito to Host transmission matrix and summary CI plot
####

ggplot(m_to_h_mat.gg.s) +
  geom_point(aes(est, mosq), lwd = 0.5) + 
  geom_errorbarh(aes(xmin = lwr, xmax = upr, y = mosq), height = 0.5, lwd = 0.3) +
  theme(
    panel.grid = element_blank()
  , legend.key.size = unit(.65, "cm")
  , legend.title = element_text(size = 12)
  , legend.text = element_text(size = 10) 
  , panel.border = element_rect(colour = "black", fill = NA, size = 0)
  , axis.line = element_line()
  , axis.text.y = element_text(size = 8)
  , axis.text.x = element_text(size = 10)
  , axis.title.x = element_text(size = 10)
  , axis.title.y = element_text(size = 12)
  ) +
  xlab("Total Mosquito
to Host 
Infections") +  
  ylab("") + scale_x_continuous(lim = c(0, 3), breaks = c(0, 1, 2, 3))
ggsave(filename = "/Users/Morgan/Desktop/mosq_B1.pdf", plot = last_plot(), width = 2.15,  height = 2.5, units = c("in"))

## Matrix component 
ggplot(m_to_h_mat.gg.mh
  , aes(host, mosq)) + 
  geom_tile(aes(fill = est)
    , colour = "white") + 
  scale_fill_gradient(low = "grey95", high = "orangered4"
  , breaks = c(0.40, 0.70, 1.00, 1.30)
  , labels = c(0.40, 0.70, 1.00, 1.30)
  , name = "Infected 
hosts") +
  theme(
    axis.text.x     = element_text(angle = 300, hjust = 0, colour = "grey50", size = 9)
  , legend.key.size = unit(.35, "cm")
  , legend.text     = element_text(size = 8)
  , legend.title    = element_text(size = 8)
  , axis.text.y     = element_text(size = 0)
  , axis.title.x = element_text(size = 10)
  , axis.title.y = element_text(size = 10)
  ) +
  ylab("") +
  xlab("Newly Infected Hosts") 
ggsave(filename = "/Users/Morgan/Desktop/mosq_B2.pdf", plot = last_plot(), width = 3.2,  height = 2.62, units = c("in"))

####
## Host Panel 3, Host to Host transmission matrix and summary CI plot
####

 ## NOTE: for the supplemental "exposure" figure (currently Supplemental figure) use:
  ## physiol_mat.s.all.gg.supp
  ## physiol_mat.gg.f.p.supp

## Summary panel components
ggplot(
# physiol_mat.s.all.gg
  physiol_mat.s.all.gg.supp
  , aes(est_tot, G1)) + 
  geom_point(lwd = 0.5) +
  geom_errorbarh(aes(xmin = lwr_tot, xmax = upr_tot, y = G1), height = 0.5, lwd = 0.3) +
  # scale_x_continuous(breaks = c(0, 1.5, 3.0, 4.5, 6.0), labels = c("0.0", "1.5", "3.0", "4.5", "6.0")) +
  scale_x_continuous(breaks = c(0, 1.0, 2.0), labels = c("0.0", "1.0", "2.0"), lim = c(0, 2)) +
  ylab("") +
  xlab("Total Host to 
Host Infections") +
  theme(
    panel.grid = element_blank()
  , legend.key.size = unit(.35, "cm")
  , legend.title = element_text(size = 12)
  , legend.text = element_text(size = 10) 
  , panel.border = element_rect(colour = "black", fill = NA, size = 0)
  , axis.line = element_line()
  , axis.text.y = element_text(size = 10)
  , axis.text.x = element_text(size = 8)
  , axis.title.x = element_text(size = 10)
  , axis.title.y = element_text(size = 12)
  ) 
ggsave(filename = "/Users/Morgan/Desktop/host_C1.pdf", plot = last_plot(), width = 1.8,  height = 2.35, units = c("in"))

ggplot(
# physiol_mat.s.all.gg
  physiol_mat.s.all.gg.supp
  , aes(est_prop, G1)) + 
  geom_point(lwd = 0.5) +
  scale_x_continuous(breaks = c(0, 0.3, 0.6, 0.9), labels = c("0", "0.3", "0.6", "0.9"), lim = c(0, 0.9)) +
  geom_errorbarh(aes(xmin = lwr_prop, xmax = upr_prop, y = G1), height = 0.5, lwd = 0.3) +
  ylab("") +
  xlab("Proportion of 
Infections Self")  +
  theme(
    panel.grid = element_blank()
  , legend.key.size = unit(.65, "cm")
  , legend.title = element_text(size = 12)
  , legend.text = element_text(size = 10) 
  , panel.border = element_rect(colour = "black", fill = NA, size = 0)
  , axis.line = element_line()
  , axis.text.y = element_text(size = 10)
  , axis.text.x = element_text(size = 10)
  , axis.title.x = element_text(size = 10)
  , axis.title.y = element_text(size = 12)
  ) 
ggsave(filename = "/Users/Morgan/Desktop/host_C2.pdf", plot = last_plot(), width = 1.8,  height = 2.35, units = c("in"))

## Matrix panel component
ggplot(
  # physiol_mat.gg.f.p
  physiol_mat.gg.f.p.supp
  , aes(G2, G1)) + 
  geom_tile(aes(fill = comp.est)
    , colour = "white") + 
  scale_fill_gradient(low = "grey95", high = "#4472C4"
  , name = "Second 
generation
hosts 
exposed 
per host") +
  theme(
    axis.text.x     = element_text(angle = 300, hjust = 0, colour = "grey50", size = 9)
  , legend.key.size = unit(.45, "cm")
  , legend.text     = element_text(size = 10)
  , legend.title    = element_text(size = 10)
  , axis.text.y     = element_text(size = 0)
  , axis.title.x = element_text(size = 10)
  , axis.title.y = element_text(size = 10)
  ) +
  ylab("") +
  xlab("Second Generation Host")
ggsave(filename = "/Users/Morgan/Desktop/host_C3.pdf", plot = last_plot(), width = 3.00,  height = 2.65, units = c("in"))

####
## Mosquito Panel 3, mosquito to mosquito transmission matrix and summary CI plot
####

## Summary panel component
ggplot(physiol_mat.s.all.gg.mm, aes(est_tot, G1)) + 
  geom_point(lwd = 0.5) +
  geom_errorbarh(aes(xmin = lwr_tot, xmax = upr_tot, y = G1), height = 0.5, lwd = 0.3) +
  ylab("") +
  xlab("Total Mosquito 
to Mosquito
Infections") +
  theme(
    panel.grid = element_blank()
  , legend.key.size = unit(.65, "cm")
  , legend.title = element_text(size = 12)
  , legend.text = element_text(size = 10) 
  , panel.border = element_rect(colour = "black", fill = NA, size = 0)
  , axis.line = element_line()
  , axis.text.y = element_text(size = 8)
  , axis.text.x = element_text(size = 10)
  , axis.title.x = element_text(size = 10)
  , axis.title.y = element_text(size = 12)
  ) + scale_x_continuous(breaks = c(0, 1.5, 3))
ggsave(filename = "/Users/Morgan/Desktop/mosq_C1.pdf", plot = last_plot(), width = 2.2,  height = 2.50, units = c("in"))

ggplot(physiol_mat.s.all.gg.mm, aes(est_prop, G1)) + 
  geom_point(lwd = 0.5) +
  geom_errorbarh(aes(xmin = lwr_prop, xmax = upr_prop, y = G1), height = 0.5, lwd = 0.3) +
  ylab("") +
  xlab("Proportion of 
Infections Self") +
  theme(
    panel.grid = element_blank()
  , legend.key.size = unit(.65, "cm")
  , legend.title = element_text(size = 12)
  , legend.text = element_text(size = 10) 
  , panel.border = element_rect(colour = "black", fill = NA, size = 0)
  , axis.line = element_line()
  , axis.text.y = element_blank()
  , axis.text.x = element_text(size = 10)
  , axis.title.x = element_text(size = 10)
  , axis.title.y = element_text(size = 12)
  ) + scale_x_continuous(lim = c(0, 1.0), breaks = c(0.0, 0.5, 1.0))
ggsave(filename = "/Users/Morgan/Desktop/mosq_C2.pdf", plot = last_plot(), width = 1.15,  height = 2.34, units = c("in"))

## Matrix panel component
ggplot(physiol_mat.gg.f.p.mm
  , aes(G2, G1)) + 
  geom_tile(aes(fill = comp.est)
    , colour = "white") + 
  scale_fill_gradient(low = "grey95", high = "orangered4"
  , breaks = c(0.0, 0.15, 0.30, 0.45)
  , labels = c(0.0, 0.15, 0.30, 0.45)
  , lim    = c(0, 0.55)
  , name = "Second 
generation
mosquitoes 
infected 
per mosquito") +
  theme(
    axis.text.x     = element_text(angle = 300, hjust = 0, colour = "grey50", size = 8)
  , legend.key.size = unit(.35, "cm")
  , legend.text     = element_text(size = 8)
  , legend.title    = element_text(size = 8)
  , axis.text.y     = element_blank()
  , axis.title.x = element_text(size = 10)
  , axis.title.y = element_text(size = 10)
  ) +
  ylab("") +
  xlab("Second Generation Mosquito")
ggsave(filename = "/Users/Morgan/Desktop/mosq_C3.pdf", plot = last_plot(), width = 3.1,  height = 2.99, units = c("in"))
