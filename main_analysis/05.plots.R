u################################################################################
# Code for the analysis in:
#
#   "The mortality burden of extreme heat in Connecticut: 
#   a time series analysis"
#   Emily Goddard, Chengyi Lin, Yiqun Ma, and Kai Chen
#
# Updated: 12 April 2023
#   this code is also available at 
#   https://github.com/EmilyAlice10/mortality_extremeheat_CT
################################################################################

################################################################################
# CREATE PLOTS
################################################################################

# LOAD LIBRARIES
library(ggnewscale)

# LOAD SUBGROUP RELATIVE RISKS
rrsubgroup <- read.csv("rrsubgroup.csv")

#SET WORKING DIRECTORY FOR OUTPUTS
setwd("Figures/")

# DEFINE LEVELS
rr$level <- ifelse(rr$temp >= exheattemp, "Extreme Heat", 
                   ifelse(rr$temp >= mintemp & rr$temp < exheattemp, "Heat", "Cold"))
rr$level <- factor(rr$level, levels = c("Cold", "Heat", "Extreme Heat"))

# DEFINE LEVELS FOR SUBGROUPS
rrsubgroup$levelmale <- ifelse(rrsubgroup$temp >= exheattemp, "Extreme Heat",
                               ifelse(rrsubgroup$temp >= mintemp & 
                                        rrsubgroup$temp < exheattemp, "Heat", "Cold"))
rrsubgroup$levelfemale <- ifelse(rrsubgroup$temp >= exheattemp, "Extreme Heat",
                                 ifelse(rrsubgroup$temp >= mintemp & 
                                          rrsubgroup$temp < exheattemp, "Heat", "Cold"))
rrsubgroup$levelold <- ifelse(rrsubgroup$temp >= exheattemp, "Extreme Heat",
                              ifelse(rrsubgroup$temp >= mintemp & 
                                       rrsubgroup$temp < exheattemp, "Heat", "Cold"))
rrsubgroup$levelyoung <- ifelse(rrsubgroup$temp >= exheattemp, "Extreme Heat",
                                ifelse(rrsubgroup$temp >= mintemp & 
                                         rrsubgroup$temp < exheattemp, "Heat", "Cold"))
rrsubgroup$levelmale <- factor(rrsubgroup$levelmale, levels = c("Cold", "Heat", "Extreme Heat"))
rrsubgroup$levelfemale <- factor(rrsubgroup$levelfemale, levels = c("Cold", "Heat", "Extreme Heat"))
rrsubgroup$levelold <- factor(rrsubgroup$levelold, levels = c("Cold", "Heat", "Extreme Heat"))
rrsubgroup$levelyoung <- factor(rrsubgroup$levelyoung, levels = c("Cold", "Heat", "Extreme Heat"))

################################################################################

# CREATE FIGURE 1
figure1 <- ggplot(rr, aes(x=temp)) +
  geom_line(aes(y=RR), colour="brown4") +
  geom_line(aes(y=RR, col=level)) +
  geom_line(aes(y=RRlow), col="grey70", linetype = "dashed") +
  geom_line(aes(y=RRhigh), col="grey70", linetype = "dashed") +
  scale_color_manual(name = "Legend", values=c("Extreme Heat"="red1", "Heat"="brown4", "Cold"="blue")) +
  geom_ribbon(aes(ymin = RRlow, ymax = RRhigh), alpha = 0.1) +
  geom_vline(xintercept = mintemp, linetype="dashed", col="dimgrey", linewidth=0.2) +
  geom_vline(xintercept = exheattemp, linetype="dashed", col="black", linewidth=0.2) +
  geom_hline(yintercept = 1.00, linewidth= 0.4) +
  scale_x_continuous(breaks = seq(8, 28, by = 1)) + 
  scale_y_continuous(breaks = seq(0.95, 1.08, by = 0.01)) + 
  theme(panel.background = element_blank())

# FORMAT AND OUTPUT FIGURE 1
tiff(file="figure1.tiff",
     width=1500, height=1125, units="px", res=300)
print(figure1 + 
        labs(y = "Relative Risk (95% CI)", x = "Temperature (°C)") +
        theme(axis.title = element_text(family = "Times New Roman", size = (12)),
              axis.text = element_text(family = "Times New Roman", size = (8)),
              legend.title = element_text(family = "Times New Roman", size = (12)),
              legend.text = element_text(family = "Times New Roman", size = (10))))
dev.off()

################################################################################

# CREATE FIGURE 2
figure2 <- ggplot(rrsubgroup, aes(x=temp)) +
  geom_line(aes(y=RRmale), colour="navy", show.legend = FALSE) +
  geom_line(aes(y=RRlowmale), colour="navy", linetype = "dashed", show.legend = FALSE) +
  geom_line(aes(y=RRhighmale), colour="navy", linetype = "dashed", show.legend = FALSE) +
  geom_line(aes(y=RRmale, col=levelmale)) +
  scale_color_manual(name = "Males", values=c("Extreme Heat"="blue1", "Heat"="navy", "Cold"="dimgrey")) +
  new_scale_color() +
  geom_line(aes(y=RRfemale), colour="darkred", show.legend = FALSE) +
  geom_line(aes(y=RRlowfemale), colour="darkred", linetype = "dashed", show.legend = FALSE) +
  geom_line(aes(y=RRhighfemale), colour="darkred", linetype = "dashed", show.legend = FALSE) +
  geom_line(aes(y=RRfemale, col=levelfemale)) +
  scale_color_manual(name = "Females", values=c("Extreme Heat"="firebrick1", "Heat"="darkred", "Cold"="grey81")) +
  geom_vline(xintercept = mintemp, linetype="dashed", col="dimgrey", linewidth=0.2) +
  geom_vline(xintercept = exheattemp, linetype="dashed", col="black", linewidth=0.2) +
  geom_hline(yintercept = 1.00, linewidth= 0.4) +
  scale_x_continuous(breaks = seq(8, 28, by = 1)) + 
  scale_y_continuous(breaks = seq(0.92, 1.1, by = 0.01)) + 
  theme(panel.background = element_blank())

# FORMAT AND OUTPUT FIGURE 2
tiff(file="figure2.tiff",
     width=1500, height=1125, units="px", res=300)
print(figure2 + 
        labs(y = "Relative Risk (95% CI)", x = "Temperature (°C)") +
        theme(axis.title = element_text(family = "Times New Roman", size = (12)),
              axis.text = element_text(family = "Times New Roman", size = (8)),
              legend.title = element_text(family = "Times New Roman", size = (12)),
              legend.text = element_text(family = "Times New Roman", size = (10))))
dev.off()

################################################################################

# CREATE FIGURE 3
figure3 <- ggplot(rrsubgroup, aes(x=temp)) +
  geom_line(aes(y=RRold), colour="chocolate4", show.legend = FALSE) +
  geom_line(aes(y=RRlowold), colour="chocolate4", linetype = "dashed", show.legend = FALSE) +
  geom_line(aes(y=RRhighold), colour="chocolate4", linetype = "dashed", show.legend = FALSE) +
  geom_line(aes(y=RRold, col=levelold)) +
  scale_color_manual(name = "65+", values=c("Extreme Heat"="darkorange1", "Heat"="chocolate4", "Cold"="grey30")) +
  new_scale_color() +
  geom_line(aes(y=RRyoung), colour="springgreen4", show.legend = FALSE) +
  geom_line(aes(y=RRlowyoung), colour="springgreen4", linetype = "dashed", show.legend = FALSE) +
  geom_line(aes(y=RRhighyoung), colour="springgreen4", linetype = "dashed", show.legend = FALSE) +
  geom_line(aes(y=RRyoung, col=levelyoung)) +
  scale_color_manual(name = "Under 65", values=c("Extreme Heat"="chartreuse1", "Heat"="springgreen4", "Cold"="grey81")) +
  geom_vline(xintercept = mintemp, linetype="dashed", col="dimgrey", linewidth=0.2) +
  geom_vline(xintercept = exheattemp, linetype="dashed", col="black", linewidth=0.2) +
  geom_hline(yintercept = 1.00, linewidth= 0.4) +
  scale_x_continuous(breaks = seq(8, 28, by = 1)) + 
  scale_y_continuous(breaks = seq(0.89, 1.11, by = 0.01)) + 
  theme(panel.background = element_blank())

# FORMAT AND OUTPUT FIGURE 3
tiff(file="figure3.tiff",
     width=1500, height=1125, units="px", res=300)
print(figure3 + 
        labs(y = "Relative Risk (95% CI)", x = "Temperature (°C)") +
        theme(axis.title = element_text(family = "Times New Roman", size = (12)),
              axis.text = element_text(family = "Times New Roman", size = (8)),
              legend.title = element_text(family = "Times New Roman", size = (12)),
              legend.text = element_text(family = "Times New Roman", size = (10))))
dev.off()

rm(figure1, figure2, figure3)

#