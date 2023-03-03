##########################################################################
## Load libraries and import data
##########################################################################
# Libraries
library(lme4)
library(emmeans)
library(tidyverse)
library(ggpubr)
library(stringr)
library(plantecophys)
library(ggpubr)

# Turn off digit rounding in emmean args
emm_options(opt.digits = FALSE)

# Load compiled datasheet
df <- read.csv("../../2022_NxCO2xI/data_sheets/NxCO2xI_compiled_datasheet.csv", 
               na.strings = "NA") %>%
  mutate(n.trt = as.numeric(n.trt),
         rd25.vcmax25 = rd25 / vcmax25,
         inoc = factor(inoc, levels = c("no.inoc", "inoc")),
         co2 = factor(co2, levels = c("amb", "elv")),
         nod.root.ratio = nodule.biomass / root.biomass) %>%
  filter(inoc == "inoc" | (inoc == "no.inoc" & nod.root.ratio < 0.05)) %>%
  unite(col = "co2.inoc", co2:inoc, sep = "_", remove = FALSE) 

##########################################################################
## Make IPCC flux figure
##########################################################################
## Create flux data frame with fluxes from Fig. 5.12 in IPCC (2021)
## report "Climate Change 2021: The Physical Science Basis"
fluxes <- data.frame(flux = c("Land in", "Land out", "Ocean in", 
                              "Ocean out", "Fossil fuels", 
                              "Land use change",
                              "Land in", "Land out", "Ocean in", 
                              "Ocean out", "Fossil fuels", 
                              "Land use change"),
                     type = c(rep("natural", 6), rep("human", 6)),
                     value = c(113, 111.1, 54.0, 54.6,
                               0, 0, 29, 25.6, 25.5, 
                               23, 9.4, 1.6))

## Prep to order fluxes in plot
fluxes$flux <- factor(fluxes$flux, levels = c("Land in", "Land out", "Ocean in", 
                                              "Ocean out", "Fossil fuels", 
                                              "Land use change"))

## Make bar plot with human and natural fluxes stacked
ipcc.2022 <- ggplot(data = fluxes, aes(x = flux, y = value, fill = type)) +
  geom_bar(stat = "identity")  + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_fill_manual(values = c("#BB5566", "#004488")) +
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, 50)) +
  labs(x = NULL,
       y = expression("Annual carbon flux (PgC yr"^"-1"*")"),
       fill = "Flux origin") +
  theme_bw(base_size = 30)

## Try to wrap fossil fuels x-axis label
ipcc.2022 + scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

png("defense_ipcc_flux_figure.png", 
    height = 10, width = 16, res = 600, units = "in")
ipcc.2022
dev.off()

##########################################################################
## Make IPCC flux figure
##########################################################################
## Add colorblind friendly palette
full.cols <- c("#DDAA33", "#004488", "#BB5566", "#555555")

## Create blank plot as spacer plot
blank.plot <- ggplot() + 
  theme_bw() +
  theme(panel.background = element_rect(color = "white",
                                        fill = "white"),
        panel.border = element_rect(color = "white"))


##########################################################################
## Make photosynthesis figure
##########################################################################
photo.elv <- data.frame(type = "elv",
                         Photosyn(Ca = c(50, 100, 200, 300, 400, 500, 600, 700,
                                         800, 900, 1000, 1100, 1200, 1300, 1400,
                                         1500, 1600, 1700, 1800, 1900, 2000),
                                  Vcmax = 50, Jmax = 110))

photo.amb <- data.frame(type = "amb",
                          Photosyn(Ca = c(50, 100, 200, 300, 400, 500, 600, 700,
                                          800, 900, 1000, 1100, 1200, 1300, 1400,
                                          1500, 1600, 1700, 1800, 1900, 2000),
                                   Vcmax = 100, Jmax = 125))

photo.merged <- photo.elv %>% full_join(photo.amb) %>%
  mutate(Anet = pmin(Ac, Aj))


aci <- ggplot(data = photo.merged, aes(x = Ci, y = Anet)) +
  geom_line(aes(linetype = type, color = type), linewidth = 3) +
  scale_y_continuous(limits = c(-0.1, 30)) +
  scale_color_manual(values = c("#BB5566", "#004488"), 
                     labels = c("ambient", "elevated")) +
  scale_linetype_manual(values = c("solid", "dotted"),
                        labels = c("ambient", "elevated")) +
  labs(x = expression(bold("Intercellular CO"["2"])),
       y = expression(bold("Net photosynthesis")),
       color = expression("CO"["2"]), 
       linetype = expression("CO"["2"])) +
  theme_bw(base_size = 18) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(linewidth = 2))
aci

aci.elv <- ggplot(data = subset(photo.merged, type == "elv"),
                          aes(x = Ci)) +
  geom_line(aes(y = Anet), linewidth = 3) +
  scale_y_continuous(limits = c(-0.1, 50)) +
  labs(x = expression(bold("Intercellular CO"["2"])),
       y = expression(bold("Net photosynthesis"))) +
  theme_bw(base_size = 18) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(linewidth = 2))

png("aci_bare.png", height = 4, width = 10, units = "in", res = 600)
ggarrange(aci.amb, aci.elv, ncol = 2, align = "hv")
dev.off()


aci.amb.limiting <- ggplot(data = subset(photo.merged, type == "amb"),
                      aes(x = Ci)) +
  geom_line(aes(y = Anet), linewidth = 3) +
  geom_line(aes(y = Ac), linewidth = 1, color = "#BB5566", linetype = "dashed") +
  geom_line(aes(y = Aj), linewidth = 1, color = "#004488", linetype = "dashed") +
  scale_y_continuous(limits = c(-0.1, 50)) +
  labs(x = expression(bold("Intercellular CO"["2"])),
       y = expression(bold("Net photosynthesis"))) +
  theme_bw(base_size = 18) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(linewidth = 2))

aci.elv.limiting <- ggplot(data = subset(photo.merged, type == "elv"),
                          aes(x = Ci)) +
  geom_line(aes(y = Anet), linewidth = 3) +
  geom_line(aes(y = Ac), linewidth = 1, color = "#BB5566", linetype = "dashed") +
  geom_line(aes(y = Aj), linewidth = 1, color = "#004488", linetype = "dashed") +
  scale_y_continuous(limits = c(-0.1, 50)) +
  labs(x = expression(bold("Intercellular CO"["2"])),
       y = expression(bold("Net photosynthesis"))) +
  theme_bw(base_size = 18) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(linewidth = 2))

aci.elv.wasteful <- ggplot(data = subset(photo.merged, type == "elv_waste"),
                           aes(x = Ci)) +
  geom_line(aes(y = Anet), linewidth = 3) +
  geom_line(aes(y = Ac), linewidth = 1, color = "#BB5566", linetype = "dashed") +
  geom_line(aes(y = Aj), linewidth = 1, color = "#004488", linetype = "dashed") +
  scale_y_continuous(limits = c(-0.1, 50)) +
  labs(x = expression(bold("Intercellular CO"["2"])),
       y = expression(bold("Net photosynthesis"))) +
  theme_bw(base_size = 18) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(linewidth = 2))

png("aci_limiting.png", height = 4, width = 10, units = "in", res = 600)
ggarrange(aci.amb.limiting, aci.elv.limiting, ncol = 2, align = "hv")
dev.off()

png("aci_limiting_waste.png", height = 4, width = 10, units = "in", res = 600)
ggarrange(aci.amb.limiting, aci.elv.wasteful, ncol = 2, align = "hv")
dev.off()

photo.fig <- ggplot(data = photo.merged, aes(x = Ci)) +
  geom_line(aes(y = ALEAF, linetype = type), linewidth = 3) +
  #geom_line(aes(y = Ac, color = type), linewidth = 1) +
  #geom_line(aes(y = Aj), linewidth = 1, color = "yellow") +
  scale_y_continuous(limits = c(-0.1, 50)) +
  #scale_color_manual(values = c("#004488", "#BB5566")) +
  scale_linetype_manual(values = c("dotted", "solid")) +
  labs(x = expression(bold("Intercellular CO"["2"])),
       y = expression(bold("Net photosynthesis"))) +
  guides(color = "none") +
  theme_bw(base_size = 18) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(linewidth = 2))
photo.fig

png("aci_example.png", height = 5, width = 6, units = "in", res = 600)
photo.fig
dev.off()

##########################################################################
## Make Ncost hypothesis fig
##########################################################################
ncost <- data.frame(pathway = c(rep("direct.uptake", 630), rep("n.fix", 630)),
                 n.trt = rep(seq(1,630,1), 2)) %>%
  mutate(ncost = ifelse(pathway == "direct.uptake", -5*log(n.trt)^2 + 20,
                        -0.02*n.trt -130))

ncost.hyp <- ggplot(data = ncost, 
                    aes(x = n.trt, y = ncost)) +
  geom_line(aes(color = pathway), size = 2) +
  scale_color_manual(values = c("#DDAA33", "#BB5566"),
                     labels = c("Direct uptake",
                                "Nitrogen fixation")) +
  labs(x = "Soil nitrogen availability",
       y = "Costs of nitrogen acquisition",
       color = NULL) +
  theme_bw(base_size = 18) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_text(face = "bold"))

png("../../compile_dissertation/defense_seminar/ncost_hyp.png",
    height = 4, width = 7, units = "in", res = 600)
ncost.hyp
dev.off()


##########################################################################
## Narea regression line prep
##########################################################################
narea <- lmer(narea ~ co2 * inoc * n.trt + (1|rack:co2), data = df)
test(emtrends(narea, ~inoc*co2, "n.trt"))

## Emmean fxns for regression lines + error ribbons
narea.regline <- data.frame(emmeans(narea, ~co2*inoc, "n.trt",
                                    at = list(n.trt = seq(0, 630, 5)),
                                    type = "response")) %>%
  mutate(co2.inoc = str_c(co2, "_", inoc))

##########################################################################
## Narea plot
##########################################################################
narea.plot <- ggplot(data = df, 
                     aes(x = n.trt, 
                         y = narea,    
                         fill = co2.inoc)) +
  geom_jitter(size = 3, alpha = 0.75, shape = 21) +
  geom_smooth(data = narea.regline,
              aes(color = co2.inoc, y = emmean), 
              size = 1.5, se = FALSE) +
  geom_ribbon(data = narea.regline,
              aes(fill = co2.inoc, y = emmean, 
                  ymin = lower.CL, ymax = upper.CL), 
              size = 1.5, alpha = 0.25) +
  scale_fill_manual(values = full.cols,
                    labels = c("Ambient, inoculated",
                               "Ambient, uninoculated",
                               "Elevated, inoculated",
                               "Elevated, uninoculated")) +
  scale_color_manual(values = full.cols,
                     labels = c("Ambient, inoculated",
                                "Ambient, uninoculated",
                                "Elevated, inoculated",
                                "Elevated, uninoculated")) +
  scale_y_continuous(limits = c(0, 3.24), breaks = seq(0, 3.2, 0.8)) +
  labs(x = "Soil N fertilization (ppm)",
       y = expression(bold(italic("N")["area"]*" (gN m"^"-2"*")")),
       fill = "Treatment", color = "Treatment") +
  theme_bw(base_size = 18) +
  theme(axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        panel.border = element_rect(size = 1.25))
narea.plot

##########################################################################
## Vcmax regression line prep
##########################################################################
## Copy removed outliers and lmer fxn
vcmax25 <- lmer(vcmax25 ~ co2 * inoc * n.trt + (1|rack:co2), data = df)
test(emtrends(vcmax25, ~co2*inoc, "n.trt"))

## Emmean fxns for regression lines + error ribbons
vcmax25.regline <- data.frame(emmeans(vcmax25, ~co2*inoc, "n.trt",
                                      at = list(n.trt = seq(0, 630, 5)),
                                      type = "response")) %>%
  mutate(co2.inoc = str_c(co2, "_", inoc),
         linetype = ifelse(inoc == "inoc", "dashed", "solid"))

##########################################################################
## Vcmax plot
##########################################################################
vcmax25.plot <- ggplot(data = df, 
                       aes(x = n.trt, 
                           y = vcmax25,    
                           fill = co2.inoc)) +
  geom_jitter(size = 3, alpha = 0.75, shape = 21) +
  geom_smooth(data = vcmax25.regline,
              aes(color = co2.inoc, y = emmean, linetype = linetype), 
              size = 1.5, se = FALSE) +
  geom_ribbon(data = vcmax25.regline,
              aes(fill = co2.inoc, y = emmean, 
                  ymin = lower.CL, ymax = upper.CL), 
              size = 1.5, alpha = 0.25) +
  scale_fill_manual(values = full.cols,
                    labels = c("Ambient, inoculated",
                               "Ambient, uninoculated",
                               "Elevated, inoculated",
                               "Elevated, uninoculated")) +
  scale_color_manual(values = full.cols,
                     labels = c("Ambient, inoculated",
                                "Ambient, uninoculated",
                                "Elevated, inoculated",
                                "Elevated, uninoculated")) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, 50)) +
  labs(x = "Soil N fertilization (ppm)",
       y = expression(bold(italic("V")["cmax25"]*" ("*mu*"mol m"^"-2"*" s"^"-1"*")")),
       fill = "Treatment", color = "Treatment") +
  theme_bw(base_size = 18) +
  theme(axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        panel.border = element_rect(size = 1.25)) +
  guides(linetype = "none")
vcmax25.plot

##########################################################################
## Total leaf area regression line prep
##########################################################################
tla <- lmer(tla ~ co2 * inoc * n.trt + (1|rack:co2), data = df)
test(emtrends(tla, ~co2*inoc, "n.trt"))

## Emmean fxns for regression lines + error ribbons
tla.regline <- data.frame(emmeans(tla, ~co2*inoc, "n.trt",
                                  at = list(n.trt = seq(0, 630, 5)),
                                  type = "response")) %>%
  mutate(co2.inoc = str_c(co2, "_", inoc))


##########################################################################
## Total leaf area plot
##########################################################################
tla.plot <- ggplot(data = df, 
                   aes(x = n.trt, 
                       y = tla,
                       fill = co2.inoc)) +
  geom_jitter(size = 3, alpha = 0.75, shape = 21) +
  geom_smooth(data = tla.regline,
              aes(color = co2.inoc, y = emmean), 
              size = 1.5, se = FALSE) +
  geom_ribbon(data = tla.regline,
              aes(fill = co2.inoc, y = emmean, 
                  ymin = lower.CL, ymax = upper.CL), 
              size = 1.5, alpha = 0.25) +
  scale_fill_manual(values = full.cols,
                    labels = c("Ambient, inoculated",
                               "Ambient, uninoculated",
                               "Elevated, inoculated",
                               "Elevated, uninoculated")) +
  scale_color_manual(values = full.cols,
                     labels = c("Ambient, inoculated",
                                "Ambient, uninoculated",
                                "Elevated, inoculated",
                                "Elevated, uninoculated")) +
  scale_y_continuous(limits = c(0, 1200), breaks = seq(0, 1200, 300)) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  labs(x = "Soil N fertilization (ppm)",
       y = expression(bold("Total leaf area (cm"^"2"*")")),
       fill = "Treatment", color = "Treatment",
       shape = "Inoculation") +
  theme_bw(base_size = 18) +
  theme(axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        panel.border = element_rect(size = 1.25)) +
  guides(linetype = "none")
tla.plot

##########################################################################
## Total biomass regression line prep
##########################################################################
tbio <- lmer(total.biomass ~ co2 * inoc * n.trt + (1|rack:co2), data = df)
test(emtrends(tbio, ~co2*inoc, "n.trt"))

## Emmean fxns for regression lines + error ribbons
tbio.regline <- data.frame(emmeans(tbio, ~co2*inoc, "n.trt",
                                   at = list(n.trt = seq(0, 630, 5)),
                                   type = "response")) %>%
  mutate(co2.inoc = str_c(co2, "_", inoc))


##########################################################################
## Total biomass plot
##########################################################################
tbio.plot <- ggplot(data = df, 
                    aes(x = n.trt, 
                        y = total.biomass,    
                        fill = co2.inoc)) +
  geom_jitter(size = 3, alpha = 0.75, shape = 21) +
  geom_smooth(data = tbio.regline,
              aes(color = co2.inoc, y = emmean), 
              size = 1.5, se = FALSE) +
  geom_ribbon(data = tbio.regline,
              aes(fill = co2.inoc, y = emmean, 
                  ymin = lower.CL, ymax = upper.CL), 
              size = 1.5, alpha = 0.25) +
  scale_fill_manual(values = full.cols,
                    labels = c("Ambient, inoculated",
                               "Ambient, uninoculated",
                               "Elevated, inoculated",
                               "Elevated, uninoculated")) +
  scale_color_manual(values = full.cols,
                     labels = c("Ambient, inoculated",
                                "Ambient, uninoculated",
                                "Elevated, inoculated",
                                "Elevated, uninoculated")) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 5)) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  labs(x = "Soil N fertilization (ppm)",
       y = "Total biomass (g)",
       fill = "Treatment", color = "Treatment") +
  theme_bw(base_size = 18) +
  theme(axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        panel.border = element_rect(size = 1.25)) +
  guides(linetype = "none")
tbio.plot

##########################################################################
## NxCO2xI Leaf acclimation fig
##########################################################################
png("../../compile_dissertation/defense_seminar/NxCO2xI_leafAcclim.png",
    height = 4, width = 12, units = "in", res = 600)
ggarrange(narea.plot, vcmax25.plot, ncol = 2, nrow = 1,
          align = "hv", legend = "right", common.legend = TRUE,
          labels = c("(a)", "(b)"), font.label = list(size = 18))
dev.off()

##########################################################################
## NxCO2xI Whole plant acclimation fig
##########################################################################
png("../../compile_dissertation/defense_seminar/NxCO2xI_wholePlantAcclim.png",
    height = 4, width = 12, units = "in", res = 600)
ggarrange(.plot, tbio.plot, ncol = 2, nrow = 1,
          align = "hv", legend = "right", common.legend = TRUE,
          labels = c("(a)", "(b)"), font.label = list(size = 18))
dev.off()





