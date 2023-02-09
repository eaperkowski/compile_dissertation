library(ggplot2)
library(stringr)

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
