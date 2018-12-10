# load library and dataset
library("ggplot2")
library("TDAstats")

# calculate persistent homology and visualize/save
data("circle2d")
phom.circ <- calculate_homology(circle2d)

plot_persist(phom.circ) +
  xlab("Feature appearance") +
  ylab("Feature disappearance")
ggsave("example-persist.png", width = 4.5, height = 3.5)

plot_barcode(phom.circ)
ggsave("example-barcode.png", width = 4.5, height = 3.5)

# make bias example for persistence diagrams
phom.bias <- matrix(c(0, 0.5, 1.0,
                      0, 0.75, 1.1),
                    byrow = TRUE,
                    ncol = 3)
colnames(phom.bias) <- c("dimension", "birth", "death")
plot_persist(phom.bias) +
  xlab("Feature appearance") +
  ylab("Feature disappearance")
ggsave("example-bias1.png", width = 4.5, height = 3.5)
plot_persist(phom.bias, flat = TRUE) +
  xlim(c(0, 1)) +
  ylim(c(0, 0.6)) +
  xlab("Feature appearance")
ggsave("example-bias2.png", width = 4.5, height = 3.5)

# make sphere example to highlight benefits
data("sphere3d")
phom.sphere <- calculate_homology(sphere3d, dim = 2)

plot_barcode(phom.sphere)
ggsave("sphere-barcode.png", width = 4.5, height = 3.5)

plot_persist(phom.sphere) +
  xlab("Feature appearance") +
  ylab("Feature disappearance")
ggsave("sphere-persist.png", width = 4.5, height = 3.5)

plot_persist(phom.sphere, flat = TRUE) +
  xlab("Feature appearance")
ggsave("sphere-flat.png", width = 4.5, height = 3.5)
