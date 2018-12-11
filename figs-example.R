# load library and dataset
library("ggplot2")
library("TDAstats")

# calculate persistent homology and visualize/save
data("circle2d")
phom.circ <- calculate_homology(circle2d)
temp.phom <- as.data.frame(phom.circ)
temp.phom$pers <- temp.phom$death - temp.phom$birth
temp.phom$dimension <- as.factor(temp.phom$dimension)

plot_persist(phom.circ) +
  geom_point(data = temp.phom, aes(x = birth,
                                   y = death,
                                   colour = dimension,
                                   shape = dimension),
             size = I(2.5)) +
  xlab("Feature appearance") +
  ylab("Feature disappearance") +
  ggtitle("Conventional persistence diagram") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 15))
ggsave("example-persist.png", width = 4.5, height = 3.5)

plot_barcode(phom.circ) +
  ggtitle("Persistence barcode") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 15))
ggsave("example-barcode.png", width = 4.5, height = 3.5)

# make bias example for persistence diagrams
phom.bias <- matrix(c(0, 0.5, 1.0,
                      0, 0.75, 1.1),
                    byrow = TRUE,
                    ncol = 3)
colnames(phom.bias) <- c("dimension", "birth", "death")
temp.phom <- as.data.frame(phom.bias)
temp.phom$pers <- temp.phom$death - temp.phom$birth
temp.phom$dimension <- as.factor(temp.phom$dimension)

plot_persist(phom.bias) +
  geom_point(data = temp.phom, aes(x = birth,
                                   y = death,
                                   colour = dimension,
                                   shape = dimension),
             size = I(2.5)) +
  xlab("Feature appearance") +
  ylab("Feature disappearance") +
  ggtitle("Conventional persistence diagram") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 15))
ggsave("example-bias1.png", width = 4.5, height = 3.5)
plot_persist(phom.bias, flat = TRUE) +
  geom_point(data = temp.phom, aes(x = birth,
                                   y = pers,
                                   colour = dimension,
                                   shape = dimension),
             size = I(2.5)) +
  xlim(c(0, 1)) +
  ylim(c(0, 0.6)) +
  xlab("Feature appearance") +
  ggtitle("Flat persistence diagram") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 15))
ggsave("example-bias2.png", width = 4.5, height = 3.5)

# make sphere example to highlight benefits
data("sphere3d")
phom.sphere <- calculate_homology(sphere3d, dim = 2)
temp.phom <- as.data.frame(phom.sphere)
temp.phom$pers <- temp.phom$death - temp.phom$birth
temp.phom$dimension <- as.factor(temp.phom$dimension)

plot_barcode(phom.sphere) +
  ggtitle("Persistence barcode") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 15))
ggsave("sphere-barcode.png", width = 4.5, height = 3.5)

plot_persist(phom.sphere) +
  geom_point(data = temp.phom, aes(x = birth,
                                   y = death,
                                   colour = dimension,
                                   shape = dimension),
             size = I(2.5)) +
  xlab("Feature appearance") +
  ylab("Feature disappearance") +
  ggtitle("Conventional persistence diagram") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 15))
ggsave("sphere-persist.png", width = 4.5, height = 3.5)

plot_persist(phom.sphere, flat = TRUE) +
  geom_point(data = temp.phom, aes(x = birth,
                                   y = pers,
                                   colour = dimension,
                                   shape = dimension),
             size = I(2.5)) +
  xlab("Feature appearance") +
  ggtitle("Flat persistence diagram") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 15))
ggsave("sphere-flat.png", width = 4.5, height = 3.5)
