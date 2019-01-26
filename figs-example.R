#####SETUP#####
# load library and dataset
library("ggplot2")
library("ggforce")
library("TDAstats")

# reproducibility
set.seed(1)

#####FIGURE 1#####
# load data and convert circle into annulus by adding noise
data("circle2d")
circle2d[, 1] <- circle2d[, 1] + rnorm(nrow(circle2d),
                                       mean = 0, sd = 0.1)
circle2d[, 2] <- circle2d[, 2] + rnorm(nrow(circle2d),
                                       mean = 0, sd = 0.1)

# setup data for visualization
phom.circ <- calculate_homology(circle2d)
temp.phom <- as.data.frame(phom.circ)
temp.phom$pers <- temp.phom$death - temp.phom$birth
temp.phom$dimension <- as.factor(temp.phom$dimension)

# plot circle2d in Cartesian
circ.df <- as.data.frame(circle2d)
colnames(circ.df) <- c("x", "y")
ggplot(data = circ.df, aes(x = x, y = y)) +
  geom_point(size = I(1.5)) +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 15)) +
  xlab("X Cartesian coordinate") +
  ylab("Y Cartesian coordinate") +
  coord_fixed() +
  ggtitle("(a) Sample dataset")
ggsave("Fig1a.eps", width = 4.5, height = 3.5)

# plot example of Vietoris-Rips complex
plot.diam <- 0.2
ggplot(data = circ.df) +
  geom_circle(aes(x0 = x, y0 = y, r = I(plot.diam / 2)),
              colour = NA, fill = "blue", alpha = 0.2) +
  geom_point(aes(x = x, y = y),
             colour = "white", size = I(1)) +
  coord_fixed() +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 15)) +
  xlab("X Cartesian coordinate") +
  ylab("Y Cartesian coordinate") +
  ggtitle(paste("(b) VR complex for d =",
                plot.diam))
ggsave("Fig1b.eps", width = 4.5, height = 3.5)

# plot conventional persistence diagram for circle2d
plot_persist(phom.circ) +
  geom_point(data = temp.phom, aes(x = birth,
                                   y = death,
                                   colour = dimension,
                                   shape = dimension),
             size = I(2.5)) +
  xlab("Feature appearance") +
  ylab("Feature disappearance") +
  ggtitle("(c) Diagonal persistence diagram") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 15))
ggsave("Fig1c.eps", width = 4.5, height = 3.5)

# plot flat persistence diagram for circle2d (unused currently)
plot_persist(phom.circ, flat = TRUE) +
  geom_point(data = temp.phom, aes(x = birth,
                                   y = pers,
                                   colour = dimension,
                                   shape = dimension),
             size = I(2.5)) +
  xlab("Feature appearance") +
  ylab("Feature persistence") +
  ggtitle("Flat persistence diagram") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 15))
ggsave("example-flat.eps", width = 4.5, height = 3.5)

# plot persistence barcode for circle2d; add black separators for JACT
phom.circ <- rbind(phom.circ[phom.circ[, "dimension"] == 0, ],
                   c(0, 0, 0), c(0, 0, 0), c(0, 0, 0),
                   phom.circ[phom.circ[, "dimension"] == 1, ])
num_0cycles <- sum(phom.circ[, "dimension"] == 0)
plot_barcode(phom.circ) +
  geom_abline(slope = 0, intercept = num_0cycles - 1,
              size = 0.25) +
  ggtitle("(d) Persistence barcode") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 15))
ggsave("Fig1d.eps", width = 4.5, height = 3.5)

#####OMITTED FIGURE#####
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
  ggtitle("(a) Diagonalpersistence diagram") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 15))
ggsave("example-bias1.eps", width = 4.5, height = 3.5)
plot_persist(phom.bias, flat = TRUE) +
  geom_point(data = temp.phom, aes(x = birth,
                                   y = pers,
                                   colour = dimension,
                                   shape = dimension),
             size = I(2.5)) +
  xlim(c(0, 1)) +
  ylim(c(0, 0.6)) +
  xlab("Feature appearance") +
  ggtitle("(b) Flat persistence diagram") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 15))
ggsave("example-bias2.eps", width = 4.5, height = 3.5)

#####FIGURE 2#####
# make sphere example to highlight benefits
data("sphere3d")
phom.sphere <- calculate_homology(sphere3d, dim = 2)
temp.phom <- as.data.frame(phom.sphere)
temp.phom$pers <- temp.phom$death - temp.phom$birth
temp.phom$dimension <- as.factor(temp.phom$dimension)

# add separators for JACT submission
phom.sphere <- rbind(phom.sphere[phom.sphere[, "dimension"] == 0, ],
                     c(0, 0, 0), c(0, 0, 0), c(0, 0, 0),
                     phom.sphere[phom.sphere[, "dimension"] == 1, ],
                     c(1, 0, 0), c(1, 0, 0), c(1, 0, 0),
                     phom.sphere[phom.sphere[, "dimension"] == 2, ])
num_0cycles <- sum(phom.sphere[, "dimension"] == 0)
num_1cycles <- sum(phom.sphere[, "dimension"] == 1)

# plot sphere barcode
plot_barcode(phom.sphere) +
  ggtitle("(a) Persistence barcode") +
  geom_abline(slope = 0,
              intercept = num_0cycles - 1,
              size = 0.25) +
  geom_abline(slope = 0,
              intercept = num_0cycles + num_1cycles - 1,
              size = 0.25) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 15))
ggsave("Fig2a.eps", width = 4.5, height = 3.5)

plot_persist(phom.sphere) +
  geom_point(data = temp.phom, aes(x = birth,
                                   y = death,
                                   colour = dimension,
                                   shape = dimension),
             size = I(2.5)) +
  xlab("Feature appearance") +
  ylab("Feature disappearance") +
  ggtitle("(b) Diagonal persistence diagram") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 15))
ggsave("Fig2b.eps", width = 4.5, height = 3.5)

plot_persist(phom.sphere, flat = TRUE) +
  geom_point(data = temp.phom, aes(x = birth,
                                   y = pers,
                                   colour = dimension,
                                   shape = dimension),
             size = I(2.5)) +
  xlab("Feature appearance") +
  ggtitle("(c) Flat persistence diagram") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 15))
ggsave("Fig2c.eps", width = 4.5, height = 3.5)
