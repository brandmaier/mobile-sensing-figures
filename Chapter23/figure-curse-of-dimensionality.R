if (!exists("mytheme")) {
  mytheme <- geom_blank()
}

dimensions.range <- 1:10
size.range <- seq(0, 1, .001)

dat <- expand.grid(Dimensions = dimensions.range, size.range)
dat$neighborhood <- dat$Var2^dat$Dimensions
dat$Dimensions <- factor(dat$Dimensions)
pl <- ggplot2::ggplot(dat,
                      aes(
                        x = Var2,
                        y = neighborhood,
                        group = Dimensions,
                        col = Dimensions
                      ))+geom_line()+
  xlab("Size of Neighborhood")+
  ylab("Coverage of Feature Space")+
  theme(legend.position = c(.2, .7))
