library(mixfishtools)
data("stfFltStkSum")
data(refTable)


stks <- unique(stfFltStkSum$stock)
stkColors <- refTable$col[match(stks, refTable$stock)]
names(stkColors) <- stks #levels(as.factor(stfFltStkSum$stock))
stkColorScale <- scale_colour_manual(name = "stock", values = stkColors,
  aesthetics = c("colour", "fill"))

p <- ggplot(data = subset(stfFltStkSum, scenario == "sq_E")) +
  aes(x = year, y = landings, group = stock, fill = stock) +
  geom_area() +
  facet_wrap(~fleet, scales = "free_y") +
  ggtitle("Landings by fleet") +
  xlab("year") + ylab("Landings") +
  theme(text = element_text(size = 8),
    strip.text = element_text(size = 6),
    axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
    legend.position="bottom") +
  stkColorScale +
  guides(col = guide_legend(nrow = 2))


fname <- file.path("tmp.png")
png(file = fname,  width = 8, height = 7, units="in", res=400)
  print(p)
x <- dev.off()

library(plotly)
ggplotly(p)
