
# ---- *** VIZ : Tile Grid *** ----

# ---- DATA ----
load("./DATA/data_sriq.Rdata")


# ---- Color palette ----

colors.QU = c(QU1="darkgreen",
              QU2="darkseagreen",
              QU3="lightslategray",
              QU4="lightpink",
              QU5="firebrick",
              TOTAL="royalblue4")

colors.signQU = c(rep("darkorange",
                      length(colors.QU)),
                  rep("#273749",
                      length(colors.QU)))
names(colors.signQU) = interaction(names(colors.QU),
                                   sign(c(-1,1)))

# scales::show_col(colors.QU)

# ---- VIZ : BASIC ----

basic.viz = data.sriq %>%
  ggplot(aes(x=COUNTRY,
             y=val)) +
  geom_bar(aes(fill=quantile),
           stat="identity",
           position="dodge") +
  theme(axis.text.x=element_markdown(size=9),
        axis.text.y=element_markdown(size=6)) +
  scale_fill_manual(breaks=names(colors.QU),
                    values=colors.QU) +
  labs(title="Savings rate (%) by Income Quintile group (QU)",
       x=NULL,
       y=NULL)

# ---- OUTPUT ----

# VIZ (1) ----

ggsave("./OUTPUT/VIZ/TileGrid 1.png",
       plot=basic.viz,
       scale=2,
       width=17,
       height=6,
       units="cm")


# VIZ (2) ----

ggsave("./OUTPUT/VIZ/TileGrid 2.png",
       plot=basic.viz +
         facet_grid(rows=vars(quantile)) ,
       scale=2,
       width=17,
       height=11,
       units="cm")


# VIZ (3) ----

ggsave("./OUTPUT/VIZ/TileGrid 3.png",
       plot=bind_rows(map(unique(data.sriq$quantile),
                          function(q) {
                            data.sriq %>%
                              mutate(QU=q)
                          })) %>%
         left_join(data.sriq %>%
                     filter(quantile=="TOTAL") %>%
                     select(COUNTRY,val) %>%
                     rename(TOTAL=val)) %>%
         mutate(val.q=ifelse(QU==quantile &
                               QU !="TOTAL",val,NA),
                val=ifelse(quantile=="TOTAL",NA,
                           ifelse(QU=="TOTAL",TOTAL,val))) %>%
         ggplot(aes(x=COUNTRY)) +
         geom_bar(aes(group=quantile,
                      y=val),
                  fill="lightgray",
                  stat="identity",
                  position="dodge") +
         geom_bar(aes(y=val.q,
                      fill=quantile),
                  stat="identity",
                  position="dodge") +
         facet_grid(rows=vars(QU),
                    scales="free_y") +
         theme(axis.text.x=element_markdown(size=9),
               axis.text.y=element_markdown(size=6)) +
         scale_fill_manual(breaks=names(colors.QU),
                           values=rep("#273749",
                                      length(colors.QU)),
                           guide=FALSE) +
         labs(title="Savings rate (%) by Income Quintile group (QU)",
              x=NULL,
              y=NULL),
       scale=2,
       width=17,
       height=11,
       units="cm")


# VIZ (4) ----

ggsave("./OUTPUT/VIZ/TileGrid 4.png",
       plot=data.sriq %>%
         ggplot() +
         geom_errorbar(aes(x=0,
                           y=0,
                           xmin=-abs(val)/2,
                           xmax=abs(val)/2,
                           ymin=-abs(val)/2,
                           ymax=abs(val)/2,
                           color=factor(sign(val)))) +
         geom_errorbarh(aes(x=0,
                           y=0,
                           xmin=-abs(val)/2,
                           xmax=abs(val)/2,
                           ymin=-abs(val)/2,
                           ymax=abs(val)/2,
                           color=factor(sign(val)))) +
         facet_grid(cols=vars(COUNTRY),
                    rows=vars(quantile)) +
         scale_color_manual(breaks=c("-1","1"),
                           values=c("darkorange","#273749"),
                           labels=c("[-]","[+]")) +
         ggtitle("Savings rate (%) by Income Quintile group (QU)") +
         theme(axis.line.x.bottom=element_blank(),
               axis.line.y.left=element_blank(),
               panel.border=element_blank(),
               plot.background=element_rect(color=NA)) +
         coord_fixed(),
       scale=2,
       width=17,
       height=4.9,
       units="cm")

# VIZ (5) ----

ggsave("./OUTPUT/VIZ/TileGrid 5.png",
       plot=data.sriq %>%
         ggplot() +
         geom_rect(aes(xmin=-abs(val)/2,
                       xmax=abs(val)/2,
                       ymin=-abs(val)/2,
                       ymax=abs(val)/2,
                       fill=factor(sign(val)))) +
         facet_grid(cols=vars(COUNTRY),
                    rows=vars(quantile)) +
         scale_fill_manual(breaks=c("-1","1"),
                           values=c("darkorange","#273749"),
                           labels=c("[-]","[+]")) +
         ggtitle("Savings rate (%) by Income Quintile group (QU)") +
         theme(axis.line.x.bottom=element_blank(),
               axis.line.y.left=element_blank(),
               panel.border=element_blank(),
               plot.background=element_rect(color=NA)) +
         coord_fixed(),
       scale=2,
       width=17,
       height=4.9,
       units="cm")
