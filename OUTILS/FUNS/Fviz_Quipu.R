
# ---- *** VIZ : QUIPU *** ----

# ---- DATA ----
load("./DATA/data_x.Rdata")


# ---- Color palette ----

colors.X = c(D1="darkgreen",
             D3="darkseagreen",
             MEDIAN="royalblue4",
             D7="lightpink",
             D9="firebrick")

#scales::show_col(colors.X)

# ---- Variable labels ----
var.lab = c(val="Absolute Value",
            val.m="Absolute difference vs. MEDIAN",
            val.M="Income distribution expansion/contraction",
            val.c=paste0("Absolute change vs. ",
                         min.year),
            val.pc=paste0("Percentage change vs. ",
                         min.year))

# ---- FUN : VIZ ----

F.viz.1 = function (.country=NULL,
                    .var) {
  
  data.viz = data.x %>%
    left_join(data.x %>%
                select(COUNTRY,
                       YEAR,
                       X,
                       !!sym(.var)) %>%
                pivot_wider(names_from="X",
                            values_from=!!sym(.var)) %>%
                select(-MEDIAN),
              by=c("COUNTRY",
                   "YEAR")) %>%
    filter(YEAR>=min.year,
           {if (!is.null(.country))
             COUNTRY==.country
             else TRUE})
  
  data.viz %>%
    ggplot(aes(x=substr(YEAR,3,4),
               y=!!sym(.var),
               group=X)) +
    {if (str_detect(.var,".") &
         !is.null(.country))
      geom_hline(yintercept=0,
                 color="lightgrey")} +
    {if (is.null(.country))
      geom_ribbon(aes(ymin=D1,
                      ymax=D9),
                  fill="lightgray")} +
    geom_line(aes(color=X),
              size=1) +
    theme(axis.text.x=element_markdown(size=6),
          axis.text.y=element_markdown(size=6),
          strip.text.y=element_text(angle=0,
                                    hjust=0)) +
    {if (is.null(.country))
      facet_grid(rows=vars(COUNTRY),
                 scales="free_y")} +
    scale_y_continuous(limits=c({if (.var=="val") 0 else NA},
                                NA),
                       labels=function(x) {
                         f.label.color(x,
                                       color.poz="#273749",
                                       color.neg="darkorange")}) +
    scale_color_manual(breaks=names(colors.X),
                       values=colors.X) +
    labs(title=var.lab[.var],
         x="Year (20..)",
         y=NULL,
         caption=.country)
}

# ---- OUTPUT ----

# VIZ (1) ----

ggsave("./OUTPUT/VIZ/Quipu 1.png",
       plot=F.viz.1("BG","val"),
       scale=2,
       width=6,
       height=6,
       units="cm")


# VIZ (2) ----

ggsave("./OUTPUT/VIZ/Quipu 2.png",
       plot=F.viz.1("BG","val.c") + 
         F.viz.1("BG","val.pc") +
         plot_layout(guides="collect") &
         theme(plot.title.position="panel") &
         labs(caption=NULL),
       scale=2,
       width=11,
       height=6,
       units="cm")


# VIZ (3) ----

ggsave("./OUTPUT/VIZ/Quipu 3.png",
       plot=F.viz.1("BG","val.m"),
       scale=2,
       width=6,
       height=6,
       units="cm")


# VIZ (4) ----

ggsave("./OUTPUT/VIZ/Quipu 4.png",
       plot=F.viz.1("BG","val.M") +
         coord_flip() +
         scale_x_discrete(limits=rev),
       scale=2,
       width=6,
       height=6,
       units="cm")


# VIZ (5) ----

ggsave("./OUTPUT/VIZ/Quipu 5.png",
       plot=(F.viz.1(.var="val.M") +
               theme(axis.text.x=element_blank(),
                     axis.text.y=element_blank(),
                     axis.title=element_blank(),
                     plot.title.position="panel") +
               coord_flip() +
               scale_x_discrete(limits=rev)) +
         (F.viz.1(.var="val") +
            theme(axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.title=element_blank(),
                  plot.title.position="panel",
                  strip.text.y=element_blank()) +
            coord_flip() +
            scale_x_discrete(limits=rev)) +
         plot_layout(guides="collect") &
         theme(legend.position='bottom'),
       scale=3,
       width=7,
       height=17,
       units="cm")


