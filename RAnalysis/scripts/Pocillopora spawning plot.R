# Code to produce plot showing spawning patterns by time and species.
# Written by Scott Burgess <sburgess@bio.fsu.edu>
# Jan 2024

# Required packages
library('dplyr')
library('ggplot2')

# set the colors for plotting
cols <- cbind.data.frame(Species = factor(c("P. tuahiniensis",
                                            "P. meandrina",
                                            "P. verrucosa",
                                            #"P. grandis",
                                            "P. cf. effusa"
                                            #,"P. acuta"
                                            )),
                         Color = c("#D55E00",
                                   "#0072B2",
                                   "#E69F00",
                                  # "#56B4E9",
                                   "#009E73"
                                  #,"#e63946"
                                  ))

# Read in the data (check working directory)
dat <- read.csv("data/Pocillopora_spawning_IDs.csv")

# Make dat$Species be the same order as col$Species, for easier plotting
dat$Species <- factor(dat$Species,levels=cols$Species)


# Calculate the number and frequency of samples for each species on each day
d <- dat %>% filter(Species!="") %>% 
  group_by(Moon, Year, Month, Date_spawned_YYYYMMDD,Day_before_after_Moon,Species) %>% 
  summarise(n=n()) %>% 
  mutate(freq=n/sum(n))

d %>% ggplot(aes(Day_before_after_Moon, freq, fill = Species, label = n)) +
  geom_col()+
  geom_text()+
  scale_fill_manual(values = c("#D55E00","#0072B2",
                                        "#E69F00","#009E73"))+
  facet_wrap(vars(Moon, Year, Month), nrow = 1)+
  theme_bw()+
  ylab("Proportion of colonies sampled each day")+
  xlab("Day after Moon")




# # Prepare plotting parameters
# lwds <- 20
# x <- min(d$Day_before_after_Moon):max(d$Day_before_after_Moon)
# y_vals <- seq(0,1,0.2)
# 
# 
# # Make plot
# # Rows are species, columns are months/moons, x-axis is days around moon
# # TO-DO: Add zeros for the days when spawning was checked, but no coral spawned.
# quartz(width=8,height=6)
# par(mfcol=c(4,3),mar=c(3,5,1,1),oma=c(1,1,1,2))
# 
# # October 2023
# tmp <- d %>% filter(Month=='10')
# plot(c(-1,4),c(0,1),type="n",ylab="",xlab="",xaxt="n",yaxt="n",bty="l")
# with(tmp, segments(Day_before_after_Moon,
#                    c(0,0),
#                    Day_before_after_Moon,
#                    freq,
#                    lend=1,
#                    lwd=lwds,
#                    col=cols$Color[match(tmp$Species,cols$Species)]))
# axis(side=1,at=x,cex.axis=1.5)
# axis(side=2,at=y_vals,las=1,cex.axis=1.5)
# mtext(side=3,adj=0,text="October 2023 - Full moon")
# with(tmp, text(Day_before_after_Moon,
#                rep(0.1,length(Day_before_after_Moon)),
#                n,
#                cex=1.2))
# 
# # Just adding blank plots to fill out the plot 'grid'
# plot(c(-1,4),c(0,1),type="n",ylab="",xlab="",axes=F)
# plot(c(-1,4),c(0,1),type="n",ylab="",xlab="",axes=F)
# plot(c(-1,4),c(0,1),type="n",ylab="",xlab="",axes=F)
# 
# 
# # November 2023 - P. meandrina
# tmp <- d %>% filter(Month=='11',Species=="P. meandrina")
# plot(c(-1,4),c(0,1),type="n",ylab="",xlab="",xaxt="n",yaxt="n",bty="l")
# with(tmp, segments(Day_before_after_Moon,
#                    c(0,0),
#                    Day_before_after_Moon,
#                    freq,
#                    lend=1,
#                    lwd=lwds,
#                    col=cols$Color[match(tmp$Species,cols$Species)]))
# axis(side=1,at=x,cex.axis=1.5)
# axis(side=2,at=y_vals,las=1,cex.axis=1.5)
# mtext(side=3,adj=0,text="November 2023 - New moon")
# with(tmp, text(Day_before_after_Moon,
#                rep(0.1,length(Day_before_after_Moon)),
#                n,
#                cex=1.2))
# 
# # November 2023 - P. verrucosa
# tmp <- d %>% filter(Month=='11',Species=="P. verrucosa")
# plot(c(-1,4),c(0,1),type="n",ylab="",xlab="",xaxt="n",yaxt="n",bty="l")
# with(tmp, segments(Day_before_after_Moon,
#                    c(0,0),
#                    Day_before_after_Moon,
#                    freq,
#                    lend=1,
#                    lwd=lwds,
#                    col=cols$Color[match(tmp$Species,cols$Species)]))
# axis(side=1,at=x,cex.axis=1.5)
# axis(side=2,at=y_vals,las=1,cex.axis=1.5)
# mtext(side=3,adj=0,text="November 2023 - New moon")
# with(tmp, text(Day_before_after_Moon,
#                rep(0.1,length(Day_before_after_Moon)),
#                n,
#                cex=1.2))
# 
# # November 2023 - P. tuahiniensis
# tmp <- d %>% filter(Month=='11',Species=="P. tuahiniensis")
# plot(c(-1,4),c(0,1),type="n",ylab="",xlab="",xaxt="n",yaxt="n",bty="l")
# with(tmp, segments(Day_before_after_Moon,
#                    c(0,0),
#                    Day_before_after_Moon,
#                    freq,
#                    lend=1,
#                    lwd=lwds,
#                    col=cols$Color[match(tmp$Species,cols$Species)]))
# axis(side=1,at=x,cex.axis=1.5)
# axis(side=2,at=y_vals,las=1,cex.axis=1.5)
# mtext(side=3,adj=0,text="November 2023 - New moon")
# with(tmp, text(Day_before_after_Moon,
#                rep(0.1,length(Day_before_after_Moon)),
#                n,
#                cex=1.2))
# 
# # November 2023 - P. cf. effusa
# tmp <- d %>% filter(Month=='11',Species=="P. cf. effusa")
# plot(c(-1,4),c(0,1),type="n",ylab="",xlab="",xaxt="n",yaxt="n",bty="l")
# with(tmp, segments(Day_before_after_Moon,
#                    c(0,0),
#                    Day_before_after_Moon,
#                    freq,
#                    lend=1,
#                    lwd=lwds,
#                    col=cols$Color[match(tmp$Species,cols$Species)]))
# axis(side=1,at=x,cex.axis=1.5)
# axis(side=2,at=y_vals,las=1,cex.axis=1.5)
# mtext(side=3,adj=0,text="November 2023 - New moon")
# with(tmp, text(Day_before_after_Moon,
#                rep(0.1,length(Day_before_after_Moon)),
#                n,
#                cex=1.2))
# 
# # legend in top right 
# plot(c(-1,4),c(0,1),type="n",ylab="",xlab="",axes=F)
# legend('topleft',
#        legend=c("P. meandrina", "P. verrucosa", "P. tuahiniensis", "P. cf. effusa"),
#        pch=15,
#        col=cols$Color[c(2,3,1,5)],
#        bty="n",
#        cex=1.5,
#        pt.cex=3)
# 
# # December 2023 - P. verrucosa
# tmp <- d %>% filter(Month=='12',Species=="P. verrucosa")
# plot(c(-1,4),c(0,1),type="n",ylab="",xlab="",xaxt="n",yaxt="n",bty="l")
# with(tmp, segments(Day_before_after_Moon,
#                    c(0,0),
#                    Day_before_after_Moon,
#                    freq,
#                    lend=1,
#                    lwd=lwds,
#                    col=cols$Color[match(tmp$Species,cols$Species)]))
# axis(side=1,at=x,cex.axis=1.5)
# axis(side=2,at=y_vals,las=1,cex.axis=1.5)
# mtext(side=3,adj=0,text="December 2023 - New moon")
# with(tmp, text(Day_before_after_Moon,
#                rep(0.1,length(Day_before_after_Moon)),
#                n,
#                cex=1.2))
# 
# # December 2023 - P. tuahiniensis
# tmp <- d %>% filter(Month=='12',Species=="P. tuahiniensis")
# plot(c(-1,4),c(0,1),type="n",ylab="",xlab="",xaxt="n",yaxt="n",bty="l")
# with(tmp, segments(Day_before_after_Moon,
#                    c(0,0),
#                    Day_before_after_Moon,
#                    freq,
#                    lend=1,
#                    lwd=lwds,
#                    col=cols$Color[match(tmp$Species,cols$Species)]))
# axis(side=1,at=x,cex.axis=1.5)
# axis(side=2,at=y_vals,las=1,cex.axis=1.5)
# mtext(side=3,adj=0,text="December 2023 - New moon")
# with(tmp, text(Day_before_after_Moon,
#      rep(0.1,length(Day_before_after_Moon)),
#      n,
#      cex=1.2))
# 
# # Just adding blank plots to fill out the plot 'grid'
# plot(c(-1,4),c(0,1),type="n",ylab="",xlab="",axes=F)
# 
# mtext(side=1,text="Days before/after moon",outer=T,cex=1.2)
# mtext(side=2,text="Proportion of colonies sampled each day",outer=T,cex=1.2,line=-1)
# 
# 
# 
