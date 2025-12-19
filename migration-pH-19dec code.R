#19 December 2025
#This is for the statistical analysis of gap closure. 

require(tidyverse)
require(ggpattern)
require(extrafont)
require(showtext)

theme_test <- function(){
  font <- "sans-serif"
  theme_minimal() %+replace%
  theme(
      panel.grid.major = element_line(colour='#9e9e9e'),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = '#f2f2f2'),
      axis.line = element_line(color = '#000000', linewidth = 1.0),
      axis.ticks = element_line(color = '#000000', linewidth =1.0),
      plot.title = element_text(
          family = font,
          size = 36,
          face = 'bold',
      ),
      axis.title = element_text (
        family = font,
        size = 18,
        face = 'bold',
      ),
      axis.text = element_text(
            family = font,
            size = 18,
        ),
      legend.title = element_text(
        family = font,
        size = 18,
      ),
      legend.text = element_text(
        family = font,
        size = 18,
      ),
  )
}

cariporide.gap.data <- read.csv('C:/Users/seeso/Downloads/woodn-gap-cariporide-5nov (1).csv')
cariporide.gap.data

cariporide.gap.data.ph7 <- subset(cariporide.gap.data,
                                  cariporide.gap.data$ph == '7.3-7.4')
cariporide.gap.data.ph69 <- subset(cariporide.gap.data,
                                   cariporide.gap.data$ph == '6.9-7.2')
cariporide.gap.data.ph65 <- subset(cariporide.gap.data,
                                   cariporide.gap.data$ph == '6.5-6.8')

cariporide.gap.data.ph7.vehicle <- subset(cariporide.gap.data.ph7,
                                          cariporide.gap.data.ph7$treatment == 'vehicle')
cariporide.gap.data.ph7.cariporide <- subset(cariporide.gap.data.ph7,
                                             cariporide.gap.data.ph7$treatment == 'cariporide')

cariporide.gap.data.ph69.vehicle <- subset(cariporide.gap.data.ph69,
                                           cariporide.gap.data.ph69$treatment == 'vehicle')
cariporide.gap.data.ph69.cariporide <- subset(cariporide.gap.data.ph69,
                                               cariporide.gap.data.ph69$treatment == 'cariporide')

cariporide.gap.data.ph65.vehicle <- subset(cariporide.gap.data.ph65,
                                           cariporide.gap.data.ph65$treatment == 'vehicle')
cariporide.gap.data.ph65.cariporide <- subset(cariporide.gap.data.ph65,
                                              cariporide.gap.data.ph65$treatment == 'cariporide')

cariporide.gap.data.ph7.cariporide.4hr <- subset(cariporide.gap.data.ph7.cariporide,
                                                 cariporide.gap.data.ph7.cariporide$time == 4)
cariporide.gap.data.ph7.vehicle.4hr <- subset(cariporide.gap.data.ph7.vehicle,
                                              cariporide.gap.data.ph7.vehicle$time == 4)
shapiro.test(cariporide.gap.data.ph7.vehicle.4hr$adjust)
shapiro.test(cariporide.gap.data.ph7.cariporide.4hr$adjust)
t.test(cariporide.gap.data.ph7.vehicle.4hr$adjust,
       cariporide.gap.data.ph7.cariporide.4hr$adjust)

cariporide.gap.data.ph7.cariporide.12hr <- subset(cariporide.gap.data.ph7.cariporide,
                                                  cariporide.gap.data.ph7.cariporide$time == 12)
cariporide.gap.data.ph7.vehicle.12hr <- subset(cariporide.gap.data.ph7.vehicle,
                                               cariporide.gap.data.ph7.vehicle$time == 12)
shapiro.test(cariporide.gap.data.ph7.vehicle.12hr$adjust)
shapiro.test(cariporide.gap.data.ph7.cariporide.12hr$adjust)
t.test(cariporide.gap.data.ph7.cariporide.12hr$adjust,
       cariporide.gap.data.ph7.vehicle.12hr$adjust)

cariporide.gap.data.ph7.cariporide.24hr <- subset(cariporide.gap.data.ph7.cariporide,
                                                  cariporide.gap.data.ph7.cariporide$time == 24)
cariporide.gap.data.ph7.vehicle.24hr <- subset(cariporide.gap.data.ph7.vehicle,
                                               cariporide.gap.data.ph7.vehicle$time == 24)
shapiro.test(cariporide.gap.data.ph7.vehicle.24hr$adjust)
shapiro.test(cariporide.gap.data.ph7.cariporide.24hr$adjust)
t.test(cariporide.gap.data.ph7.cariporide.24hr$adjust,
       cariporide.gap.data.ph7.vehicle.24hr$adjust)

cariporide.gap.data.ph69.cariporide.4hr <- subset(cariporide.gap.data.ph69.cariporide,
                                                  cariporide.gap.data.ph69.cariporide$time == 4)
cariporide.gap.data.ph69.vehicle.4hr <- subset(cariporide.gap.data.ph69.vehicle,
                                               cariporide.gap.data.ph69.vehicle$time == 4)
shapiro.test(cariporide.gap.data.ph69.vehicle.4hr$adjust)
shapiro.test(cariporide.gap.data.ph69.cariporide.4hr$adjust)
t.test(cariporide.gap.data.ph69.vehicle.4hr$adjust,
       cariporide.gap.data.ph69.cariporide.4hr$adjust)

cariporide.gap.data.ph69.cariporide.12hr <- subset(cariporide.gap.data.ph69.cariporide,
                                                   cariporide.gap.data.ph69.cariporide$time == 12)
cariporide.gap.data.ph69.vehicle.12hr <- subset(cariporide.gap.data.ph69.vehicle,
                                                cariporide.gap.data.ph69.vehicle$time == 12)
shapiro.test(cariporide.gap.data.ph69.vehicle.12hr$adjust)
shapiro.test(cariporide.gap.data.ph69.cariporide.12hr$adjust)
t.test(cariporide.gap.data.ph69.cariporide.12hr$adjust,
       cariporide.gap.data.ph69.vehicle.12hr$adjust)

cariporide.gap.data.ph69.cariporide.24hr <- subset(cariporide.gap.data.ph69.cariporide,
                                                   cariporide.gap.data.ph69.cariporide$time == 24)
cariporide.gap.data.ph69.vehicle.24hr <- subset(cariporide.gap.data.ph69.vehicle,
                                                cariporide.gap.data.ph69.vehicle$time == 24)
shapiro.test(cariporide.gap.data.ph69.vehicle.24hr$adjust)
shapiro.test(cariporide.gap.data.ph69.cariporide.24hr$adjust)
t.test(cariporide.gap.data.ph69.cariporide.24hr$adjust,
       cariporide.gap.data.ph69.vehicle.24hr$adjust)

cariporide.gap.data.ph65.cariporide.4hr <- subset(cariporide.gap.data.ph65.cariporide,
                                                  cariporide.gap.data.ph65.cariporide$time == 4)
cariporide.gap.data.ph65.vehicle.4hr <- subset(cariporide.gap.data.ph65.vehicle,
                                               cariporide.gap.data.ph65.vehicle$time == 4)
shapiro.test(cariporide.gap.data.ph65.vehicle.4hr$adjust)
shapiro.test(cariporide.gap.data.ph65.cariporide.4hr$adjust)

t.test(cariporide.gap.data.ph65.vehicle.4hr$adjust,
       cariporide.gap.data.ph65.cariporide.4hr$adjust)

cariporide.gap.data.ph65.cariporide.12hr <- subset(cariporide.gap.data.ph65.cariporide,
                                                   cariporide.gap.data.ph65.cariporide$time == 12)
cariporide.gap.data.ph65.vehicle.12hr <- subset(cariporide.gap.data.ph65.vehicle,
                                                cariporide.gap.data.ph65.vehicle$time == 12)
shapiro.test(cariporide.gap.data.ph65.vehicle.12hr$adjust)
shapiro.test(cariporide.gap.data.ph65.cariporide.12hr$adjust)
t.test(cariporide.gap.data.ph65.cariporide.12hr$adjust,
       cariporide.gap.data.ph65.vehicle.12hr$adjust)

cariporide.gap.data.ph65.cariporide.24hr <- subset(cariporide.gap.data.ph65.cariporide,
                                                   cariporide.gap.data.ph65.cariporide$time == 24)
cariporide.gap.data.ph65.vehicle.24hr <- subset(cariporide.gap.data.ph65.vehicle,
                                                cariporide.gap.data.ph65.vehicle$time == 24)
shapiro.test(cariporide.gap.data.ph65.vehicle.24hr$adjust)
shapiro.test(cariporide.gap.data.ph65.cariporide.24hr$adjust)
t.test(cariporide.gap.data.ph65.cariporide.24hr$adjust,
       cariporide.gap.data.ph65.vehicle.24hr$adjust)

#7.3-7.4, 4 hours post exposure

cariporide.gap.data.ph7.4hr <- subset(cariporide.gap.data.ph7,
                                      cariporide.gap.data.ph7$time == 4)
ggplot(data=cariporide.gap.data.ph7.4hr) +
  stat_summary(aes(x=treatment, y=adjust, fill=ph, pattern=treatment),
               fun="mean", geom="col_pattern",
               position="dodge", color="#000000", size=0.8,
               pattern_density=0.5, pattern_spacing=0.05,
               pattern_angle=45, pattern_fill="#e0ecf4", pattern_color="black") +
  stat_summary(aes(x=treatment, y=adjust, fill=treatment), fun.data=mean_sdl,
               position=position_dodge(width=0.9), geom="errorbar",
               width=0.25, fun.args=list(mult=1)) +
  geom_jitter(aes(x=treatment, y=adjust), cex=4, width=0.34, alpha=0.67) +
  geom_hline(yintercept=1) +
  scale_x_discrete(labels=c('Cariporide','Vehicle')) +
  scale_y_continuous(name="Relative Gap Closure", limits=c(0,1.25),
                     breaks=seq(0,1.25,0.25)) +
  scale_fill_manual(values=c('#e0ecf4','#000000','#000000')) +
  scale_pattern_manual(name="Treatment",
                       values=c("cariporide"="circle","vehicle"="none")) +
  guides(pattern="none", fill="none") +
  xlab('pHe 7.3-7.4 \n 4 Hours Post Exposure')+theme_test()

#7.3-7.4, 12 hours post exposure

cariporide.gap.data.ph7.12hr <- subset(cariporide.gap.data.ph7,
                                       cariporide.gap.data.ph7$time == 12)
ggplot(data=cariporide.gap.data.ph7.12hr) +
  stat_summary(aes(x=treatment, y=adjust, fill=ph, pattern=treatment),
               fun="mean", geom="col_pattern",
               position="dodge", color="#000000", size=0.8,
               pattern_density=0.5, pattern_spacing=0.05,
               pattern_angle=45, pattern_fill="#eoecf4", pattern_color="black") +
  stat_summary(aes(x=treatment, y=adjust, fill=treatment), fun.data=mean_sdl,
               position=position_dodge(width=0.9), geom="errorbar",
               width=0.25, fun.args=list(mult=1)) +
  geom_jitter(aes(x=treatment, y=adjust), cex=4, width=0.34, alpha=0.67) +
  geom_hline(yintercept=1) +
  scale_x_discrete(labels=c('Cariporide','Vehicle')) +
  scale_y_continuous(name="Relative Gap Closure", limits=c(0,1.25),
                     breaks=seq(0,1.25,0.25)) +
  scale_fill_manual(values=c('#e0ecf4','#000000','#000000')) +
  scale_pattern_manual(name="Treatment",
                       values=c("cariporide"="circle","vehicle"="none")) +
  guides(pattern="none", fill="none") +
  xlab('pHe 7.3-7.4 \n 12 Hours Post Exposure')+theme_test()

#7.3-7.4, 24 hours post exposure

cariporide.gap.data.ph7.24hr <- subset(cariporide.gap.data.ph7,
                                       cariporide.gap.data.ph7$time == 24)
ggplot(data=cariporide.gap.data.ph7.24hr) +
  stat_summary(aes(x=treatment, y=adjust, fill=ph, pattern=treatment),
               fun="mean", geom="col_pattern",
               position="dodge", color="#000000", size=0.8,
               pattern_density=0.5, pattern_spacing=0.05,
               pattern_angle=45, pattern_fill="#e0ecf4", pattern_color="black") +
  stat_summary(aes(x=treatment, y=adjust, fill=treatment), fun.data=mean_sdl,
               position=position_dodge(width=0.9), geom="errorbar",
               width=0.25, fun.args=list(mult=1)) +
  geom_jitter(aes(x=treatment, y=adjust), cex=4, width=0.34, alpha=0.67) +
  geom_hline(yintercept=1) +
  scale_x_discrete(labels=c('Cariporide','Vehicle')) +
  scale_y_continuous(name="Relative Gap Closure", limits=c(0,1.25),
                     breaks=seq(0,1.25,0.25)) +
  scale_fill_manual(values=c('#e0ecf4','#000000','#000000')) +
  scale_pattern_manual(name="Treatment",
                       values=c("cariporide"="circle","vehicle"="none")) +
  guides(pattern="none", fill="none") +
  xlab('pHe 7.3-7.4 \n 24 Hours Post Exposure')+theme_test()

#6.9-7.2, 4 hours post exposure

cariporide.gap.data.ph69.4hr <- subset(cariporide.gap.data.ph69,
                                       cariporide.gap.data.ph69$time == 4)
ggplot(data=cariporide.gap.data.ph69.4hr) +
  stat_summary(aes(x=treatment, y=adjust, fill=ph, pattern=treatment),
               fun="mean", geom="col_pattern",
               position="dodge", color="#000000", size=0.8,
               pattern_density=0.5, pattern_spacing=0.05,
               pattern_angle=45, pattern_fill="#9ebcdd", pattern_color="black") +
  stat_summary(aes(x=treatment, y=adjust, fill=treatment), fun.data=mean_sdl,
               position=position_dodge(width=0.9), geom="errorbar",
               width=0.25, fun.args=list(mult=1)) +
  geom_jitter(aes(x=treatment, y=adjust), cex=4, width=0.34, alpha=0.67) +
  geom_hline(yintercept=1) +
  scale_x_discrete(labels=c('Cariporide','Vehicle')) +
  scale_y_continuous(name="Relative Gap Closure", limits=c(0,1.25),
                     breaks=seq(0,1.25,0.25)) +
  scale_fill_manual(values=c('#9ebcdd','#000000','#000000')) +
  scale_pattern_manual(name="Treatment",
                       values=c("cariporide"="circle","vehicle"="none")) +
  guides(pattern="none", fill="none") +
  xlab('pHe 6.9-7.2 \n 4 Hours Post Exposure')+theme_test()

#6.9-7.2, 12 hours post exposure

cariporide.gap.data.ph69.12hr <- subset(cariporide.gap.data.ph69,
                                        cariporide.gap.data.ph69$time == 12)
ggplot(data=cariporide.gap.data.ph69.12hr) +
  stat_summary(aes(x=treatment, y=adjust, fill=ph, pattern=treatment),
               fun="mean", geom="col_pattern",
               position="dodge", color="#000000", size=0.8,
               pattern_density=0.5, pattern_spacing=0.05,
               pattern_angle=45, pattern_fill="#9ebcdd", pattern_color="black") +
  stat_summary(aes(x=treatment, y=adjust, fill=treatment), fun.data=mean_sdl,
               position=position_dodge(width=0.9), geom="errorbar",
               width=0.25, fun.args=list(mult=1)) +
  geom_jitter(aes(x=treatment, y=adjust), cex=4, width=0.34, alpha=0.67) +
  geom_hline(yintercept=1) +
  scale_x_discrete(labels=c('Cariporide','Vehicle')) +
  scale_y_continuous(name="Relative Gap Closure", limits=c(0,1.25),
                     breaks=seq(0,1.25,0.25)) +
  scale_fill_manual(values=c('#9ebcdd','#000000','#000000')) +
  scale_pattern_manual(name="Treatment",
                       values=c("cariporide"="circle","vehicle"="none")) +
  guides(pattern="none", fill="none") +
  xlab('pHe 6.9-7.2 \n 12 Hours Post Exposure')+theme_test()

cariporide.gap.data.ph69.24hr <- subset(cariporide.gap.data.ph69,
                                        cariporide.gap.data.ph69$time == 24)
#6.9-7.2, 24 hours post exposure

ggplot(data=cariporide.gap.data.ph69.24hr) +
  stat_summary(aes(x=treatment, y=adjust, fill=ph, pattern=treatment),
               fun="mean", geom="col_pattern",
               position="dodge", color="#000000", size=0.8,
               pattern_density=0.5, pattern_spacing=0.05,
               pattern_angle=45, pattern_fill="#9ebcdd", pattern_color="black") +
  stat_summary(aes(x=treatment, y=adjust, fill=treatment), fun.data=mean_sdl,
               position=position_dodge(width=0.9), geom="errorbar",
               width=0.25, fun.args=list(mult=1)) +
  geom_jitter(aes(x=treatment, y=adjust), cex=4, width=0.34, alpha=0.67) +
  geom_hline(yintercept=1) +
  scale_x_discrete(labels=c('Cariporide','Vehicle')) +
  scale_y_continuous(name="Relative Gap Closure", limits=c(0,1.25),
                     breaks=seq(0,1.25,0.25)) +
  scale_fill_manual(values=c('#9ebcdd','#000000','#000000')) +
  scale_pattern_manual(name="Treatment",
                       values=c("cariporide"="circle","vehicle"="none")) +
  guides(pattern="none", fill="none") +
  xlab('pHe 6.9-7.2 \n 24 Hours Post Exposure')+theme_test()

#6.5-6.8, 4 hours post exposure

cariporide.gap.data.ph65.4hr <- subset(cariporide.gap.data.ph65,
                                       cariporide.gap.data.ph65$time == 4)
ggplot(data=cariporide.gap.data.ph65.4hr) +
  stat_summary(aes(x=treatment, y=adjust, fill=ph, pattern=treatment),
               fun="mean", geom="col_pattern",
               position="dodge", color="#000000", size=0.8,
               pattern_density=0.5, pattern_spacing=0.05,
               pattern_angle=45, pattern_fill="#885697", pattern_color="black") +
  stat_summary(aes(x=treatment, y=adjust, fill=treatment), fun.data=mean_sdl,
               position=position_dodge(width=0.9), geom="errorbar",
               width=0.25, fun.args=list(mult=1)) +
  geom_jitter(aes(x=treatment, y=adjust), cex=4, width=0.34, alpha=0.67) +
  geom_hline(yintercept=1) +
  scale_x_discrete(labels=c('Cariporide','Vehicle')) +
  scale_y_continuous(name="Relative Gap Closure", limits=c(0,1.25),
                     breaks=seq(0,1.25,0.25)) +
  scale_fill_manual(values=c('#885697','#000000','#000000')) +
  scale_pattern_manual(name="Treatment",
                       values=c("cariporide"="circle","vehicle"="none")) +
  guides(pattern="none", fill="none") +
  xlab('pHe 6.5-6.8 \n 4 Hours Post Exposure')+theme_test()

#6.5-6.8, 12 hours post exposure

cariporide.gap.data.ph65.12hr <- subset(cariporide.gap.data.ph65,
                                        cariporide.gap.data.ph65$time == 12)
ggplot(data=cariporide.gap.data.ph65.12hr) +
  stat_summary(aes(x=treatment, y=adjust, fill=ph, pattern=treatment),
               fun="mean", geom="col_pattern",
               position="dodge", color="#000000", size=0.8,
               pattern_density=0.5, pattern_spacing=0.05,
               pattern_angle=45, pattern_fill="#885697", pattern_color="black") +
  stat_summary(aes(x=treatment, y=adjust, fill=treatment), fun.data=mean_sdl,
               position=position_dodge(width=0.9), geom="errorbar",
               width=0.25, fun.args=list(mult=1)) +
  geom_jitter(aes(x=treatment, y=adjust), cex=4, width=0.34, alpha=0.67) +
  geom_hline(yintercept=1) +
  scale_x_discrete(labels=c('Cariporide','Vehicle')) +
  scale_y_continuous(name="Relative Gap Closure", limits=c(0,1.25),
                     breaks=seq(0,1.25,0.25)) +
  scale_fill_manual(values=c('#885697','#000000','#000000')) +
  scale_pattern_manual(name="Treatment",
                       values=c("cariporide"="circle","vehicle"="none")) +
  guides(pattern="none", fill="none") +
  xlab('pHe 6.5-6.8 \n 12 Hours Post Exposure')+theme_test()

#6.5-6.8, 24 hours post exposure

cariporide.gap.data.ph65.24hr <- subset(cariporide.gap.data.ph65,
                                        cariporide.gap.data.ph65$time == 24)
ggplot(data=cariporide.gap.data.ph65.24hr) +
  stat_summary(aes(x=treatment, y=adjust, fill=ph, pattern=treatment),
               fun="mean", geom="col_pattern",
               position="dodge", color="#000000", size=0.8,
               pattern_density=0.5, pattern_spacing=0.05,
               pattern_angle=45, pattern_fill="#885697", pattern_color="black") +
  stat_summary(aes(x=treatment, y=adjust, fill=treatment), fun.data=mean_sdl,
               position=position_dodge(width=0.9), geom="errorbar",
               width=0.25, fun.args=list(mult=1)) +
  geom_jitter(aes(x=treatment, y=adjust), cex=4, width=0.34, alpha=0.67) +
  geom_hline(yintercept=1) +
  scale_x_discrete(labels=c('Cariporide','Vehicle')) +
  scale_y_continuous(name="Relative Gap Closure", limits=c(0,1.25),
                     breaks=seq(0,1.25,0.25)) +
  scale_fill_manual(values=c('#885697','#000000','#000000')) +
  scale_pattern_manual(name="Treatment",
                       values=c("cariporide"="circle","vehicle"="none")) +
  guides(pattern="none", fill="none") +
  xlab('pHe 6.5-6.8 \n 24 Hours Post Exposure')+theme_test()

