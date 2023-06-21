require(foreign)
require(ggplot2)
require(MASS)

dat <- read.dta("https://stats.idre.ucla.edu/stat/stata/dae/nb_data.dta")
dat <- within(dat, {
  prog <- factor(prog, levels = 1:3, labels = c("General", "Academic", "Vocational"))
  id <- factor(id)
})

summary(dat)

ggplot(dat, aes(daysabs, fill = prog)) + geom_histogram(binwidth = 1) + facet_grid(prog ~ 
                                                                                     ., margins = TRUE, scales = "free")

with(dat, tapply(daysabs, prog, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

table(dat$prog) 
library("tidyverse")
dat<-dat %>% filter(grepl("General|Academic",prog))
summary(m1 <- glm.nb(daysabs ~ prog, data = dat))
t.test(daysabs ~ prog,dat)
