install.packages("PlayerRatings")
library(PlayerRatings)



Catchers1 <- glm(Baserunning$`Catcher Success (CS)` ~ Baserunning$Catcher + Baserunning$Baserunner, data = Baserunning, family = binomial)
summary(Catchers1)

install.packages("lme4")
library(lme4)
library(arm)
CatcherOLS <- lm(Baserunning$`Catcher Success (CS)` ~ Baserunning$Catcher, data = Baserunning, family = binomial)
CatcherOLS
summary(CatcherOLS)

CatcherML <- glm(Baserunning$`Catcher Success (CS)` ~ Baserunning$Catcher, data = Baserunning, family = binomial)
CatcherML
summary(CatcherML)
AIC(CatcherML)

CatcherML2 <- glm(Baserunning$`Catcher Success (CS)` ~ Baserunning$Catcher + Baserunning$Pitcher, data = Baserunning, family = binomial)
CatcherML2
anova(CatcherML, CatcherML2, test = "F")
