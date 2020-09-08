library(tidyverse)

library(brms)
library(glue)
library(lme4)
library(modelr)

data("sleepstudy")
head(sleepstudy)

# Linear regression: 
sleepstudy %>% 
    ggplot(aes(y=Reaction, x=Days))+
    geom_point(alpha=.25, show.legend = FALSE, size=2)+
    geom_smooth(method = "lm", alpha=.5, se=FALSE, show.legend = FALSE)+
    theme_bw()

# MLM regression
sleepstudy %>% 
    ggplot(aes(y=Reaction, x=Days, group=Subject, color=Subject))+
    geom_point(alpha=.25, show.legend = FALSE, size=2)+
    geom_smooth(method = "lm", alpha=.5, se=FALSE, show.legend = FALSE)+
    theme_bw()


# lme4 implementation -------------------------------------------------------------------------------------------------

# Random intercepts only model...
yvar <- "Reaction"
xvar <- "Days"
r_eff <- "(1|Subject)"

form_base <- glue("{yvar} ~ 1 + {xvar} + {r_eff}") %>% as.character()
print(form_base)

fit_base <- lmer(form_base, sleepstudy)
summary(fit_base)

# Random intercepts and random slopes model... 
r_eff <- "(1 + Days|Subject)"

form_r_slps <- glue("{yvar} ~ 1 + {xvar} + {r_eff}") %>% as.character()
print(form_r_slps)

fit_r_slps <- lmer(form_r_slps, sleepstudy)
summary(fit_r_slps)

anova(fit_base, fit_r_slps)

# The brms way --------------------------------------------------------------------------------------------------------
# Random intercepts only model... 
brms_fit_base <- brm(form_base, data = sleepstudy, cores = 2, chains = 2, iter = 12500, warmup = 10000, 
                     save_all_pars = TRUE)
plot(brms_fit_base)
summary(brms_fit_base)

# Random Intercepts and slopes... 
brms_fit_r_slps <- brm(form_r_slps, data = sleepstudy, cores = 2, chains = 2, iter = 12500, warmup = 10000, 
                       save_all_pars = TRUE)
plot(brms_fit_r_slps, N = 6)
summary(brms_fit_r_slps)

loo_comp<- brms::loo(brms_fit_base, brms_fit_r_slps, cores=4, moment_match = TRUE)