### Model Diagnostics

```{r, fig.align='center', include=F}
# check what the underlying count distribution is
Ord_plot(risk$no_of_days_under_beach_action)

# check which variables are significant in an ANOVA test
#anova(m, test="Chisq")

# check which points are more influential
car::influencePlot(m) %>%
  
  
  # check for multicollinearity
  #car::vif(m)
  
  # check if zero-inflated or non-zero-inflated model is better
  AIC(m, m2)

# plot residuals
resids <- simulateResiduals(m, integerResponse=T)
plot(resids)

#outliers
outliers <- outliers(resids)

# test for zero-inflation
testZeroInflation(resids)
```

```{r, include=F}
beach_actions_tier1 <-beach_actions %>%
  filter(beach_id %in% beaches) %>%
  mutate(no_pollutants = case_when(
    action_reasons == "-" ~ NA_integer_,
    TRUE ~ lengths(strsplit(action_reasons, ", "))
  ))

risk3 <- inner_join(risk2, t1stats)

risk3 <- risk3 %>%
  group_by(beach_id)%>%
  slice(which.max(year))

pollution <- beach_actions_tier1 %>%
  group_by(beach_id) %>% 
  arrange(desc(year)) %>% 
  slice(1:5)

pollution <- pollution %>%
  group_by(beach_id)%>%
  summarise(no_pollutants5 = sum(no_pollutants), obs=n()) %>%
  filter(obs>=5) %>%
  inner_join(risk3) %>%
  filter(!is.na(waterbody_name))

ggplot(pollution, aes(x=no_pollutants5)) +
  geom_histogram()
```

```{r, include=F}
lmm <- lmer(no_pollutants5 ~  beach_length_mi + beach_access + beach_owner + swim_season_beach_days  + (1|waterbody_name), data = pollution)
summary(lmm)
```

```{r, include=F}
print(paste0("LMM waterbody name accounts for ", round(0.5871*100/(0.5871+1.66), 2), "% of total variance"))
```

```{r, include=F}
plot(lmm)
qqnorm(resid(lmm))
qqline(resid(lmm))
```
