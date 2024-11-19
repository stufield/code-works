# repeated measures ANOVA done with partitioning 
# the variance in the error term
aov(resp ~ treatment + Error(subject/treatment), data = data)
lme(resp ~ treatment, random = ~1 | subject, method = "ML", data = data)

# or a non-parametric equivalent Friedman test
friedman.test(resp ~ treatment | subject, data = data)

