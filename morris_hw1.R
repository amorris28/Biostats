## Andrew Morris
## Homework 1
## Due October 4, 2018

## Initialize packages
library(tidyverse)
library(magrittr)

## Import data
rnaseq <- read_csv('HW_RNAseq.csv')

## Variables & Characteristics 
# SamplID - Categorical
# Population - Categorical, Independent
# Treatment - Categorical, Independent
# Sex - Categorical, Independent
# Gene01:Gene10 - Continuous, Response

## Print expressions
rnaseq

## Plot histogram of expression levels for each gene
rnaseq %>% 
  gather(key = "Gene", value = "Expression", Gene01:Gene10) %>% 
  mutate_at(vars(SampleID, Population, Treatment, Sex, Gene), funs(factor)) %>% 
  ggplot(aes(Expression)) + 
    geom_histogram(bins = 6) +
    facet_wrap(~ Gene, scales = "free") +
    theme_bw()

## Create function for z transformation
ztransform <- function(x) {
  # Converts a 1d vector to z scores
  x_mean = mean(x)
  x_sd = sd(x)
  z = (x - x_mean) / x_sd
  z
}

## Add column of z transformed expression levels for each gene
rnaseq_z <- rnaseq %>%
  mutate_at(vars(Gene01_z = Gene01, Gene02_z = Gene02, Gene03_z = Gene03, 
                 Gene04_z = Gene04, Gene05_z = Gene05, Gene06_z = Gene06,
                 Gene07_z = Gene07, Gene08_z = Gene08, Gene09_z = Gene09,
                 Gene10_z = Gene10), ztransform) %>% 
  mutate_if(is.character, as.factor)

## Print z scores
rnaseq_z

## Histograms of raw and z-score expression levels for Gene01 and 02
rnaseq_z %>% 
  select(Gene01, Gene02, Gene01_z, Gene02_z) %>% 
  gather(key = "Gene", value = "Expression", Gene01:Gene02_z) %>%
  ggplot(aes(Expression)) +
    geom_histogram(bins = 6) +
    facet_wrap(~ Gene, scales = 'free')

## Boxplots
# rnaseq_z %>% 
#   select(SampleID:Sex, starts_with("Gene01"), starts_with("Gene02")) %>% 
#   gather(key = "Gene", value = "Expression", Gene01:Gene02_z) %>%
#   spread(key = "Gene", value = c())
#   gather(key = "Gene", value = "Expression_z", Gene01_z, Gene02_z) %>%
#   ggplot(aes(y = Expression, color = Gene)) +
#   facet_wrap(vars(Population, Treatment, Sex), scales = 'free') + 
#   geom_boxplot()


## Summary statistics
rnaseq %>% 
  group_by(Population) %>% 
  summarize
  