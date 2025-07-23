library(tidyverse)
samples <- read_tsv("data/sample_lists/samples.txt", 
                           col_names = "NMFS_DNA_ID")

metadata <- read_csv("data/herring_metadata.csv") %>%
  inner_join(., 
             samples,
             by = c("NMFS_DNA_ID"))
bam_paths <- read_tsv("data/bam.pathlist", 
                      col_names = "path")

bamlist <- bam_paths %>%
  extract(path, 
          into = "sample", 
          regex = "data/(s.+)\\.bam", 
          convert = TRUE, 
          remove = FALSE) %>%
  inner_join(.,
             metadata, 
             by = c("sample")) %>%
  mutate(group = abbreviate(WATER_NAME, minlength = 3, named = FALSE)) %>%
  select(sample, path, group)
write_tsv(bamlist, "data/bams.tsv")

