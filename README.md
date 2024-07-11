# Making a sunburst plot using the sunburstR package

### Load and install required packages

```{r}
#load packages
library(tidyverse)
#install.packages("sunburstR")
library(sunburstR)
```

### Load the data set, this code will pull the example data directly from github

This block of code also performs several manipulations on the table: 
1) filter to only include "winning" MAGs from dRep (redundant/lower quality MAGs will not be included)
2) replace any "-" in taxonomic names with "_"
3) replace any blank annotations with "Unclassified"
4) create a "Path" column with all taxonomic levels
5) add a column "V2" where all values are equal to 1 (used to calculate the abundance of each taxonomic level/annotation)
6) sort by "Path"

```{r}
#load data
table <- read.csv(url("https://raw.githubusercontent.com/lgschaer/MetaG_tutorials/main/summary_table.csv")) %>%
  filter(dRep == "winner") %>%
  mutate_at(vars(Domain, Phylum, Class, Order, Family, Genus, Species), funs(gsub("-", "_", .))) %>%
  mutate(
    Species = ifelse(is.na(Species), "Unclassified", Species),
    Genus = ifelse(is.na(Genus), "Unclassified", Genus),
    Path = paste(Domain, Phylum, Class, Order, Family, Genus, Species, sep = "-"),
    V2 = 1
  ) %>%
  select(Domain, Phylum, Class, Order, Family, Genus, Species, Path, V2) %>%
  arrange(Path)
head(table)
```

### Colors are challenging with this package since you must input a vector of colors in exactly the right order to color by phylum like this example.

This color vector is exactly right for this data. The first color does not show up, so we begin with white which is not shown on the plot.
Another option is to supply a shorter vector of colors which will automatically be repeated as many times as it is needed to fill the plot. 

```{r}
colors <- c(#domain
            "white",   "#7F7F7F", "#383838", 
            #phylum
            "#00EEEE", "#1CC9CB", "#39A5A9", "#538686", "#5B8761", "#63893D", "#71901F", "#94A713", "#B7BE07",
            "#CDC300", "#CDAA00", "#CD9100", "#C87305", "#BE5111", "#B52E1D", "#A9202F", "#9B1E46", "#8E1C5C",
            "#7C1B6C", "#6A1A7A", "#571A89", "#40298B", "#273B8B", "#104E8B",
            #class
            "#00EEEE", "#1CC9CB", "#39A5A9", "#39A5A9", "#39A5A9", "#39A5A9", "#538686", "#538686", "#538686",
            "#538686", "#5B8761", "#63893D", "#71901F", "#94A713", "#94A713", "#B7BE07", "#CDC300", "#CDAA00",
            "#CDAA00", "#CDAA00", "#CD9100", "#C87305", "#BE5111", "#B52E1D", "#B52E1D", "#A9202F", "#8E1C5C",
            "#7C1B6C", "#7C1B6C", "#7C1B6C", "#6A1A7A", "#6A1A7A", "#6A1A7A", "#571A89", "#40298B", "#40298B",
            "#273B8B", "#104E8B", "#104E8B",
            #order
            "#00EEEE", "#1CC9CB", "#39A5A9", "#39A5A9", "#39A5A9", "#39A5A9", "#39A5A9", "#39A5A9", "#538686",
            "#538686", "#538686", "#538686", "#538686", "#538686", "#538686", "#538686", "#538686", "#538686",
            "#538686", "#538686", "#538686", "#538686", "#5B8761", "#5B8761", "#5B8761", "#63893D", "#63893D",
            "#63893D", "#71901F", "#94A713", "#B7BE07", "#CDC300", "#CDAA00", "#CDAA00", "#CDAA00", "#CD9100",
            "#C87305", "#B52E1D", "#B52E1D", "#A9202F", "#9B1E46", "#8E1C5C", "#7C1B6C", "#7C1B6C", "#7C1B6C",
            "#6A1A7A", "#6A1A7A", "#6A1A7A", "#571A89", "#571A89", "#40298B", "#40298B", "#40298B", "#40298B",
            "#40298B", "#40298B", "#40298B", "#40298B", "#273B8B", "#273B8B", "#273B8B", "#104E8B", "#104E8B",
            "#104E8B", 
            #family
            "#00EEEE", "#1CC9CB", "#39A5A9", "#39A5A9", "#39A5A9", "#39A5A9" ,"#39A5A9", "#538686", "#538686", 
            "#538686", "#538686", "#538686", "#538686", "#538686", "#538686", "#538686", "#538686", "#538686",
            "#538686", "#538686", "#538686", "#538686", "#538686", "#538686", "#538686" ,"#5B8761" ,"#5B8761",
            "#5B8761", "#63893D", "#71901F" ,"#94A713" ,"#94A713" ,"#94A713", "#94A713", "#94A713", "#94A713",
            "#B7BE07", "#CDC300", "#CDAA00", "#CDAA00", "#CDAA00" ,"#CD9100" ,"#C87305", "#B52E1D", "#B52E1D",
            "#B52E1D", "#8E1C5C", "#7C1B6C", "#7C1B6C", "#7C1B6C", "#6A1A7A", "#6A1A7A" ,"#6A1A7A", "#571A89",
            "#40298B", "#40298B", "#40298B", "#40298B", "#40298B", "#40298B", "#40298B", "#40298B", "#40298B",
            "#40298B", "#40298B", "#40298B", "#40298B", "#273B8B", "#273B8B", "#104E8B", "#104E8B", "#104E8B",
            #genus
            "white",   "#1CC9CB", "#39A5A9", "#39A5A9", "#39A5A9", "#39A5A9", "#39A5A9", "#39A5A9", "#39A5A9",
            "#39A5A9", "#39A5A9", "#39A5A9", "#538686", "#538686", "#538686", "#538686", "#538686", "#538686",
            "#538686", "#538686", "#538686", "#538686", "#538686", "#538686", "#538686", "#538686", "#538686",
            "#538686", "#538686", "#538686" ,"#538686" ,"#538686", "#538686", "#538686", "#538686", "#538686",
            "#538686", "#538686", "#538686", "#538686", "#538686", "#5B8761", "#5B8761", "#5B8761", "#5B8761",
            "#5B8761", "#63893D", "#63893D", "#63893D", "#63893D", "#94A713", "#94A713", "#94A713", "#94A713",
            "#94A713", "#94A713", "#94A713", "#94A713", "#94A713", "#B7BE07", "#CDC300", "#CDAA00", "#CDAA00",
            "#CDAA00" ,"#CDAA00", "#CDAA00", "#C87305", "#C87305", "#A9202F", "#A9202F", "#7C1B6C", "#7C1B6C",
            "#7C1B6C", "#7C1B6C", "#6A1A7A", "#6A1A7A", "#571A89", "#40298B", "#40298B", "#40298B", "#40298B",
            "#40298B", "#40298B", "#40298B", "#40298B", "#40298B", "#40298B", "#40298B", "#40298B", "#40298B", 
            "#40298B", "#40298B", "#40298B", "#40298B", "#40298B", "#40298B", "#40298B", "#273B8B", "#273B8B",
            "#104E8B", "#104E8B", "#104E8B", "#104E8B", "#104E8B", 
            #species
            "#39A5A9", "#39A5A9", "#538686", "#538686", "#63893D", "#94A713", "#94A713" ,"#94A713", "#94A713",
            "#94A713", "#94A713", "#CDAA00", "#CD9100", "#A9202F", "#7C1B6C", "#273B8B", "#104E8B")

```

### This outputs an interactive html file, hover over each section to see taxonomic names. 

```{r pressure, echo=FALSE}
sunburst(data = data.frame(xtabs(V2~Path, table)), colors = colors, legend = FALSE)
```

See "example.html" (https://github.com/lgschaer/MetaG_tutorials/blob/main/example.html)

![image](https://github.com/lgschaer/MetaG_tutorials/assets/47119257/4cd7eae5-3bfc-42d5-ae8a-458be061271b)


