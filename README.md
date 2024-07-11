# Making a sunburst plot using the sunburstR package
### This outputs an interactive html file, hover over each section to see taxonomic names. 
### Colors are challenging since you must input a vector of colors in exactly the right order to color by phylum like this example.

```{r pressure, echo=FALSE}
sunburst(data = data.frame(xtabs(V2~Path, table)), colors = colors, legend = FALSE)
```
