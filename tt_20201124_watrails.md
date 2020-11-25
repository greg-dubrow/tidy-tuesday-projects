    # readin in data, create df for plots
    library(tidytuesdayR) # to load tidytuesday data
    library(tidyverse) # to do tidyverse things
    library(tidylog) # to get a log of what's happening to the data
    library(patchwork) # stitch plots together
    library(gt) # lets make tables
    library(RColorBrewer) # colors!
    library(scales) # format chart output

### First let’s read in the file using the tidytuesdayR package. We’ll also look at the raw data

    #> 
    #>  Downloading file 1 of 1: `hike_data.rds`
    #> Rows: 1,958
    #> Columns: 8
    #> $ name        <chr> "Lake Hills Greenbelt", "Snow Lake", "Skookum Flats", "Te…
    #> $ location    <chr> "Puget Sound and Islands -- Seattle-Tacoma Area", "Snoqua…
    #> $ length      <chr> "2.3 miles, roundtrip", "7.2 miles, roundtrip", "7.8 mile…
    #> $ gain        <chr> "50", "1800", "300", "1585", "500", "500", "425", "450", …
    #> $ highpoint   <chr> "330.0", "4400.0", "2550.0", "2370.0", "1000.0", "2200.0"…
    #> $ rating      <chr> "3.67", "4.16", "3.68", "3.92", "4.14", "3.14", "5.00", "…
    #> $ features    <list> [<"Dogs allowed on leash", "Wildlife", "Good for kids", …
    #> $ description <chr> "Hike through a pastoral area first settled and farmed in…

### There are a few things we want to do with the data for the working dataframe:

-   create columns for miles, direction, type from length
-   create specific location columns frolm location
-   change rating, gain and highpoint to numeric
-   create a rating group
-   separate out features and make the resulting df long. we’ll use
    distinct when we only need 1 obs per trail

<!-- -->

    #> mutate: new variable 'length_miles' (double) with 277 unique values and 0% NA
    #> mutate: converted 'gain' from character to double (0 new NA)
    #>         converted 'highpoint' from character to double (0 new NA)
    #>         converted 'rating' from character to double (0 new NA)
    #> mutate: new variable 'rating_grp' (character) with 6 unique values and 0% NA
    #> mutate: new variable 'trail_type' (character) with 3 unique values and 0% NA
    #> mutate: new variable 'location_split' (character) with 61 unique values and 0% NA
    #> Warning: Expected 2 pieces. Missing pieces filled with `NA` in 38 rows [34, 73,
    #> 271, 306, 537, 559, 599, 652, 672, 708, 718, 749, 799, 800, 835, 836, 889, 1014,
    #> 1033, 1100, ...].
    #> mutate: new variable 'feature_init' (character) with 15 unique values and 0% NA
    #> mutate: new variable 'feature_type' (character) with 2 unique values and 0% NA
    #> select: columns reordered (name, location_region, location_specific, trail_type, length_miles, …)

### To get a sense of what the data look like, I’ll run some historgrams and scatterplots to see how things cluster, if there are outliers or anything else especially noticable.

Using log10 for the length scale to even out the spread.
[Patchwork](https://patchwork.data-imaginist.com/) stitches the plots
together in a neat panel.
<img src="images/unnamed-chunk-3-1.png" width="100%" /> For the
scatterplots, I plotted length by gain, faceting by ratings groups and
then by region. We do have to be careful with ratings, as they are
user-generated and some trails have very few votes. Log10 used again for
length.
<img src="images/unnamed-chunk-4-1.png" width="100%" /><img src="images/unnamed-chunk-4-2.png" width="100%" />

The outliers in terms of gain & length clustered in a few regions, so I
wanted to see which they were. Not a surprise they clustered in the
Cascades & Rainier.

    #> # A tibble: 5 x 4
    #>   location_region    name                                     length_miles  gain
    #>   <chr>              <chr>                                           <dbl> <dbl>
    #> 1 Southwest Washing… Pacific Crest Trail (PCT) Section H - B…         148. 27996
    #> 2 South Cascades     Pacific Crest Trail (PCT) Section I - W…          99  17771
    #> 3 Central Cascades   Pacific Crest Trail (PCT) Section K - S…         117  26351
    #> 4 North Cascades     Pacific Northwest Trail - Pasayten Trav…         119  21071
    #> 5 Mount Rainier Area Wonderland Trail                                  93  22000

### Now that we see how the length, gain, highpoint & ratings spread out, I want build a table to see the averages by region.

I’ve been wanting to take a deeper dive into
[gt](https://gt.rstudio.com/) &
[reactable](https://glin.github.io/reactable/). Since I’m doing this as
a markdown doc & not HTML, I’ll output using gt here. I’ve got some
basic gt calls down, this excercise is all about conditionally
formatting columns based on value. So inspired by [Thomas Mock’s gt
primer](https://themockup.blog/posts/2020-05-16-gt-a-grammer-of-tables),
a basic table with heatmap-like formatting for some columns. See his
explainer for details on the code, and for more features than I’m
including.

    # create by region averages df
    byregion <-  tt_watraildf %>%
      distinct(name, .keep_all = TRUE) %>%
      group_by(location_region) %>%
      summarise(n_region = n(),
                avglength = mean(length_miles),
                avgrating = mean(rating),
                avggain = mean(gain),
                avghigh = mean(highpoint),
                minhigh = min(highpoint),
                maxhigh = max(highpoint)) %>%
      mutate_at(vars(avglength:avgrating), round, 2) %>%
      mutate_at(vars(avggain:avghigh), round, 0) 

    # create table - code commented out so html doesn't show up in md output. 
    # image saved from tt_20201124_watrails.r code
    # byregion %>%
    #   gt() %>%
    #   fmt_number(columns = vars(avggain, avghigh, minhigh, maxhigh), decimals = 0, use_seps = TRUE) %>%
      # sets the columns and palette to format cell color by value range
      # data_color(
      #   columns = vars(avglength, avgrating, avggain, avghigh, minhigh, maxhigh),
      #   colors = scales::col_numeric(
      #     palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
      #     domain = NULL)) %>%
      # tab_style calls add border boxes first to column labels, then body cells
      # tab_style(
      #   style = list(
      #     cell_borders(
      #       sides = "all", color = "grey", weight = px(1))),
      #   locations = list(
      #     cells_column_labels(
      #       columns = gt::everything()
      #       ))) %>%
      # tab_style(
      #   style = list(
      #     cell_borders(
      #       sides = "all", color = "grey", weight = px(1))),
      #   locations = list(
      #     cells_body(
      #       rows = gt::everything()
      #     ))) %>%
      #   cols_align(columns = TRUE, align = "center") %>%
      # cols_align(columns = "location_region", align = "left") %>%
      # tab_header(title = "Regional Averages",
      #            subtitle = md("_North Cascades have longest trails, 
      #                          all mountain areas have lots of gain and highest points_")) %>%
    #  cols_label(location_region = "Region", n_region = "N", avglength = "Avg Length (miles)",
    #            avgrating = "Avg Rating", avggain = "Avg Gain (ft)",avghigh = "Avg Highpoint",
    #             minhigh = "Lowest high point", maxhigh = "Max high point")

![](images/tt11242020_gtavgbyregion.png)

### And finally a couple of models to see what might affect a trail rating.

First a simple linear model using length, gain, & highpoint to predict
rating

    #> 
    #> Call:
    #> lm(formula = rating ~ length_miles + gain + highpoint, data = tt_watraildf_dist)
    #> 
    #> Residuals:
    #>     Min      1Q  Median      3Q     Max 
    #> -3.3527 -0.3735  0.3506  0.9626  2.5779 
    #> 
    #> Coefficients:
    #>                Estimate Std. Error t value Pr(>|t|)    
    #> (Intercept)   2.765e+00  6.531e-02  42.331  < 2e-16 ***
    #> length_miles  1.477e-04  5.794e-03   0.025    0.980    
    #> gain         -4.156e-05  3.191e-05  -1.303    0.193    
    #> highpoint     8.268e-05  1.785e-05   4.631 3.89e-06 ***
    #> ---
    #> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    #> 
    #> Residual standard error: 1.412 on 1854 degrees of freedom
    #> Multiple R-squared:  0.01242,    Adjusted R-squared:  0.01082 
    #> F-statistic: 7.769 on 3 and 1854 DF,  p-value: 3.723e-05
