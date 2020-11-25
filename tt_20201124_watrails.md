    # readin in data, create df for plots
    library(tidytuesdayR) # to load tidytuesday data
    library(tidyverse) # to do tidyverse things
    library(tidylog) # to get a log of what's happening to the data
    library(patchwork) # stitch plots together
    library(reactable) # pretty tables
    library(htmltools) # help with pretty tables
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

I’ve been wanting to take a deeper dive on [gt](https://gt.rstudio.com/)
& [reactable](https://glin.github.io/reactable/). So inspired by [Thomas
Mock’s reacable
primer](https://themockup.blog/posts/2020-05-13-reactable-tables-the-rest-of-the-owl),
a basic table with heatmap-like formatting for some columns. Since I’m
mostly recreating Thomas’ table (but with different data) I won’t
comment to much code. See his explainer for details.
<!--html_preserve-->

Averages by Trail Region

<script type="application/json" data-for="htmlwidget-76542ea27fcc8edc998f">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"location_region":["Central Cascades","Central Washington","Eastern Washington","Issaquah Alps","Mount Rainier Area","North Cascades","Olympic Peninsula","Puget Sound and Islands","Snoqualmie Region","South Cascades","Southwest Washington"],"n_region":[217,77,132,74,188,279,203,184,208,181,115],"avglength":[9.58,5.71,9.69,4.96,8.08,11.41,8.19,4.29,8.66,8.49,6.44],"avgrating":[3.04,2.82,2.23,2.52,3.37,3.1,3.32,2.82,3.22,3.07,2.7],"avggain":[2277,809,1644,954,1853,2534,1570,455,2171,1664,1166],"avghigh":[4750,2249,4449,1489,5228,5077,2825,568,4457,4770,1771],"minhigh":[600,240,300,250,800,125,20,10,450,922,20],"maxhigh":[9511,6876,7310,2948,10080,9200,6988,3750,9416,12276,7800]},"columns":[{"accessor":"location_region","name":"Region","type":"character"},{"accessor":"n_region","name":"N","type":"numeric"},{"accessor":"avglength","name":"Avg Length (miles) ","type":"numeric","align":"center","style":[{"background":"#6CC4AB"},{"background":"#D4F0BC"},{"background":"#68C3AB"},{"background":"#EBF8CC"},{"background":"#98D5AB"},{"background":"#35B0AB"},{"background":"#95D4AB"},{"background":"#FFFFFF"},{"background":"#87CEAB"},{"background":"#8DD0AB"},{"background":"#C0E7B2"}]},{"accessor":"avgrating","name":"Avg Rating","type":"numeric","align":"center","style":[{"background":"#73C7AB"},{"background":"#9AD6AC"},{"background":"#FFFFFF"},{"background":"#C8EBB3"},{"background":"#35B0AB"},{"background":"#67C2AB"},{"background":"#3EB3AB"},{"background":"#9AD6AC"},{"background":"#51BAAB"},{"background":"#6DC5AB"},{"background":"#ACDFAF"}]},{"accessor":"avggain","name":"Avg Gain","type":"numeric","format":{"cell":{"separators":true},"aggregated":{"separators":true}},"align":"center","style":[{"background":"#4FB9AB"},{"background":"#DAF2C0"},{"background":"#90D2AB"},{"background":"#CBECB5"},{"background":"#7BCAAB"},{"background":"#35B0AB"},{"background":"#97D5AB"},{"background":"#FFFFFF"},{"background":"#5ABDAB"},{"background":"#8ED1AB"},{"background":"#B9E4B1"}]},{"accessor":"avghigh","name":"Avg High Point","type":"numeric","format":{"cell":{"separators":true},"aggregated":{"separators":true}},"align":"center","style":[{"background":"#4BB8AB"},{"background":"#B5E3B0"},{"background":"#58BDAB"},{"background":"#D4F0BC"},{"background":"#35B0AB"},{"background":"#3BB2AB"},{"background":"#A0D9AD"},{"background":"#FFFFFF"},{"background":"#58BDAB"},{"background":"#4AB7AB"},{"background":"#C7EBB3"}]},{"accessor":"minhigh","name":"Min High Point","type":"numeric","format":{"cell":{"separators":true},"aggregated":{"separators":true}},"align":"center","style":[{"background":"#80CCAB"},{"background":"#C8EBB3"},{"background":"#BDE6B2"},{"background":"#C6EAB3"},{"background":"#51BAAB"},{"background":"#E4F5C7"},{"background":"#FCFEF7"},{"background":"#FFFFFF"},{"background":"#A0D9AD"},{"background":"#35B0AB"},{"background":"#FCFEF7"}]},{"accessor":"maxhigh","name":"Max High Point","type":"numeric","format":{"cell":{"separators":true},"aggregated":{"separators":true}},"align":"center","style":[{"background":"#74C7AB"},{"background":"#ABDEAF"},{"background":"#A3DAAD"},{"background":"#FFFFFF"},{"background":"#67C2AB"},{"background":"#7BCAAB"},{"background":"#A9DDAE"},{"background":"#ECF9CE"},{"background":"#76C8AB"},{"background":"#35B0AB"},{"background":"#9AD6AC"}]}],"defaultPageSize":11,"paginationType":"numbers","showPageInfo":true,"minRows":1,"striped":true,"compact":true,"dataKey":"acfa6ebc98889a0a2538b51a293f62c2","key":"acfa6ebc98889a0a2538b51a293f62c2"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>

<!--/html_preserve-->
