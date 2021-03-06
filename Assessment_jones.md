Writing Prompt: Residential Construction
----------------------------------------

Residential Construction continues to show signs of growth, though at a
slower rate than last month. In May 2021, the seasonally adjusted number
of building permits issued was ~1.7 million, with ~1.6 million housing
starts and ~1.4 million housing completions. These three numbers signal
to buyers that the rate of new home building is continuing to catch up
to the lull caused by the pandemic. This is promising for prospective
buyers as housing permits and starts imply an increased and growing
supply of homes in the near future.

Interestingly, overall building permits seem to be falling for the last
few months, suggesting a downward trend. This implies that supply and
demand are beginning to converge. However, the number housing permits,
though dropping month-on-month, remains higher than any point in the
four years before March 2020. This is a positive sign for medium-term
prospective buyers as the market is showing signs of slowly returning to
some equilibrium after the drop and surge due to the pandemic. Future
buyers will have an opportunity to ensure they are not buying into a
housing “bubble” once some equilibrium is returned. The decrease in
permits also signals to sellers that the market is starting to slowly
move towards equilibrium, so it is prime time to reap the benefits of
selling your home before supply catches up to demand.

Data Visualizations
-------------------

The two graphs below show the smoothed trends of average listing price
and total number of listings over time by Metro Area rank. The data
ranks 50 cities by household quantity, which can be loosely translated
into the size of the city (population-wise, not necessarily
geographically). A raw household ranking of 1 indicates the most
households (in this data, 1 is the NYC area), while 50 is the least
households (in this data, 50 is Rochester, NY). To simplify the
household rankings for analysis, I grouped the 50 rankings into 10th
percentiles where the a new ranking of 1 buckets the top 5 metro areas
together, new ranking of 2 is the next 5, etc. This manipulation was
performed to provide a higher level, clearer picture of how the
different calibers of cities are behaving relative to one another.

![](Assessment_jones_files/figure-markdown_strict/unnamed-chunk-1-1.png)![](Assessment_jones_files/figure-markdown_strict/unnamed-chunk-1-2.png)

Key Takeaways:

-   As expected, the most dense decile of cities sees the highest
    listing prices, as well as the largest upwards swing in listing
    price from 2019 to 2021.

-   Curiously, the 2nd, 3rd, 4th, and 8th deciles clump together,
    showing similar listing price trends.

    -   The 8th decile contains the Metro Area of Santa Clara/San Jose,
        CA which is an outlier in terms of median listing price.
        Listings in this area are some of the most expensive in the
        dataset. Housing prices in this geography are high perhaps due
        to inability to sprawl since it is surrounded by mountains and
        ocean.
    -   The 2nd decile contains “sprawling” cities that are large in
        number of households, but also large in land area, which
        provides plenty of space for growth, and thereby lower listing
        prices.

-   Observing the two graphs above together, it is clear that as the
    number of listings has decreased, the average listing price of a
    listing has increased. This phenomena is interesting as it is
    consistent with supply/demand expectations, but may be indicative of
    other trends in the housing market that are preventing supply to
    keep up with demand (i.e. labor shortage, permitting delays, lumber
    shortage, etc.)

![](Assessment_jones_files/figure-markdown_strict/unnamed-chunk-2-1.png)

The graph above shows the median days on market and percent of listings
that increased their listing price, split out by month and year. This
chart allows us to compare the trend by month in each year, and across
years.

Key Takeaways:

-   When the pandemic took hold in March/April 2020, we see a drop in
    the percent of listings increasing price, followed by a steady
    increase until August 2020. Percent of homes that increased price
    continued to decrease for most of fall 2020, before recovering in
    the winter and spring of 2021.

-   Each month of 2021 so far has seen a higher percent of homes listed
    with a price increase than in any month in 2020.

-   Year on year, the median days on market is decreasing. April 2021
    has a median of &lt;50 days on market, while the minimum monthly
    median in 2020 was between 50 and 60 days on market.

-   The market shows signs of heating up as we enter summer 2021, with
    both % of homes increasing price increasing, and median days on
    market decreasing.

Analytics Challenge
-------------------

I would first ask the reporter if they have a specific understanding or
interpretation of “off-grid” so I can use the data provided to target
the specific segment they are referring to. I would aim to understand
the point of view of the reporter so I can return information that they
find helpful and relevant to their goal.

To analyze the “off-grid” home market, I would look for trends
associated with listings classified as “off-grid” by the agent’s
description. This term is vague and likely varied in interpretation, so
I would aim to find some key similarities in the homes in order to
further split up this category into like listings. Stating the specific
definition of “off-grid” will be crucial in understanding the analyses.
I would expect that “off-grid” homes may be similar in surrounding
population density, income, and employment. Additionally, I would use
some Natural Language Processing to break down the listing descriptions
to find any common words that may indicate an “off-grid” home, and could
be used to assign buyer values.

Anecdotally, it seems that higher income people who work remotely are
moving towards living “off-grid” or at least owning a home “off-grid”,
so demographics may be shifting quickly in these areas. I would group
geographically similar listings using zip code data in the
“listing\_daily\_snapshot” data, then look at demographic and employment
data for each zip code. I would identify any trends in median household
income in areas with a significant number of “off-grid” listings. This
set of analyses would capture if higher income people are moving into
more remote areas either part or full time, thereby driving up household
median income. If this trend is occurring, you would expect to see a
corresponding increase in “off-grid” listings as homeowners take
advantage of the influx of high-income buyers and list their homes.
Comparing “off-grid” areas to other cities in the US can help identify
the rate of change in median home price relative to the country as a
whole. If median home price is increasing faster than the country as a
whole, it suggests that these “off-grid” areas are in fact seeing a
demographic shift.

In order to trust the data quality and analysis, I would want more data
around lot size or total land area as “off-grid” homes are more likely
to have a large land component which would influence price.
Additionally, I would want consistent data around qualitative factors
that may make a home “off-grid” such as cellular towers, internet
access, water status (well or otherwise), power grid access, road
access, etc. Having this data would help quantify the value of each of
these things to a buyer. For example, a segment of buyers may desire to
be somewhere remote and beautiful, but need to work full time.
Therefore, low population density would add value, but lack of internet
access would detract value. More data about the degree of “off-grid” for
each listing would help understand what customer segment is driving any
trends that arise.

In general, I would focus my analyses on determining whether an increase
in listing activity (both new listings and listing page views) and
median home price is associated with a demographic shift. Depending on
the outcome, I may be able to confirm or debunk the anecdotal evidence
that wealthy, remote workers are fleeing large cities for a more simple
lifestyle. If the house prices and household income are decreasing as
listing activity increases, I would look at the unemployment rate to see
if the economy is suffering in these areas, leading instead to an exodus
of current residents towards more populous areas with more employment
opportunities.

For further interesting analyses, it would be possible to create a model
that could predict home selling value or time on market based on the
data provided. This would be a very interesting study and could be
manipulated to watch how listing and selling activity has changed, and
will change over time as a function of the local economy and demography.
