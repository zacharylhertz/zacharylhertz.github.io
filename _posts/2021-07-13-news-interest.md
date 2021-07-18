---
title: "Tracking the Drop in News Interest"
output:
  md_document:
    variant: gfm
    preserve_yaml: TRUE
    pandoc_args: 
      - "--wrap=preserve"
knit: (function(inputFile, encoding) {
  rmarkdown::render(inputFile, encoding = encoding, output_dir = "../../_posts") })
date: 2021-07-13
permalink: /posts/2021/07/news-interest
toc: true
tags:
  - news interest
---



As [cable news viewership dropped](https://www.usatoday.com/story/entertainment/tv/2021/06/30/fox-news-leads-ratings-after-falling-behind-cnn-post-election/7801577002/) over the first half of 2021, political commentators were [quick to attribute](https://twitter.com/mattyglesias/status/1412250082343280654) this drop in news interest to the Biden administration. But the first half of 2021 has also seen the Covid-19 pandemic – one of the largest news stories in 2020 – rapidly drop in salience since January thanks to falling infection rates and the largely successful vaccine rollout. As a result, it remains difficult to determine what role the Biden administration and the decreased need to closely follow the pandemic have played as potential drivers of the drop in news interest. I used [the Data for Progress Covid-19 Tracking Poll](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/XJLZIN) to investigate.

# Share of Americans who follow politics “most of the time” fell 15 percent consistently

The Data for Progress Covid-19 Tracking Poll, with 25 waves since April 2020, has included a number of questions tracking Americans’ opinions on politics and the pandemic. As part of the survey, respondents answer a question tracking general news engagement:

> Some people follow what’s going on in government and public affairs most of the time, whether there’s an election going on or not. Others aren’t as interested. Would you say you follow what’s going on in government and public affairs…
>
> -   Most of the time
> -   Some of the time
> -   Only now and then
> -   Hardly at all

The share of American adults who say they follow politics “most of the time” dropped from 41.8 percent in January to 35.5 percent in May, marking a 15 percent decline in the highest level of news engagement. In the same time period, the share of those who follow politics “some of the time” and “hardly at all” both increased 3 percentage points, while the share of those who follow politics “only now and then” remained relatively stable.

![](/images/posts/news-interest/generalnewsint.png)<!-- -->

If this drop in the share of highly engaged Americans is driven mainly by the effects of the Biden administration, we might expect to see the trend in news interest differ among partisan lines. The data does not reflect a political difference in changing news interest, however. Breaking the sample down by party identification, presidential vote, and strength of partisan identification, we see a consistent 15 percent decline in the highest level of news engagement between January and May.

![](/images/posts/news-interest/pid.png)<!-- -->

Democrats and Republicans had similar trends in the levels of their news interest between October 2020 and January 2021. But while the share of Republicans who followed political news “most of the time” immediately started decreasing from 47.3 percent in January 2021 to 39.8 percent in May 2021, the share of Democrats who were highly-engaged followers

![](/images/posts/news-interest/partisans.png)<!-- -->

![](/images/posts/news-interest/presvote.png)<!-- -->

# Heading

![](/images/posts/news-interest/sources.png)<!-- -->

![](/images/posts/news-interest/sourcespartisans.png)<!-- -->

![](/images/posts/news-interest/sourcespid.png)<!-- -->

![](/images/posts/news-interest/sourcespresvote.png)<!-- -->

# Conclusion
