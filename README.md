About our project

Our project is focused on one of the largest sources of creative output of the 21st century: video games. Video games frequently employ detailed narratives, sophisticated and challenging gameplay, and can connect with profound elements of the human psyche. We’ve decided to look at trends in video games as a way to assess what draws people to particular games.
In large part, we kept our exploration open-ended, but we’ve focused on the factors that influence the ratings and popularity of video games on Steam, one of the largest platforms for purchasing and playing video games, with an estimated 60% of total PC game revenue captured by the platform.
In the end, we were able to draw a few interesting conclusions regarding trends in video games, but what struck us at the end was how difficult it was to find reliable data, especially for popularity metrics. We concluded that this is potentially a result of Steam’s strategy to maximize sales of video games.

Limitations of online video game ratings

Our most striking finding ended up being that user ratings on third party websites, critic ratings on third party websites, and user ratings on the Steam store did not match up very well.



We constructed a correlation matrix comparing each measure included in the dataset. The key takeaway here is that, overall, rating websites often did not agree with each other when rating video games. The ratings for each video game across different rating websites was positive in all cases; however, the relationship was weak in most cases. The greatest correlation was between critics on IGDB and Metacritic, while the smallest was between IGDB critics and GameFAQ users.

These sites aim to aggregate ratings from many different users and critics in order to produce balanced ratings of games, so it is surprising to see how little they actually line up with each other. 



This might be explained by differences in how critics and users rate games, so we compared mean critic and user ratings on IGDB and Metacritic to investigate. In both cases, critics tended to rate games lower than users. Given this disparity, in addition to the fact that ratings across the board are only weakly related, a possible explanation is that manipulation of ratings may occur for many games. Especially for games published by smaller developers that aren’t widely publicized, which make up a majority of cases in this dataset, flooding a game’s page with positive reviews would be a relatively simple task. This is an intriguing possibility, but given time constraints, it’s not something we deeply investigated.

Given the limitations of popularity metrics from third-party sites as well as Steam itself, we looked into the possibility of using Steam’s API to obtain popularity metrics for games directly from the source. However, Steam does not provide a direct way to access these metrics, possibly as a way to avoid speculation that might negatively impact promotion of certain video games. Third parties did not have reliable metrics either, because of changes Steam has made to its API over the years.

Price vs. User Ratings

Diving in deeper to the data, we decided to investigate price vs user ratings across three different websites. We chose to look closely at the ratings from users on Steam (of course), IGDB, and Metacritic. Each of these websites are highly active and have a large number of users. Our rationale was that different categories of games may inherently have higher development costs, or may be subject to different kinds of pressures from their users. Overall, we found a weak, and positive, relationship between price and popularity across categories. In the below plot, we plot how much each extra percentage point in rating translates to change in price. 



For sake of eliminating edge cases, we decided to ignore any games that are more than $70, as there are very few. After plotting and examining our results, we found that there is little correlation between user ratings and price of each video game. Ratings tend to be less variable when observing the results from the Steam user ratings, and there was less overall correlation between price and user scores when looking at either Metacritic or IGDB. In conjunction with our previous results, this might be because of manipulation, since Steam is the actual platform where users buy games and it would be more advantageous for developers to boost ratings on Steam.

How the Steam Store changed over time:

Steam was released relatively recently, which gives us a limited amount of data that we can draw general conclusions from, which would be taken into account. It seems as though Steam has been releasing more and more games steadily from the period of its release to around 2020, where the number of games published seems to take a huge leap. This could be due to a number of reasons, however a likely reason could have been due to the Pandemic, COVID 19. This is a likely reason due to the dip in games released since that year, however it is difficult to say much definitively due to the lack of number of years since 2020. 



When we examine price, it seems as though the lack of data available is quite limiting. We are unable to predict meaningfully whether or not the average price or the number of games published per year will increase or decrease, however if we continue to analyze considering COVID 19, we can say that it may have been likely that games published would have continued to decrease until stabilization, and average price would likely have continued to fluctuate. 


Ethics:
Our investigation deals with factors that influence the price and overall sucess of video games. This affects three main players: video game developers, video game players, and middlemen like Steam. The data we found is gathered from Steam, which has an incentive to increase sales and profits of the video games it sells. The possibility that Steam isn't fully transparent with the data it shares must be considered. At the same, this data was gathered by a third-party source,  which might have different incentives. One of the main problems with our investigation is that there is no reliable data  on video game popularity that we could find. This makes sense - video game developers and Steam want to make their video games seem like quality products that stand apart from other games. However, this ignores the vested interest of video game players, who currently don't have a objective standard with which they can evaluate the quality of games before they buy them.

This data was pulled from Steam’s API in 2022. Ratings were scraped from a number of different video game rating websites. It represents all games available on the Steam store as of that date, including game’s name, price, genre categories, ratings from third-party websites, and more. The data excludes games that are free to play.
We wanted to study factors that influence the ratings and popularity of video games on Steam, one of which included price. 

The majority of prices are less than $10 U.S. dollars.
