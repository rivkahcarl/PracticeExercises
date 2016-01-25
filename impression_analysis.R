# Project:  An executive, John, has come to you with a theory that muted impressions may perform 
# better according to certain metrics.  Using the attached spreadsheet of data, see if John’s theory is correct.  
# If its not, prove it.  
# 1) Are muted impressions actually worse or the same?  
# 2) If it is, how would you recommend we leverage that knowledge? 
# 3) What type of inventory would we want to target for certain types of campaigns?  
# 4) How would we price those campaigns?  Is there any risk involved in selling this way?  
# Put together 3-5 slides (PowerPoint, Google Slides, whatever you prefer) for an informal presentation to John and our Director of Analytics 
# demonstrating how you think we should proceed and why.

# As a reminder, here’s a list of the performance metrics that we monitor for each campaign:
# •	Total impressions served
# •	Click Through Rate (CTR) - % of impressions where the user actually clicked on the ad and was sent to the brand’s website
# •	Video Completion Rate (VCR) - % of impressions that played all the way to the end of the ad
# •	Viewability - % of impressions that were actually on the screen (as opposed to being off the screen at the bottom of the page) while the ad was playing
# •	Margin % - How much money are we making from this campaign? 
# The margin is simply ((what the client is paying) - (what we have to pay the exchanges)) / (what the client is paying).  The win_price is the amount we have to pay the exchange, but we obviously charge our clients more for the service we provide. 
# Just for fun, let’s assume 100% markup.  So if we contract with nike for $20 per 1,000 impressions, 
# we’ll try our best to buy from the exchanges at $10.

# Currently our clients either pay us per impression, viewable impression, or completion.  
# For simplicity, assume these are the only contract types that we offer as of now.  
# We sell viewable impressions for more because they’re harder to come by.  
# We don’t know in advance if an impression will be viewable, but we do find out after the view plays.  
# We can however try to predict this based on historical data.  
# Completions are even more expensive, for the same reasons.  We don’t know in advance if an impression will complete (play to the end).  
# We can only try to predict this based on historical data. In either case, we’ll usually have to serve multiple impressions before one of them is viewable or completes respectively.  
# Note that we talk about prices by the 1,000 so when we say someone is paying us “$50 for completions” 
# we actually mean “$50 for 1,000 completions”. We call this CPM or cost per thousand.

# Let’s take an example contract.  Say Nike has a contract with us to pay $40 for every 1,000 completions.  
# We end up serving 2,000 impressions, in order to get the 1,000 completions, at an average cost of $15 CPM.  
# In that case we had a VCR of 50% and a margin of 25%. Nike paid us $40 for the 1,000 completions, 
# we paid the exchanges $30, ($40-$30)/$40=10%.

# The section should look familiar from your first interview.  It provides a little context about digital video advertising and describes every column in the attached spreadsheet.  Don’t worry about the fact that we’ve only given you 5,000 impression worth of data.  Assume that the data provided is representative of reality in terms of percentages and trends.

# Context:  In digital advertising, every time a web page with an advertisement loads, 
# there is an auction that happens in about 1/10th of a second, to decide which company (such as Eyeview) 
# gets to show an ad and how much they’re going to pay to do it. The company that bids the highest, wins and 
# gets to show whatever ad they want.  However, the amount they actually pay to show the ad is the second highest bid. 
#  This is called a second-price auction.  The company that hosts these auctions is called an exchange, 
# and there are several major exchanges currently operating in the US. In advertising, every time we show a video ad 
# we call it an impression.    That impression was served as part of a specific campaign, 
# say the Nike Spring Sporting Goods Campaign.


current_dir <- getwd()
csv_path <- paste(current_dir, "/RivkahPersonal/PracticeExercises/sampledata.csv", sep="")
all_data <- read.csv(csv_path, header=TRUE, stringsAsFactors=FALSE)
# change win price to be numeric
all_data$win_price <- as.numeric(gsub("\\$", "", all_data$win_price))



# > table(all_data$exchange_id)

#    0    1    3    5    6    7    8   10   11   13   14   16 
#  645   81  602    2  512 2151   41  177  199  343    9  238 
# > table(all_data$campaign_id)

# 1308 1315 1316 1317 1343 1348 1410 1412 1413 1415 1416 1417 1418 1420 1421 1422 
#  181   20   43   32  218  112  349   29 2701  132   69  381  302  424    6    1 
# > table(all_data$sound)

# FALSE  TRUE 
#  1394  3606 
# > table(all_data$viewable)

# FALSE  TRUE 
#  3407  1593 
# > table(all_data$completions)

#    0    1    2    3 
# 2446 2452   56   46 
# > table(all_data$clicks)

#    0    1    2    4 
# 4974   21    4    1 

### Run through various metrics:
# 1) Total Impressions served: In this case there were a total of 5000 impressions, not looking at this yet
# 2) Click Through Rate (CTR)
# Look at total click through rate
total_ctr <- sum(all_data$clicks)/ nrow(all_data)
# Out of 5000 impressions only 33 clicked through for 0.0066 or .66 % of people clicked. 
# Question: Now check percent of those that are muted: 11 were muted 
all_data_clicked <- subset(all_data, all_data$clicks != 0)
perc_muted_from_clicked <- nrow(all_data_clicked[all_data_clicked$sound == "FALSE",])/ nrow(all_data_clicked)
# Muted from those are: 0.4230769
# Non-muted: 0.5769231
########## POINT 1: Simple measuring of click through rate, muted impressions seem to have a little if not worse effect.
# However, almost double the number of impressions were served with sound 3606 were played with sound while 1394 were played
# without meaning, this is not a fair experiment. 
# Only > 1394/5000
# [1] 0.2788
# to begin with were served muted. So ratio of muted to non-muted is ~ 1394:3606 or 1:2.5
# Which means 11/1394 is a 0.0078909 rate, while 25/3606 is only .00693289 

# 3) Video Completion Rate (VCR) - % of impressions that played all the way to the end of the ad
# Out of 5000 impressions, 2554 impressions were completed 0.5108 or ~ 51% completed.
all_data_completed <- subset(all_data, all_data$completions != 0)
# Question: How many of those were muted? 892 of those were muted: 
perc_muted_from_completed <- nrow(all_data_completed[all_data_completed$sound == "FALSE",])/ nrow(all_data_completed)
# Muted from those are: 0.3492561
# Non-muted: 0.6507439
######### POINT 2: Simple measuring of VCR, muted impressions seem to have little if not worst effect. 
# Again look at ratio of test comparison. 892/2554 were completed- thats a 0.6398852 which is 64%  vs 
# > 1662/3606
# [1] 0.4608985

# 4) Viewability
# > table(all_data$viewable)

# FALSE  TRUE 
#  3407  1593 
# Out of 5000 impressions, 1593 of them were viewable or 0.3186 ~ 32% viewable
all_data_viewable <- subset(all_data, all_data$viewable == 'TRUE')
# Question: How many of those were muted? 605 of those were muted: 
perc_muted_from_viewable <- nrow(all_data_viewable[all_data_viewable$sound == "FALSE",])/ nrow(all_data_viewable)
# Muted from those are: 0.3797866
# Non-muted from those are:  0.6202134
######## POINT 3: Simple measuring of Viewability, muted impressions do not outperform
# however relative to number 605/ 1394 total - thats 0.4340029 or 43% 
# whereas non-muted is 988/3606 or 0.2739878 or 27 % 

### Conclusion- muted impressions actually are more performant. Based on the first three metrics.
# Now take a look at the fourth- 
all_data_bool_sound <- all_data
all_data_bool_sound$sound <- ifelse(all_data_bool_sound$sound == 'FALSE', 1, 0)

margin_df_cnt <- setNames(aggregate(adviewing_id~ campaign_id, all_data_bool_sound, length), c("campaign_id", "number_of_impressions"))
margin_df_price_mute <- setNames(aggregate(cbind(sound, clicks, completions, win_price)~ campaign_id, all_data_bool_sound, sum, na.rm = TRUE), c("campaign_id", "sum_muted","sum_clicks", "sum_completions", "sum_win_price"))

margin_df <- merge(margin_df_cnt, margin_df_price_mute, by="campaign_id")
margin_df$perc_muted <- margin_df$sum_muted / margin_df$number_of_impressions
margin_df$perc_clicks <- margin_df$sum_clicks / margin_df$number_of_impressions
margin_df$perc_completions <- margin_df$sum_completions / margin_df$number_of_impressions
margin_df$avg_impression_price <- margin_df$sum_win_price / margin_df$number_of_impressions
margin_df <- margin_df[order(-margin_df$perc_muted),]
#### Not sure that was helpful calculation considering- does not look like compaign costs in total prove to be any cheaper
# than campaigns with less perc_muted. This might be a reason to consider muted impressions more. 

# Also check individual price from original dataset
price_df <- setNames(as.data.frame(cbind(all_data$win_price, all_data$sound)), c("win_price", "sound"))
price_df$sound <- ifelse(price_df$sound == 'FALSE', 1, 0)
plot(price_df)

library(ggplot2)
price_df <- setNames(as.data.frame(cbind(all_data$win_price*1000, all_data$sound, all_data$timestamp)), c("win_price", "sound", "timestamp"))
# price_df$sound <- ifelse(price_df$sound == 'FALSE', 1, 0)

qplot(timestamp, win_price, data = price_df, colour = sound)
# plot shows roughly even distribution of muted cost relative to non-muted cost- between averages above and plot here
# So not neccesarily more expensive, but also not neccesarily cheaper. 

#### Recommendations: 
# 1) We would want to run another test with equal number of muted and unmuted impressions to confirm numbers above.
# 2) Say we were able to confirm that again- that CTR, VCR and Viewability outperformed, we would 
# a) Want to ensure that we could serve as many muted impressions, is there a cap from the clients perspective on mutability?
# b) We could price combinations with higher percents of muted impressions as more expensive- better markup since it ensures 
# more clicks

# Questions: 
# Completion rate for muted impressions- will clients actually be happy with this? does this actually mean that they have just let the video
# run and not even viewed it
#  Take for example the following chart: 
# > margin_df
#    campaign_id number_of_impressions sum_muted sum_clicks sum_completions sum_win_price perc_muted perc_clicks perc_completions avg_impression_price
# 7         1410                   349       115          1             195       2.19900  0.3295129 0.002865330        0.5587393          0.006300860
# 4         1317                    32        10          2              17       0.20665  0.3125000 0.062500000        0.5312500          0.006457813
# 13        1418                   302        93          0             166       3.38132  0.3079470 0.000000000        0.5496689          0.011196424
# 2         1315                    20         6          0               6       0.12870  0.3000000 0.000000000        0.3000000          0.006435000
# 12        1417                   381       110          8             197       2.96623  0.2887139 0.020997375        0.5170604          0.007785381
# 9         1413                  2701       752         18            1467      45.29361  0.2784154 0.006664198        0.5431322          0.016769200


## Lets assume we have a contract of $20 per 1000 impressions
all_data_ten <- subset(all_data, all_data$win_price < 10.00)


# Let’s take an example contract.  Say Nike has a contract with us to pay $40 for every 1,000 completions.  We end up serving 2,000 impressions, in order to get the 1,000 completions, at an average cost of $15 CPM.  In that case we had a VCR of 50% and a margin of 25%. Nike paid us $40 for the 1,000 completions, we paid the exchanges $30, ($40-$30)/$40=10%.
# Take another sample contract- if completion rate 


# Margin % - How much money are we making from this campaign? (client pays - win_price) / client pays
# win_price is what we pay exchange
# Question to answer: Which campaign had the most mutest impressions? Check out price relat
# group by campaign_id and make a decision with regard to whether or not, they were more expensive.

# nice_key_agg <- setNames(aggregate(cbind(adword_search_volume, noodle_visits, expected_noodle_cost)~ nice_key, nice_key_by_w_sub, sum, na.rm = TRUE), c("nice_key", "total_expected_adsearch_vol", "total_expected_noodle_visits", "total_expected_noodle_cost"))


