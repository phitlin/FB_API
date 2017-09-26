##this version is 'clean' as of 9/26/17

library(tidyverse)
library(readxl)
library(dplyr)
library(gmodels)
library(Rcmdr)

setwd("/home/phitlin/FB_Science_project")


##importing coding cases
##to make file, remove '@2' etc from the column names and erase unnecessary columns - also added 'fromcoded' to most VARs to distinguish

coded <- read_excel("FINAL_coding_to_import_to_R.xlsx")


coded <- coded %>% mutate(coded_by_human = "na")
coded$coded_by_human <- as.character(coded$coded_by_human)
attach(coded)
coded$coded_by_human[Selected_for_coding_fromcodingfile == "10"] <- "not coded"
coded$coded_by_human[Selected_for_coding_fromcodingfile == "1" | Selected_for_coding_fromcodingfile == "2"] <- "was part of coded sample"
detach(coded)

##selects only those that were coded
coded <- coded %>% filter(coded_by_human == "was part of coded sample")


##group area of science and reoder codes
coded <- coded %>% mutate(areagrouped = "na")
coded$areagrouped <- coded$areaofscience
coded$areagrouped <- as.character(coded$areagrouped)
attach(coded)
coded$areagrouped[areaofscience == "Completely non-science"| areaofscience == "Non-science memes"] <- "non-science(grouped)"
detach(coded)

coded$areaofscience <-factor(coded$areaofscience, levels = c("Health and medicine", "Food and nutrition", "Neurology and mental health", "Behavioral sciences", "Energy and environment", "Animal science and behavior", "Space and astronomy", "Technology", "Engineering", "Archeology and evolution", "Geology", "Physics", "Math", "Chemistry", "Documentary/TV show NOT about a specific field of science", "Spirituality", "Paranormal", "Science other/science general", "Travel", "Feats and phenomena", "Completely non-science", "Non-science memes"))


coded$areagrouped <-factor(coded$areagrouped, levels = c("Health and medicine", "Food and nutrition", "Neurology and mental health", "Behavioral sciences", "Energy and environment", "Animal science and behavior", "Space and astronomy", "Technology", "Engineering", "Archeology and evolution", "Geology", "Physics", "Math", "Chemistry", "Documentary/TV show NOT about a specific field of science", "Spirituality", "Paranormal", "Science other/science general", "Travel", "Feats and phenomena", "non-science(grouped)"))


coded <- coded %>% mutate(storylinegrouped = "na")
coded$storylinegrouped <- coded$storyline
coded$storylinegrouped <- as.character(coded$storylinegrouped)
attach(coded)
coded$storylinegrouped[storylinegrouped == "Drug prices (other than EpiPen)"| storylinegrouped == "EpiPen price hikes"] <- "drug prices"
coded$storylinegrouped[storylinegrouped == "Paris Climate Change agreement" | storylinegrouped == "Climate change/global warming"] <- "climate change"
coded$storylinegrouped[storylinegrouped == "Water pollution or conservation" | storylinegrouped == "Air pollution" | storylinegrouped == "Light pollution"] <- "pollution (water, land, light)"
detach(coded)


coded$storylinegrouped <-factor(coded$storylinegrouped, levels = c("Health insurance debate or health reform", "drug prices", "Vaccines", "Marijuana/recreational drugs", "Alcohol or tobacco", "Gwyneth Paltrow news", "New drug discoveries (medical)", "Weight loss and/or exercise (if post is not focused on nutrition)", "Nutrition and diet", "Beauty tips (if post does not discuss health)", "Recipes (if not focused on nutrition and diet)", "Depression or other mental health conditions", "Dating, romantic relationships, sex", "GMOs", "climate change", "Discovery of animal or plant species", "pollution (water, land, light)", "Land/animal conservation", "Endangered species", "Energy sources", "Major weather events (modern day)", "Puppy Bowl on Animal Planet",  "Shark Week!", "Evolution", "Discovery of planets", "Internet of Things", "Net neutrality", "Driverless cars", "Privacy", "Large Hadron Collider (CERN) news", "March for Science (April 22, 2017)", "Budget changes to government research or agencies", "None of the above"))



coded <- coded %>% mutate(focusgrouped = "na")
coded$focusgrouped <- coded$primaryfocus
coded$focusgrouped <- as.character(coded$focusgrouped)
attach(coded)
coded$focusgrouped[focusgrouped == "Non-science memes"| focusgrouped == "Not science"] <- "not science"
coded$focusgrouped[focusgrouped == "Scientific information that helps me make decisions about everyday life for me and my family"] <- "News you can use/info to make decisions about everyday life"
detach(coded)

coded$focusgrouped <-factor(coded$focusgrouped, levels = c("A new scientific discovery or development", "Explanation of a scientific concept", "News you can use/info to make decisions about everyday life", "Promotion or ad", "Encourage activism or participation", "Visual only", "Profile of specific scientist(s)", "Disagreement among scientists OR findings that conflict with earlier research findings", "Misconduct by scientists OR controversies in scientific research and publishing", "Media", "Science funding and government agency issues", "STEM education", "Travel", "Archived post republished (except TV clips)",  "not science"))


coded$linktoresearch <-factor(coded$linktoresearch, levels = c("Yes, post does link to outside evidentiary research", "No, post does not link to outside evidentiary research"))

coded$linktonews <-factor(coded$linktonews, levels = c("Yes, post does link to outside news source", "No, post does not link to outside news source"))

coded$stats <-factor(coded$stats, levels = c("Yes, post does include stats", "No, post does not include stats"))



##entering in all historical files, then joining pages 1 by 1
billnye <- read_excel("historical_files/billnye_2014_to_June_2017_compiled.xlsx")
attach(billnye)     ##to fix one problem case with duplicate ID
billnye$id[id == "48947135361_10153553978585362" & year == "2015"] <- "48947135361_10153553978585362duplicate"
detach(billnye)
billnye <- left_join(billnye, coded, by = "id", all = TRUE)

bbcearth <- read_excel("historical_files/bbcearth_2014_to_June_2017_compiled.xlsx")
bbcearth <- left_join(bbcearth, coded, by = "id", all = TRUE)

droz <- read_excel("historical_files/droz_2014_to_June_2017_compiled.xlsx")
droz <- left_join(droz, coded, by = "id", all = TRUE)

ifls <- read_excel("historical_files/IFLS_2014_to_June_2017_compiled.xlsx")
ifls <- left_join(ifls, coded, by = "id", all = TRUE)

healthdigest <- read_excel("historical_files/healthdigest_2014_to_June_2017_compiled.xlsx")
healthdigest <- left_join(healthdigest, coded, by = "id", all = TRUE)

davidwolfe <- read_excel("historical_files/DavidWolfe_2014_to_June_2017_compiled.xlsx")
davidwolfe <- left_join(davidwolfe, coded, by = "id", all = TRUE)

sciencealert <- read_excel("historical_files/ScienceAlert_2014_to_June_2017_compiled.xlsx")
sciencealert <- left_join(sciencealert, coded, by = "id", all = TRUE)

sciencenaturepage <- read_excel("historical_files/ScienceNaturePage_2014_to_June_2017_compiled.xlsx")
sciencenaturepage <- left_join(sciencenaturepage, coded, by = "id", all = TRUE)

interestingengineering <- read_excel("historical_files/interestingengineering_2014_to_June_2017_compiled.xlsx")
interestingengineering <- left_join(interestingengineering, coded, by = "id", all = TRUE)

enjoyscience <- read_excel("historical_files/enjoy.science_2014_to_June_2017_compiled.xlsx")
enjoyscience <- left_join(enjoyscience, coded, by = "id", all = TRUE)

neildegrassetyson <- read_excel("historical_files/neildegrassetyson_2014_to_June_2017_compiled.xlsx")
neildegrassetyson <- left_join(neildegrassetyson, coded, by = "id", all = TRUE)

stephenhawking <- read_excel("historical_files/stephenhawking_2014_to_June_2017_compiled.xlsx")
stephenhawking <- left_join(stephenhawking, coded, by = "id", all = TRUE)

sciencedump <- read_excel("historical_files/sciencedump_2014_to_June_2017_compiled.xlsx")
sciencedump <- left_join(sciencedump, coded, by = "id", all = TRUE)

mindbodygreen <- read_excel("historical_files/mindbodygreen_2014_to_June_2017_compiled.xlsx")
mindbodygreen <- left_join(mindbodygreen, coded, by = "id", all = TRUE)

dailyhealthtips <- read_excel("historical_files/DailyHealthTips_2014_to_June_2017_compiled.xlsx")
dailyhealthtips <- left_join(dailyhealthtips, coded, by = "id", all = TRUE)

michiokaku <- read_excel("historical_files/michiokaku_2014_to_June_2017_compiled.xlsx")
michiokaku <- left_join(michiokaku, coded, by = "id", all = TRUE)

natgeo <- read_excel("historical_files/natgeo_2014_to_June_2017_compiled.xlsx")
natgeo <- left_join(natgeo, coded, by = "id", all = TRUE)

discovery <- read_excel("historical_files/Discovery_2014_to_June_2017_compiled.xlsx")
discovery <- left_join(discovery, coded, by = "id", all = TRUE)

animalplanet <- read_excel("historical_files/AnimalPlanet_2014_to_June_2017_compiled.xlsx")
animalplanet <- left_join(animalplanet, coded, by = "id", all = TRUE)

nasa <- read_excel("historical_files/NASA3_2014_to_June_2017_compiled.xlsx")
nasa <- left_join(nasa, coded, by = "id", all = TRUE)

nasaearth <- read_excel("historical_files/nasaearth_2014_to_June_2017_compiled.xlsx")
nasaearth <- left_join(nasaearth, coded, by = "id", all = TRUE)

##had trouble with column BD in file, so cleared it out before importing
womenshealthmagazine <- read_excel("historical_files/womenshealthmagazine2_2014_to_June_2017_compiled.xlsx")
womenshealthmagazine <- left_join(womenshealthmagazine, coded, by = "id", all = TRUE)

psychologytoday <- read_excel("historical_files/psychologytoday_2014_to_June_2017_compiled.xlsx")
psychologytoday <- left_join(psychologytoday, coded, by = "id", all = TRUE)
##had some duplicate IDs, but did not seem to cause problem when joining because none of them were coded

sciencechannel <- read_excel("historical_files/ScienceChannel_2014_to_June_2017_compiled.xlsx")
sciencechannel <- left_join(sciencechannel, coded, by = "id", all = TRUE)

mythbusters <- read_excel("historical_files/mythbusters_2014_to_June_2017_compiled.xlsx")
mythbusters <- left_join(mythbusters, coded, by = "id", all = TRUE)

health <- read_excel("historical_files/health_2014_to_June_2017_compiled.xlsx")
health <- left_join(health, coded, by = "id", all = TRUE)
health[2] <- "Health"

newscientist <- read_excel("historical_files/newscientist_2014_to_June_2017_compiled.xlsx")
newscientist <- left_join(newscientist, coded, by = "id", all = TRUE)

sciencemagazine <- read_excel("historical_files/ScienceMagazine_2014_to_June_2017_compiled.xlsx")
sciencemagazine <- left_join(sciencemagazine, coded, by = "id", all = TRUE)

popsci <- read_excel("historical_files/popsci_2014_to_June_2017_compiled.xlsx")
popsci <- left_join(popsci, coded, by = "id", all = TRUE)

physicstoday <- read_excel("historical_files/physicstoday_2014_to_June_2017_compiled.xlsx")
physicstoday <- left_join(physicstoday, coded, by = "id", all = TRUE)

all <- rbind(billnye, bbcearth, droz, ifls, healthdigest, davidwolfe, sciencealert, sciencenaturepage, interestingengineering, enjoyscience, neildegrassetyson, stephenhawking, sciencedump, mindbodygreen, dailyhealthtips, michiokaku, natgeo, discovery, animalplanet, nasa, nasaearth, womenshealthmagazine, psychologytoday, sciencechannel, mythbusters, health, newscientist, sciencemagazine, popsci, physicstoday)
all <- all[!is.na(all$`FB page`),]   #removes blank cases at end







##format columns and change names
all$`number of shares` <- as.integer(all$`number of shares`) #has some 'no # of shares' values to deal with
all$year <- as.integer(all$year)
all$month <- as.integer(all$month)
all$day <- as.integer(all$day)
colnames(all)[2] <- "FBPage"
colnames(all)[9] <- "created_time"
colnames(all)[14] <- "total_reactions"   #total reactions is the combination of likes, dislikes, wows, etc. it does NOT include shares and comments
colnames(all)[15] <- "likes"
colnames(all)[16] <- "wows"
colnames(all)[17] <- "sads"
colnames(all)[18] <- "loves"
colnames(all)[19] <- "hahas"
colnames(all)[20] <- "angrys"
colnames(all)[21] <- "thankfuls"
colnames(all)[22] <- "shares"
colnames(all)[38] <- "comments"


all <- all %>% mutate(quarter = "na")
attach(all)
all$quarter[month > 0 & month < 4] <- "first"
all$quarter[month > 3 & month < 7] <- "second"
all$quarter[month > 6 & month < 10] <- "third"
all$quarter[month > 9] <- "fourth"
all$others_link[others_link == "0"] <- "linked to own organization"
all$others_link[others_link == "1"] <- "linked to a different organization"
all$others_link[others_link == "50"] <- "no link"
detach(all)
all$yearqtr <- paste(all$year, all$quarter)
all$yearqtr <- factor(all$yearqtr, levels = c("2014 first", "2014 second", "2014 third", "2014 fourth","2015 first", "2015 second", "2015 third", "2015 fourth", "2016 first", "2016 second", "2016 third", "2016 fourth", "2017 first", "2017 second"))
all$total_interactions <- rowSums(cbind(all$total_reactions, all$shares, all$comments), na.rm = TRUE)
##all$total_interactionstest <- rowSums(cbind(all$total_reactions, all$shares, all$comments)) #doesn't work
blank <- "              "





#remove columns not needed
all$X_1 <- NULL
all$X__2 <- NULL
all$rand <- NULL
all$`API page` <- NULL
all$`post #` <- NULL
all$X__1 <- NULL
all$id1 <- NULL
all$createdtime_fromcoded <- NULL
colnames(all)[57] <- "case_number_from_coding"
colnames(all)[56] <- "link_website_combined"
all$month_fromcoded <- NULL
all$day_fromcoded <- NULL
all$year_fromcoded <- NULL
all$quarter_fromcoded <- NULL
all$message_fromcoded <- NULL


#recode into page types

all <- all %>% mutate(pagetype = "na")
attach(all)
all$pagetype[FBPage == "billnye" | FBPage == "IFeakingLoveScience" | FBPage == "healthdigest" | FBPage == "DavidAvocadoWolfe" | FBPage == "ScienceAlert" | FBPage == "ScienceNaturePage" | FBPage == "interestingengineering" | FBPage == "enjoy.science" | FBPage == "droz" | FBPage == "neildegrassetyson" | FBPage == "stephenhawking" | FBPage == "sciencedump" | FBPage == "mindbodygreen" | FBPage == "DailyHealthTips" | FBPage == "michiokaku"] <- "FB-primary" 

all$pagetype[FBPage == "bbcearth" | FBPage == "natgeo" | FBPage == "Discovery" | FBPage == "AnimalPlanet" | FBPage == "NASA" | FBPage == "nasaearth" | FBPage == "womenshealthmagazine" | FBPage == "psychologytoday" | FBPage == "ScienceChannel" | FBPage == "MythBusters" | FBPage == "Health" | FBPage == "newscientist" | FBPage == "ScienceMagazine" | FBPage == "PopSci" | FBPage == "PhysicsToday"] <- "multi-platform"
detach(all)


####rename page names

all <- all %>% mutate(FBPage = recode(FBPage, "natgeo" = "National Geographic", "billnye" = "Bill Nye", "bbcearth" = "BBC Earth", "healthdigest" = "Health Digest", "DavidAvocadoWolfe" = "David Wolfe", "ScienceAlert" = "Science Alert", "ScienceNaturePage" = "Science Nature Page (Hashem Al-Ghaili)", "interestingengineering" = "Interesting Engineering", "enjoy.science" = "Smart is the New Sexy (@enjoy.science)", "droz" = "Dr. Oz", "neildegrassetyson" = "Neil deGrasse Tyson", "stephenhawking" = "Stephen Hawking", "sciencedump" = "Science Dump", "mindbodygreen" = "Mindbodygreen", "michiokaku" = "Dr. Michio Kaku", "AnimalPlanet" = "Animal Planet", "nasaearth" = "NASA Earth", "womenshealthmagazine" = "Womens Health Magazine", "psychologytoday" = "Psychology Today", "ScienceChannel" = "Science Channel", "newscientist" = "New Scientist", "ScienceMagazine" = "Science Magazine", "PopSci" = "Popular Science", "PhysicsToday" = "Physics Today"))


                            

##reorder pages

all$FBPage <-factor(all$FBPage, levels = c("IFeakingLoveScience", "Health Digest","David Wolfe", "Science Alert", "Science Nature Page (Hashem Al-Ghaili)", "Interesting Engineering", "Smart is the New Sexy (@enjoy.science)", "Dr. Oz", "Bill Nye", "Neil deGrasse Tyson", "Stephen Hawking","Science Dump", "Mindbodygreen", "DailyHealthTips", "Dr. Michio Kaku", "National Geographic", "Discovery", "Animal Planet", "NASA", "NASA Earth", "Womens Health Magazine", "Psychology Today", "Science Channel", "MythBusters", "BBC Earth", "Health", "New Scientist", "Science Magazine", "Popular Science", "Physics Today"))



##make list

pagelist <- c("IFeakingLoveScience", "Health Digest","David Wolfe", "Science Alert", "Science Nature Page (Hashem Al-Ghaili)", "Interesting Engineering", "Smart is the New Sexy (@enjoy.science)", "Dr. Oz", "Bill Nye", "Neil deGrasse Tyson", "Stephen Hawking","Science Dump", "Mindbodygreen", "DailyHealthTips", "Dr. Michio Kaku", "National Geographic", "Discovery", "Animal Planet", "NASA", "NASA Earth", "Womens Health Magazine", "Psychology Today", "Science Channel", "MythBusters", "BBC Earth", "Health", "New Scientist", "Science Magazine", "Popular Science", "Physics Today")









##coming up with FINAL subsetof coded cases
codedfinal <- filter(all, Selected_for_coding_fromcodingfile == 1 | Selected_for_coding_fromcodingfile == 2)
##change name of old coded dataframe
codedorig <- coded
remove(coded)










##running tables with all historical data
##by group


##number of posts
total <- addmargins(table(all$pagetype, all$year))

sink('output.csv')
write.csv("historical data - all posts from 2014 to June 2017")
write.csv(blank)
write.csv(blank)
write.csv("by type of page")
write.csv(blank)
write.csv(blank)
write.csv(blank)
write.csv("number of posts per year")
write.csv(total, row.names=TRUE)
write.csv(blank)


total2 <- addmargins(table(all$pagetype, all$yearqtr))

write.csv("number of posts per qtr")
write.csv(total2, row.names=TRUE)
write.csv(blank)


#total interactions
total3 <- addmargins(with(all, tapply(total_interactions, list(pagetype=pagetype,year=year), sum)))

write.csv("total number of interactions per year (likes, comments and shares combined)")
write.csv(total3, row.names=TRUE)
write.csv(blank)

total4 <- addmargins(with(all, tapply(total_interactions, list(pagetype=pagetype,qtr=yearqtr), sum)))

write.csv("total number of interactions per quarter (likes, comments and shares combined)")
write.csv(total4, row.names=TRUE)
write.csv(blank)

total5 <- with(all, tapply(total_interactions, list(pagetype=pagetype,year=year), mean))

write.csv("avg interactions per post (likes, comments and shares combined)")
write.csv(total5, row.names=TRUE)
write.csv(blank)

total6 <- with(all, tapply(total_interactions, list(pagetype=pagetype,qtr=yearqtr), mean))

write.csv("avg interactions per post (likes, comments and shares combined)")
write.csv(total6, row.names=TRUE)
write.csv(blank)

total7 <- with(all, tapply(total_interactions, list(year=year), mean))

write.csv("ALL POSTS - avg interactions per post (likes, comments and shares combined)")
write.csv(total7, row.names=TRUE)
write.csv(blank)

total8 <- with(all, tapply(total_interactions, list(qtr=yearqtr), mean))

write.csv("ALL POSTS - avg interactions per post (likes, comments and shares combined)")
write.csv(total8, row.names=TRUE)
write.csv(blank)

total9 <- mean(all$total_interactions)

write.csv("avg interactions per ALL posts in sample (likes, comments and shares combined)")
write.csv(total9, row.names=TRUE)
write.csv(blank)







##number of reactions (liks, sads, etc.)
##discovered on 8/29/17 that if you use rm.na=T when there are no NAs, you get a wrong number. So I fixed the below.

total10 <- addmargins(with(all, tapply(total_reactions, list(pagetype=pagetype,year=year), sum)))


write.csv("total number of reactions per year (likes, sads, hahas, wows, etc.)")
write.csv(total10, row.names=TRUE)
write.csv(blank)

total11 <- addmargins(with(all, tapply(total_reactions, list(pagetype=pagetype,qtr=yearqtr), sum)))

write.csv("total number of reactions per qtr (likes, sads, hahas, wows, etc.)")
write.csv(total11, row.names=TRUE)
write.csv(blank)

total12 <- with(all, tapply(total_reactions, list(pagetype=pagetype,year=year), mean))

write.csv("avg reactions per post (likes, sads, hahas, wows, etc.)")
write.csv(total12, row.names=TRUE)
write.csv(blank)


total13 <- with(all, tapply(total_reactions, list(pagetype=pagetype,qtr=yearqtr), mean))

write.csv("avg reactions per post (likes, comments and shares combined)")
write.csv(total13, row.names=TRUE)
write.csv(blank)


total14 <- with(all, tapply(total_reactions, list(year=year), mean))

write.csv("ALL POSTS - avg reactions per post (likes, comments and shares combined)")
write.csv(total14, row.names=TRUE)
write.csv(blank)


total15 <- with(all, tapply(total_reactions, list(qtr=yearqtr), mean))

write.csv("ALL POSTS - avg reactions per post (likes, comments and shares combined)")
write.csv(total15, row.names=TRUE)
write.csv(blank)

total16 <- mean(all$total_reactions)

write.csv("ALL POSTS - avg reactions per post (likes, comments and shares combined)")
write.csv(total16, row.names=TRUE)
write.csv(blank)


##number of shares

allshares <- filter(all, is.na(shares) == FALSE)


total17 <- addmargins(with(allshares, tapply(shares, list(pagetype=pagetype,year=year), sum)))

write.csv("total number of shares per year")
write.csv(total17, row.names=TRUE)
write.csv(blank)


total18 <- addmargins(with(allshares, tapply(shares, list(pagetype=pagetype,qtr=yearqtr), sum)))

write.csv("total number of shares per quarter")
write.csv(total18, row.names=TRUE)
write.csv(blank)

total19 <- with(allshares, tapply(shares, list(pagetype=pagetype,year=year), mean))

write.csv("avg shares per post")
write.csv(total19, row.names=TRUE)
write.csv(blank)


total20 <- with(allshares, tapply(shares, list(pagetype=pagetype,qtr=yearqtr), mean))


write.csv("avg shares per post")
write.csv(total20, row.names=TRUE)
write.csv(blank)


total21 <- with(allshares, tapply(shares, list(year=year), mean))

write.csv("ALL POSTS - avg shares per post")
write.csv(total21, row.names=TRUE)
write.csv(blank)


total22 <- with(allshares, tapply(shares, list(qtr=yearqtr), mean))

write.csv("ALL POSTS - avg shares per post")
write.csv(total22, row.names=TRUE)
write.csv(blank)

total23 <- mean(allshares$shares)

write.csv("ALL POSTS - avg shares per post")
write.csv(total23, row.names=TRUE)
write.csv(blank)



##number of comments

total24 <- addmargins(with(all, tapply(comments, list(pagetype=pagetype,year=year), sum)))

write.csv("total number of comments per year")
write.csv(total24, row.names=TRUE)
write.csv(blank)


total25 <- addmargins(with(all, tapply(comments, list(pagetype=pagetype,qtr=yearqtr), sum)))

write.csv("total number of comments per quarter")
write.csv(total25, row.names=TRUE)
write.csv(blank)

total26 <- with(all, tapply(comments, list(pagetype=pagetype,year=year), mean))

write.csv("avg comments per post")
write.csv(total26, row.names=TRUE)
write.csv(blank)


total27 <- with(all, tapply(comments, list(pagetype=pagetype,qtr=yearqtr), mean))

write.csv("avg comments per post")
write.csv(total27, row.names=TRUE)
write.csv(blank)


total28 <- with(all, tapply(comments, list(year=year), mean))

write.csv("ALL POSTS - avg comments per post")
write.csv(total28, row.names=TRUE)
write.csv(blank)


total29 <- with(all, tapply(comments, list(qtr=yearqtr), mean))

write.csv("ALL POSTS - avg comments per post")
write.csv(total29, row.names=TRUE)
write.csv(blank)

total30 <- mean(all$comments)

write.csv("ALL POSTS - avg comments per post")
write.csv(total30, row.names=TRUE)
write.csv(blank)



##others link

all$others_link <- as.factor(all$others_link)

total31 <- summary(all$others_link)
total31a <- cbind(total31, prop.table(total31)*100)

write.csv("LINKS - whether the site linked to their own work, or a page from another place")
write.csv(total31a, row.names=TRUE)
write.csv(blank)

total32 <- with(all, table(others_link, year))
total32pct <- prop.table(total32, margin=2)
colnames(total32pct) <- paste("percent", colnames(total32pct), sep=".")
tbl32 <- addmargins(cbind(total32, total32pct*100)[, c(matrix(1:(2*ncol(total32)), nrow=2, byrow=2))])

write.csv("LINKS - whether the site linked to their own work, or a page from another place")
write.csv(tbl32, row.names=TRUE)
write.csv(blank)


total33 <- with(all, table(others_link, pagetype))
total33pct <- prop.table(total33, margin=2)
colnames(total33pct) <- paste("percent", colnames(total33pct), sep=".")
tbl33 <- addmargins(cbind(total33, total33pct*100)[, c(matrix(1:(2*ncol(total33)), nrow=2, byrow=2))])

write.csv("LINKS - whether the site linked to their own work, or a page from another place")
write.csv(tbl33, row.names=TRUE)
write.csv(blank)

FBprimary <- filter(all, pagetype == "FB-primary")
multiplat <- filter(all, pagetype == "multi-platform")


total34 <- with(FBprimary, table(others_link, year))
total34pct <- prop.table(total34, margin=2)
colnames(total34pct) <- paste("percent", colnames(total34pct), sep=".")
tbl34 <- addmargins(cbind(total34, total34pct*100)[, c(matrix(1:(2*ncol(total34)), nrow=2, byrow=2))])

write.csv("FB-PRIMARY pages only: LINKS - whether the site linked to their own work, or a page from another place")
write.csv("NOTE: DELETE the sum table here")
write.csv(tbl34, row.names=TRUE)
write.csv(blank)

total35 <- with(multiplat, table(others_link, year))
total35pct <- prop.table(total35, margin=2)
colnames(total35pct) <- paste("percent", colnames(total35pct), sep=".")
tbl35 <- addmargins(cbind(total35, total35pct*100)[, c(matrix(1:(2*ncol(total35)), nrow=2, byrow=2))])

write.csv("MULTI-PLATFORM pages only: LINKS - whether the site linked to their own work, or a page from another place")
write.csv("NOTE: DELETE the sum table here")
write.csv(tbl35, row.names=TRUE)
write.csv(blank)













##running tables with all historical data
##by page

write.csv("historical data - all posts from 2014 to June 2017")

##number of posts
total <- addmargins(table(all$FBPage, all$year))


write.csv("historical data - all posts from 2014 to June 2017")
write.csv(blank)
write.csv(blank)
write.csv("by individual page")
write.csv(blank)
write.csv(blank)
write.csv(blank)
write.csv("number of posts per year")
write.csv(total, row.names=TRUE)
write.csv(blank)


total2 <- addmargins(table(all$FBPage, all$yearqtr))

write.csv("number of posts per qtr")
write.csv(total2, row.names=TRUE)
write.csv(blank)






#total interactions

total3 <- addmargins(with(all, tapply(total_interactions, list(page=FBPage,year=year), sum)))

write.csv("total number of interactions per year (likes, comments and shares combined)")
write.csv(total3, row.names=TRUE)
write.csv(blank)


total4 <- addmargins(with(all, tapply(total_interactions, list(page=FBPage,qtr=yearqtr), sum)))

write.csv("total number of interactions per quarter (likes, comments and shares combined)")
write.csv(total4, row.names=TRUE)
write.csv(blank)

total5 <- with(all, tapply(total_interactions, list(page=FBPage,year=year), mean))

write.csv("avg interactions per post (likes, comments and shares combined)")
write.csv(total5, row.names=TRUE)
write.csv(blank)


total6 <- with(all, tapply(total_interactions, list(page=FBPage,qtr=yearqtr), mean))

write.csv("avg interactions per post (likes, comments and shares combined)")
write.csv(total6, row.names=TRUE)
write.csv(blank)

total7 <- group_by(all, FBPage) %>% summarise(avg=mean(total_interactions))

write.csv("ALL POSTS - avg interactions per post (likes, comments and shares combined)")
write.csv(total7, row.names=TRUE)
write.csv(blank)


##took out total8 table


total9 <- mean(all$total_interactions)

write.csv("avg interactions per ALL posts in sample (likes, comments and shares combined)")
write.csv(total9, row.names=TRUE)
write.csv(blank)




##number of reactions (liks, sads, etc.)

total10 <- with(all, tapply(total_reactions, list(page=FBPage,year=year), sum))

write.csv("total number of reactions per year (likes, sads, hahas, wows, etc.)")
write.csv(total10, row.names=TRUE)
write.csv(blank)

total11 <- with(all, tapply(total_reactions, list(page=FBPage,qtr=yearqtr), sum))

write.csv("total number of reactions per qtr (likes, sads, hahas, wows, etc.)")
write.csv(total11, row.names=TRUE)
write.csv(blank)

total12 <- with(all, tapply(total_reactions, list(page=FBPage,year=year), mean))

write.csv("avg reactions per post (likes, sads, hahas, wows, etc.)")
write.csv(total12, row.names=TRUE)
write.csv(blank)


total13 <- with(all, tapply(total_reactions, list(page=FBPage,qtr=yearqtr), mean))

write.csv("avg reactions per post (likes, comments and shares combined)")
write.csv(total13, row.names=TRUE)
write.csv(blank)


total14 <- with(all, tapply(total_reactions, list(year=year), mean))

write.csv("ALL POSTS - avg reactions per post (likes, comments and shares combined)")
write.csv(total14, row.names=TRUE)
write.csv(blank)


total15 <- with(all, tapply(total_reactions, list(qtr=yearqtr), mean))

write.csv("ALL POSTS - avg reactions per post (likes, comments and shares combined)")
write.csv(total15, row.names=TRUE)
write.csv(blank)

total16 <- mean(all$total_reactions)

write.csv("ALL POSTS - avg reactions per post (likes, comments and shares combined)")
write.csv(total16, row.names=TRUE)
write.csv(blank)

total16a <- group_by(all, FBPage) %>% summarise(avg=mean(total_reactions))

write.csv("ALL POSTS - avg reactions per post")
write.csv(total16a, row.names=TRUE)
write.csv(blank)



##number of shares

total17 <- with(allshares, tapply(shares, list(page=FBPage,year=year), sum))

write.csv("total number of shares per year")
write.csv(total17, row.names=TRUE)
write.csv(blank)


total18 <- with(allshares, tapply(shares, list(page=FBPage,qtr=yearqtr), sum))

write.csv("total number of shares per quarter")
write.csv(total18, row.names=TRUE)
write.csv(blank)

total19 <- with(allshares, tapply(shares, list(page=FBPage,year=year), mean))

write.csv("avg shares per post")
write.csv(total19, row.names=TRUE)
write.csv(blank)


total20 <- with(allshares, tapply(shares, list(page=FBPage,qtr=yearqtr), mean))

write.csv("avg shares per post")
write.csv(total20, row.names=TRUE)
write.csv(blank)


total21 <- with(allshares, tapply(shares, list(year=year), mean))

write.csv("ALL POSTS - avg shares per post")
write.csv(total21, row.names=TRUE)
write.csv(blank)


total22 <- with(allshares, tapply(shares, list(qtr=yearqtr), mean))

write.csv("ALL POSTS - avg shares per post")
write.csv(total22, row.names=TRUE)
write.csv(blank)

total23 <- mean(allshares$shares)

write.csv("ALL POSTS - avg shares per post")
write.csv(total23, row.names=TRUE)
write.csv(blank)

total23a <- group_by(allshares, FBPage) %>% summarise(avg=mean(shares))

write.csv("ALL POSTS - avg shares per post")
write.csv(total23a, row.names=TRUE)
write.csv(blank)




##number of comments

total24 <- with(all, tapply(comments, list(page=FBPage,year=year), sum))

write.csv("total number of comments per year")
write.csv(total24, row.names=TRUE)
write.csv(blank)


total25 <- with(all, tapply(comments, list(page=FBPage,qtr=yearqtr), sum))

write.csv("total number of comments per quarter")
write.csv(total25, row.names=TRUE)
write.csv(blank)

total26 <- with(all, tapply(comments, list(page=FBPage,year=year), mean))

write.csv("avg comments per post")
write.csv(total26, row.names=TRUE)
write.csv(blank)


total27 <- with(all, tapply(comments, list(page=FBPage,qtr=yearqtr), mean))

write.csv("avg comments per post")
write.csv(total27, row.names=TRUE)
write.csv(blank)

total27a <- group_by(all, FBPage) %>% summarise(avg=mean(comments))

write.csv("ALL POSTS - avg comments per post")
write.csv(total27a, row.names=TRUE)
write.csv(blank)




##others link

total33 <- with(all, table(others_link, FBPage))
total33pct <- prop.table(total33, margin=2)
colnames(total33pct) <- paste("percent", colnames(total33pct), sep=".")
tbl33 <- addmargins(cbind(total33, total33pct*100)[, c(matrix(1:(2*ncol(total33)), nrow=2, byrow=2))])

write.csv("LINKS - whether the site linked to their own work, or a page from another place")
write.csv(tbl33, row.names=TRUE)
write.csv(blank)



yr2014 <- filter(all, year == "2014")


total34 <- with(yr2014, table(others_link, FBPage))
total34pct <- prop.table(total34, margin=2)
colnames(total34pct) <- paste("percent", colnames(total34pct), sep=".")
tbl34 <- addmargins(cbind(total34, total34pct*100)[, c(matrix(1:(2*ncol(total34)), nrow=2, byrow=2))])

write.csv("Year 2014: LINKS - whether the site linked to their own work, or a page from another place")
write.csv(tbl34, row.names=TRUE)
write.csv(blank)




yr2015 <- filter(all, year == "2015")


total35 <- with(yr2015, table(others_link, FBPage))
total35pct <- prop.table(total35, margin=2)
colnames(total35pct) <- paste("percent", colnames(total35pct), sep=".")
tbl35 <- addmargins(cbind(total35, total35pct*100)[, c(matrix(1:(2*ncol(total35)), nrow=2, byrow=2))])

write.csv("Year 2015: LINKS - whether the site linked to their own work, or a page from another place")
write.csv(tbl35, row.names=TRUE)
write.csv(blank)


yr2016 <- filter(all, year == "2016")


total36 <- with(yr2016, table(others_link, FBPage))
total36pct <- prop.table(total36, margin=2)
colnames(total36pct) <- paste("percent", colnames(total36pct), sep=".")
tbl36 <- addmargins(cbind(total36, total36pct*100)[, c(matrix(1:(2*ncol(total36)), nrow=2, byrow=2))])

write.csv("Year 2016: LINKS - whether the site linked to their own work, or a page from another place")
write.csv(tbl36, row.names=TRUE)
write.csv(blank)


yr2017 <- filter(all, year == "2017")


total37 <- with(yr2017, table(others_link, FBPage))
total37pct <- prop.table(total37, margin=2)
colnames(total37pct) <- paste("percent", colnames(total37pct), sep=".")
tbl37 <- addmargins(cbind(total37, total37pct*100)[, c(matrix(1:(2*ncol(total37)), nrow=2, byrow=2))])

write.csv("Year 2017: LINKS - whether the site linked to their own work, or a page from another place")
write.csv(tbl37, row.names=TRUE)
write.csv(blank)










##running tables with coded cases only
##by group


##number of posts

total <- addmargins(table(codedfinal$pagetype, codedfinal$yearqtr))

write.csv("coded data - all posts from 2014 to June 2017")
write.csv(blank)
write.csv(blank)
write.csv("by type of page")
write.csv(blank)
write.csv(blank)
write.csv(blank)
write.csv("number of posts in sample")
write.csv(total, row.names=TRUE)
write.csv(blank)

##area of science
total40 <- table(codedfinal$areagrouped, codedfinal$pagetype)
total40pct <- prop.table(total40, margin=2)
colnames(total40pct) <- paste("percent", colnames(total40pct), sep=".")
tbl40 <- cbind(total40, total40pct*100)[, c(matrix(1:(2*ncol(total40)), nrow=2, byrow=2))]

total40 <- table(codedfinal$areagrouped)
tbl41 <- cbind(tbl40, total41, prop.table(total41)*100)


write.csv("area of science (non-science categories grouped) by page type")
write.csv(tbl41, row.names=TRUE)
write.csv(blank)

tbl42 <- table(codedfinal$areaofscience, codedfinal$pagetype)
tbl42pct <- prop.table(tbl42, margin=2)
colnames(tbl42pct) <- paste("percent", colnames(tbl42pct), sep=".")
tbl43 <- cbind(tbl42, tbl42pct*100)[, c(matrix(1:(2*ncol(tbl42)), nrow=2, byrow=2))]

tbl44 <- table(codedfinal$areaofscience)
tbl44 <- cbind(tbl43, tbl44, prop.table(tbl44)*100)

write.csv("area of science (non-science categories separated out) by page type")
write.csv(tbl44, row.names=TRUE)
write.csv(blank)

codedfinal$yearqtr <-factor(codedfinal$yearqtr, levels = c("2017 first", "2017 second"))

attach(codedfinal)
codedfinal$month[month == "1"] <- "Jan"
codedfinal$month[month == "2"] <- "Feb"
codedfinal$month[month == "3"] <- "Mar"
codedfinal$month[month == "4"] <- "Apr"
codedfinal$month[month == "5"] <- "May"
codedfinal$month[month == "6"] <- "Jun"
detach(codedfinal)
codedfinal$month <-factor(codedfinal$month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun"))

codedFBprimary <- filter(codedfinal, pagetype == "FB-primary")
codedmultiplat <- filter(codedfinal, pagetype == "multi-platform")

tbl45 <- table(codedFBprimary$areagrouped, codedFBprimary$month)
tbl45pct <- prop.table(tbl45, margin=2)
colnames(tbl45pct) <- paste("percent", colnames(tbl45pct), sep=".")
tbl46 <- cbind(tbl45, tbl45pct*100)[, c(matrix(1:(2*ncol(tbl45)), nrow=2, byrow=2))]

write.csv("FB-PRIMARY ONLY: area of science by month")
write.csv(tbl46, row.names=TRUE)
write.csv(blank)

tbl47 <- table(codedmultiplat$areagrouped, codedmultiplat$month)
tbl47pct <- prop.table(tbl47, margin=2)
colnames(tbl47pct) <- paste("percent", colnames(tbl47pct), sep=".")
tbl48 <- cbind(tbl47, tbl47pct*100)[, c(matrix(1:(2*ncol(tbl47)), nrow=2, byrow=2))]

write.csv("MULTIPLATFORM ONLY: area of science by month")
write.csv(tbl48, row.names=TRUE)
write.csv(blank)

##storyline (only grouped run here)

total49 <- table(codedfinal$storylinegrouped, codedfinal$pagetype)
total49pct <- prop.table(total49, margin=2)
colnames(total49pct) <- paste("percent", colnames(total49pct), sep=".")
tbl50 <- cbind(total49, total49pct*100)[, c(matrix(1:(2*ncol(total49)), nrow=2, byrow=2))]

total50 <- table(codedfinal$storylinegrouped)
tbl51 <- cbind(tbl50, total50, prop.table(total50)*100)

write.csv("storyline by page type")
write.csv(tbl51, row.names=TRUE)
write.csv(blank)

tbl52 <- table(codedFBprimary$storylinegrouped, codedFBprimary$month)
tbl52pct <- prop.table(tbl52, margin=2)
colnames(tbl52pct) <- paste("percent", colnames(tbl52pct), sep=".")
tbl53 <- cbind(tbl52, tbl52pct*100)[, c(matrix(1:(2*ncol(tbl52)), nrow=2, byrow=2))]

write.csv("FB-PRIMARY ONLY: storyline by month")
write.csv(tbl53, row.names=TRUE)
write.csv(blank)

tbl54 <- table(codedmultiplat$storylinegrouped, codedmultiplat$month)
tbl54pct <- prop.table(tbl54, margin=2)
colnames(tbl54pct) <- paste("percent", colnames(tbl54pct), sep=".")
tbl55 <- cbind(tbl54, tbl54pct*100)[, c(matrix(1:(2*ncol(tbl54)), nrow=2, byrow=2))]

write.csv("MULTIPLATFORM ONLY: storyline by month")
write.csv(tbl55, row.names=TRUE)
write.csv(blank)

##primary focus
total56 <- table(codedfinal$focusgrouped, codedfinal$pagetype)
total56pct <- prop.table(total56, margin=2)
colnames(total56pct) <- paste("percent", colnames(total56pct), sep=".")
tbl57 <- cbind(total56, total56pct*100)[, c(matrix(1:(2*ncol(total56)), nrow=2, byrow=2))]

total58 <- table(codedfinal$focusgrouped)
tbl58 <- cbind(tbl57, total58, prop.table(total58)*100)

write.csv("focus by page type")
write.csv(tbl58, row.names=TRUE)
write.csv(blank)

tbl59 <- table(codedFBprimary$focusgrouped, codedFBprimary$month)
tbl59pct <- prop.table(tbl59, margin=2)
colnames(tbl59pct) <- paste("percent", colnames(tbl59pct), sep=".")
tbl60 <- cbind(tbl59, tbl59pct*100)[, c(matrix(1:(2*ncol(tbl59)), nrow=2, byrow=2))]

write.csv("FB-PRIMARY ONLY: focus by month")
write.csv(tbl60, row.names=TRUE)
write.csv(blank)

tbl61 <- table(codedmultiplat$focusgrouped, codedmultiplat$month)
tbl61pct <- prop.table(tbl61, margin=2)
colnames(tbl61pct) <- paste("percent", colnames(tbl61pct), sep=".")
tbl62 <- cbind(tbl61, tbl61pct*100)[, c(matrix(1:(2*ncol(tbl61)), nrow=2, byrow=2))]

write.csv("MULTIPLATFORM ONLY: focus by month")
write.csv(tbl62, row.names=TRUE)
write.csv(blank)

##external links to stuff (did not run by time)

total63 <- table(codedfinal$linktoresearch, codedfinal$pagetype)
total63pct <- prop.table(total63, margin=2)
colnames(total63pct) <- paste("percent", colnames(total63pct), sep=".")
tbl64 <- cbind(total63, total63pct*100)[, c(matrix(1:(2*ncol(total63)), nrow=2, byrow=2))]

total65 <- table(codedfinal$linktoresearch)
tbl65 <- cbind(tbl64, total65, prop.table(total65)*100)

write.csv("link to external evidentiary research")
write.csv(tbl65, row.names=TRUE)
write.csv(blank)


total63 <- table(codedfinal$linktonews, codedfinal$pagetype)
total63pct <- prop.table(total63, margin=2)
colnames(total63pct) <- paste("percent", colnames(total63pct), sep=".")
tbl64 <- cbind(total63, total63pct*100)[, c(matrix(1:(2*ncol(total63)), nrow=2, byrow=2))]

total65 <- table(codedfinal$linktonews)
tbl65 <- cbind(tbl64, total65, prop.table(total65)*100)

write.csv("link to external news source")
write.csv(tbl65, row.names=TRUE)
write.csv(blank)

##stats

total63 <- table(codedfinal$stats, codedfinal$pagetype)
total63pct <- prop.table(total63, margin=2)
colnames(total63pct) <- paste("percent", colnames(total63pct), sep=".")
tbl64 <- cbind(total63, total63pct*100)[, c(matrix(1:(2*ncol(total63)), nrow=2, byrow=2))]

total65 <- table(codedfinal$stats)
tbl65 <- cbind(tbl64, total65, prop.table(total65)*100)

write.csv("include any statistics?")
write.csv(tbl65, row.names=TRUE)
write.csv(blank)







##by individual page
##area of science
total40 <- table(codedfinal$areagrouped, codedfinal$FBPage)
total40pct <- prop.table(total40, margin=2)
colnames(total40pct) <- paste("percent", colnames(total40pct), sep=".")
tbl40 <- cbind(total40, total40pct*100)[, c(matrix(1:(2*ncol(total40)), nrow=2, byrow=2))]

write.csv("area of science (non-science categories grouped) by page")
write.csv(tbl40, row.names=TRUE)
write.csv(blank)

tbl42 <- table(codedfinal$areaofscience, codedfinal$FBPage)
tbl42pct <- prop.table(tbl42, margin=2)
colnames(tbl42pct) <- paste("percent", colnames(tbl42pct), sep=".")
tbl43 <- cbind(tbl42, tbl42pct*100)[, c(matrix(1:(2*ncol(tbl42)), nrow=2, byrow=2))]

write.csv("area of science (non-science categories separated out) by page")
write.csv(tbl43, row.names=TRUE)
write.csv(blank)

##storyline (only grouped run here)

total49 <- table(codedfinal$storylinegrouped, codedfinal$FBPage)
total49pct <- prop.table(total49, margin=2)
colnames(total49pct) <- paste("percent", colnames(total49pct), sep=".")
tbl50 <- cbind(total49, total49pct*100)[, c(matrix(1:(2*ncol(total49)), nrow=2, byrow=2))]

write.csv("storyline by page")
write.csv(tbl50, row.names=TRUE)
write.csv(blank)


##primary focus
total56 <- table(codedfinal$focusgrouped, codedfinal$FBPage)
total56pct <- prop.table(total56, margin=2)
colnames(total56pct) <- paste("percent", colnames(total56pct), sep=".")
tbl57 <- cbind(total56, total56pct*100)[, c(matrix(1:(2*ncol(total56)), nrow=2, byrow=2))]

write.csv("focus by page")
write.csv(tbl57, row.names=TRUE)
write.csv(blank)


##links to external stuff (did not run by time)

total63 <- table(codedfinal$linktoresearch, codedfinal$FBPage)
total63pct <- prop.table(total63, margin=2)
colnames(total63pct) <- paste("percent", colnames(total63pct), sep=".")
tbl64 <- cbind(total63, total63pct*100)[, c(matrix(1:(2*ncol(total63)), nrow=2, byrow=2))]

write.csv("link to external evidentiary research")
write.csv(tbl64, row.names=TRUE)
write.csv(blank)


total63 <- table(codedfinal$linktonews, codedfinal$FBPage)
total63pct <- prop.table(total63, margin=2)
colnames(total63pct) <- paste("percent", colnames(total63pct), sep=".")
tbl64 <- cbind(total63, total63pct*100)[, c(matrix(1:(2*ncol(total63)), nrow=2, byrow=2))]

write.csv("link to external news source")
write.csv(tbl64, row.names=TRUE)
write.csv(blank)

##stats

total63 <- table(codedfinal$stats, codedfinal$FBPage)
total63pct <- prop.table(total63, margin=2)
colnames(total63pct) <- paste("percent", colnames(total63pct), sep=".")
tbl64 <- cbind(total63, total63pct*100)[, c(matrix(1:(2*ncol(total63)), nrow=2, byrow=2))]

write.csv("include any statistics?")
write.csv(tbl64, row.names=TRUE)
write.csv(blank)



##running page types and individual page data all at once

longlist <- c("FB-primary", "multi-platform", pagelist)
n <- 1

while(n < 33){
  j <- longlist[n]
  
  if (n < 3){
  x <- filter(codedfinal, pagetype == j)
  } else {
  x <- filter(codedfinal, FBPage == j)
  }
  
  l <- cat("Data for ",longlist[n])
  write.csv(l)
  write.csv(blank)
  
  ##by area of science
  
  tbl100 <- table(x$areagrouped)
  tbl100 <- cbind(tbl100, prop.table(tbl100)*100)
  
  write.csv("area of science (non-science categories grouped)")
  write.csv(tbl100, row.names=TRUE)
  write.csv(blank)
  
  tbl100 <- table(x$areaofscience)
  tbl100 <- cbind(tbl100, prop.table(tbl100)*100)
  
  write.csv("area of science (non-science categories separated out) by page type ")
  write.csv(tbl100, row.names=TRUE)
  write.csv(blank)
  
  tbl101 <- group_by(x, areagrouped) %>% summarise(mean=mean(total_interactions))
  write.csv("avg number of total interactions per post type (includes likes, shares, comments)")
  write.csv(tbl101, row.names=TRUE)
  write.csv(blank)
  
  tbl102 <- mean(x$total_interactions)
  write.csv("avg number of total interactions per all posts (includes likes, shares, comments)")
  write.csv(tbl102, row.names=TRUE)
  write.csv(blank)
  
  tbl103 <- table(x$areagrouped, x$linktoresearch)
  tbl103pct <- rowPercents(tbl103, digits=1)
  tblcomb <- cbind(tbl103, tbl103pct)
  
  tblcomb <- as.data.frame(tblcomb)
  tblcomb <- tblcomb[,c(1, 3, 2, 4, 6, 5)]
  names(tblcomb)[2] <- paste("percent")
  names(tblcomb)[4] <- paste("percent")
  
  write.csv("percent of posts by science area with links to evidentiary research")
  write.csv(tblcomb, row.names=TRUE)
  write.csv(blank)
  
  tbl103 <- table(x$areagrouped, x$linktonews)
  tbl103pct <- rowPercents(tbl103, digits=1)
  tblcomb <- cbind(tbl103, tbl103pct)
  
  tblcomb <- as.data.frame(tblcomb)
  tblcomb <- tblcomb[,c(1, 3, 2, 4, 6, 5)]
  names(tblcomb)[2] <- paste("percent")
  names(tblcomb)[4] <- paste("percent")
  
  write.csv("percent of posts by science area with links to news")
  write.csv(tblcomb, row.names=TRUE)
  write.csv(blank)
  
  tbl103 <- table(x$areagrouped, x$stats)
  tbl103pct <- rowPercents(tbl103, digits=1)
  tblcomb <- cbind(tbl103, tbl103pct)
  
  tblcomb <- as.data.frame(tblcomb)
  tblcomb <- tblcomb[,c(1, 3, 2, 4, 6, 5)]
  names(tblcomb)[2] <- paste("percent")
  names(tblcomb)[4] <- paste("percent")
  
  write.csv("percent of posts by science area with some stats")
  write.csv(tblcomb, row.names=TRUE)
  write.csv(blank)
  
  
  tbl103 <- table(x$areagrouped, x$others_link)
  tbl103pct <- rowPercents(tbl103, digits=1)
  tblcomb <- cbind(tbl103, tbl103pct)
  
  tblcomb <- as.data.frame(tblcomb)
  tblcomb <- tblcomb[,c(1, 4, 2, 5, 3, 6)]
  names(tblcomb)[2] <- paste("percent")
  names(tblcomb)[4] <- paste("percent")
  names(tblcomb)[6] <- paste("percent")
  
  write.csv("percent of posts by science area that link to other organizations")
  write.csv(tblcomb, row.names=TRUE)
  write.csv(blank)
  
  ##by focus
  tbl100 <- table(x$focusgrouped)
  tbl100 <- cbind(tbl100, prop.table(tbl100)*100)
  
  write.csv("primary focus")
  write.csv(tbl100, row.names=TRUE)
  write.csv(blank)
  
  tbl101 <- group_by(x, focusgrouped) %>% summarise(mean=mean(total_interactions))
  write.csv("avg number of total interactions per post type (focus)")
  write.csv(tbl101, row.names=TRUE)
  write.csv(blank)
  
  tbl103 <- table(x$focusgrouped, x$linktoresearch)
  tbl103pct <- rowPercents(tbl103, digits=1)
  tblcomb <- cbind(tbl103, tbl103pct)
  
  tblcomb <- as.data.frame(tblcomb)
  tblcomb <- tblcomb[,c(1, 3, 2, 4, 6, 5)]
  names(tblcomb)[2] <- paste("percent")
  names(tblcomb)[4] <- paste("percent")
  
  write.csv("percent of posts by focus with links to evidentiary research")
  write.csv(tblcomb, row.names=TRUE)
  write.csv(blank)
  
  tbl103 <- table(x$focusgrouped, x$linktonews)
  tbl103pct <- rowPercents(tbl103, digits=1)
  tblcomb <- cbind(tbl103, tbl103pct)
  
  tblcomb <- as.data.frame(tblcomb)
  tblcomb <- tblcomb[,c(1, 3, 2, 4, 6, 5)]
  names(tblcomb)[2] <- paste("percent")
  names(tblcomb)[4] <- paste("percent")
  
  write.csv("percent of posts by focus with links to news")
  write.csv(tblcomb, row.names=TRUE)
  write.csv(blank)
  
  tbl103 <- table(x$focusgrouped, x$stats)
  tbl103pct <- rowPercents(tbl103, digits=1)
  tblcomb <- cbind(tbl103, tbl103pct)
  
  tblcomb <- as.data.frame(tblcomb)
  tblcomb <- tblcomb[,c(1, 3, 2, 4, 6, 5)]
  names(tblcomb)[2] <- paste("percent")
  names(tblcomb)[4] <- paste("percent")
  
  write.csv("percent of posts by focus with some stats")
  write.csv(tblcomb, row.names=TRUE)
  write.csv(blank)
  
  tbl103 <- table(x$focusgrouped, x$others_link)
  tbl103pct <- rowPercents(tbl103, digits=1)
  tblcomb <- cbind(tbl103, tbl103pct)
  
  tblcomb <- as.data.frame(tblcomb)
  tblcomb <- tblcomb[,c(1, 4, 2, 5, 3, 6)]
  names(tblcomb)[2] <- paste("percent")
  names(tblcomb)[4] <- paste("percent")
  names(tblcomb)[6] <- paste("percent")
  
  write.csv("percent of posts by focus that link to other organizations")
  write.csv(tblcomb, row.names=TRUE)
  write.csv(blank)
  
  ##by storyline
  
  tbl100 <- table(x$storylinegrouped)
  tbl100 <- cbind(tbl100, prop.table(tbl100)*100)
  
  write.csv("storyline")
  write.csv(tbl100, row.names=TRUE)
  write.csv(blank)
  
  tbl101 <- group_by(x, storylinegrouped) %>% summarise(mean=mean(total_interactions))
  write.csv("avg number of total interactions per post (storyline)")
  write.csv(tbl101, row.names=TRUE)
  write.csv(blank)
  
  tbl103 <- table(x$storylinegrouped, x$linktoresearch)
  tbl103pct <- rowPercents(tbl103, digits=1)
  tblcomb <- cbind(tbl103, tbl103pct)
  
  tblcomb <- as.data.frame(tblcomb)
  tblcomb <- tblcomb[,c(1, 3, 2, 4, 6, 5)]
  names(tblcomb)[2] <- paste("percent")
  names(tblcomb)[4] <- paste("percent")
  
  write.csv("percent of posts by storyline with links to evidentiary research")
  write.csv(tblcomb, row.names=TRUE)
  write.csv(blank)
  
  tbl103 <- table(x$storylinegrouped, x$linktonews)
  tbl103pct <- rowPercents(tbl103, digits=1)
  tblcomb <- cbind(tbl103, tbl103pct)
  
  tblcomb <- as.data.frame(tblcomb)
  tblcomb <- tblcomb[,c(1, 3, 2, 4, 6, 5)]
  names(tblcomb)[2] <- paste("percent")
  names(tblcomb)[4] <- paste("percent")
  
  write.csv("percent of posts by storyline with links to news")
  write.csv(tblcomb, row.names=TRUE)
  write.csv(blank)
  
  tbl103 <- table(x$storylinegrouped, x$stats)
  tbl103pct <- rowPercents(tbl103, digits=1)
  tblcomb <- cbind(tbl103, tbl103pct)
  
  tblcomb <- as.data.frame(tblcomb)
  tblcomb <- tblcomb[,c(1, 3, 2, 4, 6, 5)]
  names(tblcomb)[2] <- paste("percent")
  names(tblcomb)[4] <- paste("percent")
  
  write.csv("percent of posts by storyline with some stats")
  write.csv(tblcomb, row.names=TRUE)
  write.csv(blank)
  
  tbl103 <- table(x$storylinegrouped, x$others_link)
  tbl103pct <- rowPercents(tbl103, digits=1)
  tblcomb <- cbind(tbl103, tbl103pct)
  
  tblcomb <- as.data.frame(tblcomb)
  tblcomb <- tblcomb[,c(1, 4, 2, 5, 3, 6)]
  names(tblcomb)[2] <- paste("percent")
  names(tblcomb)[4] <- paste("percent")
  names(tblcomb)[6] <- paste("percent")
  
  write.csv("percent of posts by storyline that link to other organizations")
  write.csv(tblcomb, row.names=TRUE)
  write.csv(blank)
  
  ##top other organizations linked to
  
  tbl110 <- table(x$producerofcontent)
  tbl110 <- sort(tbl110, decreasing = TRUE)
  tbl110 <- head(tbl110, n = 10)
  
  write.csv("top 10 organizations link to")
  write.csv(tbl110, row.names=TRUE)
  write.csv(blank)

  n <- n + 1
}

print("done")
sink()



##getting most popular individual posts

jj <- arrange(all, desc(total_interactions))
topFBprimary <- filter(jj, pagetype == "FB-primary")
topFBprimary <- head(topFBprimary, 100)
write.csv(topFBprimary, file = "topposts-FBP.csv")



topMPprimary <- filter(jj, pagetype == "multi-platform")
topMPprimary <- head(topMPprimary, 100)
write.csv(topMPprimary, file = "topposts-MP.csv")


