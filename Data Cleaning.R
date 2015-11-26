#Load data
dat <- read.csv("Goal Scoring Times - Goals Scored.csv", header=T)

#replace_all
#Replaces all instances of a string (a) in a vector (x) with a replacement string (b)
#Output: a vector with the replaced values
replace_all <- function(x,a,b)
{
	for(i in 1:length(x))
	{
		if(x[i]==a)
			x[i] <- b
	}
	return(x)
}

#Clean the extra levels in Home
levels(dat$Home)

dat$Home <- replace_all(dat$Home, "Arsenal ", "Arsenal")
dat$Home <- replace_all(dat$Home, "Aston Villa ", "Aston Villa")
dat$Home <- replace_all(dat$Home, "West Brom ", "West Brom")

#Clean the extra levels in Away
levels(dat$Away)

dat$Away	<- replace_all(dat$Home, "Arsenal ", "Arsenal")
dat$Away	<- replace_all(dat$Home, "Livepool", "Liverpool")
dat$Away	<- replace_all(dat$Home, "Sundeland", "Sunderland")

#Clean the extra levels in Team.Scored
levels(dat$Team.Scored)

dat$Team.Scored <- replace_all(dat$Team.Scored, "Arsenal ", "Arsenal")
dat$Team.Scored <- replace_all(dat$Team.Scored, "Everton ", "Everton")
dat$Team.Scored <- replace_all(dat$Team.Scored, "Mancheste United", "Manchester United")
dat$Team.Scored <- replace_all(dat$Team.Scored, "West Brom ", "West Brom")

#Clean the extra levels in Player
levels(dat$Player)

dat$Player <- replace_all(dat$Player, "Â Valencia E." ,"Valencia E.")
dat$Player <- replace_all(dat$Player, "Campbell" ,"Campbell F.")
dat$Player <- replace_all(dat$Player, "Koscienly L." ,"Koscielny L.")
dat$Player <- replace_all(dat$Player, "Lukako R." ,"Lukaku R.")
dat$Player <- replace_all(dat$Player, "Mcgeady A." ,"McGeady A.")
dat$Player <- replace_all(dat$Player, "Osman" ,"Osman L.")
dat$Player <- replace_all(dat$Player, "Routledtge W." ,"Routledge W.")
dat$Player <- replace_all(dat$Player, "Strerling R." ,"Sterling R.")
dat$Player <- replace_all(dat$Player, "Welbeck" ,"Welbeck D.")
dat$Player <- replace_all(dat$Player, "Zamora B. " ,"Zamora B.")

#Replace "NA" in Own.Goal with FALSE
for(i in length(dat$Own.Goal))
{
	if(is.na(dat$Own.Goal[i]))
		dat$Own.Goal <- FALSE
}

#Write clean table
write.table(dat, "Goal_Scoring_Times_Clean.csv", sep=",", row.names = FALSE)

#Check the clean table
dat1 <- read.table("Goal_Scoring_Times_Clean.csv", header=T)
levels(dat1$Home)
levels(dat1$Away)
levels(dat1$Team.Scored)
levels(dat1$Player)

