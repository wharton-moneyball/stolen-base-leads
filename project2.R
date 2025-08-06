nbpdata <- read.csv("C:\\Users\\jackw\\projects\\Moneyball\\data\\2024netbasesprevented.csv")
ipdata <- read.csv("C:\\Users\\jackw\\projects\\Moneyball\\data\\2024inningspitched.csv")
ssdata <- read.csv("C:\\Users\\jackw\\projects\\Moneyball\\data\\2024sprintspeed.csv")
ptdata <- read.csv("C:\\Users\\jackw\\projects\\Moneyball\\data\\2024poptime.csv")


nbpdata <- nbpdata %>% 
  reframe(player_id = player_id, player_name = player_name, NBP = net_attr_plus + net_attr_minus)

ipdata <- ipdata %>% 
  left_join(x = ipdata, y = nbpdata, by = c("player_id" = "player_id"))

ipdata <- ipdata %>% 
  reframe(PitcherID = player_id, Pitcher = player_name, NBP = NBP, IP = round(p_formatted_ip, 1))

ipdata <- ipdata %>% 
  mutate(IP = round(IP, 0)) %>% 
  mutate(Threat = 100 * NBP / IP) %>% 
  filter(IP >= 10)

leadsnew <- leadsnew %>% 
  left_join(x = leadsnew, y = ipdata, by = c("PitcherID" = "PitcherID"))

ptdata <- ptdata %>% 
  reframe(Catcher = entity_name, CatcherID = entity_id, poptime = pop_2b_sba, popcount = pop_2b_sba_count)

leadsnew <- leadsnew %>% 
  left_join(x = leadsnew, y = ptdata, by = c("CatcherID" = "CatcherID"))

ssdata <- ssdata %>% 
  reframe(Runner1B_ID = player_id, sprint_speed = sprint_speed)

leadsnew <- leadsnew %>% 
  left_join(x = leadsnew, y = ssdata, by = c("Runner1B_ID" = "Runner1B_ID"))

#view(nbpdata)
#view(ipdata)
#view(ptdata)
#view(ssdata)