#How many teams are you playing out of division and how many times?

teams <- 16
weeks <- 11
divisions <- 2
strengths <- NULL
gid <- 2
god <- 1
god.type <- "times" # vs "teams"
enddate <- Sys.Date()
type <- "M"

full.sim.gen <- function(teams, weeks=NULL, strengths=NULL, divisions, gid, god=NULL, 
                         god.type="times", enddate=Sys.Date(), type="BT") {
  if (divisions > 1 && is.null(god)) {
    god <- 1
  }
  if (god.type != "times" & god.type != "teams") {
    god.type <- "times"
  }
  if (god.type == "times") {
    times <- god
    odt <- (teams/divisions)*(divisions-1)
  } else {
    times <- 1
    odt <- god
  }
  if (length(teams) > 1 && length(strengths) > 1) {
    swtch <- TRUE
  } else {
    swtch <- FALSE
  }
  if (length(teams) > 1) {
    Team <- teams
    teams <- length(teams)
    if (is.null(weeks)) {
      weeks <- teams
    }
  } else {
    if (is.null(weeks)) {
      weeks <- teams
    }
    pkmn <- sort(c("Bulbasaur", "Ivysaur", "Venusaur", "Charmander", "Charmeleon", "Charizard", "Squirtle", "Wartortle", "Blastoise", "Caterpie", "Metapod", "Butterfree", "Weedle", "Kakuna", "Beedrill", "Pidgey", "Pidgeotto", "Pidgeot", "Rattata", "Raticate", "Spearow", "Fearow", "Ekans", "Arbok", "Pichu", "Pikachu", "Raichu", "Sandshrew", "Sandslash", "NidoranM","NidoranF", "Nidorina", "Nidoqueen", "Nidorino", "Nidoking", "Cleffa", "Clefairy", "Clefable", "Vulpix", "Ninetales", "Igglybuff", "Jigglypuff", "Wigglytuff", "Zubat", "Golbat", "Crobat", "Oddish", "Gloom", "Vileplume", "Bellossom", "Paras", "Parasect", "Venonat", "Venomoth", "Diglett", "Dugtrio", "Meowth",
                   "Persian", "Psyduck", "Golduck", "Mankey", "Primeape", "Growlithe", "Arcanine", "Poliwag", "Poliwhirl", "Poliwrath", "Politoed", "Abra", "Kadabra", "Alakazam", "Machop", "Machoke", "Machamp", "Bellsprout", "Weepinbell", "Victreebel", "Tentacool", "Tentacruel", "Geodude", "Graveler", "Golem", "Ponyta", "Rapidash", "Slowpoke", "Slowbro", "Slowking", "Magnemite", "Magneton", "Magnezone", "Farfetchd", "Doduo", "Dodrio", "Seel", "Dewgong", "Grimer", "Muk", "Shellder", "Cloyster", "Gastly", "Haunter", "Gengar", "Onix", "Steelix", "Drowzee", "Hypno", "Krabby", "Kingler", "Voltorb", "Electrode", "Exeggcute", "Exeggutor", "Kangaskhan", "Cubone",
                   "Marowak", "Tyrogue", "Hitmonlee", "Hitmonchan", "Hitmontop", "Lickitung", "Lickilicky", "Koffing", "Weezing", "Rhyhorn", "Rhydon", "Rhyperior", "Happiny", "Chansey", "Blissey", "Tangela", "Tangrowth", "Horsea", "Seadra", "Kingdra", "Goldeen", "Seaking", "Staryu", "Starmie", "MimeJr", "MrMime", "Scyther", "Scizor", "Smoochum", "Jynx", "Elekid", "Electabuzz", "Electivire", "Magby", "Magmar", "Magmortar", "Pinsir", "Tauros", "Miltank", "Magikarp", "Gyarados", "Lapras", "Ditto", "Eevee", "Vaporeon", "Jolteon", "Flareon", "Espeon", "Umbreon", "Leafeon", "Glaceon", "Sylveon", "Porygon", "Porygon2", "PorygonZ", "Omanyte", "Omastar", "Kabuto",
                   "Kabutops", "Aerodactyl", "Munchlax", "Snorlax", "Articuno", "Zapdos", "Moltres", "Dratini", "Dragonair", "Dragonite", "Mewtwo", "Mew", "Chikorita", "Bayleef", "Meganium", "Cyndaquil", "Quilava", "Typhlosion", "Totodile", "Croconaw", "Feraligatr", "Sentret", "Furret", "Hoothoot", "Noctowl", "Ledyba", "Ledian", "Spinarak", "Ariados", "Chinchou", "Lanturn", "Togepi", "Togetic", "Togekiss", "Natu", "Xatu", "Mareep", "Flaaffy", "Ampharos", "Azurill", "Marill", "Azumarill", "Bonsly", "Sudowoodo", "Hoppip", "Skiploom", "Jumpluff", "Aipom", "Ambipom", "Sunkern", "Sunflora", "Yanma", "Yanmega", "Wooper", "Quagsire", "Murkrow", "Honchkrow",
                   "Misdreavus", "Mismagius", "Unown", "Wynaut", "Wobbuffet", "Girafarig", "Pineco", "Forretress", "Dunsparce", "Gligar", "Gliscor", "Snubbull", "Granbull", "Qwilfish", "Shuckle", "Heracross", "Sneasel", "Weavile", "Teddiursa", "Ursaring", "Slugma", "Magcargo", "Swinub", "Piloswine", "Mamoswine", "Corsola", "Remoraid", "Octillery", "Delibird", "Mantyke", "Mantine", "Skarmory", "Houndour", "Houndoom", "Phanpy", "Donphan", "Stantler", "Smeargle", "Raikou", "Entei", "Suicune", "Larvitar", "Pupitar", "Tyranitar", "Lugia", "HoOh", "Celebi", "Treecko", "Grovyle", "Sceptile", "Torchic", "Combusken", "Blaziken", "Mudkip", "Marshtomp", "Swampert",
                   "Poochyena", "Mightyena", "Zigzagoon", "Linoone", "Wurmple", "Silcoon", "Beautifly", "Cascoon", "Dustox", "Lotad", "Lombre", "Ludicolo", "Seedot", "Nuzleaf", "Shiftry", "Taillow", "Swellow", "Wingull", "Pelipper", "Ralts", "Kirlia", "Gardevoir", "Gallade", "Surskit", "Masquerain", "Shroomish", "Breloom", "Slakoth", "Vigoroth", "Slaking", "Nincada", "Ninjask", "Shedinja", "Whismur", "Loudred", "Exploud", "Makuhita", "Hariyama", "Nosepass", "Probopass", "Skitty", "Delcatty", "Sableye", "Mawile", "Aron", "Lairon", "Aggron", "Meditite", "Medicham", "Electrike", "Manectric", "Plusle", "Minun", "Volbeat", "Illumise", "Budew", "Roselia",
                   "Roserade", "Gulpin", "Swalot", "Carvanha", "Sharpedo", "Wailmer", "Wailord", "Numel", "Camerupt", "Torkoal", "Spoink", "Grumpig", "Spinda", "Trapinch", "Vibrava", "Flygon", "Cacnea", "Cacturne", "Swablu", "Altaria", "Zangoose", "Seviper", "Lunatone", "Solrock", "Barboach", "Whiscash", "Corphish", "Crawdaunt", "Baltoy", "Claydol", "Lileep", "Cradily", "Anorith", "Armaldo", "Feebas", "Milotic", "Castform", "Kecleon", "Shuppet", "Banette", "Duskull", "Dusclops", "Dusknoir", "Tropius", "Chingling", "Chimecho", "Absol", "Snorunt", "Glalie", "Froslass", "Spheal", "Sealeo", "Walrein", "Clamperl", "Huntail", "Gorebyss", "Relicanth", "Luvdisc",
                   "Bagon", "Shelgon", "Salamence", "Beldum", "Metang", "Metagross", "Regirock", "Regice", "Registeel", "Latias", "Latios", "Kyogre", "Groudon", "Rayquaza", "Jirachi", "Deoxys", "Turtwig", "Grotle", "Torterra", "Chimchar", "Monferno", "Infernape", "Piplup", "Prinplup", "Empoleon", "Starly", "Staravia", "Staraptor", "Bidoof", "Bibarel", "Kricketot", "Kricketune", "Shinx", "Luxio", "Luxray", "Cranidos", "Rampardos", "Shieldon", "Bastiodon", "Burmy", "Wormadam", "Mothim", "Combee", "Vespiquen", "Pachirisu", "Buizel", "Floatzel", "Cherubi", "Cherrim", "Shellos", "Gastrodon", "Drifloon", "Drifblim", "Buneary", "Lopunny", "Glameow", "Purugly",
                   "Stunky", "Skuntank", "Bronzor", "Bronzong", "Chatot", "Spiritomb", "Gible", "Gabite", "Garchomp", "Riolu", "Lucario", "Hippopotas", "Hippowdon", "Skorupi", "Drapion", "Croagunk", "Toxicroak", "Carnivine", "Finneon", "Lumineon", "Snover", "Abomasnow", "Rotom", "Uxie", "Mesprit", "Azelf", "Dialga", "Palkia", "Heatran", "Regigigas", "Giratina", "Cresselia", "Phione", "Manaphy", "Darkrai", "Shaymin", "Arceus", "Victini", "Snivy", "Servine", "Serperior", "Tepig", "Pignite", "Emboar", "Oshawott", "Dewott", "Samurott", "Patrat", "Watchog", "Lillipup", "Herdier", "Stoutland", "Purrloin", "Liepard", "Pansage", "Simisage", "Pansear", "Simisear",
                   "Panpour", "Simipour", "Munna", "Musharna", "Pidove", "Tranquill", "Unfezant", "Blitzle", "Zebstrika", "Roggenrola", "Boldore", "Gigalith", "Woobat", "Swoobat", "Drilbur", "Excadrill", "Audino", "Timburr", "Gurdurr", "Conkeldurr", "Tympole", "Palpitoad", "Seismitoad", "Throh", "Sawk", "Sewaddle", "Swadloon", "Leavanny", "Venipede", "Whirlipede", "Scolipede", "Cottonee", "Whimsicott", "Petilil", "Lilligant", "Basculin", "Sandile", "Krokorok", "Krookodile", "Darumaka", "Darmanitan", "Maractus", "Dwebble", "Crustle", "Scraggy", "Scrafty", "Sigilyph", "Yamask", "Cofagrigus", "Tirtouga", "Carracosta", "Archen", "Archeops", "Trubbish",
                   "Garbodor", "Zorua", "Zoroark", "Minccino", "Cinccino", "Gothita", "Gothorita", "Gothitelle", "Solosis", "Duosion", "Reuniclus", "Ducklett", "Swanna", "Vanillite", "Vanillish", "Vanilluxe", "Deerling", "Sawsbuck", "Emolga", "Karrablast", "Escavalier", "Foongus", "Amoonguss", "Frillish", "Jellicent", "Alomomola", "Joltik", "Galvantula", "Ferroseed", "Ferrothorn", "Klink", "Klang", "Klinklang", "Tynamo", "Eelektrik", "Eelektross", "Elgyem", "Beheeyem", "Litwick", "Lampent", "Chandelure", "Axew", "Fraxure", "Haxorus", "Cubchoo", "Beartic", "Cryogonal", "Shelmet", "Accelgor", "Stunfisk", "Mienfoo", "Mienshao", "Druddigon", "Golett", "Golurk",
                   "Pawniard", "Bisharp", "Bouffalant", "Rufflet", "Braviary", "Vullaby", "Mandibuzz", "Heatmor", "Durant", "Deino", "Zweilous", "Hydreigon", "Larvesta", "Volcarona", "Cobalion", "Terrakion", "Virizion", "Tornadus", "Thundurus", "Reshiram", "Zekrom", "Landorus", "Kyurem", "Keldeo", "Meloetta", "Genesect", "Chespin", "Quilladin", "Chesnaught", "Fennekin", "Braixen", "Delphox", "Froakie", "Frogadier", "Greninja", "Bunnelby", "Diggersby", "Fletchling", "Fletchinder", "Talonflame", "Scatterbug",
                   "Spewpa", "Vivillon", "Litleo", "Pyroar", "Flabébé", "Floette", "Florges", "Skiddo", "Gogoat", "Pancham", "Pangoro", "Furfrou", "Espurr", "Meowstic", "Honedge", "Doublade", "Aegislash", "Spritzee", "Aromatisse", "Swirlix", "Slurpuff", "Inkay", "Malamar", "Binacle", "Barbaracle", "Skrelp", "Dragalge", "Clauncher", "Clawitzer", "Helioptile", "Heliolisk", "Tyrunt", "Tyrantrum", "Amaura", "Aurorus", "Hawlucha", "Dedenne", "Carbink", "Goomy", "Sliggoo", "Goodra", "Klefki", "Phantump", "Trevenant", "Pumpkaboo", "Gourgeist", "Bergmite", "Avalugg", "Noibat", "Noivern", "Xerneas", "Yveltal", "Zygarde", "Diancie", "Hoopa", "Volcanion", "Magearna"))
    Team <- c(LETTERS[1:min(teams,26)],
              letters[ifelse(teams>26,1,0):ifelse(teams>26,min(teams-26,26),0)],
              month.abb[ifelse(teams>52,1,0):ifelse(teams>52,min(teams-52,12),0)],
              month.name[ifelse(teams>64,1,0):ifelse(teams>64,min(teams-64,12),0)],
              pkmn[ifelse(teams>76,1,0):ifelse(teams>76,min(teams-76,722),0)])
  }
  
  if (is.null(strengths)) {
    Strength <- seq(2,.001,length.out=teams)
  } else {
    Strength <- strengths
  }
  if (swtch == FALSE) {
    Strength <- rev(sort(Strength))
  }
  
  TST <- data.frame(Team,Strength)
  divs <- matrix(Team,divisions,teams/divisions)
  #GID
  tpd <- ncol(divs)
  gpw <- ((tpd-1)*divisions+odt*times)/weeks
  VM <- matrix(nrow=weeks,ncol=gpw*teams)
  #GOD or instead try to do the whole thing in one loop
  Home <- vector()
  Visitor <- vector()
  ODM <- vector()
  for (j in 1:divisions) {
    for (i in 1:tpd) {
      opponents <- vector()
      if (i != tpd) {
        for (l in 1:gid) {
          opponents <- c(opponents,divs[j,(i+1):tpd])
        }
      }
      if (j != divisions) {
        for (l in 1:times) {
          aodt <- ifelse(divs[j,i] %in% ODM,odt-table(ODM)[names(table(ODM))==divs[j,i]],odt)
          newO <- sample(divs[-(1:j),],aodt)
          opponents <- c(opponents,newO)
          if (god.type == "teams") {
            ODM <- c(ODM,newO,rep(divs[j,i],length(newO)))
          }
        }
      }
      order <- sample(opponents)#,(tpd-i)*divisions+(odt-tpd*(j-1))*times)
      
      Home <- c(Home,rep(divs[j,i],length(order)))
      Visitor <- c(Visitor,order)
    }
  }
  Dat <- vector()
  L <- length(Home)/weeks
  for (i in (weeks-1):0) {
    Dat <- c(Dat,enddate-i*7-2)
  }
  #Dat <- as.Date(Dat,origin="1970-01-01")
  Date <- c(rep(Dat,floor(L)),Dat[1:((L-floor(L))*weeks)])
  if (length(Home) > length(Date)) {
    Date <- c(Dat[(length(Dat)+1-abs(length(Home)-length(Date))):length(Dat)],Date)
  } else {
    if (length(Home) < length(Date)) {
      Date <- Date[1:(length(Date)-abs(length(Home)-length(Date)))]
    } 
  }
  HPTS <- vector()
  VPTS <- vector()
  for (i in 1:length(Home)) {
    if (type=="BT") {
      prb <- BTWP(TST$Strength[TST$Team==Home[i]],TST$Strength[TST$Team==Visitor[i]],FALSE)
      HPTS <- c(HPTS,sample(1:0,1,prob=c(prb,1-prb)))
      VPTS <- c(VPTS,ifelse(HPTS[i]==1,0,1))
    } else {
      prb <- MWP(TST$Strength[TST$Team==Home[i]],TST$Strength[TST$Team==Visitor[i]],FALSE)
      HPTS <- c(HPTS,sample(1:0,1,prob=c(prb,1-prb)))
      VPTS <- c(VPTS,ifelse(HPTS[i]==1,0,1))
    }
  }
  
  Winner <- ifelse(VPTS > HPTS,Visitor,Home)
  Loser <- ifelse(VPTS < HPTS,Visitor,Home)
  df <- data.frame(Date,Visitor,VPTS,Home,HPTS,Winner,Loser)
  #df <- df[sample(1:nrow(df)), ]
  #df <- data.frame(Date,df)
  df <- df[order(df$Date),]
  df$Date <- as.Date(df$Date,origin="1970-01-01")
  df$Day <- substr(weekdays(df$Date),1,3)
  `OT?` <- rep("NO",length(Home))
  df <- data.frame(Date=df$Date,Day=df$Day,Visitor=df$Visitor,VPTS=df$VPTS,Home=df$Home,HPTS=df$HPTS,`OT?`,Winner=df$Winner,Loser=df$Loser)
  df$Date <- as.Date(df$Date,origin="1970-01-01")
  df$Day <- as.character(df$Day)
  df$Home <- as.character(df$Home)
  df$Visitor <- as.character(df$Visitor)
  df$Winner <- as.character(df$Winner)
  df$Loser <- as.character(df$Loser)
  df$HPTS <- as.numeric(df$HPTS)
  df$VPTS <- as.numeric(df$VPTS)
  return(df)
}

yayish <- full.sim.gen(teams, weeks, strengths=NULL, divisions, gid, god=NULL, 
                       god.type="times", enddate=Sys.Date(), type="BT")
View(yayish)

go <- proc.time()
(pp <- progresspredict(yayish,mf=3))
pp$Comparison
clock <- proc.time() - go
clock

pt <- pp$ProgressionTable

plot(pt$Week,pt$Mosteller)
points(pt$Week,pt$BradleyTerry,pch=3)
table(pt$Model)

ppdt <- pp$DataTable
View(ppdt)

yayish <- full.sim.gen(teams, weeks, strengths=NULL, divisions, gid, god=NULL, 
                       god.type="times", enddate=Sys.Date(), type="M")
View(yayish)

go <- proc.time()
(pp <- progresspredict(yayish))
pp$Comparison
clock <- proc.time() - go
clock

pt <- pp$ProgressionTable

plot(pt$Week,pt$Mosteller)
points(pt$Week,pt$BradleyTerry,pch=3)
table(pt$Model)

ppdt <- pp$DataTable
View(ppdt)
