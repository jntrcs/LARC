go <- proc.time()
progresspredict.simulation <- function(teams, weeks=NULL, strengths=NULL, drawtype="set", divisions,
                gid, god=NULL, god.type="times", enddate=Sys.Date(), type="BT",simulations=1000,
                arg1=NULL, arg2=NULL, arg3=NULL) {
  if (length(teams) > 1) {
    N <- length(teams)
  } else {
    N <- teams
  }
  strengthsM <- rep(0, N)
  strengthsBT <- rep(0, N)
  wins <- rep(0, N)
  strongM <- matrix(0, N, simulations)
  strongBT <- matrix(0, N, simulations)
  varsM <- rep(0, N)
  varsBT <- rep(0, N)
  prcntM <- vector()
  prcntBT <- vector()
  if (length(teams) > 1 && length(strengths) > 1) {
    swtch <- TRUE
  } else {
    swtch <- FALSE
  }
  if (length(teams) > 1) {
    Team <- teams
    teamsC <- length(teams)
    if (is.null(weeks)) {
      weeks <- teamsC
    }
  } else {
    if (is.null(weeks)) {
      weeks <- teams
    }
    teamsC <- teams
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
    Strength <- seq(2,.001,length.out=teamsC)
  } else {
    Strength <- strengths
  }
  if (swtch == FALSE) {
    Strength <- rev(sort(Strength))
  }
  if (drawtype == "set") {
    strengths <- Strength
  }
  
  TST <- data.frame(Team,Strength)
  for (i in 1:simulations) {
    if (drawtype == "set") {
      df <- full.sim.gen(teams, weeks, strengths, divisions, gid, god, god.type, enddate, type)
    } else {
      if (is.null(arg1)) {
        df <- full.sim.gen(teams, weeks, (abs(drawtype(teamsC))+.001), divisions, gid, god,
                           god.type, enddate, type)
      } else {
        if (is.null(arg2)) {
          df <- full.sim.gen(teams, weeks, (abs(drawtype(teamsC,arg1))+.001), divisions, gid, 
                             god, god.type, enddate, type)
        } else {
          if (is.null(arg3)) {
            df <- full.sim.gen(teams, weeks, (abs(drawtype(teamsC,arg1,arg2))+.001), divisions, gid, 
                               god, god.type, enddate, type)
          } else {
            df <- full.sim.gen(teams, weeks, (abs(drawtype(teamsC,arg1,arg2,arg3))+.001), divisions,
                               gid, god, god.type, enddate, type)
          }
        }
      }
    }
    round <- dataconfigure(df)
    pp <- progresspredict(df)
    pt <- pp$ProgressionTable
    if (i == 1) {
      plot(pt$Week,pt$Mosteller,col=2,ylim=c(0,teams))
    } else {
      points(pt$Week,pt$Mosteller,pch=i,col=2)
    }
    points(pt$Week,pt$BradleyTerry,pch=i,col=4,cex=.75)
    
    samdf <- data.frame(Team=round$Team,WT=round$WinsTotal,MS=pp$FinalMStrenghs,BTS=pp$FinalBTStrenghs)
    
    prpM <- table(c(TST[order(-TST$Strength),]$Team,1,0)==c(samdf[order(-samdf$MS),]$Team,0,1))
    prpM <- prpM-1
    prcntM <- c(prcnt,prop.table(prpM)[2])
    prpBT <- table(c(TST[order(-TST$Strength),]$Team,1,0)==c(samdf[order(-samdf$BTS),]$Team,0,1))
    prpBT <- prpBT-1
    prcntBT <- c(prcnt,prop.table(prpBT)[2])
    
    strongM[,i] <- pp$FinalMStrenghs
    strongBT[,i] <- pp$FinalBTStrenghs
    strengthsM <- ((i - 1) * strengthsM + pp$FinalMStrenghs)/i
    strengthsBT <- ((i - 1) * strengthsBT + pp$FinalBTStrenghs)/i
    wins <- ((i - 1) * wins + round$WinsTotal)/i
  }
  varsM <- vector()
  varsBT <- vector()
  for (i in 1:N) {
    varsM[i] <- var(strongM[i,])
    varsBT[i] <- var(strongBT[i,])
  }
  biM <- strengthsM/mean(strengthsM) - TST$Strength/mean(TST$Strength)
  biBT <- strengthsBT/mean(strengthsBT) - TST$Strength/mean(TST$Strength)
  newdf <- data.frame(TST$Team, strengthsBT, strengthsM, wins)
  names(newdf) <- c("Team","BradleyTerrySimulatedStrength","MostellerSimulatedStrength",
                    "WinsSummary")
  
  return(list(SimulatedData=newdf,BradleyTerryStrengths=strongBT,MostellerStrengths=strongM,
              BradleyTerryVariance=varsBT,MostellerVariance=varsM,BradleyTerryBias=biBT,
              MostellerBias=biM,BradleyTerryPercentPerfect=prcntBT,MostellerPercentPerfect=prcntM))
}

progresspredict.simulation(teams, weeks=NULL, strengths=NULL, divisions, gid, god=NULL, 
                           god.type="times", enddate=Sys.Date(), type="BT",simulations=2)
clock <- proc.time() - go
clock
