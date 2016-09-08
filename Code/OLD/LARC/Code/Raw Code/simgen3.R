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

full.sim.gen <- function(teams, weeks, strengths=NULL, divisions, gid, god=NULL, 
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
  if (length(teams) > 1) {
    Team <- teams
    teams <- length(teams)
  } else {
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
  
  TST <- data.frame(Team,Strength)
  divs <- matrix(Team,divisions,teams/divisions)
  #GID
  tpd <- ncol(divs)
  gpw <- ((tpd-1)*divisions+odt*times)/weeks
  VM <- matrix(nrow=weeks,ncol=gpw*teams)
  #GOD or instead try to do the whole thing in one loop
  Home <- vector()
  Visitor <- vector()
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
          opponents <- c(opponents,sample(divs[-(1:j),],odt))
        }
      }
      order <- sample(opponents,(tpd-i)*divisions+(odt-tpd*(j-1))*times)
      for (o in gpw:1) {
        sched <- order[(length(order)-length(order)/gpw*(gpw-o+1)+1):(length(order)-length(order)/gpw*(gpw-o))]
        skips <- vector()
        rejects <- vector()
        if (nrow(VM)*gpw*(tpd*(j-1)+i-1) != 0) {
          for (m in 1:(nrow(VM)*gpw*(tpd*(j-1)+i-1))) {
            if (VM[m] == divs[j,i]) {
              skips <- c(skips, ifelse(m%%weeks == 0,weeks,m%%weeks))
            }
          }
          skip <- matrix(sort(skips)#[(length(skips)-length(skips)/gpw*(gpw-o+1)+1):(length(skips)-length(skips)/gpw*(gpw-o))]
                         ,gpw,length(skips)/gpw)
          x <- 1
          for (m in sort(skip[o,])) {
            fit <- length(sched)
            tst <- "YES"
            #x <- 1
            bgn <- 1
            while (tst == "YES") {
              tst <- "NO"
              fit <- length(sched)
              for (n in bgn:min(ifelse(m-1==0,1,m-1),fit)) {
                if (sched[n] != "NA") {
                  if(table(c(VM[n,],Team))[names(table(c(VM[n,],Team))) == sched[n]]-1 >= gpw) {
                    if (n-1 == 0) {
                      strt <- NULL
                    } else {
                      strt <- sched[1:(n-1)]
                    }
                    if (n >= fit) {
                      nd <- NULL
                    } else {
                      nd <- sched[n:(fit-1)]
                    }
                    sched <- c(strt,sched[fit],nd)
                    tst <- "YES"
                  } else {
                    safe <- n
                  }
                } else {
                  bgn <- n + 1
                }
              }
              x <- x + 1
              if (x >= fit) { #moij
                xst <- 1
                compat <- "NO"
                for (l in 1:safe) {
                  xst <- 2
                  if (sched[l] != "NA" && compat != "YES") {
                    xst <- 3
                    for (n in bgn:length(sched)) {
                      xst <- 4
                      if(table(c(VM[n,],Team))[names(table(c(VM[n,],Team))) == sched[l]]-1 < gpw &&
                         compat != "YES") {
                        xst <- 5
                        suc <- FALSE
                        for (p in bgn:length(sched)) {
                          if(table(c(VM[l,],Team))[names(table(c(VM[l,],Team))) == sched[p]]-1 < gpw &&
                             suc == FALSE && (sched[l] %in% rejects)==FALSE &&
                             names(table(sched[bgn:length(sched)] != sched[l]))[1]=="TRUE") {
                            nf <- sched[p]
                            nb <- sched[l]
                            sched[p] <- nb
                            sched[l] <- nf
                            rejects <- c(rejects,nf)
                            compat <- "YES"
                            suc <- TRUE
                            xst <- 6
                            x <- 1
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
            if (m-1 == 0) {
              strt <- NULL
            } else {
              strt <- sched[1:(m-1)]
            }
            if (m > fit) {
              nd <- NULL
            } else {
              nd <- sched[m:fit]
            }
            sched <- c(strt,"NA",nd)
            if (length(sched) == weeks) {
              tst <- "YES"
              while (tst == "YES") {
                tst <- "NO"
                for (w in min(m+1,fit+1):(fit+1)) {
                  if (sched[w] != "NA") {
                    if(table(c(VM[w,],Team))[names(table(c(VM[w,],Team))) == sched[w]]-1 >= gpw) {
                      if (w-1 == 0) {
                        strt <- NULL
                      } else {
                        strt <- sched[1:(w-1)]
                      }
                      if (w > fit) {
                        nd <- NULL
                      } else {
                        nd <- sched[w:(fit)]
                      }
                      sched <- c(strt,sched[fit+1],nd)
                      tst <- "YES"
                    }
                  }
                }
              }
            }
          }
        }
        if (o == 1) {
          for (m in j:divisions) {
            if (j == 1) {
              for (n in i:tpd) {
                if(table(c(VM[,(i+(j-1)*tpd)*gpw-1],sched,Team))[names(table(c(VM[,(i+(j-1)*tpd)*gpw-1],sched,Team))) == divs[m,n]] > 3) {
                  sched <- c(sched,sched)
                }
              }
            } else {
              for (n in 1:tpd) {
                if(table(c(VM[,(i+(j-1)*tpd)*gpw-1],sched,Team))[names(table(c(VM[,(i+(j-1)*tpd)*gpw-1],sched,Team))) == divs[m,n]] > 3) {
                  sched <- c(sched,sched)
                }
              }
            }
          }
        }
        VM[,(i+(j-1)*tpd)*gpw-o+1] <- sched
      }
      Home <- c(Home,rep(divs[j,i],length(order)))
      Visitor <- c(Visitor,order)
    }
  }
  # currently only works if they play each other at least twice
  #GOD
  for (j in 1:divisions) {
    for (n in 1:tpd) {
      for (i in (j+1):divisions) {
        if ((j+1) > divisions) {break}
        Home <- c(Home,rep(divs[j,n],tpd*god/2))
        Visitor <- c(Visitor,rep(divs[i,1:tpd],god/2))
        Visitor <- c(Visitor,rep(divs[j,n],tpd*god/2))
        Home <- c(Home,rep(divs[i,1:tpd],god/2))
      }
    }
  }
  # currently only works if they play each other at least twice
  HPTS <- vector()
  VPTS <- vector()
  for (i in 1:length(Home)) {
    if (type=="BT") {
      prb <- BTWP(TST$Strength[TST$Team==Home[i]],TST$Strength[TST$Team==Visitor[i]],FALSE)
      HPTS <- c(HPTS,sample(0:1,1,prob=c(prb,1-prb)))
      VPTS <- c(VPTS,ifelse(HPTS[i]==1,0,1))
    } else {
      prb <- MWP(TST$Strength[TST$Team==Home[i]],TST$Strength[TST$Team==Visitor[i]],FALSE)
      HPTS <- c(HPTS,sample(0:1,1,prob=c(prb,1-prb)))
      VPTS <- c(VPTS,ifelse(HPTS[i]==1,0,1))
    }
  }
  Date <- vector()
  L <- length(Home)/weeks
  for (i in (weeks-1):0) {
    Date <- c(Date,rep(enddate-i*7-2,L))
  }
  Date <- as.Date(Date,origin="1970-01-01")
  Day <- substr(weekdays(Date),1,3)
  `OT?` <- rep("NO",length(Home))
  Winner <- ifelse(VPTS > HPTS,Visitor,Home)
  Loser <- ifelse(VPTS < HPTS,Visitor,Home)
  df <- data.frame(Day,Visitor,VPTS,Home,HPTS,`OT?`,Winner,Loser)
  df <- df[sample(1:nrow(df)), ]
  df <- data.frame(Date,df)
  df$Home <- as.character(df$Home)
  df$Visitor <- as.character(df$Visitor)
  df$Winner <- as.character(df$Winner)
  df$Loser <- as.character(df$Loser)
  df$HPTS <- as.numeric(df$HPTS)
  df$VPTS <- as.numeric(df$VPTS)
  df$Day <- as.character(df$Day)
  return(df)
}

