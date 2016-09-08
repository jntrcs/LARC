teams <- 5
games <- 25

VS.matrix <- function(teams, games) {
  mm <- matrix(0,teams,teams)
  nogo <- vector()
  tt <- 1:teams^2
  for (i in 1:teams) {
    nogo <- c(nogo,teams*(i-1)+i)
  }
  tt <- tt[-which(tt==nogo)]
  for (i in 1:games) {
    sam <- sample(tt,1)
    mm[sam] <- mm[sam] + 1
  }
  vs <- mm+t(mm)
  return(vs)
}

VS.matrix(5,10)

sim.gen <- function(teams, games, strengths=NULL) {
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
              month.abb[ifelse(teams>26,1,0):ifelse(teams>52,min(teams-52,12),0)],
              month.name[ifelse(teams>26,1,0):ifelse(teams>64,min(teams-64,12),0)],
              pkmn[ifelse(teams>26,1,0):ifelse(teams>76,min(teams-76,722),0)])
  }
  if (is.null(strengths)) {
    Strength <- seq(2,.001,length.out=teams)
  } else {
    Strength <- strengths
  }
  Versus <- VS.matrix(teams, games)
  spit  <- data.frame(Team,Strength)
  spit$Versus <- Versus
  spit$Team <- as.character(spit$Team)
  return(spit)
}

go <- proc.time()
sim.10.20 <- sim.gen(10, 20)

res.10.20bt <- simulation(sim.10.20,compares=TRUE)
res.10.20bt$SimulatedData
for (i in 1:10) { hist(res.10.20bt$AllStrengths[i,], main=sim.10.20$Team[i]) }
res.10.20bt$Variance
res.10.20bt$Bias
res.10.20bt$Proportion
res.10.20bt$Comparison

res.10.20m <- simulation(sim.10.20,type="M",compares=TRUE)
res.10.20m$SimulatedData
for (i in 1:10) { hist(res.10.20m$AllStrengths[i,], main=sim.10.20$Team[i]) }
res.10.20m$Variance
res.10.20m$Bias
res.10.20m$Proportion
res.10.20m$Comparison
clock.10.20 <- proc.time() - go
clock.10.20

go <- proc.time()
sim.5.10 <- sim.gen(5, 10)

res.5.10bt <- simulation(sim.5.10,compares=TRUE)
res.5.10bt$SimulatedData
for (i in 1:5) { hist(res.5.10bt$AllStrengths[i,], main=sim.5.10$Team[i]) }
res.5.10bt$Variance
res.5.10bt$Bias
res.5.10bt$Proportion
res.5.10bt$Comparison

res.5.10m <- simulation(sim.5.10,type="M",compares=TRUE)
res.5.10m$SimulatedData
for (i in 1:5) { hist(res.5.10m$AllStrengths[i,], main=sim.5.10$Team[i]) }
res.5.10m$Variance
res.5.10m$Bias
res.5.10m$Proportion
res.5.10m$Comparison
clock.5.10 <- proc.time() - go
clock.5.10

table(res.5.10bt$PercentPerfect)
table(res.5.10m$PercentPerfect)

go <- proc.time()
sim.5.20 <- sim.gen(5, 20)

res.5.20bt <- simulation(sim.5.20,compares=TRUE)
res.5.20bt$SimulatedData
for (i in 1:5) { hist(res.5.20bt$AllStrengths[i,], main=sim.5.20$Team[i]) }
res.5.20bt$Variance
res.5.20bt$Bias
res.5.20bt$Proportion
res.5.20bt$Comparison

res.5.20m <- simulation(sim.5.20,type="M",compares=TRUE)
res.5.20m$SimulatedData
for (i in 1:5) { hist(res.5.20m$AllStrengths[i,], main=sim.5.20$Team[i]) }
res.5.20m$Variance
res.5.20m$Bias
res.5.20m$Proportion
res.5.20m$Comparison
clock.5.20 <- proc.time() - go
clock.5.20

go <- proc.time()
sim.5.1200 <- sim.gen(5, 1200)

res.5.1200bt <- simulation(sim.5.1200,compares=TRUE)
res.5.1200bt$SimulatedData
for (i in 1:5) { hist(res.5.1200bt$AllStrengths[i,], main=sim.5.1200$Team[i]) }
res.5.1200bt$Variance
res.5.1200bt$Bias
res.5.1200bt$Proportion
res.5.1200bt$Comparison

res.5.1200m <- simulation(sim.5.1200,type="M",compares=TRUE)
res.5.1200m$SimulatedData
for (i in 1:5) { hist(res.5.1200m$AllStrengths[i,], main=sim.5.1200$Team[i]) }
res.5.1200m$Variance
res.5.1200m$Bias
res.5.1200m$Proportion
res.5.1200m$Comparison
clock.5.1200 <- proc.time() - go
clock.5.1200

go <- proc.time()
sim.50.1200 <- sim.gen(50, 1200)

res.50.1200bt <- simulation(sim.50.1200,compares=TRUE)
res.50.1200bt$SimulatedData
for (i in 1:50) { hist(res.50.1200bt$AllStrengths[i,], main=sim.50.1200$Team[i]) }
res.50.1200bt$Variance
res.50.1200bt$Bias
res.50.1200bt$Proportion
res.50.1200bt$Comparison

res.50.1200m <- simulation(sim.50.1200,type="M",compares=TRUE)
res.50.1200m$SimulatedData
for (i in 1:50) { hist(res.50.1200m$AllStrengths[i,], main=sim.50.1200$Team[i]) }
res.50.1200m$Variance
res.50.1200m$Bias
res.50.1200m$Proportion
res.50.1200m$Comparison
clock.50.1200 <- proc.time() - go
clock.50.1200

go <- proc.time()
sim.100.1200 <- sim.gen(100, 1200)

res.100.1200bt <- simulation(sim.100.1200,compares=TRUE)
res.100.1200bt$SimulatedData
for (i in 1:100) { hist(res.100.1200bt$AllStrengths[i,], main=sim.100.1200$Team[i]) }
res.100.1200bt$Variance
res.100.1200bt$Bias
res.100.1200bt$Proportion
res.100.1200bt$Comparison

res.100.1200m <- simulation(sim.100.1200,type="M",compares=TRUE)
res.100.1200m$SimulatedData
for (i in 1:100) { hist(res.100.1200m$AllStrengths[i,], main=sim.100.1200$Team[i]) }
res.100.1200m$Variance
res.100.1200m$Bias
res.100.1200m$Proportion
res.100.1200m$Comparison
clock.100.1200 <- proc.time() - go
clock.100.1200

sim.gen(c("A","G","T","C"), 10)

sim.gen(c("A","G","T","C"), 10, c(2,1,.5,1.5))

big.sim <- sim.gen(100, 1000)
