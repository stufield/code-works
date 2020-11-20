######################################
### Calculation of Seed Germination
### From Keane et al. 1990 & Tomback 1982
###########################################
rm(list=ls(all=T))
#############################
### Calculate LAI
#############################
dbh = c(0, 0, 0, 2.05, 12.5, 37)              ### Mean dbh for stage classes
LA = 0.117*dbh^1.925; LA[3]= 0.456   ### Don't forget the seedlings!!!!!!!!
Sum.LA = sum(LA * c(0, 0, 1000, 72, 99, 250)) ### NUMBER OF INDIVIDUALS IN EACH CLASS FROM POPN.MAT VECTOR!!!
LAI = Sum.LA/10000

####################################
### For Animal Dispersal species
### TNS = total number of seedlings
####################################
N.seed = 1000                 # Total number of seeds produced by reproductive adults per year
P.find = 0.8                  # Proportion of seeds reclaimed by Nutcrackers following season 
SpC = c(0.8, 6.6, 3.7)        # Seeds per cache (HIGH, LOW, MEAN)
P.cons = c(0.1, 0.5, 0.3)     # Proportion of seeds eaten
r.cache = c(0.2, 0.96, 0.37)  # Cacheability via Nutcrackers
r.srf = c(0.05, 1, 0.11)      # Seedling reduction factor - cache probability with increasing distance from source
r.ALs = 1/(1+exp(2*(LAI-3)))    # shade tolerance factor, dependent on leaf area index for pines relative area on ground
          
### Total Number of Seedlings
TNS.a = N.seed*(((1-P.find)*(1-P.cons))/SpC) * r.cache * r.ALs # remove r.srf because not a spatial model????
##########################################
### Calculate Seedling Recruitment (SR)
##########################################
SR.a = TNS.a/N.seed; SR.a






####################################
### For Wind Dispersal species (Sheppard et al. 2006 PIPO)
### TNS = total number of seedlings
####################################
P.Cons = 0.48       # 48% consumed by animals
P.via = 0.42        # 42% viable (when > 200,000 seeds/ha)
r.cap = 0.66        # 66% germinative capacity

TNS.w = N.seed * (1-P.Cons) * (P.via) * (r.cap) * r.ALs
SR.w = TNS.w/N.seed; SR.w
