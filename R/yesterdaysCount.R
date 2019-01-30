
# Create daily window count
yesterdaysCount <- function(){
  
  source('./R/queryWindowCnts.R')
  
              # mutate(Chinook = Chinook + Jack_Chinook,
              #          Coho = Coho + Jack_Coho,
              #          Dam = case_when(
              #            Site == 'BON' ~ 'Bonneville',
              #            Site == 'TDA' ~ 'The Dalles',
              #            Site == 'JDA' ~ 'John Day',
              #            Site == 'MCN' ~ 'McNary',
              #            Site == 'IHR' ~ 'Ice Harbor',
              #            Site == 'LMN' ~ 'Lower Monumental',
              #            Site == 'LGS' ~ 'Little Goose',
              #            Site == 'LWG' ~ 'Lower Granite'
              #          )) %>% 
              #   select(Site, Dam, Date, Chinook, Coho, Steelhead, Wild_Steelhead, Lamprey)

# Bonneville
BON <- queryWindowCnts(dam = 'BON', spp_code = c('fc', 'fcj', 'fk', 'fkj', 'fs', 'fsw', 'fl'),
                       spawn_yr = 2018, #year(Sys.Date()), 
                       start_day = format(Sys.Date()-1, '%m/%d'), 
                       end_day = format(Sys.Date()-1, '%m/%d')) %>%
 select(-Year)#%>%
 # mutate(Site = 'BON')

# # The Dalles
# TDA <- queryWindowCnts(dam = 'TDA', spp_code = c('fc', 'fcj', 'fk', 'fkj', 'fs', 'fsw', 'fl'),
#                    spawn_yr = 2018, #year(Sys.Date()), 
#                    start_day = '01/17', #format(Sys.Date()-1, '%m/%d'), 
#                    end_day = '01/17') %>%#format(Sys.Date()-1, '%m/%d')) %>%
#   mutate(Site = 'TDA')
# 
# # John Day
# JDA <- queryWindowCnts(dam = 'JDA', spp_code = c('fc', 'fcj', 'fk', 'fkj', 'fs', 'fsw', 'fl'),
#                    spawn_yr = 2018, #year(Sys.Date()), 
#                    start_day = format(Sys.Date()-1, '%m/%d'), 
#                    end_day = format(Sys.Date()-1, '%m/%d')) %>%
#   mutate(Site = 'JDA')
# 
# # McNary
# MCN <- queryWindowCnts(dam = 'MCN', spp_code = c('fc', 'fcj', 'fk', 'fkj', 'fs', 'fsw', 'fl'),
#                    spawn_yr = 2018, #year(Sys.Date()), 
#                    start_day = format(Sys.Date()-1, '%m/%d'), 
#                    end_day = format(Sys.Date()-1, '%m/%d')) %>%
#   mutate(Site = 'MCN')
# 
# # Ice Harbor
# IHR <- queryWindowCnts(dam = 'IHR', spp_code = c('fc', 'fcj', 'fk', 'fkj', 'fs', 'fsw', 'fl'),
#                    spawn_yr = 2018, #year(Sys.Date()), 
#                    start_day = format(Sys.Date()-1, '%m/%d'), 
#                    end_day = format(Sys.Date()-1, '%m/%d')) %>%
#   mutate(Site = 'IHR')
# 
# # Lower Monumental
# LMN <- queryWindowCnts(dam = 'LMN', spp_code = c('fc', 'fcj', 'fk', 'fkj', 'fs', 'fsw', 'fl'),
#                    spawn_yr = 2018, #year(Sys.Date()), 
#                    start_day = format(Sys.Date()-1, '%m/%d'), 
#                    end_day = format(Sys.Date()-1, '%m/%d')) %>%
#   mutate(Site = 'LMN')
# 
# # Little Goose
# LGS <- queryWindowCnts(dam = 'LGS', spp_code = c('fc', 'fcj', 'fk', 'fkj', 'fs', 'fsw', 'fl'),
#                    spawn_yr = 2018, #year(Sys.Date()), 
#                    start_day = format(Sys.Date()-1, '%m/%d'), 
#                    end_day = format(Sys.Date()-1, '%m/%d')) %>%
#   mutate(Site = 'LGS')
# 
# # Lower Granite
# LWG <- queryWindowCnts(dam = 'LWG', spp_code = c('fc', 'fcj', 'fk', 'fkj', 'fs', 'fsw', 'fl'),
#                    spawn_yr = 2018, #year(Sys.Date()), 
#                    start_day = format(Sys.Date()-1, '%m/%d'), 
#                    end_day = format(Sys.Date()-1, '%m/%d')) %>%
#   mutate(Site = 'LWG')

return(dailycounts = list(BON, TDA, JDA, MCN, IHR, LMN, LGS, LWG))
}