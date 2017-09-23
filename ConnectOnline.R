library(rsconnect)
rsconnect::setAccountInfo(name='asashinyapps',
                          token='46FBBDA6673FC6E11B42172A4E39534C',
                          secret='5pG2TDGgzjNdcXKVR1PHXdbuorjIvLgmd3Ub65G+')
rsconnect::deployApp(getwd(), account = 'asashinyapps')


