#----- SPOTIFY CREDENTIAL FOR SPOTIFYR -----#

if (Sys.getenv('SPOTIFY_CLIENT_ID') == '')
  Sys.setenv(SPOTIFY_CLIENT_ID = '...')
if (Sys.getenv('SPOTIFY_CLIENT_SECRET') == '')
  Sys.setenv(SPOTIFY_CLIENT_SECRET = '...')