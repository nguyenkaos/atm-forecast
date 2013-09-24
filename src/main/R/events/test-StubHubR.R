

# stubhub sandbox
#loginURL <- "https://api.stubhubsandbox.com/login"
#key = "bHNLF1gNLtpeReUTP9kLfZ9_TD4a"
#secret = "f06ftUVojbJzGyNTJPfdr6A0lh4a"

# stubhub production
key="B_a4P1xVGfQktWujHLRdrsfgeJoa"
secret="VxLRBSX6RRBX417rufxh0NBRBZAa"
username="nick@nickallen.org"
password="algore2000"

source("StubHubR.R")
stub <- StubHubR$new()
stub$login(key, secret, username, password)
stub$searchInventory(verbose=T)