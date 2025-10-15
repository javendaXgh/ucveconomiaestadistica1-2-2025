
library(httr)
myapp <- oauth_app("github",
                   key = "",
                   secret = "")
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

gtoken <- config("2a54f5def213207ed2eeff777692755c492708b1")
req1 <- GET("https://api.github.com/users/jtleek/repos", gtoken)
req <- GET("https://api.github.com/rate_limit", gtoken)

stop_for_status(req1)
content(req1)

library(jsonlite)
jsondata<- fromJSON("https://api.github.com/users/jtleek/repos", simplifyDataFrame = T)
jsondata
class (jsondata)
jsondata$name
names(jsondata)

names(jsondata$owner)
jsondata$owner$login

#escribir data frame como JSON
mi_json <- toJSON(iris, pretty = TRUE)
cat(mi_json)

#regresar de json a data frame
iris2 <- fromJSON(mi_json)
