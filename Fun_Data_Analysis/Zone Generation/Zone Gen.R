#Generating Post Code Zone for AusPost
df <- read.csv(file = "Zone2.csv", header = FALSE)

lpostcode = list()
lzone = list()
ldzone = list()
lstate = list()

for (n in 1:18)
{
  for (j in 4:nrow(df))
  {
    if (df[j,2*n] != 0)
    {
      if(length(lzone) == 0)
      {
        i = seq(df[j,(2*n-1)],df[j,(2*n)])
        lpostcode=i
        lzone=i
        ldzone = i
        lzone[1:length(i)]=df[1,(2*n-1)]
        ldzone[1:length(i)]=df[3,(2*n-1)]
        lstate[1:length(i)]=df[2,(2*n-1)]
      }
      else
      {
        i = seq(df[j,(2*n-1)],df[j,(2*n)])
        no.i = length(i)
        no.row = length(lzone)
        lzone[(no.row+1):(no.row+no.i)] = df[1,(2*n-1)]
        ldzone[(no.row+1):(no.row+no.i)] = df[3,(2*n-1)]
        lstate[(no.row+1):(no.row+no.i)]=df[2,(2*n-1)]
        lpostcode[(no.row+1):(no.row+no.i)]=i
      }
    }
    else
    {
      no.row = length(lzone)
      lzone[(no.row+1)] = df[1,(2*n-1)]
      ldzone[(no.row+1)] = df[3,(2*n-1)]
      lstate[(no.row+1)]=df[2,(2*n-1)]
      lpostcode[(no.row+1)] = df[j,(2*n-1)]
    }
  }
}

zone1 <- do.call(rbind, Map(data.frame,Zone=lzone,Post.Code=lpostcode, Destination.zone=ldzone,State = lstate))
zone1 <- zone1[zone1$Post.Code !=0,]
write.csv(zone1, file = "zone_done.csv", row.names = FALSE)
