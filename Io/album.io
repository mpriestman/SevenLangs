Track := Object clone

Track title ::= ""
Track length ::= 0

Track init := method(title, mins, secs,
  self setTitle(title);
  self setLength((60 * mins) + secs);
  
  return(self);
  )
  
Track play := method("Playing #{self title}, #{length} seconds long" interpolate println)

Album := List clone

Album title ::= ""
Album artist ::= ""

Album init := method(title, artist,
  self setTitle(title)
  self setArtist(artist)
)

Album addTrack := method(track,
  self append(track)
)

Album play := method(
  "Playing #{self title} by #{self artist}" interpolate println;
  for(i, 0, self size - 1, "#{i + 1} - " interpolate print; at(i) play)
)

track1 := Track clone init("Leave Home", 5, 33)
track2 := Track clone init("In Dust We Trust", 5, 18)
track3 := Track clone init("Song To The Siren", 3, 17)

exitDust := Album clone init("Exit Planet Dust", "Chemical Brothers")

exitDust addTrack(track1)
exitDust addTrack(track2)
exitDust addTrack(track3)

exitDust play
