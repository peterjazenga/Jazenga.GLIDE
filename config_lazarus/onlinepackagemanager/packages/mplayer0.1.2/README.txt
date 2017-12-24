Package mplayercontrollaz.lpk

TMPlayerControl is a LCL control that embeds "mplayer" a movie player.

Requirements:
Only works under X/gtk2 and Windows.
mplayer must be installed and in the PATH.

Usage:
When you install the package you get a new component in the palette under
Multimedia.

See the example mplayer/examples/project1.lpi.

Hints:
TMPlayerControl only implemented a very small set of the mplayer features:
Play, Stop, Pause, Loop, Volume.
You can use SendMPlayerCommand to send arbitrary other commands. The complete
list of mplayer commands execute in terminal:
  mplayer -input cmdlist
and read http://www.mplayerhq.hu/DOCS/tech/slave.txt

