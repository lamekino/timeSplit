timeSplit
---
Takes a list of timestamps and an audio file and splits it into individual
songs. It is meant to be a companion tool to
[youtube-dl](https://youtube-dl.org/) to turn large, single file albums into
more music player friendly format and for archival purposes of obscure albums.

It depends on [ffmpeg](https://ffmpeg.org/) being installed and in the user's
``$PATH``.

### Installation
WIP

### Usage
For reading timestamps/titles from stdin:
```sh
$ timesplit <audio> <end timestamp>
```

For reading timestamps/titles from a file:
```sh
$ timesplit <audio> <end timestamp> <timestamps.txt>
```

### Roadmap
- [x] Timestamp and title parsing.
- [x] Read audio files.
- [x] Audio file creation (splitting).
    - [ ] Job creation.
- [ ] Command line arguments.
- [ ] Automatic tag creation.

### Limitations
- Since timestamps on YouTube are precise only to the second, clipping might
  occur when 2 songs aren't completely aligned to the second.
- Currently, it cannot find the total length of a song, so it needs to be
  provided by the user.
