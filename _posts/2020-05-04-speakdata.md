---
layout: post
title: speakdata
---

Have you ever looked for a quick and easy way to say `84:a3:fe:67:c5:01`, or maybe `192.168.0.1`?

Look no further than [speakdata](https://github.com/tyehle/speakdata), a canonical way to translate bytes into easy pronounceable syllables!

```python
>>> import speakdata
>>>
>>> speakdata.pronounce(b'\x84\xa3\xfe\x67\xc5\x01')
'Lifufing torleyking sijabing.'
>>>
>>> speakdata.pronounce([192, 168, 0, 1])
'Saful bababing.'
```

Are you now wonding how to pronounce "Saful bababing?" Just pipe to `say`

```shell
$ say "Saful bababing"
```

See its easy to pronounce!

It can also create beautiful poetry:

```shell
$ curl https://tobin.yehle.us/favicon.ico -s | gzip | speakdata
Bortaning bubab taikaling naibowt babafing kaseyb kabaj jalajing.
Bijak baifailing nabubing sufaib fafaik safaib nababing sibaib.
Fajak bisak luboil beybal faibas jeynub beytaib bowkub.
Baitaij saisaib boitus borkaij jorjeyb baifaib najab nainowking.
Fabaib jabaib fitib teysab baiforting nisoifing torlab tainoifing.
Koilub towtaf bakoib beyteyt bileys taneyjing jorkuling tortowking.
Koibowt teynorting soifiling faiseyb borbowt kibaiking lownorling forkoifing.
Tuteybing kinab seyfaling jowkowt biforjing joibowting sorbafing sowsaifing.
Lunorsing lusoiking joikorning torteyling saisuking faifaik kiloiting boifafing.
Torfak susas lorsus fasas towkowj nownorting tafibing torjoil.
Soijab seykafing noifik leyforb saleyking forsas fowsait foilaif.
Faikiking jeyforning jukaling kusors towlifing kusut forlorjing lujoijing.
Lorlorb taituning fituk boisoin teysoiking toibowling buniking nowtas.
Laikuf nutorsing faforning kaborf seyfeyj lukoijing fortuking fukaf.
Foiforning feyteyk lowfufing seyfoiting towtorl bownois towloining finun.
Kanowl boijain tutub norbabing jaisafing fulorting lufoijing tortowj.
Nijufing kaforbing nitaiting bortaifing sowkaj bowbown saneyjing jakork.
Fakowning norsorb towjais kaibowt bitailing lakowl seyforf fuborn.
Lornaiking kijoiting nailorl jorlorning bifows boikowfing lownoiking nakeyl.
Bitijing feytowl sulik kowforning jajors koiborbing lataining korlowl.
Neylaking seyjaning julaking tubors kowborfing bufowling boitorfing forsus.
Korforb seyjating failiting tilowting jaiforn siteyking joinorj nowfajing.
Lornaik jaforbing norsorsing soisuk laineyf borsaij fataking forjais.
Foikais joikoib kowbeyl borluling lajoifing torluling jukorting sufeyling.
Boilowling foijul toinibing saisubing faikeyling lortowf tailaking bortuk.
Jitorling jisuning teytibing susijing koifowl foijaising kutowl seylaiting.
Junorsing loisating baisowning torsowfing sainort sakak jujeysing tortab.
Faifoiling toijoiting kaneysing sitowf tornoining lafut loitit toijowting.
Noijab koilafing borjis toijorb jeyleyfing jaifowting batowj bisaf.
Neyjaibing feytib kisafing borkeysing korlab tunaking boijeyb kowsorfing.
Bowfas baloib babab lowkeyjing nitifing forlibing babab.
```


---

## How does it work?

The system uses a one-to-one mapping with 8 consonants and 8 vowels to build words. Each two byte chunk of the input forms a single word with three consonants and two vowels in the output. These words are further grouped into sentences of 8 words.

But the sequence of vowels and consonants only encodes 15 bits of data! The last bit is encoded with the optional suffix "--ing". If the last bit of the chunk is a 1 then the word ends with --ing, otherwise it is left as is.

```
 c    v    c    v    c  ing?
...  ...  ...  ...  ...  .
```

The list of consonants and vowels were carefully chosen with help from [Gavin Yehle](https://gavinyehle.com) to be far away from each other (eg: only one of f/v and s/z are present). The vowels are:
```python
["a", "i", "u", "oi", "ai", "ow", "ey", "or"]
```
and the consonants are
```python
["b", "f", "j", "k", "l", "n", "s", "t"]
```

If there are an odd number of bytes in the input then the last 16 bit chunk is padded with a zero byte.

### Example


To convert `0x60ff`:

Start with the bits.
<br>`0110 0000 1111 1111`

Break into 3 bit chunks.
<br>`011 000 001 111 111 1`

Look up sylables.
<br>`"k" "a" "f" "or" "t" 1`

Set the ending.
<br>`kaforting`

Cavort with joy!
