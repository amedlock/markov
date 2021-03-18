import strutils, tables, random
import db_sqlite, os
import encodings

const MaxWords = 15;
const MaxWidth = 80;

type
  Follow = ref object
    word: string
    freq : int

  Phrase = ref object
    total : int
    prefix,suffix : bool
    items: OrderedTable[string,Follow]

  Markov* = ref object
    size: int
    phrases : Table[string,Phrase]
    head: seq[string]
    tail : seq[string] 
    fragment: seq[string] # used for generation and learning


proc isfirstWord( s : string) : bool = 
  const Leading = {'A'..'Z', '"'}
  result = not(s.len==0) and (s[0] in Leading)
  
proc islastWord( s : string ) : bool = 
  const Punct : set[char]= { '.', '!', '?', ';' }
  result = not(s.len==0) and (s[^1] in Punct)


# Data model methods ----------------------

proc newMarkov*( sz: int ): Markov =
  new(result)
  assert( sz > 1 )
  result.size = sz
  

proc addPhrase*( markov:Markov, word:string ) : Phrase =
  let ws = word.strip()
  if markov.phrases.hasKey(ws):
    return markov.phrases[ws]
  new(result)
  result.total = 0
  markov.phrases[ws] = result
  if ws.isFirstWord:
    markov.head.add( ws )
    result.prefix = true
  if ws.isLastWord:
    markov.tail.add( ws )
    result.suffix = true

proc addWord*( phrase:Phrase, word: string, freq: int = 1 ) =
  var follow: Follow
  if phrase.items.contains(word):
    follow = phrase.items[word]
    follow.freq += freq
  else:
    new(follow)
    follow.word = word
    follow.freq = freq
    phrase.items[word] = follow
  phrase.total += freq


#  Persistence to sqlite ----------------

proc load*( markov: Markov, fname: string ) =
  if not fileExists(fname):
    echo("Data file $1 was not found!  Skipping")
    return
  var con = db_sqlite.open(fname, "","","")
  for row in con.fastRows(sql("SELECT id, txt, prefix, suffix FROM phrase ORDER BY id ASC")):
    let id = row[0].parseInt
    var phrase = markov.addPhrase( row[1] )
    phrase.prefix = (row[2].parseInt() != 0)
    phrase.suffix = (row[3].parseInt() != 0)
    for row2 in con.fastRows(sql("SELECT word, freq FROM follow WHERE phrase_id=? ORDER BY id ASC"), id):
      let word = row2[0]
      let freq = row2[1].parseInt
      phrase.addWord( word, freq )
  con.close()

proc init( con : DbConn ) =  
  con.exec( sql("DROP TABLE IF EXISTS phrase ;"))
  con.exec( sql("DROP INDEX IF EXISTS phrase_idx ;"))
  con.exec( sql("DROP TABLE IF EXISTS follow ;"))
  con.exec( sql("DROP INDEX IF EXISTS follow_idx;"))
  con.exec( sql("CREATE TABLE phrase( id INTEGER PRIMARY KEY ASC, txt  TEXT, prefix INTEGER, suffix INTEGER );"))
  con.exec( sql("CREATE INDEX phrase_idx on phrase( txt );") )
  con.exec( sql("CREATE TABLE follow( id INTEGER PRIMARY KEY, phrase_id INTEGER NOT NULL, word TEXT not null, freq INTEGER default(1) );") )
  con.exec( sql("CREATE INDEX follow_idx on follow( word );") )  

proc save*( markov:Markov, fname : string ) =
  let con = db_sqlite.open(fname, "","", "")
  con.init() # blow it away
  con.exec(sql("BEGIN TRANSACTION;"))
  for word, phrase in markov.phrases:
    let prefix = (if phrase.prefix: 1 else: 0)
    let suffix = (if phrase.suffix: 1 else: 0)
    let pid = con.insertID(sql("INSERT INTO phrase(txt, prefix, suffix) VALUES (?,?,?)"), word, prefix, suffix )
    for word, follow in phrase.items:
      con.exec(sql("INSERT INTO follow(phrase_id,word,freq) VALUES(?,?,?)"), pid,word, follow.freq )
  con.exec(sql("COMMIT;"))
  con.close()


#  Loading a corpus ----------------


# shift a word onto the markov generator
proc shift( markov:Markov, word : string ) =
  markov.fragment.add( word )
  let SZ = markov.size
  if len(markov.fragment) > SZ:
    let key = markov.fragment[0..SZ-1].join(" ")
    let val = markov.fragment[SZ]
    let phrase = markov.addPhrase( key ) 
    phrase.addWord( val )
    for n in 0..high(markov.fragment)-1:
      markov.fragment[n] = markov.fragment[n+1]
    markov.fragment.setLen( SZ )


iterator allWords( s : string ) : string =
  for word in s.split(Whitespace):
    yield word


proc toUtf( s : string ): string = 
  if getCurrentEncoding()!="UTF-8":
    return convert( s, getCurrentEncoding(), "UTF-8" )
  else:
    return s


proc learn*( markov: Markov, text : string ) =
  for line in text.splitLines:
    for word in line.allWords:
      if not (word.len==0):
        markov.shift( word )


proc learnFile*( markov:Markov, name: string ) =
  if not fileExists(name):
    echo("Text file $1 was not found, skipping")
  else:
    markov.learn( readFile( name ).toUtf )


proc find_next( markov:Markov, start : string ) : string = 
  if not markov.phrases.contains( start ):
    raise newException(Exception,"Could not find phrase ($1) in markov".format(start))
  let ph = markov.phrases[start]
  var choice = rand( ph.total )
  for word, follow  in ph.items:
    choice -= follow.freq
    if choice<=0:
      return word
  return ""

proc sentence( markov: Markov ) : seq[string] =
  var phrase = sample( markov.head )
  result = @[ phrase ]
  var count= markov.size
  while true:
    var follow = markov.find_next(phrase)
    result.add( follow )
    if follow.isLastWord:
      break
    inc(count)
    if count >= MaxWords or follow=="":
      result.add( sample(markov.tail) )
      break
    let rest = phrase.split(WhiteSpace,1)[1]
    phrase = [rest, follow].join(" ").strip()


proc generate*( markov:Markov, count : int = 500 ) : string =
  if len(markov.phrases)==0:
    raise newException(Exception, "No phrases in database")
  randomize()
  result = ""
  var width = 0
  for x in 0..count:
    for w in markov.sentence:
      width += len(w)
      if width >= MaxWidth:
        result &= "\n"
        width = 0
      else:
        result &= w & " "


when defined(MarkovCmd):
  import parseopt, system;

  type 
    Cfg = object
      files: seq[string]
      order: int
      load: string
      save: string
      count: int 
    
  proc usage() =
    echo("\n********* Usage: *********\n")
    echo("   markov --order 3 --count 500 --read <corpus> --load myfile.db --save myfile.db")
    echo("Alternate syntax:")
    echo("   markov -o 3 -c 500 -r <corpus> -l myfile.db -s mfile.db")
    echo("** Note that the save option --save will clear target file if it exists! **\n")


  proc doMarkov(cfg: Cfg) =
    var m = newMarkov( cfg.order )
    if cfg.load!="":
      m.load( cfg.load )
    for f in cfg.files:
      m.learnFile( f )
    echo( m.generate( cfg.count ) )
    if cfg.save!="":
      m.save( cfg.save )

  if defined(MarkovCmd):
    if paramCount()==0:
      usage()
      quit(QuitSuccess)

    var cfg = Cfg(files: @[], order:2, count:500)
    var opts = initOptParser()
    var cmd : string = ""
    for kind, key, val in opts.getopt():
      case kind
      of cmdArgument:
        if cmd=="count": 
          cfg.count = key.parseInt
        elif cmd=="order": 
          cfg.order = key.parseInt
        elif cmd=="read":
          cfg.files.add( key )
        elif cmd=="load": 
          assert( cfg.load=="" )
          cfg.load = key
        elif cmd=="save":
          assert( cfg.save=="" ) 
          cfg.save = key;
        else: 
          echo("Unknown command: $1".format(cmd))
          usage()
          quit(QuitFailure)
      of cmdLongOption, cmdShortOption:
        case key 
        of "order", "o": cmd = "order"
        of "count", "c": cmd = "count"
        of "read", "r": cmd = "read"
        of "load", "l": cmd = "load"
        of "save", "s": cmd = "save"
      of cmdEnd:
        discard
    doMarkov( cfg )



