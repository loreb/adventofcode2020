(* Skipped yesterday because real life got in the way... *)
(* polyc needs libpolyml-dev;
* can't use command line args from the interpreter?
* it interprets ALL args as  source files!
*)

(*
* """
* 939
* 7,13,x,x,59,x,31,19
* """
* means: every seven minutes, every 13, unavailable, unavailable, every 59, ...
* must depart as soon as possible but not before t=939
* => 944%59 = 0, so the 1st bus that passes at a time >= 939 is bus 59
*)

exception crap of string

fun println s = ( print s; print "\n" )
fun atoi s =
  case Int.fromString s of
       NONE => raise crap s
     | SOME n => n

fun printn n = print(Int.toString n)

fun readlines fd =
  case TextIO.inputLine fd of
       SOME line => line :: readlines(fd)
     | NONE      => []

(* "7,13,x,x,59,x,31,19\n" => [7,13,59,31,19] *)
fun parseIDs line =
let
  (* splitting uses "substrings", not plain "strings" (ie slices) *)
  (* XXX split is weird, so just use the "fields" or "tokens" fn *)
  fun extractIDs fields =
    case fields of
         [] => []
       | "x"::rest => extractIDs rest
       | id::rest => atoi(id) :: (extractIDs rest)
  fun iscomma ch = ch = #","
  val substrings = Substring.fields iscomma (Substring.full line)
in
  extractIDs(map Substring.string substrings)
end

fun firstDepartureAfter(period, minimum) =
  if minimum mod period = 0
  then minimum
  else period + period * (minimum div period)

val _ = if firstDepartureAfter(59, 939) = 944
        then 42
        else raise crap "firstDepartureAfter"

fun printDeparture(departure, id) =
  ( print "departure="; printn departure; print " id="; printn id; println "" )

fun process1(departure, ids) =
  (* map println (map Int.toString ids) *)
  let
    fun earlier((dep1,id1), (dep2,id2)) =
      if dep1 < dep2
      then (dep1,id1)
      else (dep2,id2)
    val id1 = hd(ids)
    val dep1 = firstDepartureAfter(id1, departure)
    (*val alldeps = (map (fn id => (firstDepartureAfter(id, departure), id)),
    * ids)*)
    (* ask me what I think of diagnostics in SML... *)
    val alldeps = (map (fn id => (firstDepartureAfter(id, departure), id)) ids)
    val init = (dep1,id1)
    val first = foldl earlier init alldeps
  in
    printDeparture(first)
  end


fun part1 filename =
  let
    val qwerty = println("file=" ^ filename)
    val input = TextIO.openIn(filename)
    val lines = readlines(input)
    val firstline = hd(lines)
    val secondline = hd(tl(lines))
  in
    (
    TextIO.closeIn(input) ;
    process1(atoi(firstline), parseIDs(secondline))
    )
  end


fun doit args =
  case args of
       [] => 42
     | arg::rest => ( part1(arg); doit rest )
fun main() = ( doit(CommandLine.arguments()); print "OK\n" ;
  OS.Process.exit(OS.Process.success) )

val _ = main()
